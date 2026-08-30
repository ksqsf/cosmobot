{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.RPC.Configuration
Description : Authenticated configuration inspection and mutation RPC methods
Stability   : experimental
-}

module Bot.RPC.Configuration
  ( Configuration
  , newConfiguration
  , configurationMethods
  , dispatchConfigurationRequest
  )
where

import Bot.Prelude
import qualified Bot.Config as Config
import qualified Bot.Config.Edit as Edit
import qualified Bot.JSONRPC as RPC
import qualified Crypto.Hash as CryptoHash
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Effectful.FileSystem.IO as FileSystemIO
import qualified Effectful.Temporary as Temporary
import System.FilePath (takeDirectory, (</>))
import qualified System.Posix.Files as Posix

data Configuration = Configuration
  { path :: !FilePath
  , activeDocument :: !Config.ConfigDocument
  , activeRevision :: !Text
  , writeLock :: !(MVar.MVar ())
  }

data ChangeParams = ChangeParams
  { revision :: !Text
  , changes :: ![Edit.ConfigChange]
  }

data RollbackParams = RollbackParams
  { revision :: !Text
  , backupRevision :: !Text
  }

newtype ConfigTargetError = ConfigTargetError Text
  deriving stock (Show)

instance Exception ConfigTargetError

newConfiguration :: Concurrent :> es => FilePath -> Config.ConfigDocument -> Eff es Configuration
newConfiguration path activeDocument = do
  writeLock <- MVar.newMVar ()
  pure Configuration
    { path
    , activeDocument
    , activeRevision = sourceRevision (Config.configDocumentSource activeDocument)
    , writeLock
    }

configurationMethods :: [Text]
configurationMethods = ["config.get", "config.validate", "config.update", "config.rollback"]

dispatchConfigurationRequest
  :: (Concurrent :> es, FileSystem.FileSystem :> es, IOE :> es)
  => Configuration
  -> RPC.RpcRequest
  -> Eff es (Maybe (Either RPC.RpcError Aeson.Value))
dispatchConfigurationRequest configuration request = case RPC.requestMethod request of
  "config.get" -> Just <$> getConfiguration configuration request
  "config.validate" -> Just <$> validateConfiguration configuration request
  "config.update" -> Just <$> MVar.withMVar configuration.writeLock (const (updateConfiguration configuration request))
  "config.rollback" -> Just <$> MVar.withMVar configuration.writeLock (const (rollbackConfiguration configuration request))
  _ -> pure Nothing

getConfiguration
  :: (FileSystem.FileSystem :> es, IOE :> es)
  => Configuration
  -> RPC.RpcRequest
  -> Eff es (Either RPC.RpcError Aeson.Value)
getConfiguration configuration request =
  case AesonTypes.parseEither parseNoParams (RPC.requestParams request) of
    Left err -> pure (Left (invalidParams err))
    Right () -> do
      readCurrent configuration >>= \case
        Left message -> pure (Left (RPC.rpcError "config_write_failed" message))
        Right (source, revision) -> do
          backup <- backupMetadata configuration
          pure . Right $ case Config.parseConfigDocument configuration.path source of
            Left diagnostics -> configurationJson configuration.activeDocument revision configuration.activeRevision False diagnostics backup
            Right document -> configurationJson document revision configuration.activeRevision True [] backup

validateConfiguration
  :: (FileSystem.FileSystem :> es, IOE :> es)
  => Configuration
  -> RPC.RpcRequest
  -> Eff es (Either RPC.RpcError Aeson.Value)
validateConfiguration configuration request =
  case AesonTypes.parseEither parseChangeParams (RPC.requestParams request) of
    Left err -> pure (Left (invalidParams err))
    Right params -> validateAgainstCurrent configuration params

validateAgainstCurrent
  :: (FileSystem.FileSystem :> es, IOE :> es)
  => Configuration
  -> ChangeParams
  -> Eff es (Either RPC.RpcError Aeson.Value)
validateAgainstCurrent configuration params =
  readCurrent configuration >>= \case
    Left message -> pure (Left (RPC.rpcError "config_write_failed" message))
    Right (source, revision)
      | revision /= params.revision -> pure (Left revisionConflict)
      | otherwise -> pure $ do
          current <- first invalidSource (Config.parseConfigDocument configuration.path source)
          changedSource <- first editError (Edit.applyConfigChanges current params.changes)
          case Config.parseConfigDocument configuration.path changedSource of
            Left diagnostics -> Right $ Aeson.object
              [ "valid" Aeson..= False
              , "revision" Aeson..= revision
              , "diagnostics" Aeson..= diagnostics
              , "diff" Aeson..= ([] :: [Aeson.Value])
              , "restartRequired" Aeson..= True
              ]
            Right changed -> Right $ validationJson revision current changed

updateConfiguration
  :: (Concurrent :> es, FileSystem.FileSystem :> es, IOE :> es)
  => Configuration
  -> RPC.RpcRequest
  -> Eff es (Either RPC.RpcError Aeson.Value)
updateConfiguration configuration request =
  case AesonTypes.parseEither parseChangeParams (RPC.requestParams request) of
    Left err -> pure (Left (invalidParams err))
    Right params -> do
      readCurrent configuration >>= \case
        Left message -> pure (Left (RPC.rpcError "config_write_failed" message))
        Right (source, revision)
          | revision /= params.revision -> pure (Left revisionConflict)
          | otherwise -> case Config.parseConfigDocument configuration.path source of
              Left _ -> pure (Left (RPC.rpcError "validation_failed" "Current configuration is invalid"))
              Right current -> case Edit.applyConfigChanges current params.changes of
                Left err -> pure (Left (editError err))
                Right changedSource -> case Config.parseConfigDocument configuration.path changedSource of
                  Left _ -> pure (Left (RPC.rpcError "validation_failed" "Configuration changes did not pass validation"))
                  Right changed
                    | changedSource == source -> pure (Right $ updateJson False revision revision (Edit.semanticDiff current changed))
                    | otherwise -> do
                        writeResult <- trySync (atomicReplace configuration.path source changedSource)
                        pure $ case writeResult of
                          Left (_ :: SomeException) -> Left (RPC.rpcError "config_write_failed" "Configuration could not be written")
                          Right () -> Right $ updateJson True (sourceRevision changedSource) revision (Edit.semanticDiff current changed)

rollbackConfiguration
  :: (Concurrent :> es, FileSystem.FileSystem :> es, IOE :> es)
  => Configuration
  -> RPC.RpcRequest
  -> Eff es (Either RPC.RpcError Aeson.Value)
rollbackConfiguration configuration request =
  case AesonTypes.parseEither parseRollbackParams (RPC.requestParams request) of
    Left err -> pure (Left (invalidParams err))
    Right params -> do
      let backupPath = configuration.path <> ".cosmobot.bak"
      backupExists <- FileSystem.doesFileExist backupPath
      if not backupExists
        then pure (Left (RPC.rpcError "backup_unavailable" "No configuration backup is available"))
        else do
          currentResult <- readUtf8File configuration.path
          backupResult <- readUtf8File backupPath
          case (currentResult, backupResult) of
            (Right currentSource, Right backupSource)
              | sourceRevision currentSource /= params.revision -> pure (Left revisionConflict)
              | sourceRevision backupSource /= params.backupRevision -> pure (Left (RPC.rpcError "revision_conflict" "Backup revision changed"))
              | Left _ <- Config.parseConfigDocument configuration.path currentSource -> pure (Left (RPC.rpcError "validation_failed" "Current configuration is invalid"))
              | Left _ <- Config.parseConfigDocument configuration.path backupSource -> pure (Left (RPC.rpcError "validation_failed" "Backup configuration is invalid"))
              | otherwise -> do
                  writeResult <- trySync (atomicReplace configuration.path currentSource backupSource)
                  pure $ case writeResult of
                    Left (_ :: SomeException) -> Left (RPC.rpcError "config_write_failed" "Configuration could not be rolled back")
                    Right () -> Right $ Aeson.object
                      [ "rolledBack" Aeson..= True
                      , "revision" Aeson..= sourceRevision backupSource
                      , "backupRevision" Aeson..= sourceRevision currentSource
                      , "restartRequired" Aeson..= True
                      ]
            _ -> pure (Left (RPC.rpcError "backup_unavailable" "Configuration backup could not be read"))

configurationJson
  :: Config.ConfigDocument
  -> Text
  -> Text
  -> Bool
  -> [Config.ConfigDiagnostic]
  -> Maybe Aeson.Value
  -> Aeson.Value
configurationJson document revision activeRevision valid diagnostics backup =
  Aeson.object
    [ "schemaVersion" Aeson..= (1 :: Int)
    , "revision" Aeson..= revision
    , "activeRevision" Aeson..= activeRevision
    , "sourceState" Aeson..= if valid then ("valid" :: Text) else "invalid"
    , "editable" Aeson..= valid
    , "diagnostics" Aeson..= diagnostics
    , "configuration" Aeson..= Config.configDocumentInspection document
    , "backup" Aeson..= backup
    ]

validationJson :: Text -> Config.ConfigDocument -> Config.ConfigDocument -> Aeson.Value
validationJson revision current changed = Aeson.object
  [ "valid" Aeson..= True
  , "revision" Aeson..= revision
  , "diagnostics" Aeson..= ([] :: [Aeson.Value])
  , "diff" Aeson..= Edit.semanticDiff current changed
  , "restartRequired" Aeson..= True
  ]

updateJson :: Bool -> Text -> Text -> [Aeson.Value] -> Aeson.Value
updateJson updated revision backupRevision diff = Aeson.object
  [ "updated" Aeson..= updated
  , "revision" Aeson..= revision
  , "backupRevision" Aeson..= backupRevision
  , "diff" Aeson..= diff
  , "restartRequired" Aeson..= updated
  ]

parseChangeParams :: Aeson.Value -> AesonTypes.Parser ChangeParams
parseChangeParams = Aeson.withObject "configuration change params" \object ->
  ChangeParams <$> object Aeson..: "revision" <*> object Aeson..: "changes"

parseRollbackParams :: Aeson.Value -> AesonTypes.Parser RollbackParams
parseRollbackParams = Aeson.withObject "configuration rollback params" \object ->
  RollbackParams <$> object Aeson..: "revision" <*> object Aeson..: "backupRevision"

parseNoParams :: Aeson.Value -> AesonTypes.Parser ()
parseNoParams Aeson.Null = pure ()
parseNoParams value = Aeson.withObject "empty params" (\object -> unless (null object) (fail "params must be empty")) value

invalidParams :: String -> RPC.RpcError
invalidParams = RPC.rpcError "invalid_params" . toText

invalidSource :: [Config.ConfigDiagnostic] -> RPC.RpcError
invalidSource _ = RPC.rpcError "validation_failed" "Current configuration is invalid"

editError :: Edit.ConfigEditError -> RPC.RpcError
editError Edit.ConfigEditError{code, message} = RPC.rpcError code message

revisionConflict :: RPC.RpcError
revisionConflict = RPC.rpcError "revision_conflict" "Configuration revision changed"

readCurrent
  :: (FileSystem.FileSystem :> es, IOE :> es)
  => Configuration
  -> Eff es (Either Text (Text, Text))
readCurrent configuration =
  readUtf8File configuration.path <&> fmap \source -> (source, sourceRevision source)

readUtf8File
  :: (FileSystem.FileSystem :> es, IOE :> es)
  => FilePath
  -> Eff es (Either Text Text)
readUtf8File path = do
  result <- trySync (FileSystemByteString.readFile path)
  pure $ case result of
    Left (_ :: SomeException) -> Left "Configuration file could not be read"
    Right bytes -> first (const "Configuration file is not valid UTF-8") (TextEncoding.decodeUtf8' bytes)

backupMetadata
  :: (FileSystem.FileSystem :> es, IOE :> es)
  => Configuration
  -> Eff es (Maybe Aeson.Value)
backupMetadata configuration = do
  let backupPath = configuration.path <> ".cosmobot.bak"
  exists <- FileSystem.doesFileExist backupPath
  if not exists then pure Nothing else
    readUtf8File backupPath <&> either (const Nothing) (Just . (\revision -> Aeson.object ["revision" Aeson..= revision]) . sourceRevision)

sourceRevision :: Text -> Text
sourceRevision source =
  toText (show (CryptoHash.hash (TextEncoding.encodeUtf8 source) :: CryptoHash.Digest CryptoHash.SHA256) :: String)

atomicReplace
  :: (FileSystem.FileSystem :> es, IOE :> es)
  => FilePath
  -> Text
  -> Text
  -> Eff es ()
atomicReplace path previous replacement = do
  status <- checkedTarget path
  let directory = takeDirectory path
      backupPath = path <> ".cosmobot.bak"
  backupExists <- FileSystem.doesPathExist backupPath
  when backupExists do
    backupIsLink <- FileSystem.pathIsSymbolicLink backupPath
    when backupIsLink (throwIO (ConfigTargetError "backup target is a symbolic link"))
    backupStatus <- liftIO (Posix.getFileStatus backupPath)
    unless (Posix.isRegularFile backupStatus) (throwIO (ConfigTargetError "backup target is not a regular file"))
  Temporary.runTemporary $
    Temporary.withTempDirectory directory ".cosmobot-config-" \temporaryDirectory -> do
      let newPath = temporaryDirectory </> "new"
          oldPath = temporaryDirectory </> "previous"
      writeReplacement status newPath replacement
      writeReplacement status oldPath previous
      FileSystem.renameFile oldPath backupPath
      FileSystem.renameFile newPath path

checkedTarget
  :: (FileSystem.FileSystem :> es, IOE :> es)
  => FilePath
  -> Eff es Posix.FileStatus
checkedTarget path = do
  isLink <- FileSystem.pathIsSymbolicLink path
  when isLink (throwIO (ConfigTargetError "configuration target is a symbolic link"))
  status <- liftIO (Posix.getFileStatus path)
  unless (Posix.isRegularFile status) (throwIO (ConfigTargetError "configuration target is not a regular file"))
  pure status

writeReplacement
  :: (FileSystem.FileSystem :> es, IOE :> es)
  => Posix.FileStatus
  -> FilePath
  -> Text
  -> Eff es ()
writeReplacement status path source = do
  FileSystemIO.withBinaryFile path FileSystemIO.WriteMode \fileHandle -> do
    FileSystemByteString.hPut fileHandle (TextEncoding.encodeUtf8 source)
    FileSystemIO.hFlush fileHandle
  liftIO do
    Posix.setFileMode path (Posix.fileMode status)
    Posix.setOwnerAndGroup path (Posix.fileOwner status) (Posix.fileGroup status)
