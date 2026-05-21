{-|
Module      : Bot.Storage.Attachment
Description : Durable metadata and file ownership for RPC attachments
Stability   : experimental
-}
{-# LANGUAGE OverloadedLabels #-}

module Bot.Storage.Attachment
  ( AttachmentConfig (..)
  , AttachmentUpload (..)
  , StoredAttachment (..)
  , StoredAttachmentRef (..)
  , ensureAttachmentTables
  , storeAttachment
  , loadAttachment
  , deleteUnreferencedAttachment
  )
where

import Bot.Prelude
import qualified Bot.Effect.Storage as Storage
import Bot.Storage.Prelude (runSelda, transaction)
import Bot.Storage.Attachment.Internal
  ( AttachmentUpload (..)
  , StoredAttachment (..)
  , StoredAttachmentRef (..)
  )
import qualified Bot.Storage.Attachment.Internal as Internal
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Unique as Unique
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import System.FilePath ((<.>), (</>), takeExtension)

data AttachmentConfig = AttachmentConfig
  { directory :: !FilePath
  , maxBytes :: !Int
  }
  deriving (Eq, Show)

ensureAttachmentTables :: Storage.Storage :> es => Eff es ()
ensureAttachmentTables =
  Internal.ensureAttachmentTables

storeAttachment
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es)
  => AttachmentConfig
  -> AttachmentUpload
  -> Eff es (Either Text StoredAttachmentRef)
storeAttachment cfg upload = do
  let actualSize = ByteString.length upload.bytes
      limit = cfg.maxBytes
  if actualSize > limit
    then pure (Left [i|attachment is #{actualSize} bytes; limit is #{limit} bytes|])
    else do
      FileSystem.createDirectoryIfMissing True cfg.directory
      attachmentId <- newAttachmentId
      let finalPath = attachmentPath cfg upload.name attachmentId
      let stored = StoredAttachment
            { attachmentId
            , name = cleanName upload.name
            , mediaType = cleanMediaType upload.mediaType
            , kind = cleanKind upload.kind upload.mediaType
            , size = actualSize
            , path = finalPath
            , refCount = 0
            }
      finalExists <- FileSystem.doesFileExist finalPath
      if finalExists
        then pure (Left "attachment id collision; retry upload")
        else
          withStoredAttachmentFile finalPath upload.bytes do
            ensureAttachmentTables
            runSelda $
              Internal.insertAttachment stored
            pure (Right (Internal.attachmentRef stored))

loadAttachment :: Storage.Storage :> es => Text -> Eff es (Maybe StoredAttachment)
loadAttachment targetAttachmentId = do
  ensureAttachmentTables
  rows <- runSelda $
    Internal.loadAttachmentById targetAttachmentId
  pure rows

deleteUnreferencedAttachment
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es)
  => Text
  -> Eff es Bool
deleteUnreferencedAttachment targetAttachmentId = do
  claimUnreferencedAttachmentFile targetAttachmentId >>= \case
    Nothing ->
      pure False
    Just attachment -> do
      removeIfExistsBestEffort attachment.path
      pure True

claimUnreferencedAttachmentFile :: Storage.Storage :> es => Text -> Eff es (Maybe StoredAttachment)
claimUnreferencedAttachmentFile targetAttachmentId = do
  ensureAttachmentTables
  runSelda $
    transaction (Internal.claimUnreferencedAttachment targetAttachmentId)

data StagedAttachmentBlob = StagedAttachmentBlob
  { tempPath :: !FilePath
  , finalPath :: !FilePath
  }

withStagedAttachmentBlob
  :: FileSystem.FileSystem :> es
  => FilePath
  -> ByteString.ByteString
  -> (StagedAttachmentBlob -> Eff es a)
  -> Eff es a
withStagedAttachmentBlob finalPath bytes =
  bracketOnError acquire cleanup
  where
    acquire = do
      let staged =
            StagedAttachmentBlob
              { tempPath = finalPath <> ".uploading"
              , finalPath
              }
      FileSystemByteString.writeFile staged.tempPath bytes
      pure staged

    cleanup staged = do
      removeIfExists staged.tempPath

withStoredAttachmentFile
  :: FileSystem.FileSystem :> es
  => FilePath
  -> ByteString.ByteString
  -> Eff es a
  -> Eff es a
withStoredAttachmentFile finalPath bytes persist =
  withStagedAttachmentBlob finalPath bytes \staged ->
    FileSystem.renameFile staged.tempPath staged.finalPath *> persist

removeIfExists :: FileSystem.FileSystem :> es => FilePath -> Eff es ()
removeIfExists path = do
  exists <- FileSystem.doesFileExist path
  when exists $
    FileSystem.removeFile path

removeIfExistsBestEffort :: FileSystem.FileSystem :> es => FilePath -> Eff es ()
removeIfExistsBestEffort path =
  removeIfExists path `catchSync` \_ ->
    pure ()

newAttachmentId :: IOE :> es => Eff es Text
newAttachmentId = do
  uniq <- liftIO Unique.newUnique
  timestamp <- liftIO POSIX.getPOSIXTime
  let micros = floor (timestamp * 1000000) :: Integer
  pure [i|att-#{micros}-#{Unique.hashUnique uniq}|]

attachmentPath :: AttachmentConfig -> Text -> Text -> FilePath
attachmentPath cfg originalName attachmentId =
  cfg.directory </> Text.unpack attachmentId <.> extension
  where
    rawExtension = Text.pack (dropWhile (== '.') (takeExtension (Text.unpack originalName)))
    extension =
      Text.unpack $
        if Text.all validExtensionChar rawExtension && not (Text.null rawExtension)
          then rawExtension
          else "bin"

validExtensionChar :: Char -> Bool
validExtensionChar char =
  (char >= 'a' && char <= 'z')
    || (char >= 'A' && char <= 'Z')
    || (char >= '0' && char <= '9')

cleanName :: Text -> Text
cleanName value =
  case Text.strip value of
    "" -> "attachment"
    stripped -> stripped

cleanMediaType :: Text -> Text
cleanMediaType value =
  case Text.strip value of
    stripped
      | validMediaType stripped -> stripped
      | otherwise -> "application/octet-stream"

validMediaType :: Text -> Bool
validMediaType value =
  case Text.splitOn "/" value of
    [mainType, subtype] ->
      validToken mainType && validToken subtype
    _ ->
      False

validToken :: Text -> Bool
validToken value =
  not (Text.null value) && Text.all validTokenChar value

validTokenChar :: Char -> Bool
validTokenChar char =
  (char >= 'a' && char <= 'z')
    || (char >= 'A' && char <= 'Z')
    || (char >= '0' && char <= '9')
    || char `elem` ("!#$&^_.+-" :: String)

cleanKind :: Text -> Text -> Text
cleanKind kind mediaType =
  case Text.toLower (Text.strip kind) of
    "image" -> "image"
    "audio" -> "audio"
    "file" -> "file"
    _ | "image/" `Text.isPrefixOf` media -> "image"
      | "audio/" `Text.isPrefixOf` media -> "audio"
      | otherwise -> "file"
  where
    media = Text.toLower (Text.strip mediaType)
