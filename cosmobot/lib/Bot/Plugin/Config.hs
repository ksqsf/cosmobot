{-|
Module      : Bot.Plugin.Config
Description : External plugin bundle discovery and lifecycle configuration
Stability   : experimental
-}
module Bot.Plugin.Config
  ( PluginConfigError (..)
  , pluginConfigFileName
  , parseBundleConfig
  , loadPluginBundle
  , discoverPluginBundles
  )
where

import Bot.Plugin.Types
import Bot.Prelude
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import System.FilePath ((</>))
import qualified Toml
import qualified Toml.Semantics.Types as TomlTypes

data PluginConfigError = PluginConfigError
  { path :: !FilePath
  , message :: !Text
  }
  deriving (Eq, Show)

pluginConfigFileName :: FilePath
pluginConfigFileName = "config.toml"

parseBundleConfig :: Text -> Either Text PluginLifecycleConfig
parseBundleConfig source = do
  table <- first toText (Toml.parse source)
  parseRoot (TomlTypes.forgetTableAnns table)

loadPluginBundle
  :: (FileSystem.FileSystem :> es, IOE :> es)
  => FilePath
  -> PluginId
  -> Eff es (Either PluginConfigError PluginBundle)
loadPluginBundle bundleDir pluginId = do
  let idText = pluginId.unPluginId
      executablePath = bundleDir </> Text.unpack idText
      configPath = bundleDir </> pluginConfigFileName
  case validatePluginId idText of
    Left err -> pure (Left (failure bundleDir err))
    Right _ -> do
      bundleExists <- FileSystem.doesDirectoryExist bundleDir
      bundleIsLink <- if bundleExists then FileSystem.pathIsSymbolicLink bundleDir else pure False
      executableExists <- FileSystem.doesFileExist executablePath
      configExists <- FileSystem.doesFileExist configPath
      executableIsLink <- if executableExists then FileSystem.pathIsSymbolicLink executablePath else pure False
      configIsLink <- if configExists then FileSystem.pathIsSymbolicLink configPath else pure False
      if not bundleExists
        then pure (Left (failure bundleDir "plugin bundle directory is missing"))
        else if bundleIsLink
        then pure (Left (failure bundleDir "plugin bundle must not be a symbolic link"))
        else if executableIsLink
          then pure (Left (failure executablePath "plugin executable must not be a symbolic link"))
        else if configIsLink
          then pure (Left (failure configPath "plugin configuration must not be a symbolic link"))
        else if not executableExists
        then pure (Left (failure executablePath "plugin executable is missing"))
        else if not configExists
          then pure (Left (failure configPath "plugin configuration is missing"))
          else do
            permissionsResult <- trySync (FileSystem.getPermissions executablePath)
            case permissionsResult of
              Left err -> pure (Left (failure executablePath ("cannot inspect plugin executable: " <> show err)))
              Right permissions
                | not permissions.executable ->
                    pure (Left (failure executablePath "plugin executable is not executable"))
                | otherwise -> do
                    contentResult <- trySync (FileSystemByteString.readFile configPath)
                    pure do
                      content <- first (failure configPath . ("cannot read plugin configuration: " <>) . show) contentResult
                      source <- first (failure configPath . ("plugin configuration is not UTF-8: " <>) . show)
                        (TextEncoding.decodeUtf8' content)
                      lifecycle <- first (failure configPath) (parseBundleConfig source)
                      pure PluginBundle{pluginId, bundleDir, executablePath, configPath, lifecycle}

discoverPluginBundles
  :: (FileSystem.FileSystem :> es, IOE :> es)
  => FilePath
  -> Eff es (Either PluginConfigError [PluginBundle])
discoverPluginBundles pluginDir = do
  exists <- FileSystem.doesDirectoryExist pluginDir
  if not exists
    then pure (Right [])
    else trySync (FileSystem.listDirectory pluginDir) >>= \case
      Left err -> pure (Left (failure pluginDir ("cannot list plugin directory: " <> show err)))
      Right entries -> do
        directories <- filterM (FileSystem.doesDirectoryExist . (pluginDir </>)) (List.sort entries)
        sequence <$> traverse load directories
  where
    load entry =
      loadPluginBundle (pluginDir </> entry) (PluginId (Text.pack entry))

parseRoot :: TomlTypes.Table -> Either Text PluginLifecycleConfig
parseRoot (TomlTypes.MkTable root) = case Map.lookup "plugin" root of
  Nothing -> Right defaultPluginLifecycleConfig
  Just (_, TomlTypes.Table pluginTable) -> parseLifecycle pluginTable
  Just _ -> Left "[plugin] must be a table"

parseLifecycle :: TomlTypes.Table -> Either Text PluginLifecycleConfig
parseLifecycle (TomlTypes.MkTable table) = do
  let allowedKeys = Set.fromList
        [ "required"
        , "sandboxed"
        , "route_timeout_seconds"
        , "tool_timeout_seconds"
        , "restart_limit"
        ]
      unknownKeys = Map.keysSet table `Set.difference` allowedKeys
  unless (Set.null unknownKeys) $
    Left ("unknown [plugin] keys: " <> Text.intercalate ", " (Set.toAscList unknownKeys))
  required <- boolean "required" defaultPluginLifecycleConfig.required table
  sandboxed <- boolean "sandboxed" defaultPluginLifecycleConfig.sandboxed table
  routeTimeoutSeconds <- positiveSeconds
    "route_timeout_seconds" defaultPluginLifecycleConfig.routeTimeoutSeconds table
  toolTimeoutSeconds <- positiveSeconds
    "tool_timeout_seconds" defaultPluginLifecycleConfig.toolTimeoutSeconds table
  restartLimit <- nonNegativeInteger "restart_limit" defaultPluginLifecycleConfig.restartLimit table
  pure PluginLifecycleConfig
    { required
    , sandboxed
    , routeTimeoutSeconds
    , toolTimeoutSeconds
    , restartLimit
    }

boolean :: Text -> Bool -> Map.Map Text ((), TomlTypes.Value) -> Either Text Bool
boolean key defaultValue table = case snd <$> Map.lookup key table of
  Nothing -> Right defaultValue
  Just (TomlTypes.Bool value) -> Right value
  Just value -> wrongType key "boolean" value

positiveInteger :: Text -> Int -> Map.Map Text ((), TomlTypes.Value) -> Either Text Int
positiveInteger key defaultValue table = do
  value <- integer key defaultValue table
  if value > 0 then Right value else Left (key <> " must be positive")

positiveSeconds :: Text -> Int -> Map.Map Text ((), TomlTypes.Value) -> Either Text Int
positiveSeconds key defaultValue table = do
  value <- positiveInteger key defaultValue table
  if value <= maxBound `div` 1_000_000
    then Right value
    else Left (key <> " is too large")

nonNegativeInteger :: Text -> Int -> Map.Map Text ((), TomlTypes.Value) -> Either Text Int
nonNegativeInteger key defaultValue table = do
  value <- integer key defaultValue table
  if value >= 0 then Right value else Left (key <> " must not be negative")

integer :: Text -> Int -> Map.Map Text ((), TomlTypes.Value) -> Either Text Int
integer key defaultValue table = case snd <$> Map.lookup key table of
  Nothing -> Right defaultValue
  Just (TomlTypes.Integer value)
    | value <= toInteger (maxBound :: Int) && value >= toInteger (minBound :: Int) ->
        Right (fromInteger value)
    | otherwise -> Left (key <> " is outside the supported integer range")
  Just value -> wrongType key "integer" value

wrongType :: Text -> Text -> TomlTypes.Value -> Either Text value
wrongType key expected actual =
  Left (key <> " must be a " <> expected <> ", not " <> toText (TomlTypes.valueType actual))

failure :: FilePath -> Text -> PluginConfigError
failure path message = PluginConfigError{path, message}
