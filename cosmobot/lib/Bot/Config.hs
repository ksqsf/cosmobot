{-|
Module      : Bot.Config
Description : Application configuration
Stability   : experimental
-}

module Bot.Config
  ( -- * Top-level configuration
    BotConfig (..)
  , PluginsConfig (..)
  , HandlersConfig (..)
  , AdminConfig (..)
  , AskHandlerConfig (..)
  , ConsoleHandlerConfig (..)
  , SaucenaoConfig (..)
  , Memory.MemoryConfig (..)
  , Skills.SkillsConfig (..)
  , MediaConfig.Config (..)
  , loadConfig
  , loadConfigDocument
  , ConfigDocument
  , ConfigDiagnostic (..)
  , parseConfigDocument
  , configDocumentRuntime
  , configDocumentSource
  , configDocumentInspection
  , configDocumentOptions
  , renderConfigValueAt
  , configOptionIsSecretAt
  , configOptionIsRequiredAt
  , configOptionKnownAt
  , configDocumentOptionValues
  , configRepeatableSection
  )
where

import qualified Bot.Chat.Driver.QQ as QQ
import qualified Bot.Chat.Driver.QQ.Config as QQConfig
import qualified Bot.Chat.Driver.Discord as Discord
import qualified Bot.Chat.Driver.Discord.Config as DiscordConfig
import qualified Bot.Chat.Driver.Matrix as Matrix
import qualified Bot.Chat.Driver.Matrix.Config as MatrixConfig
import qualified Bot.Chat.Driver.Telegram as Telegram
import qualified Bot.Chat.Driver.Telegram.Config as TelegramConfig
import qualified Bot.ACP.Config as ACPConfig
import qualified Bot.Config.Schema as Schema
import qualified Bot.LLM.OpenAI.Config as LLMConfig
import qualified Bot.Media.Config as MediaConfig
import qualified Bot.RPC.Config as RPCConfig
import qualified Bot.Agent.Types as Agent
import qualified Bot.Agent.Config as AgentConfig
import Bot.Core.Message (ChatPlatform (..))
import Bot.Handler.Admin.Config
  ( AdminConfig (..)
  )
import qualified Bot.Handler.Admin.Config as AdminConfig
import Bot.Handler.Ask.Config
  ( AskHandlerConfig (..)
  )
import qualified Bot.Handler.Ask.Config as AskConfig
import Bot.Handler.Console.Config
  ( ConsoleHandlerConfig (..)
  )
import qualified Bot.Handler.Console.Config as ConsoleConfig
import qualified Bot.Handler.Saucenao.Config as SaucenaoConfig
import Bot.Handler.Saucenao.Config
  ( SaucenaoConfig (..)
  )
import qualified Bot.Handler.ShutUp.Config as ShutUpConfig
import Bot.Handler.ShutUp.Config
  ( ShutUpConfig (..)
  )
import qualified Bot.Memory as Memory
import qualified Bot.Memory.Config as MemoryConfig
import qualified Bot.Resource.Sandbox as Sandbox
import qualified Bot.Skills as Skills
import qualified Bot.Skills.Config as SkillsConfig
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Toml.Semantics.Types as TomlValue
import qualified Toml.Schema.Matcher as Matcher
import qualified Toml.Semantics as TomlSemantics
import qualified Toml.Syntax.Position as TomlPosition
import qualified Toml.Syntax.Parser as TomlParser
import Toml.Schema
import System.FilePath (isRelative, normalise, takeDirectory, (</>))
import qualified Prelude

-- | Fully normalized runtime configuration.
data BotConfig = BotConfig
  { qq       :: !(Maybe QQ.Config)
  , telegram :: !(Maybe Telegram.Config)
  , matrix   :: !(Maybe Matrix.Config)
  , discord  :: !(Maybe Discord.Config)
  , llm      :: !LLMConfig.Config
  , media    :: !MediaConfig.Config
  , tool     :: !Agent.ToolConfig
  , saucenao :: !SaucenaoConfig
  , memory   :: !Memory.MemoryConfig
  , skills   :: !Skills.SkillsConfig
  , rpc      :: !RPCConfig.Config
  , acp      :: !ACPConfig.Config
  , plugins  :: !PluginsConfig
  , handlers :: !HandlersConfig
  , logLevel :: !Severity
  , sqlitePath :: !FilePath
  }

instance Show BotConfig where
  showsPrec _ _ = Prelude.showString "<BotConfig>"

-- | Configuration for all handler groups.
data HandlersConfig = HandlersConfig
  { admin :: !AdminConfig
  , ask :: !AskHandlerConfig
  , console :: !ConsoleHandlerConfig
  , shutup :: !ShutUpConfig
  }
  deriving (Show)

newtype PluginsConfig = PluginsConfig
  { pluginDir :: FilePath
  }
  deriving (Eq, Show)

defaultPluginsConfig :: PluginsConfig
defaultPluginsConfig = PluginsConfig{pluginDir = "plugins"}

instance FromValue PluginsConfig where
  fromValue = parseTableFromValue $ PluginsConfig
    <$> fmap (fromMaybe defaultPluginsConfig.pluginDir) (optKey "plugin_dir")

-- | Read and normalize the TOML configuration used by the executable.
loadConfig :: (IOE :> es, Fail :> es) => FilePath -> Eff es BotConfig
loadConfig path = do
  (.runtimeConfig) <$> loadConfigDocument path

loadConfigDocument :: (IOE :> es, Fail :> es) => FilePath -> Eff es ConfigDocument
loadConfigDocument path = do
  content <- liftIO $ TextIO.readFile path
  case parseConfigDocument path content of
    Left errors ->
      fail [i|Failed to parse configuration: #{Text.unlines (map diagnosticMessage errors)}|]
    Right document ->
      pure document

parseConfigDocument :: FilePath -> Text -> Either [ConfigDiagnostic] ConfigDocument
parseConfigDocument configPath source =
  case TomlParser.parseRawToml source of
    Left located -> Left [positionedDiagnostic [] "syntax_error" "Invalid TOML syntax" located.locPosition]
    Right expressions -> case TomlSemantics.semantics expressions of
      Left semantic -> Left
        [ positionedDiagnostic [semantic.errorKey] "semantic_error" (semanticMessage semantic) semantic.errorAnn
        ]
      Right parsedTable -> case Matcher.runMatcher (Schema.schemaFromValue configSchema (TomlValue.Table' TomlPosition.startPos parsedTable)) of
        Matcher.Failure errors -> Left (map matchDiagnostic errors)
        Matcher.Success _warnings fileConfig -> Right ConfigDocument
          { source
          , table = parsedTable
          , fileConfig
          , runtimeConfig = toBotConfig configPath fileConfig
          }

configDocumentRuntime :: ConfigDocument -> BotConfig
configDocumentRuntime = (.runtimeConfig)

configDocumentSource :: ConfigDocument -> Text
configDocumentSource = (.source)

diagnosticMessage :: ConfigDiagnostic -> Text
diagnosticMessage ConfigDiagnostic{message} = message

positionedDiagnostic :: [Text] -> Text -> Text -> TomlPosition.Position -> ConfigDiagnostic
positionedDiagnostic path code message position = ConfigDiagnostic
  { path
  , code
  , message
  , line = Just position.posLine
  , column = Just position.posColumn
  }

matchDiagnostic :: Matcher.MatchMessage TomlPosition.Position -> ConfigDiagnostic
matchDiagnostic match =
  case match.matchAnn of
    Nothing -> ConfigDiagnostic path "schema_error" message Nothing Nothing
    Just position -> positionedDiagnostic path "schema_error" message position
  where
    path = map scopeText match.matchPath
    message
      | secretDiagnosticPath path = "Secret value is invalid"
      | otherwise = toText match.matchMessage
    scopeText = \case
      Matcher.ScopeKey key -> key
      Matcher.ScopeIndex index -> show index

secretDiagnosticPath :: [Text] -> Bool
secretDiagnosticPath path =
  any (\option -> Schema.optionPath option == path && Schema.optionIsSecret option) configSchema.options
    || case path of
      ["llm", family, _name, "api_key"] -> family `elem` ["chat_provider", "image_provider", "audio_provider"]
      _ -> False

semanticMessage :: TomlSemantics.SemanticError annotation -> Text
semanticMessage semantic =
  "invalid TOML assignment for " <> semantic.errorKey <> ": " <> case semantic.errorKind of
    TomlSemantics.AlreadyAssigned -> "key is already assigned"
    TomlSemantics.ClosedTable -> "table is already closed"
    TomlSemantics.ImplicitlyTable -> "key is already an implicit table"

data FileConfig = FileConfig
  { log      :: !LogFileConfig
  , storage  :: !StorageFileConfig
  , driver   :: !DriverFileConfig
  , llm      :: !LLMConfig.FileConfig
  , media    :: !MediaConfig.Config
  , tool     :: !AgentConfig.FileConfig
  , resource :: !ResourceFileConfig
  , memory   :: !MemoryConfig.FileConfig
  , skills   :: !SkillsConfig.FileConfig
  , rpc      :: !RPCConfig.FileConfig
  , acp      :: !ACPConfig.FileConfig
  , plugins  :: !PluginsConfig
  , handler  :: !HandlerFileConfig
  }
  deriving (Show)

data ConfigDocument = ConfigDocument
  { source :: !Text
  , table :: !(TomlValue.Table' TomlPosition.Position)
  , fileConfig :: !FileConfig
  , runtimeConfig :: !BotConfig
  }

data ConfigDiagnostic = ConfigDiagnostic
  { path :: ![Text]
  , code :: !Text
  , message :: !Text
  , line :: !(Maybe Int)
  , column :: !(Maybe Int)
  }
  deriving (Eq, Show)

instance Aeson.ToJSON ConfigDiagnostic where
  toJSON diagnostic = Aeson.object
    [ "path" Aeson..= diagnostic.path
    , "code" Aeson..= diagnostic.code
    , "message" Aeson..= diagnostic.message
    , "line" Aeson..= diagnostic.line
    , "column" Aeson..= diagnostic.column
    ]

configSchema :: Schema.ConfigSchema FileConfig BotConfig
configSchema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue $ FileConfig
    <$> fmap (fromMaybe defaultLogFileConfig) (optKey "log")
    <*> fmap (fromMaybe defaultStorageFileConfig) (optKey "storage")
    <*> fmap (fromMaybe defaultDriverFileConfig) (optKey "driver")
    <*> reqKey "llm"
    <*> fmap (fromMaybe MediaConfig.defaultConfig) (optKey "media")
    <*> fmap (fromMaybe AgentConfig.defaultFileConfig) (optKey "tool")
    <*> fmap (fromMaybe defaultResourceFileConfig) (optKey "resource")
    <*> fmap (fromMaybe MemoryConfig.defaultFileConfig) (optKey "memory")
    <*> fmap (fromMaybe SkillsConfig.defaultFileConfig) (optKey "skills")
    <*> fmap (fromMaybe RPCConfig.defaultFileConfig) (optKey "rpc")
    <*> fmap (fromMaybe ACPConfig.defaultFileConfig) (optKey "acp")
    <*> fmap (fromMaybe defaultPluginsConfig) (optKey "plugins")
    <*> reqKey "handler"
  , Schema.options =
      [ Schema.option ["log", "level"] "Log level" "Minimum emitted log severity." "Bot.Config" (Schema.enum severityNames) "info" Aeson.Null (severityText . (.level) . (.log)) (severityText . (.logLevel))
      , Schema.option ["storage", "sqlite_path"] "SQLite path" "SQLite database path." "Bot.Config" Schema.text (toText defaultStorageFileConfig.sqlitePath) Aeson.Null (toText . (.sqlitePath) . (.storage)) (toText . (.sqlitePath))
      , Schema.option ["plugins", "plugin_dir"] "Plugin directory" "Directory containing plugin bundles." "Bot.Config" Schema.text (toText defaultPluginsConfig.pluginDir) Aeson.Null (toText . (.pluginDir) . (.plugins)) (toText . (.pluginDir) . (.plugins))
      ]
      <> owner ["acp"] (.acp) (.acp) ACPConfig.schema.options
      <> owner ["rpc"] (.rpc) (.rpc) RPCConfig.schema.options
      <> owner ["tool"] (.tool) (.tool) AgentConfig.schema.options
      <> owner ["media"] (.media) (.media) MediaConfig.schema.options
      <> owner ["memory"] (.memory) (.memory) MemoryConfig.schema.options
      <> owner ["skills"] (.skills) (.skills) SkillsConfig.schema.options
      <> owner ["resource", "sandbox"] ((.sandbox) . (.resource)) (\cfg -> Sandbox.Config cfg.tool.sandboxImage) Sandbox.schema.options
      <> optionalOwner ["driver", "qq"] ((.qq) . (.driver)) (.qq) QQConfig.schema.options
      <> optionalOwner ["driver", "telegram"] ((.telegram) . (.driver)) (.telegram) TelegramConfig.schema.options
      <> optionalOwner ["driver", "matrix"] ((.matrix) . (.driver)) (.matrix) MatrixConfig.schema.options
      <> optionalOwner ["driver", "discord"] ((.discord) . (.driver)) (.discord) DiscordConfig.schema.options
      <> owner ["handler", "admin"] ((.admin) . (.handler)) ((.admin) . (.handlers)) AdminConfig.schema.options
      <> owner ["handler", "saucenao"] ((.saucenao) . (.handler)) (.saucenao) SaucenaoConfig.schema.options
      <> owner ["handler", "ask"] ((.ask) . (.handler)) ((.ask) . (.handlers)) AskConfig.schema.options
      <> owner ["handler", "console"] ((.console) . (.handler)) ((.console) . (.handlers)) ConsoleConfig.schema.options
      <> owner ["handler", "shutup"] ((.shutup) . (.handler)) ((.shutup) . (.handlers)) ShutUpConfig.schema.options
  , Schema.sections =
      [ Schema.section ["log"] "Logging" ["runtime"] "Runtime"
      , Schema.section ["storage"] "Storage" ["runtime"] "Runtime"
      , Schema.section ["plugins"] "Plugins" ["runtime"] "Runtime"
      ]
      <> ownerSections ["acp"] ACPConfig.schema
      <> ownerSections ["rpc"] RPCConfig.schema
      <> ownerSections ["tool"] AgentConfig.schema
      <> ownerSections ["media"] MediaConfig.schema
      <> ownerSections ["memory"] MemoryConfig.schema
      <> ownerSections ["skills"] SkillsConfig.schema
      <> ownerSections ["resource", "sandbox"] Sandbox.schema
      <> optionalOwnerSections ["driver", "qq"] QQConfig.schema
      <> optionalOwnerSections ["driver", "telegram"] TelegramConfig.schema
      <> optionalOwnerSections ["driver", "matrix"] MatrixConfig.schema
      <> optionalOwnerSections ["driver", "discord"] DiscordConfig.schema
      <> ownerSections ["handler", "admin"] AdminConfig.schema
      <> ownerSections ["handler", "saucenao"] SaucenaoConfig.schema
      <> ownerSections ["handler", "ask"] AskConfig.schema
      <> ownerSections ["handler", "console"] ConsoleConfig.schema
      <> ownerSections ["handler", "shutup"] ShutUpConfig.schema
  , Schema.repeatableSections = []
  }
  where
    owner prefix source runtime = Schema.prefixOptions prefix . Schema.mapOptions source runtime
    optionalOwner prefix source runtime = Schema.prefixOptions prefix . Schema.mapMaybeOptions source runtime
    ownerSections prefix = Schema.prefixSections prefix . (.sections)
    optionalOwnerSections prefix ownerSchema =
      [ configSection{Schema.optional = True}
      | configSection <- ownerSections prefix ownerSchema
      ]

instance FromValue FileConfig where
  fromValue = Schema.schemaFromValue configSchema

severityNames :: [Text]
severityNames = ["debug", "info", "notice", "warning", "error", "critical", "alert", "emergency"]

severityText :: Severity -> Text
severityText = \case
  DebugS -> "debug"
  InfoS -> "info"
  NoticeS -> "notice"
  WarningS -> "warning"
  ErrorS -> "error"
  CriticalS -> "critical"
  AlertS -> "alert"
  EmergencyS -> "emergency"

configDocumentOptions :: ConfigDocument -> [Schema.ConfigOption ConfigDocument ConfigDocument]
configDocumentOptions document =
  Schema.mapOptions (.fileConfig) (.runtimeConfig) configSchema.options
    <> Schema.prefixOptions ["llm"]
      (Schema.mapOptions ((.llm) . (.fileConfig)) ((.llm) . (.fileConfig)) LLMConfig.schema.options)
    <> providerDocumentOptions "chat_provider" (.chatProviders) LLMConfig.chatProviderSchema.options document
    <> providerDocumentOptions "image_provider" (.imageProviders) LLMConfig.imageProviderSchema.options document
    <> providerDocumentOptions "audio_provider" (.audioProviders) LLMConfig.audioProviderSchema.options document

providerDocumentOptions
  :: Text
  -> (LLMConfig.FileConfig -> Map Text provider)
  -> [Schema.ConfigOption provider provider]
  -> ConfigDocument
  -> [Schema.ConfigOption ConfigDocument ConfigDocument]
providerDocumentOptions family getter options document =
  concatMap instantiate (Map.toList (getter document.fileConfig.llm))
  where
    instantiate (name, provider) =
      Schema.prefixOptions ["llm", family, name] $
        Schema.mapOptions
          (\doc -> fromMaybe provider (Map.lookup name (getter doc.fileConfig.llm)))
          (const provider)
          options

configDocumentInspection :: ConfigDocument -> ConfigDocument -> Aeson.Value
configDocumentInspection document activeDocument = Aeson.object
  [ "sections" Aeson..= map sectionJson (Map.toList grouped)
  , "repeatableSections" Aeson..= repeatableSections
  ]
  where
    options = inspectionOptions document activeDocument
    inspected = Schema.inspectOptions (configPathPresent document) document activeDocument options
    grouped = foldl' addOption Map.empty (zip options inspected)
    addOption sections (configOption, value) =
      Map.insertWith (flip (<>)) (safeInit (Schema.optionPath configOption)) [value] sections
    sectionJson (path, values) = Aeson.object
      [ "path" Aeson..= path
      , "label" Aeson..= metadata.label
      , "group" Aeson..= groupJson metadata.group
      , "optional" Aeson..= metadata.optional
      , "present" Aeson..= configSectionPresent document path
      , "repeatable" Aeson..= isRepeatableInstance path
      , "options" Aeson..= values
      ]
      where
        metadata = fromMaybe (error [i|missing configuration section metadata for #{path}|]) $
          find ((== path) . (.path)) (configDocumentSections document activeDocument)

inspectionOptions :: ConfigDocument -> ConfigDocument -> [Schema.ConfigOption ConfigDocument ConfigDocument]
inspectionOptions document activeDocument =
  Schema.mapOptions (.fileConfig) (.runtimeConfig) configSchema.options
    <> Schema.prefixOptions ["llm"]
      (Schema.mapOptions ((.llm) . (.fileConfig)) ((.llm) . (.fileConfig)) LLMConfig.schema.options)
    <> providerInspectionOptions "chat_provider" (.chatProviders) LLMConfig.chatProviderSchema.options
    <> providerInspectionOptions "image_provider" (.imageProviders) LLMConfig.imageProviderSchema.options
    <> providerInspectionOptions "audio_provider" (.audioProviders) LLMConfig.audioProviderSchema.options
  where
    providerInspectionOptions
      :: Text
      -> (LLMConfig.FileConfig -> Map Text provider)
      -> [Schema.ConfigOption provider provider]
      -> [Schema.ConfigOption ConfigDocument ConfigDocument]
    providerInspectionOptions family getter options =
      concatMap instantiate providerNames
      where
        providerNames = Map.keysSet (getter document.fileConfig.llm) `mappend` Map.keysSet (getter activeDocument.fileConfig.llm)
        instantiate name =
          Schema.prefixOptions ["llm", family, name] $
            Schema.mapMaybeOptions
              (Map.lookup name . getter . (.llm) . (.fileConfig))
              (Map.lookup name . getter . (.llm) . (.fileConfig))
              options

renderConfigValueAt :: ConfigDocument -> [Text] -> Aeson.Value -> Either Text Text
renderConfigValueAt document path value =
  maybe (Left "unknown configuration path") (first toText . (`Schema.optionToml` value)) (findConfigOption document path)

configOptionIsSecretAt :: ConfigDocument -> [Text] -> Bool
configOptionIsSecretAt document path =
  maybe False Schema.optionIsSecret (findConfigOption document path)

configOptionIsRequiredAt :: ConfigDocument -> [Text] -> Bool
configOptionIsRequiredAt document path =
  maybe False Schema.optionIsRequired (findConfigOption document path)

configOptionKnownAt :: ConfigDocument -> [Text] -> Bool
configOptionKnownAt document = isJust . findConfigOption document

configDocumentOptionValues :: ConfigDocument -> Map [Text] Aeson.Value
configDocumentOptionValues document = Map.fromList
  [ (Schema.optionPath option, Schema.optionEffectiveJson document option)
  | option <- configDocumentOptions document
  ]

findConfigOption :: ConfigDocument -> [Text] -> Maybe (Schema.ConfigOption ConfigDocument ConfigDocument)
findConfigOption document path =
  find ((== path) . Schema.optionPath) (configDocumentOptions document)
    <|> repeatableOption path
  where
    repeatableOption ["llm", family, _name, key] =
      case family of
        "chat_provider" -> templateConfigOption (safeInit path) LLMConfig.defaultChatProviderFileConfig LLMConfig.chatProviderSchema.options key
        "image_provider" -> templateConfigOption (safeInit path) LLMConfig.defaultImageProviderFileConfig LLMConfig.imageProviderSchema.options key
        "audio_provider" -> templateConfigOption (safeInit path) LLMConfig.defaultAudioProviderFileConfig LLMConfig.audioProviderSchema.options key
        _ -> Nothing
    repeatableOption _ = Nothing

templateConfigOption
  :: [Text]
  -> provider
  -> [Schema.ConfigOption provider provider]
  -> Text
  -> Maybe (Schema.ConfigOption ConfigDocument ConfigDocument)
templateConfigOption prefix defaults options key = do
  configOption <- find ((== [key]) . Schema.optionPath) options
  viaNonEmpty head $
    Schema.prefixOptions prefix (Schema.mapOptions (const defaults) (const defaults) [configOption])

configRepeatableSection :: [Text] -> Bool
configRepeatableSection ["llm", family, name] =
  not (Text.null name) && family `elem` ["chat_provider", "image_provider", "audio_provider"]
configRepeatableSection _ = False

repeatableSections :: [Aeson.Value]
repeatableSections = map template (Schema.prefixRepeatableSections ["llm"] LLMConfig.schema.repeatableSections)
  where
    template metadata@Schema.RepeatableSection{path = ["llm", family]} = Aeson.object
      [ "path" Aeson..= metadata.path
      , "label" Aeson..= metadata.label
      , "group" Aeson..= groupJson metadata.group
      , "options" Aeson..= templateOptions family
      ]
    template Schema.RepeatableSection{path = sectionPath} =
      error [i|invalid repeatable configuration section path #{sectionPath}|]

    templateOptions = \case
      "chat_provider" -> inspect LLMConfig.defaultChatProviderFileConfig LLMConfig.chatProviderSchema.options "chat_provider"
      "image_provider" -> inspect LLMConfig.defaultImageProviderFileConfig LLMConfig.imageProviderSchema.options "image_provider"
      "audio_provider" -> inspect LLMConfig.defaultAudioProviderFileConfig LLMConfig.audioProviderSchema.options "audio_provider"
      family -> error [i|missing repeatable configuration options for #{family}|]

    inspect defaults options family =
      Schema.inspectOptions (const False) defaults defaults (Schema.prefixOptions ["llm", family, "*"] options)

configDocumentSections :: ConfigDocument -> ConfigDocument -> [Schema.ConfigSection]
configDocumentSections document activeDocument =
  configSchema.sections
    <> Schema.prefixSections ["llm"] LLMConfig.schema.sections
    <> providerSections "chat_provider" (.chatProviders)
    <> providerSections "image_provider" (.imageProviders)
    <> providerSections "audio_provider" (.audioProviders)
  where
    providerSections
      :: Text
      -> (LLMConfig.FileConfig -> Map Text provider)
      -> [Schema.ConfigSection]
    providerSections family getter =
      [ Schema.ConfigSection
          { path = ["llm", family, name]
          , label = name
          , group = metadata.group
          , optional = False
          }
      | name <- toList (Map.keysSet (getter document.fileConfig.llm) `mappend` Map.keysSet (getter activeDocument.fileConfig.llm))
      ]
      where
        metadata = fromMaybe (error [i|missing repeatable configuration metadata for #{family}|]) $
          find ((== [family]) . (.path)) LLMConfig.schema.repeatableSections

groupJson :: Schema.ConfigGroup -> Aeson.Value
groupJson sectionGroup = Aeson.object
  [ "path" Aeson..= sectionGroup.path
  , "label" Aeson..= sectionGroup.label
  ]

configPathPresent :: ConfigDocument -> [Text] -> Bool
configPathPresent document = isJust . lookupTablePath document.table

configSectionPresent :: ConfigDocument -> [Text] -> Bool
configSectionPresent document = isJust . lookupTablePath document.table

lookupTablePath :: TomlValue.Table' annotation -> [Text] -> Maybe (TomlValue.Value' annotation)
lookupTablePath _ [] = Nothing
lookupTablePath (TomlValue.MkTable values) [key] = snd <$> Map.lookup key values
lookupTablePath (TomlValue.MkTable values) (key : rest) = do
  (_, TomlValue.Table' _ nestedTable) <- Map.lookup key values
  lookupTablePath nestedTable rest

safeInit :: [a] -> [a]
safeInit [] = []
safeInit [_] = []
safeInit (value : rest) = value : safeInit rest

isRepeatableInstance :: [Text] -> Bool
isRepeatableInstance path = case path of
  ["llm", family, _] -> family `elem` ["chat_provider", "image_provider", "audio_provider"]
  _ -> False

data DriverFileConfig = DriverFileConfig
  { qq       :: !(Maybe QQConfig.FileConfig)
  , telegram :: !(Maybe TelegramConfig.FileConfig)
  , matrix   :: !(Maybe MatrixConfig.FileConfig)
  , discord :: !(Maybe DiscordConfig.FileConfig)
  }
  deriving (Show)

defaultDriverFileConfig :: DriverFileConfig
defaultDriverFileConfig = DriverFileConfig
  { qq = Nothing
  , telegram = Nothing
  , matrix = Nothing
  , discord = Nothing
  }

instance FromValue DriverFileConfig where
  fromValue = parseTableFromValue $ DriverFileConfig
    <$> optKey "qq"
    <*> optKey "telegram"
    <*> optKey "matrix"
    <*> optKey "discord"

newtype ResourceFileConfig = ResourceFileConfig
  { sandbox :: Sandbox.Config
  }
  deriving (Show)

defaultResourceFileConfig :: ResourceFileConfig
defaultResourceFileConfig = ResourceFileConfig
  { sandbox = Sandbox.defaultConfig
  }

instance FromValue ResourceFileConfig where
  fromValue = parseTableFromValue $ ResourceFileConfig
    <$> fmap (fromMaybe Sandbox.defaultConfig) (optKey "sandbox")

data HandlerFileConfig = HandlerFileConfig
  { admin   :: !AdminConfig
  , saucenao :: !SaucenaoConfig
  , ask      :: !AskHandlerConfig
  , console  :: !ConsoleHandlerConfig
  , shutup   :: !ShutUpConfig
  }
  deriving (Show)

instance FromValue HandlerFileConfig where
  fromValue = parseTableFromValue $ HandlerFileConfig
    <$> fmap (fromMaybe AdminConfig.defaultAdminConfig) (optKey "admin")
    <*> fmap (fromMaybe SaucenaoConfig.defaultSaucenaoConfig) (optKey "saucenao")
    <*> reqKey "ask"
    <*> reqKey "console"
    <*> fmap (fromMaybe ShutUpConfig.defaultShutUpConfig) (optKey "shutup")

newtype LogFileConfig = LogFileConfig
  { level :: Severity
  }
  deriving (Show)

newtype ConfigLogLevel = ConfigLogLevel
  { unConfigLogLevel :: Severity
  }
  deriving (Show)

defaultLogFileConfig :: LogFileConfig
defaultLogFileConfig = LogFileConfig
  { level = InfoS
  }

instance FromValue LogFileConfig where
  fromValue = parseTableFromValue do
    level <- fmap (\(ConfigLogLevel value) -> value) <$> optKey "level"
    pure LogFileConfig
      { level = fromMaybe defaultLogFileConfig.level level
      }

newtype StorageFileConfig = StorageFileConfig
  { sqlitePath :: FilePath
  }
  deriving (Show)

defaultStorageFileConfig :: StorageFileConfig
defaultStorageFileConfig = StorageFileConfig
  { sqlitePath = "cosmobot.sqlite3"
  }

instance FromValue StorageFileConfig where
  fromValue = parseTableFromValue do
    sqlitePath <- fromMaybe defaultStorageFileConfig.sqlitePath <$> optKey "sqlite_path"
    pure StorageFileConfig{sqlitePath}

instance FromValue ConfigLogLevel where
  fromValue = \case
    TomlValue.Text' _ value ->
      case textToSeverity value of
        Just level -> pure (ConfigLogLevel level)
        Nothing    -> fail [i|invalid log.level #{value}; expected debug, info, notice, warning, error, critical, alert, or emergency|]
    _ ->
      fail "log.level must be a string"

toBotConfig :: FilePath -> FileConfig -> BotConfig
toBotConfig configPath cfg =
  let
    qqFileConfig = cfg.driver.qq
    telegramFileConfig = cfg.driver.telegram
    matrixFileConfig = cfg.driver.matrix
    discordFileConfig = cfg.driver.discord
    askConfig = cfg.handler.ask
      { botIds = configuredBotIds qqFileConfig telegramFileConfig matrixFileConfig discordFileConfig
      }
  in
  BotConfig
    { qq = QQConfig.toRuntimeConfig <$> qqFileConfig
    , telegram = TelegramConfig.toRuntimeConfig <$> telegramFileConfig
    , matrix = MatrixConfig.toRuntimeConfig <$> (matrixFileConfig >>= configuredMatrixFileConfig)
    , discord = DiscordConfig.toRuntimeConfig <$> (discordFileConfig >>= configuredDiscordFileConfig)
    , llm = LLMConfig.toRuntimeConfig cfg.llm
    , media = cfg.media
    , tool = (AgentConfig.toToolConfig cfg.tool)
        { Agent.sandboxImage = cfg.resource.sandbox.image
        }
    , saucenao = cfg.handler.saucenao
    , memory = MemoryConfig.toMemoryConfig cfg.memory
    , skills = SkillsConfig.toSkillsConfig cfg.skills
    , rpc = RPCConfig.toRuntimeConfig cfg.rpc
    , acp = ACPConfig.toRuntimeConfig cfg.acp
    , plugins = cfg.plugins
        { pluginDir = resolveBesideConfig configPath cfg.plugins.pluginDir
        }
    , handlers = HandlersConfig
        { admin = cfg.handler.admin
        , ask = askConfig
        , console = cfg.handler.console
        , shutup = cfg.handler.shutup
        }
    , logLevel = cfg.log.level
    , sqlitePath = cfg.storage.sqlitePath
    }

resolveBesideConfig :: FilePath -> FilePath -> FilePath
resolveBesideConfig configPath path
  | isRelative path = normalise (takeDirectory configPath </> path)
  | otherwise = normalise path

configuredBotIds :: Maybe QQConfig.FileConfig -> Maybe TelegramConfig.FileConfig -> Maybe MatrixConfig.FileConfig -> Maybe DiscordConfig.FileConfig -> [(ChatPlatform, Text)]
configuredBotIds qqCfg telegramCfg matrixCfg discordCfg =
  catMaybes
    [ (PlatformQQ,) . Text.pack . show <$> (qqCfg >>= (.botId))
    , (PlatformTelegram,) <$> (telegramCfg >>= telegramBotIdText . (.botId))
    , (PlatformMatrix,) <$> (matrixCfg >>= (.botId))
    , (PlatformDiscord,) <$> (discordCfg >>= (.botId))
    ]

configuredMatrixFileConfig :: MatrixConfig.FileConfig -> Maybe MatrixConfig.FileConfig
configuredMatrixFileConfig cfg =
  cfg <$ guard (isJust cfg.loginUser && isJust cfg.loginPassword)

configuredDiscordFileConfig :: DiscordConfig.FileConfig -> Maybe DiscordConfig.FileConfig
configuredDiscordFileConfig cfg =
  cfg <$ guard (not (Text.null (Text.strip cfg.botToken)))

telegramBotIdText :: Maybe TelegramConfig.TelegramBotId -> Maybe Text
telegramBotIdText = \case
  Just (TelegramConfig.TelegramBotNumeric botId) ->
    Just (Text.pack (show botId))
  Just (TelegramConfig.TelegramBotUsername username) ->
    Just (TelegramConfig.normalizeUsername username)
  Nothing ->
    Nothing
