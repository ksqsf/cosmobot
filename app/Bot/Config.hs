{-|
Module      : Bot.Config
Description : Application configuration
Stability   : experimental
-}

module Bot.Config
  ( -- * Top-level configuration
    BotConfig (..)
  , HandlersConfig (..)
  , AskHandlerConfig (..)
  , SaucenaoConfig (..)
  , Memory.MemoryConfig (..)
  , loadConfig
  )
where

import qualified Bot.Chat.Driver.QQ as QQ
import qualified Bot.Chat.Driver.QQ.Config as QQConfig
import qualified Bot.Chat.Driver.Matrix as Matrix
import qualified Bot.Chat.Driver.Matrix.Config as MatrixConfig
import qualified Bot.Chat.Driver.Telegram as Telegram
import qualified Bot.Chat.Driver.Telegram.Config as TelegramConfig
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.LLM.Config as LLMConfig
import qualified Bot.Agent.Types as Agent
import qualified Bot.Agent.Config as AgentConfig
import Bot.Core.Message (ChatPlatform (..))
import Bot.Handler.Ask.Config
  ( AskHandlerConfig (..)
  , HandlersConfig (..)
  )
import qualified Bot.Handler.Saucenao.Config as SaucenaoConfig
import Bot.Handler.Saucenao.Config
  ( SaucenaoConfig (..)
  )
import qualified Bot.Memory as Memory
import qualified Bot.Memory.Config as MemoryConfig
import Bot.Prelude
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Toml.Semantics.Types as TomlValue
import qualified Toml
import Toml.Schema

-- | Fully normalized runtime configuration.
data BotConfig = BotConfig
  { qq       :: !QQ.Config
  , telegram :: !Telegram.Config
  , matrix   :: !Matrix.Config
  , llm      :: !LLM.Config
  , tool     :: !Agent.ToolConfig
  , saucenao :: !SaucenaoConfig
  , memory   :: !Memory.MemoryConfig
  , handlers :: !HandlersConfig
  , logLevel :: !LogLevel
  , sqlitePath :: !FilePath
  }
  deriving (Show)

-- | Read and normalize the TOML configuration used by the executable.
loadConfig :: FilePath -> IO BotConfig
loadConfig path = do
  content <- TextIO.readFile path
  case Toml.decode content of
    Toml.Failure errors ->
      fail [i|Failed to parse #{path}: #{unlines (map toText errors)}|]
    Toml.Success warnings config_ -> do
      traverse_ (putStrLn . ("TOML warning: " <>)) warnings
      pure (toBotConfig config_)

data FileConfig = FileConfig
  { log      :: !LogFileConfig
  , storage  :: !StorageFileConfig
  , driver   :: !DriverFileConfig
  , llm      :: !LLMConfig.FileConfig
  , tool     :: !AgentConfig.FileConfig
  , memory   :: !MemoryConfig.FileConfig
  , handler  :: !HandlerFileConfig
  }
  deriving (Show)

instance FromValue FileConfig where
  fromValue = parseTableFromValue $ FileConfig
    <$> fmap (fromMaybe defaultLogFileConfig) (optKey "log")
    <*> fmap (fromMaybe defaultStorageFileConfig) (optKey "storage")
    <*> reqKey "driver"
    <*> reqKey "llm"
    <*> fmap (fromMaybe AgentConfig.defaultFileConfig) (optKey "tool")
    <*> fmap (fromMaybe MemoryConfig.defaultFileConfig) (optKey "memory")
    <*> reqKey "handler"

data DriverFileConfig = DriverFileConfig
  { qq       :: !QQConfig.FileConfig
  , telegram :: !TelegramConfig.FileConfig
  , matrix   :: !MatrixConfig.FileConfig
  }
  deriving (Show)

instance FromValue DriverFileConfig where
  fromValue = parseTableFromValue $ DriverFileConfig
    <$> reqKey "qq"
    <*> reqKey "telegram"
    <*> fmap (fromMaybe MatrixConfig.defaultFileConfig) (optKey "matrix")

data HandlerFileConfig = HandlerFileConfig
  { saucenao :: !SaucenaoConfig
  , ask      :: !AskHandlerConfig
  }
  deriving (Show)

instance FromValue HandlerFileConfig where
  fromValue = parseTableFromValue $ HandlerFileConfig
    <$> fmap (fromMaybe SaucenaoConfig.defaultSaucenaoConfig) (optKey "saucenao")
    <*> reqKey "ask"

newtype LogFileConfig = LogFileConfig
  { level :: LogLevel
  }
  deriving (Show)

newtype ConfigLogLevel = ConfigLogLevel
  { unConfigLogLevel :: LogLevel
  }
  deriving (Show)

defaultLogFileConfig :: LogFileConfig
defaultLogFileConfig = LogFileConfig
  { level = LogInfo
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
      case readLogLevelEither value of
        Right level -> pure (ConfigLogLevel level)
        Left err    -> fail err
    _ ->
      fail "log.level must be a string"

toBotConfig :: FileConfig -> BotConfig
toBotConfig cfg =
  let
    qqFileConfig = cfg.driver.qq
    telegramFileConfig = cfg.driver.telegram
    matrixFileConfig = cfg.driver.matrix
    askConfig = cfg.handler.ask
      { botIds = configuredBotIds qqFileConfig telegramFileConfig matrixFileConfig
      }
  in
  BotConfig
    { qq = QQConfig.toRuntimeConfig qqFileConfig
    , telegram = TelegramConfig.toRuntimeConfig telegramFileConfig
    , matrix = MatrixConfig.toRuntimeConfig matrixFileConfig
    , llm = LLMConfig.toRuntimeConfig cfg.llm
    , tool = AgentConfig.toToolConfig cfg.tool
    , saucenao = cfg.handler.saucenao
    , memory = MemoryConfig.toMemoryConfig cfg.memory
    , handlers = HandlersConfig askConfig
    , logLevel = cfg.log.level
    , sqlitePath = cfg.storage.sqlitePath
    }

configuredBotIds :: QQConfig.FileConfig -> TelegramConfig.FileConfig -> MatrixConfig.FileConfig -> [(ChatPlatform, Text)]
configuredBotIds qqCfg telegramCfg matrixCfg =
  catMaybes
    [ (PlatformQQ,) . Text.pack . show <$> qqCfg.botId
    , (PlatformTelegram,) <$> telegramBotIdText telegramCfg.botId
    , (PlatformMatrix,) <$> matrixCfg.botId
    ]

telegramBotIdText :: Maybe TelegramConfig.TelegramBotId -> Maybe Text
telegramBotIdText = \case
  Just (TelegramConfig.TelegramBotNumeric botId) ->
    Just (Text.pack (show botId))
  Just (TelegramConfig.TelegramBotUsername username) ->
    Just (TelegramConfig.normalizeUsername username)
  Nothing ->
    Nothing
