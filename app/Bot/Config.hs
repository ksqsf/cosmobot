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
  , TelegramChatRef (..)
  , SaucenaoConfig (..)
  , loadConfig

    -- * Handler predicates
  , isAllowedGroup
  , isAllowedPrivate
  , isSuperuser
  , canStartConversation
  , canStartFromReply
  , mentionsConfiguredBot
  , normalizeUsername
  )
where

import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Chat.QQ as QQ
import qualified Bot.Effect.Chat.Telegram as Telegram
import Bot.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Toml.Semantics.Types as TomlValue
import qualified Toml
import Toml.Schema

-- | Fully normalized runtime configuration.
data BotConfig = BotConfig
  { qq       :: !QQ.Config
  , telegram :: !Telegram.Config
  , llm      :: !LLM.Config
  , saucenao :: !SaucenaoConfig
  , handlers :: !HandlersConfig
  , logLevel :: !LogLevel
  , sqlitePath :: !FilePath
  }
  deriving (Show)

-- | Configuration for all handler groups.
newtype HandlersConfig = HandlersConfig
  { ask :: AskHandlerConfig
  }
  deriving (Show)

-- | Admission, identity, and prompt settings for the ask handler.
data AskHandlerConfig = AskHandlerConfig
  { command          :: !Text
  , drawCommand      :: !Text
  , qqGroupWhitelist :: ![Integer]
  , telegramChatWhitelist :: ![TelegramChatRef]
  , qqSuperusers     :: ![Integer]
  , telegramSuperusers :: ![Text]
  , botQQ            :: !(Maybe Integer)
  , botTelegramIds   :: ![Integer]
  , botTelegramUsernames :: ![Text]
  , systemPrompt     :: !Text
  , agentMaxTurns    :: !Int
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
  , qq       :: !QQFileConfig
  , telegram :: !TelegramFileConfig
  , llm      :: !LLMFileConfig
  , saucenao :: !SaucenaoConfig
  , handlers :: !HandlersConfig
  }
  deriving (Show)

instance FromValue FileConfig where
  fromValue = parseTableFromValue $ FileConfig
    <$> fmap (fromMaybe defaultLogFileConfig) (optKey "log")
    <*> fmap (fromMaybe defaultStorageFileConfig) (optKey "storage")
    <*> reqKey "qq"
    <*> reqKey "telegram"
    <*> reqKey "llm"
    <*> fmap (fromMaybe defaultSaucenaoConfig) (optKey "saucenao")
    <*> reqKey "handlers"

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

data QQFileConfig = QQFileConfig
  { host  :: !String
  , port  :: !Int
  , path  :: !String
  , token :: !(Maybe Text)
  , botQQ :: !(Maybe Integer)
  , superusers :: ![Integer]
  }
  deriving (Show)

instance FromValue QQFileConfig where
  fromValue = parseTableFromValue $ QQFileConfig
    <$> reqKey "host"
    <*> reqKey "port"
    <*> reqKey "path"
    <*> optToken "token"
    <*> optKey "bot_qq"
    <*> fmap (fromMaybe []) (optKey "superusers")

data TelegramFileConfig = TelegramFileConfig
  { botToken :: !Text
  , botId    :: !(Maybe TelegramBotId)
  , superusers :: ![Text]
  }
  deriving (Show)

data TelegramBotId
  = TelegramBotNumeric !Integer
  | TelegramBotUsername !Text
  deriving (Show)

-- | Telegram chat whitelist entry, by numeric id or username/title.
data TelegramChatRef
  = TelegramChatNumeric !Integer
  | TelegramChatUsername !Text
  deriving (Eq, Show)

instance FromValue TelegramBotId where
  fromValue = \case
    TomlValue.Integer' _ value ->
      pure (TelegramBotNumeric value)
    TomlValue.Text' _ value ->
      pure (TelegramBotUsername (normalizeUsername value))
    _ ->
      fail "telegram.bot_id must be an integer id or a username string"

instance FromValue TelegramChatRef where
  fromValue = \case
    TomlValue.Integer' _ value ->
      pure (TelegramChatNumeric value)
    TomlValue.Text' _ value ->
      pure (TelegramChatUsername (normalizeUsername value))
    _ ->
      fail "handlers.ask.telegram_chat_whitelist entries must be integer chat ids or username strings"

instance FromValue TelegramFileConfig where
  fromValue = parseTableFromValue $ TelegramFileConfig
    <$> reqKey "bot_token"
    <*> optKey "bot_id"
    <*> fmap (fromMaybe []) (optKey "superusers")

data LLMFileConfig = LLMFileConfig
  { endpoint :: !Text
  , apiKey   :: !(Maybe Text)
  , model    :: !Text
  , webSearch :: !Bool
  , webSearchMaxResults :: !(Maybe Int)
  , webFetch :: !Bool
  , webFetchMaxUses :: !(Maybe Int)
  , webFetchMaxContentTokens :: !(Maybe Int)
  , datetime :: !Bool
  , imageGeneration :: !Bool
  , imageGenerationModel :: !(Maybe Text)
  , imageGenerationQuality :: !(Maybe Text)
  , imageGenerationSize :: !(Maybe Text)
  , imageGenerationAspectRatio :: !(Maybe Text)
  , imageGenerationBackground :: !(Maybe Text)
  , imageGenerationOutputFormat :: !(Maybe Text)
  , imageGenerationModeration :: !(Maybe Text)
  }
  deriving (Show)

-- | SauceNAO integration settings.
newtype SaucenaoConfig = SaucenaoConfig
  { apiKey :: Maybe Text
  }
  deriving (Show)

defaultSaucenaoConfig :: SaucenaoConfig
defaultSaucenaoConfig = SaucenaoConfig
  { apiKey = Nothing
  }

instance FromValue SaucenaoConfig where
  fromValue = parseTableFromValue $ SaucenaoConfig
    <$> optToken "api_key"

instance FromValue LLMFileConfig where
  fromValue = parseTableFromValue $ LLMFileConfig
    <$> fmap (fromMaybe LLM.defaultConfig.endpoint) (optKey "endpoint")
    <*> optToken "api_key"
    <*> reqKey "model"
    <*> fmap (fromMaybe LLM.defaultConfig.webSearch) (optKey "web_search")
    <*> optKey "web_search_max_results"
    <*> fmap (fromMaybe LLM.defaultConfig.webFetch) (optKey "web_fetch")
    <*> optKey "web_fetch_max_uses"
    <*> optKey "web_fetch_max_content_tokens"
    <*> fmap (fromMaybe LLM.defaultConfig.datetime) (optKey "datetime")
    <*> fmap (fromMaybe LLM.defaultConfig.imageGeneration) (optKey "image_generation")
    <*> optKey "image_generation_model"
    <*> optKey "image_generation_quality"
    <*> optKey "image_generation_size"
    <*> optKey "image_generation_aspect_ratio"
    <*> optKey "image_generation_background"
    <*> optKey "image_generation_output_format"
    <*> optKey "image_generation_moderation"

instance FromValue HandlersConfig where
  fromValue = parseTableFromValue $ HandlersConfig
    <$> reqKey "ask"

instance FromValue AskHandlerConfig where
  fromValue = parseTableFromValue do
    command <- reqKey "command"
    drawCommand <- fromMaybe "!draw" <$> optKey "draw_command"
    qqGroupWhitelist <- reqKey "qq_group_whitelist"
    telegramChatWhitelist <- fromMaybe [] <$> optKey "telegram_chat_whitelist"
    systemPrompt <- reqKey "system_prompt"
    agentMaxTurns <- fromMaybe 4 <$> optKey "agent_max_turns"
    pure AskHandlerConfig
      { command = command
      , drawCommand = drawCommand
      , qqGroupWhitelist = qqGroupWhitelist
      , telegramChatWhitelist = telegramChatWhitelist
      , qqSuperusers = []
      , telegramSuperusers = []
      , botQQ = Nothing
      , botTelegramIds = []
      , botTelegramUsernames = []
      , systemPrompt = systemPrompt
      , agentMaxTurns = agentMaxTurns
      }

optToken :: Text -> ParseTable l (Maybe Text)
optToken key = normalizeToken <$> optKey key

normalizeToken :: Maybe Text -> Maybe Text
normalizeToken = \case
  Nothing -> Nothing
  Just "" -> Nothing
  Just t  -> Just t

toBotConfig :: FileConfig -> BotConfig
toBotConfig cfg =
  BotConfig
    { qq = QQ.Config
        { host  = cfg.qq.host
        , port  = cfg.qq.port
        , path  = cfg.qq.path
        , token = cfg.qq.token
        }
    , telegram = Telegram.Config
        { botToken = cfg.telegram.botToken
        }
    , llm = LLM.Config
        { endpoint = cfg.llm.endpoint
        , apiKey   = cfg.llm.apiKey
        , model    = cfg.llm.model
        , webSearch = cfg.llm.webSearch
        , webSearchMaxResults = cfg.llm.webSearchMaxResults
        , webFetch = cfg.llm.webFetch
        , webFetchMaxUses = cfg.llm.webFetchMaxUses
        , webFetchMaxContentTokens = cfg.llm.webFetchMaxContentTokens
        , datetime = cfg.llm.datetime
        , imageGeneration = cfg.llm.imageGeneration
        , imageGenerationModel = cfg.llm.imageGenerationModel
        , imageGenerationQuality = cfg.llm.imageGenerationQuality
        , imageGenerationSize = cfg.llm.imageGenerationSize
        , imageGenerationAspectRatio = cfg.llm.imageGenerationAspectRatio
        , imageGenerationBackground = cfg.llm.imageGenerationBackground
        , imageGenerationOutputFormat = cfg.llm.imageGenerationOutputFormat
        , imageGenerationModeration = cfg.llm.imageGenerationModeration
        }
    , saucenao = cfg.saucenao
    , handlers = withPlatformConfig cfg.qq cfg.telegram cfg.handlers
    , logLevel = cfg.log.level
    , sqlitePath = cfg.storage.sqlitePath
    }

withPlatformConfig :: QQFileConfig -> TelegramFileConfig -> HandlersConfig -> HandlersConfig
withPlatformConfig qq telegram (HandlersConfig askCfg) =
  HandlersConfig AskHandlerConfig
    { command = askCfg.command
    , drawCommand = askCfg.drawCommand
    , qqGroupWhitelist = askCfg.qqGroupWhitelist
    , telegramChatWhitelist = askCfg.telegramChatWhitelist
    , qqSuperusers = qq.superusers
    , telegramSuperusers = map normalizeUsername telegram.superusers
    , botQQ = qq.botQQ
    , botTelegramIds = telegramBotIds telegram.botId
    , botTelegramUsernames = telegramBotUsernames telegram.botId
    , systemPrompt = askCfg.systemPrompt
    , agentMaxTurns = askCfg.agentMaxTurns
    }

-- | Whether a group message is from an explicitly allowed chat.
isAllowedGroup :: AskHandlerConfig -> IncomingMessage -> Bool
isAllowedGroup cfg message =
  message.kind == ChatGroup && case message.platform of
    PlatformQQ ->
      maybe False (`elem` cfg.qqGroupWhitelist) message.chatId
    PlatformTelegram ->
      any (telegramChatAllowed message) cfg.telegramChatWhitelist

-- | Whether the message mentions one of the configured bot identities.
mentionsConfiguredBot :: AskHandlerConfig -> IncomingMessage -> Bool
mentionsConfiguredBot cfg message =
  case message.platform of
    PlatformQQ ->
      maybe False (`elem` message.mentions) cfg.botQQ
    PlatformTelegram ->
      any (`elem` message.mentions) cfg.botTelegramIds ||
        any (`elem` message.mentionUsernames) cfg.botTelegramUsernames

-- | Whether a private message may start a bot conversation.
isAllowedPrivate :: AskHandlerConfig -> IncomingMessage -> Bool
isAllowedPrivate cfg message =
  message.kind == ChatPrivate && case message.platform of
    PlatformQQ ->
      maybe False (`elem` cfg.qqSuperusers) message.senderId
    PlatformTelegram ->
      maybe False (`elem` map normalizeUsername cfg.telegramSuperusers) (normalizeUsername <$> message.senderUsername)

-- | Whether the message sender may use privileged agent tools.
isSuperuser :: AskHandlerConfig -> IncomingMessage -> Bool
isSuperuser cfg message =
  case message.platform of
    PlatformQQ ->
      maybe False (`elem` cfg.qqSuperusers) message.senderId
    PlatformTelegram ->
      maybe False (`elem` map normalizeUsername cfg.telegramSuperusers) (normalizeUsername <$> message.senderUsername)

-- | General admission predicate for starting a new conversation.
canStartConversation :: AskHandlerConfig -> IncomingMessage -> Bool
canStartConversation cfg message =
  case message.kind of
    ChatPrivate -> isAllowedPrivate cfg message
    ChatGroup   -> isAllowedGroup cfg message || isTelegramMention cfg message
    _           -> False

-- | Admission predicate for starting from a reply to an unknown message.
canStartFromReply :: AskHandlerConfig -> IncomingMessage -> Bool
canStartFromReply cfg message =
  case message.kind of
    ChatPrivate -> isAllowedPrivate cfg message
    ChatGroup   -> isAllowedGroup cfg message && mentionsConfiguredBot cfg message
    _           -> False

isTelegramMention :: AskHandlerConfig -> IncomingMessage -> Bool
isTelegramMention cfg message =
  message.platform == PlatformTelegram && mentionsConfiguredBot cfg message

-- | Normalize Telegram-style usernames for config and message comparison.
normalizeUsername :: Text -> Text
normalizeUsername =
  Text.toLower . Text.dropWhile (== '@') . Text.strip

telegramChatAllowed :: IncomingMessage -> TelegramChatRef -> Bool
telegramChatAllowed message = \case
  TelegramChatNumeric chatId ->
    message.chatId == Just chatId
  TelegramChatUsername username ->
    username `elem` telegramMessageChatNames message

telegramMessageChatNames :: IncomingMessage -> [Text]
telegramMessageChatNames message =
  map normalizeUsername $
    fromMaybe [] (AesonTypes.parseMaybe telegramChatNamesFromRaw message.raw)

telegramChatNamesFromRaw :: Aeson.Value -> AesonTypes.Parser [Text]
telegramChatNamesFromRaw =
  Aeson.withObject "TelegramMessage" $ \o -> do
    direct <- fromMaybe [] <$> do
      chat <- o Aeson..:? "chat"
      traverse telegramChatNamesFromChat chat
    original <- fromMaybe [] <$> do
      originalMessage <- o Aeson..:? "original_message"
      traverse telegramChatNamesFromRaw originalMessage
    pure (direct <> original)

telegramChatNamesFromChat :: Aeson.Value -> AesonTypes.Parser [Text]
telegramChatNamesFromChat =
  Aeson.withObject "TelegramChat" $ \o -> do
    username <- o Aeson..:? "username"
    title <- o Aeson..:? "title"
    pure (catMaybes [username, title])

telegramBotIds :: Maybe TelegramBotId -> [Integer]
telegramBotIds = \case
  Just (TelegramBotNumeric botId) -> [botId]
  _ -> []

telegramBotUsernames :: Maybe TelegramBotId -> [Text]
telegramBotUsernames = \case
  Just (TelegramBotUsername username) -> [normalizeUsername username]
  _ -> []
