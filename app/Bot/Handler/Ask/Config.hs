{-|
Module      : Bot.Handler.Ask.Config
Description : Ask handler configuration and admission predicates
Stability   : experimental
-}

module Bot.Handler.Ask.Config
  ( HandlersConfig (..)
  , AskHandlerConfig (..)
  , TelegramChatRef (..)
  , isAllowedGroup
  , isAllowedPrivate
  , isSuperuser
  , canStartConversation
  , canStartFromReply
  , mentionsConfiguredBot
  , normalizeUsername
  )
where

import qualified Bot.Chat.Driver.Telegram.Config as TelegramConfig
import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Toml.Semantics.Types as TomlValue
import Toml.Schema

-- | Configuration for all handler groups.
newtype HandlersConfig = HandlersConfig
  { ask :: AskHandlerConfig
  }
  deriving (Show)

-- | Admission, identity, and prompt settings for the ask handler.
data AskHandlerConfig = AskHandlerConfig
  { name             :: !(Maybe Text)
  , command          :: !Text
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

-- | Telegram chat whitelist entry, by numeric id or username/title.
data TelegramChatRef
  = TelegramChatNumeric !Integer
  | TelegramChatUsername !Text
  deriving (Eq, Show)

instance FromValue HandlersConfig where
  fromValue = parseTableFromValue $ HandlersConfig
    <$> reqKey "ask"

instance FromValue AskHandlerConfig where
  fromValue = parseTableFromValue do
    name <- optKey "name"
    command <- reqKey "command"
    drawCommand <- fromMaybe "!draw" <$> optKey "draw_command"
    qqGroupWhitelist <- reqKey "qq_group_whitelist"
    telegramChatWhitelist <- fromMaybe [] <$> optKey "telegram_chat_whitelist"
    systemPrompt <- reqKey "system_prompt"
    agentMaxTurns <- fromMaybe 4 <$> optKey "agent_max_turns"
    pure AskHandlerConfig
      { name = name
      , command = command
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

instance FromValue TelegramChatRef where
  fromValue = \case
    TomlValue.Integer' _ value ->
      pure (TelegramChatNumeric value)
    TomlValue.Text' _ value ->
      pure (TelegramChatUsername (normalizeUsername value))
    _ ->
      fail "handler.ask.telegram_chat_whitelist entries must be integer chat ids or username strings"

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
  TelegramConfig.normalizeUsername

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
