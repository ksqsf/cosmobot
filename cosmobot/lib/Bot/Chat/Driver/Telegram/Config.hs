{-|
Module      : Bot.Chat.Driver.Telegram.Config
Description : Telegram driver file configuration
Stability   : experimental
-}

module Bot.Chat.Driver.Telegram.Config
  ( FileConfig (..)
  , TelegramBotId (..)
  , TelegramChatRef (..)
  , normalizeUsername
  , telegramBotIds
  , telegramBotUsernames
  , toRuntimeConfig
  , schema
  )
where

import qualified Bot.Chat.Driver.Telegram as Telegram
import qualified Bot.Config.Schema as Schema
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Toml.Semantics.Types as TomlValue
import Toml.Schema
import qualified Prelude

data FileConfig = FileConfig
  { botToken :: !Text
  , botId    :: !(Maybe TelegramBotId)
  , allowedChats :: ![TelegramChatRef]
  , superusers :: ![Text]
  }

instance Show FileConfig where
  showsPrec _ _ = Prelude.showString "<Telegram.FileConfig>"

data TelegramBotId
  = TelegramBotNumeric !Integer
  | TelegramBotUsername !Text
  deriving (Show)

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
      fail "driver.telegram.bot_id must be an integer id or a username string"

instance FromValue TelegramChatRef where
  fromValue = \case
    TomlValue.Integer' _ value ->
      pure (TelegramChatNumeric value)
    TomlValue.Text' _ value ->
      pure (TelegramChatUsername (normalizeUsername value))
    _ ->
      fail "driver.telegram.allowed_chats entries must be integer chat ids or username/title strings"

schema :: Schema.ConfigSchema FileConfig Telegram.Config
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue $ FileConfig
    <$> reqKey "bot_token"
    <*> optKey "bot_id"
    <*> fmap (fromMaybe []) (optKey "allowed_chats")
    <*> fmap (fromMaybe []) (optKey "superusers")
  , Schema.options =
      [ Schema.optionalOption ["bot_token"] "Bot token" "Telegram Bot API token." owner Schema.secret True Aeson.Null (Just . Schema.Secret . (.botToken)) (Just . Schema.Secret . (.botToken))
      , Schema.optionalOption ["bot_id"] "Bot ID" "Telegram numeric bot id or username." owner Schema.identity False Aeson.Null (fmap botIdValue . (.botId)) runtimeBotId
      , Schema.option ["allowed_chats"] "Allowed chats" "Allowed numeric chat ids or aliases." owner Schema.identityList [] Aeson.Null (map chatRefValue . (.allowedChats)) runtimeAllowedChats
      , Schema.option ["superusers"] "Superusers" "Telegram usernames with administrative access." owner (Schema.list "text") [] Aeson.Null (.superusers) (.superusers)
      ]
  }
  where owner = "Bot.Chat.Driver.Telegram.Config"

instance FromValue FileConfig where
  fromValue = Schema.schemaFromValue schema

botIdValue :: TelegramBotId -> Aeson.Value
botIdValue = \case
  TelegramBotNumeric value -> Aeson.toJSON value
  TelegramBotUsername value -> Aeson.toJSON value

chatRefValue :: TelegramChatRef -> Aeson.Value
chatRefValue = \case
  TelegramChatNumeric value -> Aeson.toJSON value
  TelegramChatUsername value -> Aeson.toJSON value

runtimeBotId :: Telegram.Config -> Maybe Aeson.Value
runtimeBotId cfg =
  Aeson.toJSON <$> viaNonEmpty head cfg.botIds
    <|> Aeson.toJSON <$> viaNonEmpty head cfg.botUsernames

runtimeAllowedChats :: Telegram.Config -> [Aeson.Value]
runtimeAllowedChats cfg = map Aeson.toJSON cfg.allowedChatIds <> map Aeson.toJSON cfg.allowedChatAliases

normalizeUsername :: Text -> Text
normalizeUsername =
  Text.toLower . Text.dropWhile (== '@') . Text.strip

telegramBotIds :: Maybe TelegramBotId -> [Integer]
telegramBotIds = \case
  Just (TelegramBotNumeric botId) -> [botId]
  _ -> []

telegramBotUsernames :: Maybe TelegramBotId -> [Text]
telegramBotUsernames = \case
  Just (TelegramBotUsername username) -> [normalizeUsername username]
  _ -> []

telegramChatIds :: [TelegramChatRef] -> [Integer]
telegramChatIds =
  mapMaybe \case
    TelegramChatNumeric chatId -> Just chatId
    _ -> Nothing

telegramChatAliases :: [TelegramChatRef] -> [Text]
telegramChatAliases =
  mapMaybe \case
    TelegramChatUsername username -> Just (normalizeUsername username)
    _ -> Nothing

toRuntimeConfig :: FileConfig -> Telegram.Config
toRuntimeConfig cfg =
  Telegram.Config
    { botToken = cfg.botToken
    , botIds = telegramBotIds cfg.botId
    , botUsernames = telegramBotUsernames cfg.botId
    , allowedChatIds = telegramChatIds cfg.allowedChats
    , allowedChatAliases = telegramChatAliases cfg.allowedChats
    , superusers = map normalizeUsername cfg.superusers
    }
