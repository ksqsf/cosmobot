{-|
Module      : Bot.Chat.Driver.Telegram.Config
Description : Telegram driver file configuration
Stability   : experimental
-}

module Bot.Chat.Driver.Telegram.Config
  ( FileConfig (..)
  , TelegramBotId (..)
  , normalizeUsername
  , telegramBotIds
  , telegramBotUsernames
  , toRuntimeConfig
  )
where

import qualified Bot.Chat.Driver.Telegram as Telegram
import Bot.Prelude
import qualified Data.Text as Text
import qualified Toml.Semantics.Types as TomlValue
import Toml.Schema

data FileConfig = FileConfig
  { botToken :: !Text
  , botId    :: !(Maybe TelegramBotId)
  , superusers :: ![Text]
  }
  deriving (Show)

data TelegramBotId
  = TelegramBotNumeric !Integer
  | TelegramBotUsername !Text
  deriving (Show)

instance FromValue TelegramBotId where
  fromValue = \case
    TomlValue.Integer' _ value ->
      pure (TelegramBotNumeric value)
    TomlValue.Text' _ value ->
      pure (TelegramBotUsername (normalizeUsername value))
    _ ->
      fail "driver.telegram.bot_id must be an integer id or a username string"

instance FromValue FileConfig where
  fromValue = parseTableFromValue $ FileConfig
    <$> reqKey "bot_token"
    <*> optKey "bot_id"
    <*> fmap (fromMaybe []) (optKey "superusers")

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

toRuntimeConfig :: FileConfig -> Telegram.Config
toRuntimeConfig cfg =
  Telegram.Config
    { botToken = cfg.botToken
    }
