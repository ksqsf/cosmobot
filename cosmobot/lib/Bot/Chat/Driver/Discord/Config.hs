{-|
Module      : Bot.Chat.Driver.Discord.Config
Description : Discord driver file configuration
Stability   : experimental
-}

module Bot.Chat.Driver.Discord.Config
  ( FileConfig (..)
  , defaultFileConfig
  , toRuntimeConfig
  , schema
  )
where

import qualified Bot.Chat.Driver.Discord.Types as Discord
import qualified Bot.Config.Schema as Schema
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Toml.Semantics.Types as TomlValue
import Toml.Schema
import qualified Prelude

data FileConfig = FileConfig
  { botToken :: !Text
  , botId :: !(Maybe Text)
  , applicationId :: !(Maybe Text)
  , allowedGuilds :: ![Integer]
  , allowedChannels :: ![Integer]
  , allowedUsers :: ![Text]
  , superusers :: ![Text]
  , gatewayHost :: !String
  , gatewayPath :: !String
  }

instance Show FileConfig where
  showsPrec _ _ = Prelude.showString "<Discord.FileConfig>"

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { botToken = ""
  , botId = Nothing
  , applicationId = Nothing
  , allowedGuilds = []
  , allowedChannels = []
  , allowedUsers = []
  , superusers = []
  , gatewayHost = "gateway.discord.gg"
  , gatewayPath = "/?v=10&encoding=json"
  }

schema :: Schema.ConfigSchema FileConfig Discord.Config
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue $ FileConfig
    <$> fmap (fromMaybe defaultFileConfig.botToken) (optKey "bot_token")
    <*> optSnowflakeText "bot_id"
    <*> optSnowflakeText "application_id"
    <*> fmap (fromMaybe []) (optKey "allowed_guilds")
    <*> fmap (fromMaybe []) (optKey "allowed_channels")
    <*> fmap (maybe [] (map discordSnowflakeText)) (optKey "allowed_users")
    <*> fmap (maybe [] (map discordSnowflakeText)) (optKey "superusers")
    <*> fmap (fromMaybe defaultFileConfig.gatewayHost) (optKey "gateway_host")
    <*> fmap (fromMaybe defaultFileConfig.gatewayPath) (optKey "gateway_path")
  , Schema.options =
      [ Schema.option ["bot_token"] "Bot token" "Discord bot token." owner Schema.secret (Schema.Secret defaultFileConfig.botToken) Aeson.Null (Schema.Secret . (.botToken)) (Schema.Secret . (.botToken))
      , snowflake "bot_id" "Bot ID" "Discord bot user id." (.botId) (.botId)
      , snowflake "application_id" "Application ID" "Discord application id." (.applicationId) (.applicationId)
      , Schema.option ["allowed_guilds"] "Allowed guilds" "Discord guild ids allowed to use the bot." owner (Schema.list "integer") [] Aeson.Null (.allowedGuilds) (.allowedGuilds)
      , Schema.option ["allowed_channels"] "Allowed channels" "Discord channel ids allowed to use the bot." owner (Schema.list "integer") [] Aeson.Null (.allowedChannels) (.allowedChannels)
      , identities "allowed_users" "Allowed users" "Discord user ids allowed to use the bot." (.allowedUsers) (.allowedUsers)
      , identities "superusers" "Superusers" "Discord users with administrative access." (.superusers) (.superusers)
      , Schema.option ["gateway_host"] "Gateway host" "Discord gateway host." owner Schema.text (toText defaultFileConfig.gatewayHost) Aeson.Null (toText . (.gatewayHost)) (toText . (.gatewayHost))
      , Schema.option ["gateway_path"] "Gateway path" "Discord gateway websocket path." owner Schema.text (toText defaultFileConfig.gatewayPath) Aeson.Null (toText . (.gatewayPath)) (toText . (.gatewayPath))
      ]
  }
  where
    owner = "Bot.Chat.Driver.Discord.Config"
    snowflake key label description source runtime = Schema.optionalOption [key] label description owner Schema.identity False Aeson.Null (fmap Aeson.toJSON . source) (fmap Aeson.toJSON . runtime)
    identities key label description source runtime = Schema.option [key] label description owner Schema.identityList [] Aeson.Null (map Aeson.toJSON . source) (map Aeson.toJSON . runtime)

instance FromValue FileConfig where
  fromValue = Schema.schemaFromValue schema

toRuntimeConfig :: FileConfig -> Discord.Config
toRuntimeConfig cfg =
  Discord.Config
    { botToken = cfg.botToken
    , botId = cfg.botId
    , applicationId = cfg.applicationId
    , allowedGuilds = cfg.allowedGuilds
    , allowedChannels = cfg.allowedChannels
    , allowedUsers = cfg.allowedUsers
    , superusers = cfg.superusers
    , gatewayHost = cfg.gatewayHost
    , gatewayPath = cfg.gatewayPath
    }

optSnowflakeText :: Text -> ParseTable l (Maybe Text)
optSnowflakeText key =
  fmap discordSnowflakeText <$> optKey key

newtype DiscordSnowflake = DiscordSnowflake Text

discordSnowflakeText :: DiscordSnowflake -> Text
discordSnowflakeText (DiscordSnowflake value) =
  value

instance FromValue DiscordSnowflake where
  fromValue = \case
    TomlValue.Text' _ value ->
      pure (DiscordSnowflake (Text.strip value))
    TomlValue.Integer' _ value ->
      pure (DiscordSnowflake (show value))
    _ ->
      fail "expected Discord snowflake string or integer"
