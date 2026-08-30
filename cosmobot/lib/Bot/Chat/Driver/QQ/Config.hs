{-|
Module      : Bot.Chat.Driver.QQ.Config
Description : QQ driver file configuration
Stability   : experimental
-}

module Bot.Chat.Driver.QQ.Config
  ( FileConfig (..)
  , toRuntimeConfig
  , schema
  )
where

import qualified Bot.Chat.Driver.QQ.Types as QQ
import qualified Bot.Config.Schema as Schema
import Bot.Util.Toml
import Bot.Prelude
import qualified Data.Aeson as Aeson
import Toml.Schema
import qualified Prelude

data FileConfig = FileConfig
  { host  :: !String
  , port  :: !Int
  , path  :: !String
  , token :: !(Maybe Text)
  , botId :: !(Maybe Integer)
  , allowedGroups :: ![Integer]
  , allowedUsers :: ![Integer]
  , superusers :: ![Integer]
  }

instance Show FileConfig where
  showsPrec _ _ = Prelude.showString "<QQ.FileConfig>"

schema :: Schema.ConfigSchema FileConfig QQ.Config
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue $ FileConfig
    <$> reqKey "host"
    <*> reqKey "port"
    <*> reqKey "path"
    <*> optToken "token"
    <*> optKey "bot_id"
    <*> fmap (fromMaybe []) (optKey "allowed_groups")
    <*> fmap (fromMaybe []) (optKey "allowed_users")
    <*> fmap (fromMaybe []) (optKey "superusers")
  , Schema.options =
      [ requiredText "host" "Host" "OneBot websocket host." (toText . (.host)) (toText . (.host))
      , requiredInt "port" "Port" "OneBot websocket port." (.port) (.port)
      , requiredText "path" "Path" "OneBot websocket path." (toText . (.path)) (toText . (.path))
      , Schema.optionalOption ["token"] "Token" "OneBot access token." owner Schema.secret False Aeson.Null (fmap Schema.Secret . (.token)) (fmap Schema.Secret . (.token))
      , Schema.optionalOption ["bot_id"] "Bot ID" "QQ bot account id." owner Schema.integer False Aeson.Null (.botId) (.botQQ)
      , Schema.option ["allowed_groups"] "Allowed groups" "QQ groups allowed to use the bot." owner (Schema.list "integer") [] Aeson.Null (.allowedGroups) (.allowedGroups)
      , Schema.option ["allowed_users"] "Allowed users" "QQ users allowed to use the bot." owner (Schema.list "integer") [] Aeson.Null (.allowedUsers) (.allowedUsers)
      , Schema.option ["superusers"] "Superusers" "QQ users with administrative access." owner (Schema.list "integer") [] Aeson.Null (.superusers) (.superusers)
      ]
  }
  where
    owner = "Bot.Chat.Driver.QQ.Config"
    requiredText key label description source runtime = Schema.optionalOption [key] label description owner Schema.text True Aeson.Null (Just . source) (Just . runtime)
    requiredInt key label description source runtime = Schema.optionalOption [key] label description owner Schema.integer True Aeson.Null (Just . source) (Just . runtime)

instance FromValue FileConfig where
  fromValue = Schema.schemaFromValue schema

toRuntimeConfig :: FileConfig -> QQ.Config
toRuntimeConfig cfg =
  QQ.Config
    { host = cfg.host
    , port = cfg.port
    , path = cfg.path
    , token = cfg.token
    , botQQ = cfg.botId
    , allowedGroups = cfg.allowedGroups
    , allowedUsers = cfg.allowedUsers
    , superusers = cfg.superusers
    }
