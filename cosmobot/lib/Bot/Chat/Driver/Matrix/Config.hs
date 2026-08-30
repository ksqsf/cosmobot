{-|
Module      : Bot.Chat.Driver.Matrix.Config
Description : Matrix driver file configuration
Stability   : experimental
-}

module Bot.Chat.Driver.Matrix.Config
  ( FileConfig (..)
  , defaultFileConfig
  , toRuntimeConfig
  , schema
  )
where

import Bot.Util.Toml
import qualified Bot.Config.Schema as Schema
import qualified Bot.Chat.Driver.Matrix as Matrix
import Bot.Prelude
import qualified Data.Aeson as Aeson
import Toml.Schema
import qualified Prelude

data FileConfig = FileConfig
  { homeserver :: !Text
  , loginUser :: !(Maybe Text)
  , loginPassword :: !(Maybe Text)
  , deviceId :: !(Maybe Text)
  , directRooms :: ![Text]
  , botId :: !(Maybe Text)
  , allowedRooms :: ![Text]
  , superusers :: ![Text]
  }

instance Show FileConfig where
  showsPrec _ _ = Prelude.showString "<Matrix.FileConfig>"

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { homeserver = "https://matrix.org"
  , loginUser = Nothing
  , loginPassword = Nothing
  , deviceId = Nothing
  , directRooms = []
  , botId = Nothing
  , allowedRooms = []
  , superusers = []
  }

schema :: Schema.ConfigSchema FileConfig Matrix.Config
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue $ FileConfig
    <$> fmap (fromMaybe defaultFileConfig.homeserver) (optKey "homeserver")
    <*> optToken "login_user"
    <*> optToken "login_password"
    <*> optToken "device_id"
    <*> fmap (fromMaybe []) (optKey "direct_rooms")
    <*> optKey "bot_id"
    <*> fmap (fromMaybe []) (optKey "allowed_rooms")
    <*> fmap (fromMaybe []) (optKey "superusers")
  , Schema.options =
      [ Schema.option ["homeserver"] "Homeserver" "Matrix homeserver URL." owner Schema.text defaultFileConfig.homeserver Aeson.Null (.homeserver) (.homeserver)
      , Schema.optionalOption ["login_user"] "Login user" "Matrix login user." owner Schema.text False Aeson.Null (.loginUser) (.loginUser)
      , Schema.optionalOption ["login_password"] "Login password" "Matrix login password." owner Schema.secret False Aeson.Null (fmap Schema.Secret . (.loginPassword)) (fmap Schema.Secret . (.loginPassword))
      , Schema.optionalOption ["device_id"] "Device ID" "Matrix device identifier." owner Schema.text False Aeson.Null (.deviceId) (.deviceId)
      , Schema.option ["direct_rooms"] "Direct rooms" "Rooms treated as direct chats." owner (Schema.list "text") [] Aeson.Null (.directRooms) (.directRooms)
      , Schema.optionalOption ["bot_id"] "Bot ID" "Matrix user id for the bot." owner Schema.text False Aeson.Null (.botId) (.userId)
      , Schema.option ["allowed_rooms"] "Allowed rooms" "Rooms allowed to use the bot." owner (Schema.list "text") [] Aeson.Null (.allowedRooms) (.allowedRooms)
      , Schema.option ["superusers"] "Superusers" "Matrix users with administrative access." owner (Schema.list "text") [] Aeson.Null (.superusers) (.superusers)
      ]
  , Schema.sections = [Schema.section [] "Matrix" ["drivers"] "Chat drivers"]
  , Schema.repeatableSections = []
  }
  where owner = "Bot.Chat.Driver.Matrix.Config"

instance FromValue FileConfig where
  fromValue = Schema.schemaFromValue schema

toRuntimeConfig :: FileConfig -> Matrix.Config
toRuntimeConfig cfg =
  Matrix.Config
    { homeserver = cfg.homeserver
    , loginUser = cfg.loginUser
    , loginPassword = cfg.loginPassword
    , deviceId = cfg.deviceId
    , directRooms = cfg.directRooms
    , userId = cfg.botId
    , allowedRooms = cfg.allowedRooms
    , superusers = cfg.superusers
    }
