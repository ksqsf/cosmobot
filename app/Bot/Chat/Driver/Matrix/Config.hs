{-|
Module      : Bot.Chat.Driver.Matrix.Config
Description : Matrix driver file configuration
Stability   : experimental
-}

module Bot.Chat.Driver.Matrix.Config
  ( FileConfig (..)
  , defaultFileConfig
  , toRuntimeConfig
  )
where

import Bot.Config.Toml
import qualified Bot.Chat.Driver.Matrix as Matrix
import Bot.Prelude
import Toml.Schema

data FileConfig = FileConfig
  { homeserver :: !Text
  , accessToken :: !(Maybe Text)
  , userId :: !(Maybe Text)
  , allowedRooms :: ![Text]
  , superusers :: ![Text]
  }
  deriving (Show)

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { homeserver = "https://matrix.org"
  , accessToken = Nothing
  , userId = Nothing
  , allowedRooms = []
  , superusers = []
  }

instance FromValue FileConfig where
  fromValue = parseTableFromValue $ FileConfig
    <$> fmap (fromMaybe defaultFileConfig.homeserver) (optKey "homeserver")
    <*> optToken "access_token"
    <*> optKey "user_id"
    <*> fmap (fromMaybe []) (optKey "allowed_rooms")
    <*> fmap (fromMaybe []) (optKey "superusers")

toRuntimeConfig :: FileConfig -> Matrix.Config
toRuntimeConfig cfg =
  Matrix.Config
    { homeserver = cfg.homeserver
    , accessToken = cfg.accessToken
    , userId = cfg.userId
    , allowedRooms = cfg.allowedRooms
    , superusers = cfg.superusers
    }
