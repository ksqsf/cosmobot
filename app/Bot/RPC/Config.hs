{-|
Module      : Bot.RPC.Config
Description : Local JSON-RPC websocket configuration
Stability   : experimental
-}

module Bot.RPC.Config
  ( Config (..)
  , FileConfig (..)
  , defaultFileConfig
  , toRuntimeConfig
  )
where

import Bot.Prelude
import qualified Data.Text as Text
import Toml.Schema

data Config = Config
  { enabled :: !Bool
  , host :: !String
  , port :: !Int
  , token :: !Text
  , staticDir :: !FilePath
  }
  deriving (Eq, Show)

data FileConfig = FileConfig
  { enabled :: !Bool
  , host :: !String
  , port :: !Int
  , token :: !Text
  , staticDir :: !FilePath
  }
  deriving (Eq, Show)

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { enabled = False
  , host = "127.0.0.1"
  , port = 38765
  , token = ""
  , staticDir = "web/dist"
  }

instance FromValue FileConfig where
  fromValue = parseTableFromValue do
    enabled <- fromMaybe defaultFileConfig.enabled <$> optKey "enabled"
    host <- fromMaybe defaultFileConfig.host <$> optKey "host"
    port <- fromMaybe defaultFileConfig.port <$> optKey "port"
    token <- fromMaybe defaultFileConfig.token <$> optKey "token"
    staticDir <- fromMaybe defaultFileConfig.staticDir <$> optKey "static_dir"
    when (enabled && Text.null token) $
      fail "rpc.token must be non-empty when rpc.enabled is true"
    pure FileConfig{enabled, host, port, token, staticDir}

toRuntimeConfig :: FileConfig -> Config
toRuntimeConfig FileConfig{enabled, host, port, token, staticDir} =
  Config{enabled, host, port, token, staticDir}
