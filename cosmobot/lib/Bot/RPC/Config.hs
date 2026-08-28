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
  , allowedBrowserOrigins :: ![Text]
  }
  deriving (Eq, Show)

data FileConfig = FileConfig
  { enabled :: !Bool
  , host :: !String
  , port :: !Int
  , token :: !Text
  , allowedBrowserOrigins :: ![Text]
  }
  deriving (Eq, Show)

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { enabled = False
  , host = "127.0.0.1"
  , port = 38765
  , token = ""
  , allowedBrowserOrigins = []
  }

instance FromValue FileConfig where
  fromValue = parseTableFromValue do
    enabled <- fromMaybe defaultFileConfig.enabled <$> optKey "enabled"
    host <- fromMaybe defaultFileConfig.host <$> optKey "host"
    port <- fromMaybe defaultFileConfig.port <$> optKey "port"
    token <- fromMaybe defaultFileConfig.token <$> optKey "token"
    allowedBrowserOrigins <- fromMaybe defaultFileConfig.allowedBrowserOrigins <$> optKey "allowed_browser_origins"
    when (enabled && Text.null token) $
      fail "rpc.token must be non-empty when rpc.enabled is true"
    pure FileConfig{enabled, host, port, token, allowedBrowserOrigins}

toRuntimeConfig :: FileConfig -> Config
toRuntimeConfig FileConfig{enabled, host, port, token, allowedBrowserOrigins} =
  Config{enabled, host, port, token, allowedBrowserOrigins}
