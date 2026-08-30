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
  , schema
  )
where

import Bot.Prelude
import qualified Bot.Config.Schema as Schema
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Toml.Schema
import qualified Prelude

data Config = Config
  { enabled :: !Bool
  , host :: !String
  , port :: !Int
  , token :: !Text
  , allowedBrowserOrigins :: ![Text]
  }
  deriving (Eq)

instance Show Config where
  showsPrec _ _ = Prelude.showString "<RPC.Config>"

data FileConfig = FileConfig
  { enabled :: !Bool
  , host :: !String
  , port :: !Int
  , token :: !Text
  , allowedBrowserOrigins :: ![Text]
  }
  deriving (Eq)

instance Show FileConfig where
  showsPrec _ _ = Prelude.showString "<RPC.FileConfig>"

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { enabled = False
  , host = "127.0.0.1"
  , port = 38765
  , token = ""
  , allowedBrowserOrigins = []
  }

schema :: Schema.ConfigSchema FileConfig Config
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue do
    enabled <- fromMaybe defaultFileConfig.enabled <$> optKey "enabled"
    host <- fromMaybe defaultFileConfig.host <$> optKey "host"
    port <- fromMaybe defaultFileConfig.port <$> optKey "port"
    token <- fromMaybe defaultFileConfig.token <$> optKey "token"
    allowedBrowserOrigins <- fromMaybe defaultFileConfig.allowedBrowserOrigins <$> optKey "allowed_browser_origins"
    when (enabled && Text.null token) $
      fail "rpc.token must be non-empty when rpc.enabled is true"
    pure FileConfig{enabled, host, port, token, allowedBrowserOrigins}
  , Schema.options =
      [ Schema.option ["enabled"] "Enabled" "Start the RPC server." owner Schema.boolean defaultFileConfig.enabled Aeson.Null (.enabled) (.enabled)
      , Schema.option ["host"] "Host" "RPC listen address." owner Schema.text (toText defaultFileConfig.host) Aeson.Null (toText . (.host)) (toText . (.host))
      , Schema.option ["port"] "Port" "RPC listen port." owner Schema.integer defaultFileConfig.port (Aeson.object ["minimum" Aeson..= (1 :: Int), "maximum" Aeson..= (65535 :: Int)]) (.port) (.port)
      , Schema.option ["token"] "Token" "RPC bearer token." owner Schema.secret defaultFileConfig.token Aeson.Null (.token) (.token)
      , Schema.option ["allowed_browser_origins"] "Allowed browser origins" "Origins allowed to authenticate from a browser." owner (Schema.list "text") defaultFileConfig.allowedBrowserOrigins Aeson.Null (.allowedBrowserOrigins) (.allowedBrowserOrigins)
      ]
  }
  where owner = "Bot.RPC.Config"

instance FromValue FileConfig where
  fromValue = Schema.schemaFromValue schema

toRuntimeConfig :: FileConfig -> Config
toRuntimeConfig FileConfig{enabled, host, port, token, allowedBrowserOrigins} =
  Config{enabled, host, port, token, allowedBrowserOrigins}
