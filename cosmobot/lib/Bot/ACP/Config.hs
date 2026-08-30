{-|
Module      : Bot.ACP.Config
Description : Agent Client Protocol server configuration
Stability   : experimental
-}

module Bot.ACP.Config
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
  }
  deriving (Eq)

instance Show Config where
  showsPrec _ _ = Prelude.showString "<ACP.Config>"

data FileConfig = FileConfig
  { enabled :: !Bool
  , host :: !String
  , port :: !Int
  , token :: !Text
  }
  deriving (Eq)

instance Show FileConfig where
  showsPrec _ _ = Prelude.showString "<ACP.FileConfig>"

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { enabled = False
  , host = "127.0.0.1"
  , port = 38766
  , token = ""
  }

schema :: Schema.ConfigSchema FileConfig Config
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue do
    enabled <- fromMaybe defaultFileConfig.enabled <$> optKey "enabled"
    host <- fromMaybe defaultFileConfig.host <$> optKey "host"
    port <- fromMaybe defaultFileConfig.port <$> optKey "port"
    token <- fromMaybe defaultFileConfig.token <$> optKey "token"
    when (enabled && Text.null token) $
      fail "acp.token must be non-empty when acp.enabled is true"
    pure FileConfig{enabled, host, port, token}
  , Schema.options =
      [ Schema.option ["enabled"] "Enabled" "Start the ACP server." owner Schema.boolean defaultFileConfig.enabled Aeson.Null (.enabled) (.enabled)
      , Schema.option ["host"] "Host" "ACP listen address." owner Schema.text (toText defaultFileConfig.host) Aeson.Null (toText . (.host)) (toText . (.host))
      , Schema.option ["port"] "Port" "ACP listen port." owner Schema.integer defaultFileConfig.port (Aeson.object ["minimum" Aeson..= (1 :: Int), "maximum" Aeson..= (65535 :: Int)]) (.port) (.port)
      , Schema.option ["token"] "Token" "ACP bearer token." owner Schema.secret defaultFileConfig.token Aeson.Null (.token) (.token)
      ]
  }
  where owner = "Bot.ACP.Config"

instance FromValue FileConfig where
  fromValue = Schema.schemaFromValue schema

toRuntimeConfig :: FileConfig -> Config
toRuntimeConfig FileConfig{enabled, host, port, token} =
  Config{enabled, host, port, token}
