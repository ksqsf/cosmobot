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
  , attachmentDir :: !FilePath
  , attachmentMaxBytes :: !Int
  }
  deriving (Eq, Show)

data FileConfig = FileConfig
  { enabled :: !Bool
  , host :: !String
  , port :: !Int
  , token :: !Text
  , staticDir :: !FilePath
  , attachmentDir :: !FilePath
  , attachmentMaxBytes :: !Int
  }
  deriving (Eq, Show)

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { enabled = False
  , host = "127.0.0.1"
  , port = 38765
  , token = ""
  , staticDir = "web/dist"
  , attachmentDir = "attachments"
  , attachmentMaxBytes = 25 * 1024 * 1024
  }

instance FromValue FileConfig where
  fromValue = parseTableFromValue do
    enabled <- fromMaybe defaultFileConfig.enabled <$> optKey "enabled"
    host <- fromMaybe defaultFileConfig.host <$> optKey "host"
    port <- fromMaybe defaultFileConfig.port <$> optKey "port"
    token <- fromMaybe defaultFileConfig.token <$> optKey "token"
    staticDir <- fromMaybe defaultFileConfig.staticDir <$> optKey "static_dir"
    attachmentDir <- fromMaybe defaultFileConfig.attachmentDir <$> optKey "attachment_dir"
    attachmentMaxBytes <- fromMaybe defaultFileConfig.attachmentMaxBytes <$> optKey "attachment_max_bytes"
    when (enabled && Text.null token) $
      fail "rpc.token must be non-empty when rpc.enabled is true"
    when (attachmentMaxBytes <= 0) $
      fail "rpc.attachment_max_bytes must be positive"
    pure FileConfig{enabled, host, port, token, staticDir, attachmentDir, attachmentMaxBytes}

toRuntimeConfig :: FileConfig -> Config
toRuntimeConfig FileConfig{enabled, host, port, token, staticDir, attachmentDir, attachmentMaxBytes} =
  Config{enabled, host, port, token, staticDir, attachmentDir, attachmentMaxBytes}
