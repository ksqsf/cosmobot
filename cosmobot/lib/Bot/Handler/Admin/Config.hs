{-|
Module      : Bot.Handler.Admin.Config
Description : Administrative handler configuration
Stability   : experimental
-}

module Bot.Handler.Admin.Config
  ( AdminConfig (..)
  , UpgradeConfig (..)
  , defaultAdminConfig
  , schema
  )
where

import Bot.Prelude
import qualified Bot.Config.Schema as Schema
import qualified Data.Aeson as Aeson
import Toml.Schema

-- | Administrative command settings.
newtype AdminConfig = AdminConfig
  { upgrade :: Maybe UpgradeConfig
  }
  deriving (Show)

-- | Configured maintenance script for @!upgrade@.
newtype UpgradeConfig = UpgradeConfig
  { script :: FilePath
  }
  deriving (Show)

defaultAdminConfig :: AdminConfig
defaultAdminConfig = AdminConfig
  { upgrade = Nothing
  }

schema :: Schema.ConfigSchema AdminConfig AdminConfig
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue $ AdminConfig <$> optKey "upgrade"
  , Schema.options =
      [ Schema.optionalOption ["upgrade", "script"] "Upgrade script" "Executable used by the upgrade command." "Bot.Handler.Admin.Config" Schema.text True Aeson.Null
          (fmap (toText . (.script)) . (.upgrade))
          (fmap (toText . (.script)) . (.upgrade))
      ]
  }

instance FromValue AdminConfig where
  fromValue = Schema.schemaFromValue schema

instance FromValue UpgradeConfig where
  fromValue = parseTableFromValue $ UpgradeConfig
    <$> reqKey "script"
