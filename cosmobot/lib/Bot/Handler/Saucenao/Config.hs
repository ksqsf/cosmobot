{-|
Module      : Bot.Handler.Saucenao.Config
Description : SauceNAO handler file configuration
Stability   : experimental
-}

module Bot.Handler.Saucenao.Config
  ( SaucenaoConfig (..)
  , defaultSaucenaoConfig
  , schema
  )
where

import Bot.Util.Toml
import Bot.Prelude
import qualified Bot.Config.Schema as Schema
import qualified Data.Aeson as Aeson
import Toml.Schema
import qualified Prelude

-- | SauceNAO integration settings.
newtype SaucenaoConfig = SaucenaoConfig
  { apiKey :: Maybe Text
  }

instance Show SaucenaoConfig where
  showsPrec _ _ = Prelude.showString "<SaucenaoConfig>"

defaultSaucenaoConfig :: SaucenaoConfig
defaultSaucenaoConfig = SaucenaoConfig
  { apiKey = Nothing
  }

schema :: Schema.ConfigSchema SaucenaoConfig SaucenaoConfig
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue $ SaucenaoConfig <$> optToken "api_key"
  , Schema.options =
      [ Schema.optionalOption ["api_key"] "API key" "SauceNAO API key." "Bot.Handler.Saucenao.Config" Schema.secret False Aeson.Null (fmap Schema.Secret . (.apiKey)) (fmap Schema.Secret . (.apiKey))
      ]
  }

instance FromValue SaucenaoConfig where
  fromValue = Schema.schemaFromValue schema
