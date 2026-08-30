{-|
Module      : Bot.Media.S3.Config
Description : S3 media storage configuration
Stability   : experimental
-}

module Bot.Media.S3.Config
  ( Config (..)
  , defaultConfig
  , schema
  )
where

import Bot.Prelude
import qualified Bot.Config.Schema as Schema
import qualified Data.Aeson as Aeson
import Toml.Schema
import qualified Prelude

data Config = Config
  { enabled :: !Bool
  , bucket :: !(Maybe Text)
  , region :: !Text
  , endpoint :: !(Maybe Text)
  , accessKeyId :: !(Maybe Text)
  , secretAccessKey :: !(Maybe Text)
  , prefix :: !Text
  , publicReadAcl :: !Bool
  , addressingStyle :: !Text
  }
  deriving (Eq)

instance Show Config where
  showsPrec _ _ = Prelude.showString "<S3.Config>"

defaultConfig :: Config
defaultConfig = Config
  { enabled = False
  , bucket = Nothing
  , region = "us-east-1"
  , endpoint = Nothing
  , accessKeyId = Nothing
  , secretAccessKey = Nothing
  , prefix = "cosmobot/media"
  , publicReadAcl = False
  , addressingStyle = "auto"
  }

schema :: Schema.ConfigSchema Config Config
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue do
    enabled <- fromMaybe defaultConfig.enabled <$> optKey "enabled"
    bucket <- optKey "bucket"
    region <- fromMaybe defaultConfig.region <$> optKey "region"
    endpoint <- optKey "endpoint"
    accessKeyId <- optKey "access_key_id"
    secretAccessKey <- optKey "secret_access_key"
    prefix <- fromMaybe defaultConfig.prefix <$> optKey "prefix"
    publicReadAcl <- fromMaybe defaultConfig.publicReadAcl <$> optKey "public_read_acl"
    addressingStyle <- fromMaybe defaultConfig.addressingStyle <$> optKey "addressing_style"
    pure Config{enabled, bucket, region, endpoint, accessKeyId, secretAccessKey, prefix, publicReadAcl, addressingStyle}
  , Schema.options =
      [ Schema.option ["enabled"] "Enabled" "Mirror media to S3-compatible storage." owner Schema.boolean defaultConfig.enabled Aeson.Null (.enabled) (.enabled)
      , Schema.optionalOption ["bucket"] "Bucket" "S3 bucket name." owner Schema.text False Aeson.Null (.bucket) (.bucket)
      , Schema.option ["region"] "Region" "S3 signing region." owner Schema.text defaultConfig.region Aeson.Null (.region) (.region)
      , Schema.optionalOption ["endpoint"] "Endpoint" "Optional S3-compatible endpoint." owner Schema.text False Aeson.Null (.endpoint) (.endpoint)
      , Schema.optionalOption ["access_key_id"] "Access key ID" "S3 access key identifier." owner Schema.secret False Aeson.Null (.accessKeyId) (.accessKeyId)
      , Schema.optionalOption ["secret_access_key"] "Secret access key" "S3 secret access key." owner Schema.secret False Aeson.Null (.secretAccessKey) (.secretAccessKey)
      , Schema.option ["prefix"] "Prefix" "Object key prefix." owner Schema.text defaultConfig.prefix Aeson.Null (.prefix) (.prefix)
      , Schema.option ["public_read_acl"] "Public read ACL" "Apply a public-read object ACL." owner Schema.boolean defaultConfig.publicReadAcl Aeson.Null (.publicReadAcl) (.publicReadAcl)
      , Schema.option ["addressing_style"] "Addressing style" "Bucket addressing style." owner Schema.text defaultConfig.addressingStyle Aeson.Null (.addressingStyle) (.addressingStyle)
      ]
  }
  where owner = "Bot.Media.S3.Config"

instance FromValue Config where
  fromValue = Schema.schemaFromValue schema
