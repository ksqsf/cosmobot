{-|
Module      : Bot.Media.Config
Description : Media cache and public mirror configuration
Stability   : experimental
-}

module Bot.Media.Config
  ( Config (..)
  , GcConfig (..)
  , defaultConfig
  , defaultGcConfig
  , schema
  )
where

import Bot.Prelude
import qualified Bot.Config.Schema as Schema
import qualified Bot.Media.S3.Config as S3
import qualified Data.Aeson as Aeson
import Toml.Schema

data Config = Config
  { cacheDir :: !FilePath
  , publicBaseUrl :: !(Maybe Text)
  , gc :: !GcConfig
  , s3 :: !S3.Config
  }
  deriving (Show, Eq)

data GcConfig = GcConfig
  { enabled :: !Bool
  , olderThanDays :: !Int
  , intervalHours :: !Int
  }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config
  { cacheDir = "media-cache"
  , publicBaseUrl = Nothing
  , gc = defaultGcConfig
  , s3 = S3.defaultConfig
  }

defaultGcConfig :: GcConfig
defaultGcConfig = GcConfig
  { enabled = False
  , olderThanDays = 7
  , intervalHours = 24
  }

schema :: Schema.ConfigSchema Config Config
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue do
    cacheDir <- fromMaybe defaultConfig.cacheDir <$> optKey "cache_dir"
    publicBaseUrl <- optKey "public_base_url"
    gc <- fromMaybe defaultConfig.gc <$> optKey "gc"
    s3 <- fromMaybe defaultConfig.s3 <$> optKey "s3"
    pure Config{cacheDir, publicBaseUrl, gc, s3}
  , Schema.options =
      [ Schema.option ["cache_dir"] "Cache directory" "Local media cache directory." owner Schema.text (toText defaultConfig.cacheDir) Aeson.Null (toText . (.cacheDir)) (toText . (.cacheDir))
      , Schema.optionalOption ["public_base_url"] "Public base URL" "Public URL prefix for cached media." owner Schema.text False Aeson.Null (.publicBaseUrl) (.publicBaseUrl)
      , Schema.option ["gc", "enabled"] "Garbage collection" "Periodically remove old unreferenced media." owner Schema.boolean defaultGcConfig.enabled Aeson.Null ((.enabled) . (.gc)) ((.enabled) . (.gc))
      , Schema.option ["gc", "older_than_days"] "Maximum age" "Age in days before unreferenced media is eligible." owner Schema.integer defaultGcConfig.olderThanDays (Aeson.object ["minimum" Aeson..= (0 :: Int)]) ((.olderThanDays) . (.gc)) ((.olderThanDays) . (.gc))
      , Schema.option ["gc", "interval_hours"] "Collection interval" "Hours between garbage collection passes." owner Schema.integer defaultGcConfig.intervalHours (Aeson.object ["minimum" Aeson..= (1 :: Int)]) ((.intervalHours) . (.gc)) ((.intervalHours) . (.gc))
      ] <> Schema.prefixOptions ["s3"] (Schema.mapOptions (.s3) (.s3) S3.schema.options)
  , Schema.sections =
      [ Schema.section [] "General" ["media"] "Media"
      , Schema.section ["gc"] "GC" ["media"] "Media"
      ] <> Schema.prefixSections ["s3"] S3.schema.sections
  , Schema.repeatableSections = []
  }
  where owner = "Bot.Media.Config"

instance FromValue Config where
  fromValue = Schema.schemaFromValue schema

instance FromValue GcConfig where
  fromValue = parseTableFromValue do
    enabled <- fromMaybe defaultGcConfig.enabled <$> optKey "enabled"
    olderThanDays <- fromMaybe defaultGcConfig.olderThanDays <$> optKey "older_than_days"
    intervalHours <- fromMaybe defaultGcConfig.intervalHours <$> optKey "interval_hours"
    when (olderThanDays < 0) $
      fail "media.gc.older_than_days must not be negative"
    when (intervalHours <= 0) $
      fail "media.gc.interval_hours must be positive"
    pure GcConfig{enabled, olderThanDays, intervalHours}
