{-|
Module      : Bot.Media.Config
Description : Media cache and public mirror configuration
Stability   : experimental
-}

module Bot.Media.Config
  ( Config (..)
  , defaultConfig
  )
where

import Bot.Prelude
import qualified Bot.Media.S3.Config as S3
import Toml.Schema

data Config = Config
  { cacheDir :: !FilePath
  , publicBaseUrl :: !(Maybe Text)
  , s3 :: !S3.Config
  }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config
  { cacheDir = "media-cache"
  , publicBaseUrl = Nothing
  , s3 = S3.defaultConfig
  }

instance FromValue Config where
  fromValue = parseTableFromValue do
    cacheDir <- fromMaybe defaultConfig.cacheDir <$> optKey "cache_dir"
    publicBaseUrl <- optKey "public_base_url"
    s3 <- fromMaybe defaultConfig.s3 <$> optKey "s3"
    pure Config{cacheDir, publicBaseUrl, s3}
