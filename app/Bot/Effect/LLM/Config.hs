{-|
Module      : Bot.Effect.LLM.Config
Description : LLM file configuration
Stability   : experimental
-}

module Bot.Effect.LLM.Config
  ( FileConfig (..)
  , toRuntimeConfig
  )
where

import Bot.Util.Toml
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import Toml.Schema

data FileConfig = FileConfig
  { endpoint :: !Text
  , apiKey   :: !(Maybe Text)
  , model    :: !Text
  , reasoningEffort :: !Text
  , imageGeneration :: !Bool
  , imageGenerationEndpoint :: !(Maybe Text)
  , imageGenerationApiKey :: !(Maybe Text)
  , imageGenerationModel :: !(Maybe Text)
  , imageGenerationQuality :: !(Maybe Text)
  , imageGenerationSize :: !(Maybe Text)
  , imageGenerationAspectRatio :: !(Maybe Text)
  , imageGenerationBackground :: !(Maybe Text)
  , imageGenerationOutputFormat :: !(Maybe Text)
  , imageGenerationOutputCompression :: !(Maybe Int)
  , imageGenerationModeration :: !(Maybe Text)
  }
  deriving (Show)

instance FromValue FileConfig where
  fromValue = parseTableFromValue $ FileConfig
    <$> fmap (fromMaybe LLM.defaultConfig.endpoint) (optKey "endpoint")
    <*> optToken "api_key"
    <*> reqKey "model"
    <*> fmap (fromMaybe LLM.defaultConfig.reasoningEffort) (optKey "reasoning_effort")
    <*> fmap (fromMaybe LLM.defaultConfig.imageGeneration) (optKey "image_generation")
    <*> optKey "image_generation_endpoint"
    <*> optToken "image_generation_api_key"
    <*> optKey "image_generation_model"
    <*> optKey "image_generation_quality"
    <*> optKey "image_generation_size"
    <*> optKey "image_generation_aspect_ratio"
    <*> optKey "image_generation_background"
    <*> optKey "image_generation_output_format"
    <*> optKey "image_generation_output_compression"
    <*> optKey "image_generation_moderation"

toRuntimeConfig :: FileConfig -> LLM.Config
toRuntimeConfig cfg =
  LLM.Config
    { endpoint = cfg.endpoint
    , apiKey = cfg.apiKey
    , model = cfg.model
    , reasoningEffort = cfg.reasoningEffort
    , imageGeneration = cfg.imageGeneration
    , imageGenerationEndpoint = cfg.imageGenerationEndpoint
    , imageGenerationApiKey = cfg.imageGenerationApiKey
    , imageGenerationModel = cfg.imageGenerationModel
    , imageGenerationQuality = cfg.imageGenerationQuality
    , imageGenerationSize = cfg.imageGenerationSize
    , imageGenerationAspectRatio = cfg.imageGenerationAspectRatio
    , imageGenerationBackground = cfg.imageGenerationBackground
    , imageGenerationOutputFormat = cfg.imageGenerationOutputFormat
    , imageGenerationOutputCompression = cfg.imageGenerationOutputCompression
    , imageGenerationModeration = cfg.imageGenerationModeration
    }
