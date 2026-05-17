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
import qualified Toml.Semantics.Types as TomlValue
import Toml.Schema

data FileConfig = FileConfig
  { baseUrl :: !Text
  , apiKey   :: !(Maybe Text)
  , model    :: !Text
  , reasoningEffort :: !Text
  , requestTimeout :: !Int
  , imageGeneration :: !Bool
  , imageGenerationApi :: !LLM.ImageGenerationApi
  , imageGenerationBaseUrl :: !(Maybe Text)
  , imageGenerationApiKey :: !(Maybe Text)
  , imageGenerationModel :: !(Maybe Text)
  , imageGenerationModelCanEdit :: !Bool
  , imageGenerationTimeout :: !Int
  , imageGenerationQuality :: !(Maybe Text)
  , imageGenerationSize :: !(Maybe Text)
  , imageGenerationAspectRatio :: !(Maybe Text)
  , imageGenerationBackground :: !(Maybe Text)
  , imageGenerationOutputFormat :: !(Maybe Text)
  , imageGenerationOutputCompression :: !(Maybe Int)
  , imageGenerationModeration :: !(Maybe Text)
  }
  deriving (Show)

newtype FileImageGenerationApi = FileImageGenerationApi
  { toRuntimeImageGenerationApi :: LLM.ImageGenerationApi
  }
  deriving (Show)

instance FromValue FileConfig where
  fromValue = parseTableFromValue do
    baseUrl <- fmap (fromMaybe LLM.defaultConfig.baseUrl) (optKey "base_url")
    apiKey <- optToken "api_key"
    model <- reqKey "model"
    reasoningEffort <- fmap (fromMaybe LLM.defaultConfig.reasoningEffort) (optKey "reasoning_effort")
    requestTimeout <- fmap (fromMaybe LLM.defaultConfig.requestTimeout) (optKey "timeout")
    imageGeneration <- fmap (fromMaybe LLM.defaultConfig.imageGeneration) (optKey "image_generation")
    imageGenerationApiConfig <- (optKey "image_generation_api" :: ParseTable l (Maybe FileImageGenerationApi))
    let imageGenerationApi =
          maybe
            LLM.defaultConfig.imageGenerationApi
            (\FileImageGenerationApi{toRuntimeImageGenerationApi} -> toRuntimeImageGenerationApi)
            imageGenerationApiConfig
    imageGenerationBaseUrl <- optKey "image_generation_base_url"
    imageGenerationApiKey <- optToken "image_generation_api_key"
    imageGenerationModel <- optKey "image_generation_model"
    imageGenerationModelCanEdit <- fmap (fromMaybe LLM.defaultConfig.imageGenerationModelCanEdit) (optKey "image_generation_model_can_edit")
    imageGenerationTimeout <- fmap (fromMaybe LLM.defaultConfig.imageGenerationTimeout) (optKey "image_generation_timeout")
    imageGenerationQuality <- optKey "image_generation_quality"
    imageGenerationSize <- optKey "image_generation_size"
    imageGenerationAspectRatio <- optKey "image_generation_aspect_ratio"
    imageGenerationBackground <- optKey "image_generation_background"
    imageGenerationOutputFormat <- optKey "image_generation_output_format"
    imageGenerationOutputCompression <- optKey "image_generation_output_compression"
    imageGenerationModeration <- optKey "image_generation_moderation"
    when (requestTimeout <= 0) (fail "llm.timeout must be positive")
    when (imageGenerationTimeout <= 0) (fail "llm.image_generation_timeout must be positive")
    pure FileConfig
      { baseUrl = baseUrl
      , apiKey = apiKey
      , model = model
      , reasoningEffort = reasoningEffort
      , requestTimeout = requestTimeout
      , imageGeneration = imageGeneration
      , imageGenerationApi = imageGenerationApi
      , imageGenerationBaseUrl = imageGenerationBaseUrl
      , imageGenerationApiKey = imageGenerationApiKey
      , imageGenerationModel = imageGenerationModel
      , imageGenerationModelCanEdit = imageGenerationModelCanEdit
      , imageGenerationTimeout = imageGenerationTimeout
      , imageGenerationQuality = imageGenerationQuality
      , imageGenerationSize = imageGenerationSize
      , imageGenerationAspectRatio = imageGenerationAspectRatio
      , imageGenerationBackground = imageGenerationBackground
      , imageGenerationOutputFormat = imageGenerationOutputFormat
      , imageGenerationOutputCompression = imageGenerationOutputCompression
      , imageGenerationModeration = imageGenerationModeration
      }

toRuntimeConfig :: FileConfig -> LLM.Config
toRuntimeConfig cfg =
  LLM.Config
    { baseUrl = cfg.baseUrl
    , apiKey = cfg.apiKey
    , model = cfg.model
    , reasoningEffort = cfg.reasoningEffort
    , requestTimeout = cfg.requestTimeout
    , imageGeneration = cfg.imageGeneration
    , imageGenerationApi = cfg.imageGenerationApi
    , imageGenerationBaseUrl = cfg.imageGenerationBaseUrl
    , imageGenerationApiKey = cfg.imageGenerationApiKey
    , imageGenerationModel = cfg.imageGenerationModel
    , imageGenerationModelCanEdit = cfg.imageGenerationModelCanEdit
    , imageGenerationTimeout = cfg.imageGenerationTimeout
    , imageGenerationQuality = cfg.imageGenerationQuality
    , imageGenerationSize = cfg.imageGenerationSize
    , imageGenerationAspectRatio = cfg.imageGenerationAspectRatio
    , imageGenerationBackground = cfg.imageGenerationBackground
    , imageGenerationOutputFormat = cfg.imageGenerationOutputFormat
    , imageGenerationOutputCompression = cfg.imageGenerationOutputCompression
    , imageGenerationModeration = cfg.imageGenerationModeration
    }

instance FromValue FileImageGenerationApi where
  fromValue = \case
    TomlValue.Text' _ value ->
      case value of
        "chat_completions" -> pure (FileImageGenerationApi LLM.ImageGenerationChatCompletions)
        "images" -> pure (FileImageGenerationApi LLM.ImageGenerationImages)
        _ -> fail "llm.image_generation_api must be \"chat_completions\" or \"images\""
    _ ->
      fail "llm.image_generation_api must be a string"
