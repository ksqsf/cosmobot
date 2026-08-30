{-|
Module      : Bot.LLM.OpenAI.Config
Description : LLM file configuration
Stability   : experimental
-}

module Bot.LLM.OpenAI.Config
  ( Config (..)
  , ChatProviderConfig (..)
  , ImageProviderConfig (..)
  , AudioProviderConfig (..)
  , defaultConfig
  , defaultChatProviderConfig
  , defaultImageProviderConfig
  , defaultAudioProviderConfig
  , defaultChatProviderFileConfig
  , defaultImageProviderFileConfig
  , defaultAudioProviderFileConfig
  , FileConfig (..)
  , ChatProviderFileConfig (..)
  , ImageProviderFileConfig (..)
  , AudioProviderFileConfig (..)
  , schema
  , chatProviderSchema
  , imageProviderSchema
  , audioProviderSchema
  , toRuntimeConfig
  )
where

import Bot.Util.Toml
import qualified Bot.Config.Schema as Schema
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Toml.Schema
import qualified Prelude

-- | Runtime configuration for OpenAI-compatible LLM endpoints.
data Config = Config
  { chatProvider :: !(Maybe ChatProviderConfig)
  , imageProvider :: !(Maybe ImageProviderConfig)
  , audioProvider :: !(Maybe AudioProviderConfig)
  }
  deriving (Eq)

instance Show Config where
  showsPrec _ _ = Prelude.showString "<LLM.Config>"

data ChatProviderConfig = ChatProviderConfig
  { baseUrl :: !Text
  , apiKey :: !(Maybe Text)
  , model :: !Text
  , reasoningEffort :: !Text
  , requestTimeout :: !Int
  }
  deriving (Eq)

instance Show ChatProviderConfig where
  showsPrec _ _ = Prelude.showString "<LLM.ChatProviderConfig>"

data ImageProviderConfig = ImageProviderConfig
  { baseUrl :: !Text
  , apiKey :: !(Maybe Text)
  , model :: !Text
  , canGenerate :: !Bool
  , canEdit :: !Bool
  , requestTimeout :: !Int
  , outputFormat :: !(Maybe Text)
  , quality :: !(Maybe Text)
  , size :: !(Maybe Text)
  , aspectRatio :: !(Maybe Text)
  , background :: !(Maybe Text)
  , moderation :: !(Maybe Text)
  }
  deriving (Eq)

instance Show ImageProviderConfig where
  showsPrec _ _ = Prelude.showString "<LLM.ImageProviderConfig>"

data AudioProviderConfig = AudioProviderConfig
  { baseUrl :: !Text
  , apiKey :: !(Maybe Text)
  , model :: !Text
  , voice :: !Text
  , responseFormat :: !Text
  , requestTimeout :: !Int
  , speed :: !(Maybe Double)
  , instructions :: !(Maybe Text)
  }
  deriving (Eq)

instance Show AudioProviderConfig where
  showsPrec _ _ = Prelude.showString "<LLM.AudioProviderConfig>"

-- | Defaults for optional LLM features.
defaultConfig :: Config
defaultConfig = Config
  { chatProvider = Nothing
  , imageProvider = Nothing
  , audioProvider = Nothing
  }

defaultChatProviderConfig :: ChatProviderConfig
defaultChatProviderConfig = ChatProviderConfig
  { baseUrl = "https://openrouter.ai/api/v1"
  , apiKey = Nothing
  , model = "openai/gpt-4o-mini"
  , reasoningEffort = "low"
  , requestTimeout = 60
  }

defaultImageProviderConfig :: ImageProviderConfig
defaultImageProviderConfig = ImageProviderConfig
  { baseUrl = "https://api.openai.com/v1"
  , apiKey = Nothing
  , model = "gpt-image-1.5"
  , canGenerate = True
  , canEdit = False
  , requestTimeout = 300
  , outputFormat = Nothing
  , quality = Nothing
  , size = Nothing
  , aspectRatio = Nothing
  , background = Nothing
  , moderation = Nothing
  }

defaultAudioProviderConfig :: AudioProviderConfig
defaultAudioProviderConfig = AudioProviderConfig
  { baseUrl = "https://api.openai.com/v1"
  , apiKey = Nothing
  , model = "gpt-4o-mini-tts"
  , voice = "coral"
  , responseFormat = "mp3"
  , requestTimeout = 300
  , speed = Nothing
  , instructions = Nothing
  }

data FileConfig = FileConfig
  { selectedChat :: !(Maybe Text)
  , selectedImage :: !(Maybe Text)
  , selectedAudio :: !(Maybe Text)
  , chatProviders :: !(Map Text ChatProviderFileConfig)
  , imageProviders :: !(Map Text ImageProviderFileConfig)
  , audioProviders :: !(Map Text AudioProviderFileConfig)
  , chatProvider :: !(Maybe ChatProviderFileConfig)
  , imageProvider :: !(Maybe ImageProviderFileConfig)
  , audioProvider :: !(Maybe AudioProviderFileConfig)
  }

instance Show FileConfig where
  showsPrec _ _ = Prelude.showString "<LLM.FileConfig>"

data ChatProviderFileConfig = ChatProviderFileConfig
  { baseUrl :: !Text
  , apiKey :: !(Maybe Text)
  , model :: !Text
  , reasoningEffort :: !Text
  , requestTimeout :: !Int
  }

instance Show ChatProviderFileConfig where
  showsPrec _ _ = Prelude.showString "<LLM.ChatProviderFileConfig>"

data ImageProviderFileConfig = ImageProviderFileConfig
  { baseUrl :: !Text
  , apiKey :: !(Maybe Text)
  , model :: !Text
  , canGenerate :: !Bool
  , canEdit :: !Bool
  , requestTimeout :: !Int
  , outputFormat :: !(Maybe Text)
  , quality :: !(Maybe Text)
  , size :: !(Maybe Text)
  , aspectRatio :: !(Maybe Text)
  , background :: !(Maybe Text)
  , moderation :: !(Maybe Text)
  }

instance Show ImageProviderFileConfig where
  showsPrec _ _ = Prelude.showString "<LLM.ImageProviderFileConfig>"

data AudioProviderFileConfig = AudioProviderFileConfig
  { baseUrl :: !Text
  , apiKey :: !(Maybe Text)
  , model :: !Text
  , voice :: !Text
  , responseFormat :: !Text
  , requestTimeout :: !Int
  , speed :: !(Maybe Double)
  , instructions :: !(Maybe Text)
  }

instance Show AudioProviderFileConfig where
  showsPrec _ _ = Prelude.showString "<LLM.AudioProviderFileConfig>"

defaultChatProviderFileConfig :: ChatProviderFileConfig
defaultChatProviderFileConfig = ChatProviderFileConfig
  { baseUrl = defaultChatProviderConfig.baseUrl
  , apiKey = defaultChatProviderConfig.apiKey
  , model = defaultChatProviderConfig.model
  , reasoningEffort = defaultChatProviderConfig.reasoningEffort
  , requestTimeout = defaultChatProviderConfig.requestTimeout
  }

defaultImageProviderFileConfig :: ImageProviderFileConfig
defaultImageProviderFileConfig = ImageProviderFileConfig
  { baseUrl = defaultImageProviderConfig.baseUrl
  , apiKey = defaultImageProviderConfig.apiKey
  , model = defaultImageProviderConfig.model
  , canGenerate = defaultImageProviderConfig.canGenerate
  , canEdit = defaultImageProviderConfig.canEdit
  , requestTimeout = defaultImageProviderConfig.requestTimeout
  , outputFormat = defaultImageProviderConfig.outputFormat
  , quality = defaultImageProviderConfig.quality
  , size = defaultImageProviderConfig.size
  , aspectRatio = defaultImageProviderConfig.aspectRatio
  , background = defaultImageProviderConfig.background
  , moderation = defaultImageProviderConfig.moderation
  }

defaultAudioProviderFileConfig :: AudioProviderFileConfig
defaultAudioProviderFileConfig = AudioProviderFileConfig
  { baseUrl = defaultAudioProviderConfig.baseUrl
  , apiKey = defaultAudioProviderConfig.apiKey
  , model = defaultAudioProviderConfig.model
  , voice = defaultAudioProviderConfig.voice
  , responseFormat = defaultAudioProviderConfig.responseFormat
  , requestTimeout = defaultAudioProviderConfig.requestTimeout
  , speed = defaultAudioProviderConfig.speed
  , instructions = defaultAudioProviderConfig.instructions
  }

schema :: Schema.ConfigSchema FileConfig FileConfig
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue do
    selectedChat <- optKey "chat"
    selectedImage <- optKey "image"
    selectedAudio <- optKey "audio"
    chatProviders <- fmap (fromMaybe Map.empty) (optKey "chat_provider")
    imageProviders <- fmap (fromMaybe Map.empty) (optKey "image_provider")
    audioProviders <- fmap (fromMaybe Map.empty) (optKey "audio_provider")
    chatProvider <- selectedProvider "llm.chat" "llm.chat_provider" selectedChat chatProviders
    imageProvider <- selectedProvider "llm.image" "llm.image_provider" selectedImage imageProviders
    audioProvider <- selectedProvider "llm.audio" "llm.audio_provider" selectedAudio audioProviders
    pure FileConfig
      { selectedChat
      , selectedImage
      , selectedAudio
      , chatProviders
      , imageProviders
      , audioProviders
      , chatProvider = chatProvider
      , imageProvider = imageProvider
      , audioProvider = audioProvider
      }
  , Schema.options =
      [ selector "chat" "Chat provider" "Named provider used for chat completions." (.selectedChat)
      , selector "image" "Image provider" "Named provider used for image generation and editing." (.selectedImage)
      , selector "audio" "Audio provider" "Named provider used for speech generation." (.selectedAudio)
      ]
  }
  where
    selector key label description getter = Schema.optionalOption [key] label description "Bot.LLM.OpenAI.Config" Schema.text False Aeson.Null getter getter

instance FromValue FileConfig where
  fromValue = Schema.schemaFromValue schema

selectedProvider
  :: Text
  -> Text
  -> Maybe Text
  -> Map Text provider
  -> ParseTable l (Maybe provider)
selectedProvider selectorName tableName selected providers =
  case Text.strip <$> selected of
    Nothing ->
      pure Nothing
    Just "" ->
      pure Nothing
    Just name ->
      case Map.lookup name providers of
        Just provider ->
          pure (Just provider)
        Nothing ->
          fail [i|#{selectorName} selects #{name}, but #{tableName}.#{name} is not defined|]

chatProviderSchema :: Schema.ConfigSchema ChatProviderFileConfig ChatProviderFileConfig
chatProviderSchema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue do
    baseUrl <- fmap (fromMaybe defaultChatProviderConfig.baseUrl) (optKey "base_url")
    apiKey <- optToken "api_key"
    model <- fmap (fromMaybe defaultChatProviderConfig.model) (optKey "model")
    reasoningEffort <- fmap (fromMaybe defaultChatProviderConfig.reasoningEffort) (optKey "reasoning_effort")
    requestTimeout <- fmap (fromMaybe defaultChatProviderConfig.requestTimeout) (optKey "timeout")
    when (requestTimeout <= 0) (fail "llm.chat_provider.<name>.timeout must be positive")
    pure ChatProviderFileConfig
      { baseUrl = baseUrl
      , apiKey = apiKey
      , model = model
      , reasoningEffort = reasoningEffort
      , requestTimeout = requestTimeout
      }
  , Schema.options =
      [ Schema.option ["base_url"] "Base URL" "OpenAI-compatible API base URL." owner Schema.text defaultChatProviderConfig.baseUrl Aeson.Null (.baseUrl) (.baseUrl)
      , Schema.optionalOption ["api_key"] "API key" "Provider API key." owner Schema.secret False Aeson.Null (fmap Schema.Secret . (.apiKey)) (fmap Schema.Secret . (.apiKey))
      , Schema.option ["model"] "Model" "Chat model identifier." owner Schema.text defaultChatProviderConfig.model Aeson.Null (.model) (.model)
      , Schema.option ["reasoning_effort"] "Reasoning effort" "Requested reasoning effort." owner Schema.text defaultChatProviderConfig.reasoningEffort Aeson.Null (.reasoningEffort) (.reasoningEffort)
      , Schema.option ["timeout"] "Timeout" "Request timeout in seconds." owner Schema.integer defaultChatProviderConfig.requestTimeout positive (.requestTimeout) (.requestTimeout)
      ]
  }
  where
    owner = "Bot.LLM.OpenAI.Config"
    positive = Aeson.object ["minimum" Aeson..= (1 :: Int)]

instance FromValue ChatProviderFileConfig where
  fromValue = Schema.schemaFromValue chatProviderSchema

imageProviderSchema :: Schema.ConfigSchema ImageProviderFileConfig ImageProviderFileConfig
imageProviderSchema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue do
    baseUrl <- fmap (fromMaybe defaultImageProviderConfig.baseUrl) (optKey "base_url")
    apiKey <- optToken "api_key"
    model <- fmap (fromMaybe defaultImageProviderConfig.model) (optKey "model")
    canGenerate <- fmap (fromMaybe defaultImageProviderConfig.canGenerate) (optKey "can_generate")
    canEdit <- fmap (fromMaybe defaultImageProviderConfig.canEdit) (optKey "can_edit")
    requestTimeout <- fmap (fromMaybe defaultImageProviderConfig.requestTimeout) (optKey "timeout")
    outputFormat <- optKey "output_format"
    quality <- optKey "quality"
    size <- optKey "size"
    aspectRatio <- optKey "aspect_ratio"
    background <- optKey "background"
    moderation <- optKey "moderation"
    when (requestTimeout <= 0) (fail "llm.image_provider.<name>.timeout must be positive")
    pure ImageProviderFileConfig
      { baseUrl = baseUrl
      , apiKey = apiKey
      , model = model
      , canGenerate = canGenerate
      , canEdit = canEdit
      , requestTimeout = requestTimeout
      , outputFormat = outputFormat
      , quality = quality
      , size = size
      , aspectRatio = aspectRatio
      , background = background
      , moderation = moderation
      }
  , Schema.options =
      [ Schema.option ["base_url"] "Base URL" "OpenAI-compatible API base URL." owner Schema.text defaultImageProviderConfig.baseUrl Aeson.Null (.baseUrl) (.baseUrl)
      , Schema.optionalOption ["api_key"] "API key" "Provider API key." owner Schema.secret False Aeson.Null (fmap Schema.Secret . (.apiKey)) (fmap Schema.Secret . (.apiKey))
      , Schema.option ["model"] "Model" "Image model identifier." owner Schema.text defaultImageProviderConfig.model Aeson.Null (.model) (.model)
      , Schema.option ["can_generate"] "Can generate" "Provider supports image generation." owner Schema.boolean defaultImageProviderConfig.canGenerate Aeson.Null (.canGenerate) (.canGenerate)
      , Schema.option ["can_edit"] "Can edit" "Provider supports image editing." owner Schema.boolean defaultImageProviderConfig.canEdit Aeson.Null (.canEdit) (.canEdit)
      , Schema.option ["timeout"] "Timeout" "Request timeout in seconds." owner Schema.integer defaultImageProviderConfig.requestTimeout positive (.requestTimeout) (.requestTimeout)
      , optionalText "output_format" "Output format" "Optional image output format." (.outputFormat)
      , optionalText "quality" "Quality" "Optional image quality." (.quality)
      , optionalText "size" "Size" "Optional image size." (.size)
      , optionalText "aspect_ratio" "Aspect ratio" "Optional image aspect ratio." (.aspectRatio)
      , optionalText "background" "Background" "Optional background mode." (.background)
      , optionalText "moderation" "Moderation" "Optional moderation mode." (.moderation)
      ]
  }
  where
    owner = "Bot.LLM.OpenAI.Config"
    positive = Aeson.object ["minimum" Aeson..= (1 :: Int)]
    optionalText key label description getter = Schema.optionalOption [key] label description owner Schema.text False Aeson.Null getter getter

instance FromValue ImageProviderFileConfig where
  fromValue = Schema.schemaFromValue imageProviderSchema

audioProviderSchema :: Schema.ConfigSchema AudioProviderFileConfig AudioProviderFileConfig
audioProviderSchema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue do
    baseUrl <- fmap (fromMaybe defaultAudioProviderConfig.baseUrl) (optKey "base_url")
    apiKey <- optToken "api_key"
    model <- fmap (fromMaybe defaultAudioProviderConfig.model) (optKey "model")
    voice <- fmap (fromMaybe defaultAudioProviderConfig.voice) (optKey "voice")
    responseFormat <- fmap (fromMaybe defaultAudioProviderConfig.responseFormat) (optKey "response_format")
    requestTimeout <- fmap (fromMaybe defaultAudioProviderConfig.requestTimeout) (optKey "timeout")
    speed <- optKey "speed"
    instructions <- optKey "instructions"
    when (requestTimeout <= 0) (fail "llm.audio_provider.<name>.timeout must be positive")
    traverse_ (\value -> when (value <= 0) (fail "llm.audio_provider.<name>.speed must be positive")) speed
    pure AudioProviderFileConfig
      { baseUrl = baseUrl
      , apiKey = apiKey
      , model = model
      , voice = voice
      , responseFormat = responseFormat
      , requestTimeout = requestTimeout
      , speed = speed
      , instructions = instructions
      }
  , Schema.options =
      [ Schema.option ["base_url"] "Base URL" "OpenAI-compatible API base URL." owner Schema.text defaultAudioProviderConfig.baseUrl Aeson.Null (.baseUrl) (.baseUrl)
      , Schema.optionalOption ["api_key"] "API key" "Provider API key." owner Schema.secret False Aeson.Null (fmap Schema.Secret . (.apiKey)) (fmap Schema.Secret . (.apiKey))
      , Schema.option ["model"] "Model" "Audio model identifier." owner Schema.text defaultAudioProviderConfig.model Aeson.Null (.model) (.model)
      , Schema.option ["voice"] "Voice" "Speech voice identifier." owner Schema.text defaultAudioProviderConfig.voice Aeson.Null (.voice) (.voice)
      , Schema.option ["response_format"] "Response format" "Generated audio format." owner Schema.text defaultAudioProviderConfig.responseFormat Aeson.Null (.responseFormat) (.responseFormat)
      , Schema.option ["timeout"] "Timeout" "Request timeout in seconds." owner Schema.integer defaultAudioProviderConfig.requestTimeout positive (.requestTimeout) (.requestTimeout)
      , Schema.optionalOption ["speed"] "Speed" "Optional speech speed multiplier." owner Schema.number False positive (.speed) (.speed)
      , Schema.optionalOption ["instructions"] "Instructions" "Optional speech instructions." owner Schema.text False Aeson.Null (.instructions) (.instructions)
      ]
  }
  where
    owner = "Bot.LLM.OpenAI.Config"
    positive = Aeson.object ["exclusiveMinimum" Aeson..= (0 :: Int)]

instance FromValue AudioProviderFileConfig where
  fromValue = Schema.schemaFromValue audioProviderSchema

toRuntimeConfig :: FileConfig -> Config
toRuntimeConfig cfg =
  Config
    { chatProvider = toRuntimeChatProviderConfig <$> cfg.chatProvider
    , imageProvider = toRuntimeImageProviderConfig <$> cfg.imageProvider
    , audioProvider = toRuntimeAudioProviderConfig <$> cfg.audioProvider
    }

toRuntimeChatProviderConfig :: ChatProviderFileConfig -> ChatProviderConfig
toRuntimeChatProviderConfig cfg =
  ChatProviderConfig
    { baseUrl = cfg.baseUrl
    , apiKey = cfg.apiKey
    , model = cfg.model
    , reasoningEffort = cfg.reasoningEffort
    , requestTimeout = cfg.requestTimeout
    }

toRuntimeImageProviderConfig :: ImageProviderFileConfig -> ImageProviderConfig
toRuntimeImageProviderConfig cfg =
  ImageProviderConfig
    { baseUrl = cfg.baseUrl
    , apiKey = cfg.apiKey
    , model = cfg.model
    , canGenerate = cfg.canGenerate
    , canEdit = cfg.canEdit
    , requestTimeout = cfg.requestTimeout
    , outputFormat = cfg.outputFormat
    , quality = cfg.quality
    , size = cfg.size
    , aspectRatio = cfg.aspectRatio
    , background = cfg.background
    , moderation = cfg.moderation
    }

toRuntimeAudioProviderConfig :: AudioProviderFileConfig -> AudioProviderConfig
toRuntimeAudioProviderConfig cfg =
  AudioProviderConfig
    { baseUrl = cfg.baseUrl
    , apiKey = cfg.apiKey
    , model = cfg.model
    , voice = cfg.voice
    , responseFormat = cfg.responseFormat
    , requestTimeout = cfg.requestTimeout
    , speed = cfg.speed
    , instructions = cfg.instructions
    }
