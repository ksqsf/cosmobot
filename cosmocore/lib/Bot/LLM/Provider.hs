module Bot.LLM.Provider
  ( Config (..), ChatProviderConfig (..), ImageProviderConfig (..), AudioProviderConfig (..)
  , defaultConfig, defaultChatProviderConfig, defaultImageProviderConfig, defaultAudioProviderConfig
  ) where

import Bot.Prelude
import qualified Prelude

-- | Runtime configuration for named LLM endpoints.
data Config = Config
  { chatProvider :: !(Maybe ChatProviderConfig)
  , imageProvider :: !(Maybe ImageProviderConfig)
  , audioProvider :: !(Maybe AudioProviderConfig)
  }
  deriving (Eq)

instance Show Config where
  showsPrec _ _ = Prelude.showString "<LLM.Config>"

data ChatProviderConfig = ChatProviderConfig
  { api :: !Text
  , anthropicAuth :: !Text
  , thinking :: !(Maybe Text)
  , maxTokens :: !Int
  , baseUrl :: !Text
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
  { api = "chat_completions"
  , anthropicAuth = "api_key"
  , thinking = Nothing
  , maxTokens = 16384
  , baseUrl = "https://openrouter.ai/api/v1"
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
