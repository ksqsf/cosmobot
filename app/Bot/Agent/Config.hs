{-|
Module      : Bot.Agent.Config
Description : Agent tool file configuration
Stability   : experimental
-}

module Bot.Agent.Config
  ( FileConfig (..)
  , WebFetchFileConfig (..)
  , WebSearchFileConfig (..)
  , defaultFileConfig
  , toToolConfig
  )
where

import qualified Bot.Agent.Types as Agent
import Bot.Config.Toml
import Bot.Prelude
import qualified Data.Text as Text
import Toml.Schema

data FileConfig = FileConfig
  { webSearch :: !WebSearchFileConfig
  , webFetch :: !WebFetchFileConfig
  , datetime :: !Bool
  }
  deriving (Show)

data WebFetchFileConfig = WebFetchFileConfig
  { enable :: !Bool
  , maxUses :: !(Maybe Int)
  , maxContentTokens :: !(Maybe Int)
  }
  deriving (Show)

data WebSearchFileConfig = WebSearchFileConfig
  { enable :: !Bool
  , api :: !Agent.WebSearchApi
  , maxResults :: !(Maybe Int)
  , braveApiKey :: !(Maybe Text)
  , tavilyApiKey :: !(Maybe Text)
  }
  deriving (Show)

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { webSearch = defaultWebSearchFileConfig
  , webFetch = defaultWebFetchFileConfig
  , datetime = Agent.defaultToolConfig.datetime
  }

defaultWebFetchFileConfig :: WebFetchFileConfig
defaultWebFetchFileConfig = WebFetchFileConfig
  { enable = Agent.defaultToolConfig.webFetch
  , maxUses = Agent.defaultToolConfig.webFetchMaxUses
  , maxContentTokens = Agent.defaultToolConfig.webFetchMaxContentTokens
  }

defaultWebSearchFileConfig :: WebSearchFileConfig
defaultWebSearchFileConfig = WebSearchFileConfig
  { enable = Agent.defaultToolConfig.webSearchEnable
  , api = Agent.defaultToolConfig.webSearchApi
  , maxResults = Agent.defaultToolConfig.webSearchMaxResults
  , braveApiKey = Agent.defaultToolConfig.braveApiKey
  , tavilyApiKey = Agent.defaultToolConfig.tavilyApiKey
  }

instance FromValue FileConfig where
  fromValue = parseTableFromValue do
    webSearch <- fromMaybe defaultFileConfig.webSearch <$> optKey "web_search"
    webFetch <- fromMaybe defaultFileConfig.webFetch <$> optKey "web_fetch"
    datetime <- fromMaybe defaultFileConfig.datetime <$> optKey "datetime"
    pure FileConfig
      { webSearch = webSearch
      , webFetch = webFetch
      , datetime = datetime
      }

instance FromValue WebFetchFileConfig where
  fromValue = parseTableFromValue do
    enable <- fromMaybe defaultWebFetchFileConfig.enable <$> optKey "enable"
    maxUses <- optKey "max_uses"
    maxContentTokens <- optKey "max_content_tokens"
    pure WebFetchFileConfig
      { enable = enable
      , maxUses = maxUses
      , maxContentTokens = maxContentTokens
      }

instance FromValue WebSearchFileConfig where
  fromValue = parseTableFromValue do
    enable <- fromMaybe defaultWebSearchFileConfig.enable <$> optKey "enable"
    api <- maybe (pure defaultWebSearchFileConfig.api) parseWebSearchApi =<< optKey "api"
    maxResults <- optKey "max_results"
    braveApiKey <- optToken "brave_api_key"
    tavilyApiKey <- optToken "tavily_api_key"
    pure WebSearchFileConfig
      { enable = enable
      , api = api
      , maxResults = maxResults
      , braveApiKey = braveApiKey
      , tavilyApiKey = tavilyApiKey
      }

parseWebSearchApi :: Text -> ParseTable l Agent.WebSearchApi
parseWebSearchApi value =
  case Text.toLower (Text.strip value) of
    "tavily" -> pure Agent.WebSearchTavily
    "brave"  -> pure Agent.WebSearchBrave
    "ddg"    -> pure Agent.WebSearchDDG
    _        -> fail "tool.web_search.api must be one of: tavily, brave, ddg"

toToolConfig :: FileConfig -> Agent.ToolConfig
toToolConfig cfg =
  Agent.ToolConfig
    { webSearchEnable = cfg.webSearch.enable
    , webSearchApi = cfg.webSearch.api
    , webSearchMaxResults = cfg.webSearch.maxResults
    , braveApiKey = cfg.webSearch.braveApiKey
    , tavilyApiKey = cfg.webSearch.tavilyApiKey
    , webFetch = cfg.webFetch.enable
    , webFetchMaxUses = cfg.webFetch.maxUses
    , webFetchMaxContentTokens = cfg.webFetch.maxContentTokens
    , datetime = cfg.datetime
    }
