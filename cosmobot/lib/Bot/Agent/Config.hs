{-|
Module      : Bot.Agent.Config
Description : Agent tool file configuration
Stability   : experimental
-}

module Bot.Agent.Config
  ( FileConfig (..)
  , PythonFileConfig (..)
  , WebFetchFileConfig (..)
  , WebSearchFileConfig (..)
  , defaultFileConfig
  , toToolConfig
  , schema
  )
where

import qualified Bot.Agent.Types as Agent
import qualified Bot.Config.Schema as Schema
import Bot.Util.Toml
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Toml.Semantics.Types as TomlValue
import Toml.Schema
import qualified Prelude

data FileConfig = FileConfig
  { webSearch :: !WebSearchFileConfig
  , webFetch :: !WebFetchFileConfig
  , datetime :: !Bool
  , python :: !PythonFileConfig
  }

instance Show FileConfig where
  showsPrec _ _ = Prelude.showString "<Agent.FileConfig>"

data PythonFileConfig = PythonFileConfig
  { enable :: !Bool
  , wallTimeoutSeconds :: !Int
  , cpuSeconds :: !Int
  , memoryMiB :: !Int
  , maxToolCalls :: !Int
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
  , exaApiKey :: !(Maybe Text)
  }

instance Show WebSearchFileConfig where
  showsPrec _ _ = Prelude.showString "<Agent.WebSearchFileConfig>"

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { webSearch = defaultWebSearchFileConfig
  , webFetch = defaultWebFetchFileConfig
  , datetime = Agent.defaultToolConfig.datetime
  , python = defaultPythonFileConfig
  }

defaultPythonFileConfig :: PythonFileConfig
defaultPythonFileConfig = PythonFileConfig
  { enable = Agent.defaultPythonConfig.enabled
  , wallTimeoutSeconds = Agent.defaultPythonConfig.wallTimeoutSeconds
  , cpuSeconds = Agent.defaultPythonConfig.cpuSeconds
  , memoryMiB = Agent.defaultPythonConfig.memoryMiB
  , maxToolCalls = Agent.defaultPythonConfig.maxToolCalls
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
  , exaApiKey = Agent.defaultToolConfig.exaApiKey
  }

schema :: Schema.ConfigSchema FileConfig Agent.ToolConfig
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue do
    webSearch <- fromMaybe defaultFileConfig.webSearch <$> optKey "web_search"
    webFetch <- fromMaybe defaultFileConfig.webFetch <$> optKey "web_fetch"
    datetime <- fromMaybe defaultFileConfig.datetime <$> optKey "datetime"
    python <- fromMaybe defaultFileConfig.python <$> optKey "python"
    pure FileConfig
      { webSearch = webSearch
      , webFetch = webFetch
      , datetime = datetime
      , python = python
      }
  , Schema.options =
      [ Schema.option ["datetime"] "Date and time tool" "Allow the date/time tool." owner Schema.boolean defaultFileConfig.datetime Aeson.Null (.datetime) (.datetime)
      , Schema.option ["python", "enable"] "Python tool" "Allow Python worker execution." owner Schema.boolean defaultPythonFileConfig.enable Aeson.Null ((.enable) . (.python)) ((.enabled) . (.python))
      , Schema.option ["python", "wall_timeout_seconds"] "Wall timeout" "Python wall-clock timeout in seconds." owner Schema.integer defaultPythonFileConfig.wallTimeoutSeconds (positiveMaximum Agent.maxPythonWallTimeoutSeconds) ((.wallTimeoutSeconds) . (.python)) ((.wallTimeoutSeconds) . (.python))
      , Schema.option ["python", "cpu_seconds"] "CPU limit" "Python CPU limit in seconds." owner Schema.integer defaultPythonFileConfig.cpuSeconds positive ((.cpuSeconds) . (.python)) ((.cpuSeconds) . (.python))
      , Schema.option ["python", "memory_mib"] "Memory limit" "Python memory limit in MiB." owner Schema.integer defaultPythonFileConfig.memoryMiB positive ((.memoryMiB) . (.python)) ((.memoryMiB) . (.python))
      , Schema.option ["python", "max_tool_calls"] "Maximum tool calls" "Maximum nested tool calls per Python run." owner Schema.integer defaultPythonFileConfig.maxToolCalls positive ((.maxToolCalls) . (.python)) ((.maxToolCalls) . (.python))
      , Schema.option ["web_fetch", "enable"] "Web fetch" "Allow direct web fetching." owner Schema.boolean defaultWebFetchFileConfig.enable Aeson.Null ((.enable) . (.webFetch)) (.webFetch)
      , Schema.optionalOption ["web_fetch", "max_uses"] "Maximum fetches" "Optional fetch-use limit." owner Schema.integer False positive ((.maxUses) . (.webFetch)) (.webFetchMaxUses)
      , Schema.optionalOption ["web_fetch", "max_content_tokens"] "Maximum content tokens" "Optional fetched-content token limit." owner Schema.integer False positive ((.maxContentTokens) . (.webFetch)) (.webFetchMaxContentTokens)
      , Schema.option ["web_search", "enable"] "Web search" "Allow web search." owner Schema.boolean defaultWebSearchFileConfig.enable Aeson.Null ((.enable) . (.webSearch)) (.webSearchEnable)
      , Schema.option ["web_search", "api"] "Search API" "Web search provider." owner (Schema.enum ["tavily", "brave", "exa"]) (webSearchApiText defaultWebSearchFileConfig.api) Aeson.Null (webSearchApiText . (.api) . (.webSearch)) (webSearchApiText . (.webSearchApi))
      , Schema.optionalOption ["web_search", "max_results"] "Maximum results" "Optional result limit." owner Schema.integer False positive ((.maxResults) . (.webSearch)) (.webSearchMaxResults)
      , Schema.optionalOption ["web_search", "brave_api_key"] "Brave API key" "Brave Search API key." owner Schema.secret False Aeson.Null (fmap Schema.Secret . (.braveApiKey) . (.webSearch)) (fmap Schema.Secret . (.braveApiKey))
      , Schema.optionalOption ["web_search", "tavily_api_key"] "Tavily API key" "Tavily API key." owner Schema.secret False Aeson.Null (fmap Schema.Secret . (.tavilyApiKey) . (.webSearch)) (fmap Schema.Secret . (.tavilyApiKey))
      , Schema.optionalOption ["web_search", "exa_api_key"] "Exa API key" "Exa API key." owner Schema.secret False Aeson.Null (fmap Schema.Secret . (.exaApiKey) . (.webSearch)) (fmap Schema.Secret . (.exaApiKey))
      ]
  }
  where
    owner = "Bot.Agent.Config"
    positive = Aeson.object ["minimum" Aeson..= (1 :: Int)]
    positiveMaximum maximumValue = Aeson.object ["minimum" Aeson..= (1 :: Int), "maximum" Aeson..= maximumValue]

instance FromValue FileConfig where
  fromValue = Schema.schemaFromValue schema

instance FromValue PythonFileConfig where
  fromValue = parseTableFromValue do
    enable <- fromMaybe defaultPythonFileConfig.enable <$> optKey "enable"
    wallTimeoutSeconds <- fromMaybe defaultPythonFileConfig.wallTimeoutSeconds <$> optKey "wall_timeout_seconds"
    cpuSeconds <- fromMaybe defaultPythonFileConfig.cpuSeconds <$> optKey "cpu_seconds"
    memoryMiB <- fromMaybe defaultPythonFileConfig.memoryMiB <$> optKey "memory_mib"
    maxToolCalls <- fromMaybe defaultPythonFileConfig.maxToolCalls <$> optKey "max_tool_calls"
    rejectUnknownPythonKeys
    requirePositive "wall_timeout_seconds" wallTimeoutSeconds
    requirePositive "cpu_seconds" cpuSeconds
    requirePositive "memory_mib" memoryMiB
    requirePositive "max_tool_calls" maxToolCalls
    when (wallTimeoutSeconds > Agent.maxPythonWallTimeoutSeconds) $
      fail [i|tool.python.wall_timeout_seconds must not exceed #{Agent.maxPythonWallTimeoutSeconds}|]
    requireSafeProduct "memory_mib" (1024 * 1024) memoryMiB
    pure PythonFileConfig{enable, wallTimeoutSeconds, cpuSeconds, memoryMiB, maxToolCalls}

rejectUnknownPythonKeys :: ParseTable l ()
rejectUnknownPythonKeys = do
  TomlValue.MkTable remaining <- getTable
  unless (Map.null remaining) $
    fail [i|unknown tool.python keys: #{Text.intercalate ", " (Map.keys remaining)}|]

requirePositive :: Text -> Int -> ParseTable l ()
requirePositive name value =
  unless (value > 0) $
    fail [i|tool.python.#{name} must be positive|]

requireSafeProduct :: Text -> Int -> Int -> ParseTable l ()
requireSafeProduct name multiplier value =
  when (value > maxBound `div` multiplier) $
    fail [i|tool.python.#{name} is too large|]

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
    exaApiKey <- optToken "exa_api_key"
    pure WebSearchFileConfig
      { enable = enable
      , api = api
      , maxResults = maxResults
      , braveApiKey = braveApiKey
      , tavilyApiKey = tavilyApiKey
      , exaApiKey = exaApiKey
      }

parseWebSearchApi :: Text -> ParseTable l Agent.WebSearchApi
parseWebSearchApi value =
  case Text.toLower (Text.strip value) of
    "tavily" -> pure Agent.WebSearchTavily
    "brave"  -> pure Agent.WebSearchBrave
    "exa"    -> pure Agent.WebSearchExa
    _        -> fail "tool.web_search.api must be one of: tavily, brave, exa"

webSearchApiText :: Agent.WebSearchApi -> Text
webSearchApiText = \case
  Agent.WebSearchTavily -> "tavily"
  Agent.WebSearchBrave -> "brave"
  Agent.WebSearchExa -> "exa"

toToolConfig :: FileConfig -> Agent.ToolConfig
toToolConfig cfg =
  Agent.ToolConfig
    { webSearchEnable = cfg.webSearch.enable
    , webSearchApi = cfg.webSearch.api
    , webSearchMaxResults = cfg.webSearch.maxResults
    , braveApiKey = cfg.webSearch.braveApiKey
    , tavilyApiKey = cfg.webSearch.tavilyApiKey
    , exaApiKey = cfg.webSearch.exaApiKey
    , webFetch = cfg.webFetch.enable
    , webFetchMaxUses = cfg.webFetch.maxUses
    , webFetchMaxContentTokens = cfg.webFetch.maxContentTokens
    , datetime = cfg.datetime
    , python = Agent.PythonConfig
        { enabled = cfg.python.enable
        , wallTimeoutSeconds = cfg.python.wallTimeoutSeconds
        , cpuSeconds = cfg.python.cpuSeconds
        , memoryMiB = cfg.python.memoryMiB
        , maxToolCalls = cfg.python.maxToolCalls
        }
    , sandboxImage = Agent.defaultToolConfig.sandboxImage
    }
