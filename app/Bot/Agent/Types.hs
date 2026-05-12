{-|
Module      : Bot.Agent.Types
Description : Agent tool and context types
Stability   : experimental
-}

module Bot.Agent.Types
  ( Tool (..)
  , AgentContext (..)
  , ToolConfig (..)
  , WebSearchApi (..)
  , defaultToolConfig
  , ToolResult (..)
  , toolText
  , toolMessage
  )
where

import Bot.Core.Conversation
import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson

-- | Runtime configuration for agent tools.
data ToolConfig = ToolConfig
  { webSearchEnable :: !Bool
  , webSearchApi :: !WebSearchApi
  , webSearchMaxResults :: !(Maybe Int)
  , braveApiKey :: !(Maybe Text)
  , tavilyApiKey :: !(Maybe Text)
  , webFetch :: !Bool
  , webFetchMaxUses :: !(Maybe Int)
  , webFetchMaxContentTokens :: !(Maybe Int)
  , datetime :: !Bool
  }
  deriving (Show)

data WebSearchApi
  = WebSearchTavily
  | WebSearchBrave
  deriving (Eq, Show)

defaultToolConfig :: ToolConfig
defaultToolConfig = ToolConfig
  { webSearchEnable = False
  , webSearchApi = WebSearchTavily
  , webSearchMaxResults = Nothing
  , braveApiKey = Nothing
  , tavilyApiKey = Nothing
  , webFetch = False
  , webFetchMaxUses = Nothing
  , webFetchMaxContentTokens = Nothing
  , datetime = False
  }

-- | Tool definition exposed to the LLM function-calling API.
data Tool es = Tool
  { name        :: !Text
  , description :: !Text
  , parameters  :: !Aeson.Value
  , allowed     :: AgentContext es -> Bool
  , start       :: AgentContext es -> Eff es (Aeson.Value -> Eff es ToolResult)
  }

-- | Per-message capabilities and permissions made available to tools.
data AgentContext es = AgentContext
  { message :: IncomingMessage
  , superuser :: !Bool
  , askCommand :: !Text
  , toolConfig :: !ToolConfig
  , remember :: Maybe Integer -> Conversation -> Eff es ()
  , recordBotMessage :: Maybe Integer -> Text -> Eff es ()
  }

-- | Text returned to the LLM plus any bot message ids produced by a tool.
data ToolResult = ToolResult
  { content    :: !Text
  , messageIds :: ![Maybe Integer]
  }

toolText :: Text -> ToolResult
toolText content =
  ToolResult content []

toolMessage :: Maybe Integer -> Text -> ToolResult
toolMessage messageId content =
  ToolResult content [messageId]
