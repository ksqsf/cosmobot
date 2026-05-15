{-|
Module      : Bot.Agent.Types
Description : Agent tool and context types
Stability   : experimental
-}

module Bot.Agent.Types
  ( Tool (..)
  , AgentContext (..)
  , AgentEvent (..)
  , AgentObserver (..)
  , ToolConfig (..)
  , WebSearchApi (..)
  , defaultToolConfig
  , ignoreAgentObserver
  , ToolResult (..)
  , toolText
  , toolTextWithImages
  , toolMessage
  , toolMessageWithImages
  )
where

import Bot.Core.Conversation
import Bot.Core.Message
import qualified Bot.Effect.LLM as LLM
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
  , systemContext :: !Text
  , askCommand :: !Text
  , toolConfig :: !ToolConfig
  , remember :: Maybe Integer -> Conversation -> Eff es ()
  , recordBotMessage :: Maybe Integer -> Text -> Eff es ()
  }

-- | Semantic lifecycle events emitted by the agent engine.
--
-- Observers translate these into concrete side effects such as persistent
-- audit rows. The loop itself should only emit these domain events.
data AgentEvent
  = AgentRunStarted
      { runId :: !Text
      , messageId :: !(Maybe Integer)
      , maxTurns :: !Int
      , exposedTools :: ![Text]
      }
  | ModelTurnStarted
      { runId :: !Text
      , turn :: !Int
      , messageCount :: !Int
      , exposedTools :: ![Text]
      }
  | ModelTurnFinished
      { runId :: !Text
      , turn :: !Int
      , answerKind :: !Text
      , contentLength :: !Int
      , toolCalls :: ![LLM.ToolCall]
      }
  | ToolCallStarted
      { runId :: !Text
      , turn :: !Int
      , toolCall :: !LLM.ToolCall
      }
  | ToolCallFinished
      { runId :: !Text
      , turn :: !Int
      , toolCallId :: !Text
      , toolName :: !Text
      , status :: !Text
      , result :: !Text
      , resultLength :: !Int
      , messageIds :: ![Maybe Integer]
      }
  | AgentRunFinished
      { runId :: !Text
      , status :: !Text
      , finalLength :: !Int
      , turnsUsed :: !Int
      }
  | AgentRunInterrupted
      { runId :: !Text
      , reason :: !Text
      }
  | AgentConversationLinked
      { runId :: !Text
      , linkedMessageId :: !Integer
      , parentMessageId :: !(Maybe Integer)
      }
  deriving (Eq, Show)

newtype AgentObserver es = AgentObserver
  { observe :: AgentEvent -> Eff es ()
  }

ignoreAgentObserver :: AgentObserver es
ignoreAgentObserver =
  AgentObserver{observe = \_ -> pure ()}

-- | Text returned to the LLM plus any bot message ids produced by a tool.
data ToolResult = ToolResult
  { content    :: !Text
  , imageUrls  :: ![Text]
  , messageIds :: ![Maybe Integer]
  }

toolText :: Text -> ToolResult
toolText content =
  ToolResult content [] []

toolTextWithImages :: Text -> [Text] -> ToolResult
toolTextWithImages content imageUrls =
  ToolResult content imageUrls []

toolMessage :: Maybe Integer -> Text -> ToolResult
toolMessage messageId content =
  ToolResult content [] [messageId]

toolMessageWithImages :: Maybe Integer -> Text -> [Text] -> ToolResult
toolMessageWithImages messageId content imageUrls =
  ToolResult content imageUrls [messageId]
