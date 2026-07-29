{-|
Module      : Bot.Agent.Types
Description : Agent context, events, and tool results
Stability   : experimental
-}
module Bot.Agent.Types
  ( ToolCallMetadata (..)
  , Context (..)
  , Event (..)
  , Observer
  , FailureCategory (..)
  , Failure (..)
  , failureFromException
  , failureStatus
  , permanentArgumentFailure
  , permissionDeniedFailure
  , ToolConfig (..)
  , WebSearchApi (..)
  , defaultToolConfig
  , ignoreObserver
  , ToolResult (..)
  , toolText
  , toolTextWithImages
  , toolFailure
  , toolResultContent
  , toolResultImageUrls
  , toolResultFailure
  )
where

import Bot.Agent.Failure
import Bot.Core.Message
import Bot.Core.Thread (ThreadMessageKey)
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude

-- | Runtime configuration for agent tools.
data ToolConfig = ToolConfig
  { webSearchEnable :: !Bool
  , webSearchApi :: !WebSearchApi
  , webSearchMaxResults :: !(Maybe Int)
  , braveApiKey :: !(Maybe Text)
  , tavilyApiKey :: !(Maybe Text)
  , exaApiKey :: !(Maybe Text)
  , webFetch :: !Bool
  , webFetchMaxUses :: !(Maybe Int)
  , webFetchMaxContentTokens :: !(Maybe Int)
  , datetime :: !Bool
  , sandboxImage :: !Text
  }
  deriving (Show)

data WebSearchApi
  = WebSearchTavily
  | WebSearchBrave
  | WebSearchExa
  deriving (Eq, Show)

defaultToolConfig :: ToolConfig
defaultToolConfig = ToolConfig
  { webSearchEnable = False
  , webSearchApi = WebSearchTavily
  , webSearchMaxResults = Nothing
  , braveApiKey = Nothing
  , tavilyApiKey = Nothing
  , exaApiKey = Nothing
  , webFetch = False
  , webFetchMaxUses = Nothing
  , webFetchMaxContentTokens = Nothing
  , datetime = False
  , sandboxImage = "localhost/cosmobox:latest"
  }

data ToolCallMetadata = ToolCallMetadata
  { agentRunId :: !Text
  , originRunId :: !Text
  , parent :: !(Maybe Concurrency.Handle)
  }

-- | Per-message capabilities and permissions made available to tools.
data Context = Context
  { message :: IncomingMessage
  , input :: !MessageInput
  , superuser :: !Bool
  , systemContext :: !Text
  , askCommand :: !Text
  , toolConfig :: !ToolConfig
  }

-- | Semantic lifecycle events emitted by the agent engine.
--
-- Observers translate these into concrete side effects such as persistent
-- audit rows. The loop itself should only emit these domain events.
data Event
  = AgentRunStarted
      { runId :: !Text
      , messageId :: !(Maybe MessageId)
      , maxTurns :: !Int
      , exposedTools :: ![Text]
      }
  | ModelTurnStarted
      { runId :: !Text
      , turn :: !Int
      , messageCount :: !Int
      , exposedTools :: ![Text]
      , toolGroups :: ![(Text, Int)]
      }
  | ModelTurnFinished
      { runId :: !Text
      , turn :: !Int
      , answerKind :: !Text
      , contentLength :: !Int
      , toolCalls :: ![LLM.ToolCall]
      , tokenUsage :: !(Maybe LLM.TokenUsage)
      }
  | ContextCompacted
      { runId :: !Text
      , turn :: !Int
      , messageCount :: !Int
      , tokenUsage :: !(Maybe LLM.TokenUsage)
      }
  | SubAgentRunStarted
      { runId :: !Text
      , childRunId :: !Text
      , subagentId :: !Text
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
      , messageIds :: ![Maybe MessageId]
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
  | AgentThreadLinked
      { runId :: !Text
      , linkedMessageId :: !MessageId
      , linkedMessageKey :: !ThreadMessageKey
      , parentMessageId :: !(Maybe MessageId)
      }
  deriving (Eq, Show)

type Observer ctx es =
  Event -> Eff es ctx

ignoreObserver :: ctx -> Observer ctx es
ignoreObserver ctx =
  const (pure ctx)

-- | One tool call outcome. Failures are still returned as tool results because
-- OpenAI-compatible history requires every requested tool call to have a
-- corresponding tool-result message.
data ToolResult
  = ToolSucceeded
      { content :: !Text
      , imageUrls :: ![Text]
      }
  | ToolFailed
      { failure :: !Failure
      }

toolText :: Text -> ToolResult
toolText content =
  ToolSucceeded content []

toolTextWithImages :: Text -> [Text] -> ToolResult
toolTextWithImages content imageUrls =
  ToolSucceeded content imageUrls

toolFailure :: Failure -> ToolResult
toolFailure failure =
  ToolFailed failure

toolResultContent :: ToolResult -> Text
toolResultContent = \case
  ToolSucceeded{content} ->
    content
  ToolFailed{failure} ->
    failure.userMessage

toolResultImageUrls :: ToolResult -> [Text]
toolResultImageUrls = \case
  ToolSucceeded{imageUrls} ->
    imageUrls
  ToolFailed{} ->
    []

toolResultFailure :: ToolResult -> Maybe Failure
toolResultFailure = \case
  ToolSucceeded{} ->
    Nothing
  ToolFailed{failure} ->
    Just failure
