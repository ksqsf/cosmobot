{-|
Module      : Bot.Agent.Types
Description : Agent context, events, and tool results
Stability   : experimental
-}
module Bot.Agent.Types
  ( BotTool
  , BotRuntime
  , AgentRunId
  , ContextStrategy (..)
  , ToolCallMetadata (..)
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
  , PythonConfig (..)
  , defaultPythonConfig
  , maxPythonWallTimeoutSeconds
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

import qualified Bot.Agent.Core as Core
import qualified Bot.Agent.Tool as Tool
import Bot.Agent.Result
import Bot.Core.Message
import Bot.Core.Thread (ThreadMessageKey)
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Prelude

type BotTool = Tool.Tool Context ToolCallMetadata

type BotRuntime = Core.Runtime Context ToolCallMetadata

type AgentRunId = Text

data ContextStrategy
  = ContextCompaction
  | RecursiveTranscript
  deriving (Eq, Show)

-- | Runtime configuration for agent tools.
data ToolConfig = ToolConfig
  { webSearchEnable :: !Bool
  , webSearchApi :: !WebSearchApi
  , webSearchMaxResults :: !(Maybe Int)
  , braveApiKey :: !(Maybe Text)
  , tavilyApiKey :: !(Maybe Text)
  , exaApiKey :: !(Maybe Text)
  , deepseekApiKey :: !(Maybe Text)
  , webFetch :: !Bool
  , webFetchMaxUses :: !(Maybe Int)
  , webFetchMaxContentTokens :: !(Maybe Int)
  , datetime :: !Bool
  , python :: !PythonConfig
  , sandboxImage :: !Text
  }

instance Show ToolConfig where
  showsPrec _ _ = Prelude.showString "<ToolConfig>"

-- | Host-controlled limits for the Python composition tool.
data PythonConfig = PythonConfig
  { enabled :: !Bool
  , wallTimeoutSeconds :: !Int
  , cpuSeconds :: !Int
  , memoryMiB :: !Int
  , maxToolCalls :: !Int
  }
  deriving (Eq, Show)

data WebSearchApi
  = WebSearchTavily
  | WebSearchBrave
  | WebSearchExa
  | WebSearchDeepSeek
  deriving (Eq, Show)

defaultToolConfig :: ToolConfig
defaultToolConfig = ToolConfig
  { webSearchEnable = False
  , webSearchApi = WebSearchTavily
  , webSearchMaxResults = Nothing
  , braveApiKey = Nothing
  , tavilyApiKey = Nothing
  , exaApiKey = Nothing
  , deepseekApiKey = Nothing
  , webFetch = False
  , webFetchMaxUses = Nothing
  , webFetchMaxContentTokens = Nothing
  , datetime = False
  , python = defaultPythonConfig
  , sandboxImage = "localhost/cosmobox:latest"
  }

defaultPythonConfig :: PythonConfig
defaultPythonConfig = PythonConfig
  { enabled = True
  , wallTimeoutSeconds = 30
  , cpuSeconds = 20
  , memoryMiB = 512
  , maxToolCalls = 64
  }

maxPythonWallTimeoutSeconds :: Int
maxPythonWallTimeoutSeconds = 60 * 60

data ToolCallMetadata = ToolCallMetadata
  { agentRunId :: !AgentRunId
  , originRunId :: !AgentRunId
  , resourceOwner :: !(Maybe Concurrency.Handle)
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
      , contextStrategy :: !ContextStrategy
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
  | RecursiveTranscriptFlushed
      { runId :: !Text
      , turn :: !Int
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

type Observer ctx m =
  Event -> m ctx

ignoreObserver :: Applicative m => ctx -> Observer ctx m
ignoreObserver ctx =
  const (pure ctx)
