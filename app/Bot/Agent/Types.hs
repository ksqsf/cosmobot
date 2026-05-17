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
  , AgentFailureCategory (..)
  , AgentFailure (..)
  , AgentException (..)
  , agentFailureFromException
  , agentFailureStatus
  , permanentArgumentFailure
  , permissionDeniedFailure
  , ToolConfig (..)
  , WebSearchApi (..)
  , defaultToolConfig
  , ignoreAgentObserver
  , ToolResult (..)
  , toolText
  , toolTextWithImages
  , toolFailure
  , toolMessage
  , toolMessageWithImages
  , toolResultContent
  , toolResultImageUrls
  , toolResultMessageIds
  , toolResultFailure
  )
where

import Bot.Agent.Failure
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
  , noisy       :: !Bool
  , allowed     :: AgentContext es -> Bool
  , start       :: AgentContext es -> Eff es (Aeson.Value -> Eff es ToolResult)
  }

-- | Per-message capabilities and permissions made available to tools.
data AgentContext es = AgentContext
  { message :: IncomingMessage
  , input :: !MessageInput
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

newtype AgentObserver ctx es = AgentObserver
  { observe :: AgentEvent -> Eff es ctx
  }

ignoreAgentObserver :: ctx -> AgentObserver ctx es
ignoreAgentObserver ctx =
  AgentObserver{observe = \_ -> pure ctx}

-- | One tool call outcome. Failures are still returned as tool results because
-- OpenAI-compatible history requires every requested tool call to have a
-- corresponding tool-result message.
data ToolResult
  = ToolSucceeded
      { content :: !Text
      , imageUrls :: ![Text]
      , messageIds :: ![Maybe Integer]
      }
  | ToolFailed
      { failure :: !AgentFailure
      }

toolText :: Text -> ToolResult
toolText content =
  ToolSucceeded content [] []

toolTextWithImages :: Text -> [Text] -> ToolResult
toolTextWithImages content imageUrls =
  ToolSucceeded content imageUrls []

toolFailure :: AgentFailure -> ToolResult
toolFailure failure =
  ToolFailed failure

toolMessage :: Maybe Integer -> Text -> ToolResult
toolMessage messageId content =
  ToolSucceeded content [] [messageId]

toolMessageWithImages :: Maybe Integer -> Text -> [Text] -> ToolResult
toolMessageWithImages messageId content imageUrls =
  ToolSucceeded content imageUrls [messageId]

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

toolResultMessageIds :: ToolResult -> [Maybe Integer]
toolResultMessageIds = \case
  ToolSucceeded{messageIds} ->
    messageIds
  ToolFailed{} ->
    []

toolResultFailure :: ToolResult -> Maybe AgentFailure
toolResultFailure = \case
  ToolSucceeded{} ->
    Nothing
  ToolFailed{failure} ->
    Just failure
