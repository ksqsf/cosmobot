{-|
Module      : Bot.AgentAudit.Types
Description : Agent audit domain types
Stability   : experimental
-}

module Bot.AgentAudit.Types
  ( AgentAuditEvent (..)
  , AgentAuditRecord (..)
  , ToolCallTrace (..)
  , ToolUseDetail (..)
  , ToolUseStatus (..)
  , eventRunId
  )
where

import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson

data ToolCallTrace = ToolCallTrace
  { id :: !Text
  , name :: !Text
  , arguments :: !Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data AgentAuditEvent
  = ToolCallStarted
      { runId :: !Text
      , turn :: !Int
      , toolCall :: !ToolCallTrace
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
  | AgentRunInterrupted
      { runId :: !Text
      , reason :: !Text
      }
  | AgentConversationLinked
      { runId :: !Text
      , linkedMessageId :: !MessageId
      , parentMessageId :: !(Maybe MessageId)
      }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data AgentAuditRecord = AgentAuditRecord
  { id :: !(Maybe Integer)
  , occurredAt :: !UTCTime
  , event :: !AgentAuditEvent
  }
  deriving (Eq, Show)

data ToolUseStatus
  = ToolUseInProgress
  | ToolUseFinished
      { status :: !Text
      , durationMilliseconds :: !Integer
      }
  | ToolUseInterrupted
      { reason :: !Text
      , durationMilliseconds :: !Integer
      }
  deriving (Eq, Show)

data ToolUseDetail = ToolUseDetail
  { auditId :: !Integer
  , occurredAt :: !UTCTime
  , finishedAt :: !(Maybe UTCTime)
  , runId :: !Text
  , turn :: !Int
  , toolName :: !Text
  , toolCallId :: !Text
  , arguments :: !Text
  , status :: !ToolUseStatus
  , result :: !(Maybe Text)
  , messageIds :: ![Maybe MessageId]
  }
  deriving (Eq, Show)

eventRunId :: AgentAuditEvent -> Text
eventRunId = \case
  ToolCallStarted{runId} -> runId
  ToolCallFinished{runId} -> runId
  AgentRunInterrupted{runId} -> runId
  AgentConversationLinked{runId} -> runId
