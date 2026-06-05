{-|
Module      : Bot.AgentAudit.Projection
Description : Query projections for agent audit records
Stability   : experimental
-}

module Bot.AgentAudit.Projection
  ( maxInMemoryAgentAuditEvents
  , toolUsesFromAuditRecords
  , toolUsesFromRecords
  )
where

import Bot.AgentAudit.Types
import Bot.Prelude
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, diffUTCTime)

maxInMemoryAgentAuditEvents :: Int
maxInMemoryAgentAuditEvents =
  5000

toolUsesFromRecords :: UTCTime -> Int -> [AgentAuditRecord] -> [ToolUseDetail]
toolUsesFromRecords processStartedAt limit records =
  takeLast (max 0 limit) (map (markStaleRunningToolUse processStartedAt) (toolUsesFromAuditRecords records))

toolUsesFromAuditRecords :: [AgentAuditRecord] -> [ToolUseDetail]
toolUsesFromAuditRecords records =
  mapMaybe (toolUseDetail finishes interruptions) (filter isToolStart records)
  where
    finishes =
      Map.fromList
        [ ((event.runId, event.toolCallId), record)
        | record@AgentAuditRecord{event = event@ToolCallFinished{}} <- records
        ]

    interruptions =
      Map.fromList
        [ (event.runId, record)
        | record@AgentAuditRecord{event = event@AgentRunInterrupted{}} <- records
        ]

    toolUseDetail finishesByCall interruptionsByRun AgentAuditRecord{id = auditId, occurredAt, event = ToolCallStarted{runId, turn, toolCall}} =
      let finished = Map.lookup (runId, toolCall.id) finishesByCall
          interrupted = Map.lookup runId interruptionsByRun
      in Just ToolUseDetail
        { auditId = auditId
        , occurredAt = occurredAt
        , finishedAt = (.occurredAt) <$> (finished <|> interrupted)
        , runId = runId
        , turn = turn
        , toolName = toolCall.name
        , toolCallId = toolCall.id
        , arguments = toolCall.arguments
        , status = toolUseStatus occurredAt finished interrupted
        , result = finished >>= finishedResult
        , messageIds = maybe [] finishedMessageIds finished
        }
    toolUseDetail _ _ _ =
      Nothing

    finishedStatus startedAt AgentAuditRecord{occurredAt = finishedAt, event = ToolCallFinished{status}} =
      ToolUseFinished
        { status = status
        , durationMilliseconds = floor (diffUTCTime finishedAt startedAt * 1000)
        }
    finishedStatus _ _ =
      ToolUseInProgress

    interruptedStatus startedAt AgentAuditRecord{occurredAt = interruptedAt, event = AgentRunInterrupted{reason}} =
      ToolUseInterrupted
        { reason = reason
        , durationMilliseconds = floor (diffUTCTime interruptedAt startedAt * 1000)
        }
    interruptedStatus _ _ =
      ToolUseInProgress

    toolUseStatus startedAt (Just finished) _ =
      finishedStatus startedAt finished
    toolUseStatus startedAt Nothing (Just interrupted) =
      interruptedStatus startedAt interrupted
    toolUseStatus _ Nothing Nothing =
      ToolUseInProgress

    finishedResult AgentAuditRecord{event = ToolCallFinished{result}} =
      Just result
    finishedResult _ =
      Nothing

    finishedMessageIds AgentAuditRecord{event = ToolCallFinished{messageIds}} =
      messageIds
    finishedMessageIds _ =
      []

    isToolStart AgentAuditRecord{event = ToolCallStarted{}} =
      True
    isToolStart _ =
      False

takeLast :: Int -> [a] -> [a]
takeLast n values =
  drop (max 0 (length values - n)) values

markStaleRunningToolUse :: UTCTime -> ToolUseDetail -> ToolUseDetail
markStaleRunningToolUse processStartedAt toolUse
  | toolUse.status == ToolUseInProgress
  , toolUse.occurredAt < processStartedAt =
      toolUse
        { finishedAt = Just processStartedAt
        , status = ToolUseInterrupted
            { reason = "restarted"
            , durationMilliseconds = floor (diffUTCTime processStartedAt toolUse.occurredAt * 1000)
            }
        }
  | otherwise =
      toolUse
