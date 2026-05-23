{-|
Module      : Bot.AgentAudit.Projection
Description : Query projections for agent audit records
Stability   : experimental
-}

module Bot.AgentAudit.Projection
  ( maxInMemoryAgentAuditEvents
  , markStaleRunningToolUses
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

markStaleRunningToolUses :: UTCTime -> [AgentAuditRecord] -> [AgentAuditRecord]
markStaleRunningToolUses processStartedAt records =
  let restartedRunIds =
        ordNub
          [ toolUse.runId
          | toolUse <- toolUsesFromAuditRecords records
          , toolUse.status == ToolUseInProgress
          , toolUse.occurredAt < processStartedAt
          ]
      restartedRecords =
        [ AgentAuditRecord
            { id = Nothing
            , occurredAt = processStartedAt
            , event = AgentRunInterrupted{runId, reason = "restarted"}
            }
        | runId <- restartedRunIds
        ]
  in records <> restartedRecords

toolUsesFromRecords :: Int -> [AgentAuditRecord] -> [ToolUseDetail]
toolUsesFromRecords limit records =
  takeLast (max 0 limit) (toolUsesFromAuditRecords records)

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

    toolUseDetail finishesByCall interruptionsByRun AgentAuditRecord{id = Just auditId, occurredAt, event = ToolCallStarted{runId, turn, toolCall}} =
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
