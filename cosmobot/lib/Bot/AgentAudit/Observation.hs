{-|
Module      : Bot.AgentAudit.Observation
Description : Agent observation adapter for audit events
Stability   : experimental
-}

module Bot.AgentAudit.Observation
  ( agentAuditObserverWith
  )
where

import qualified Bot.Agent.Middleware.Observation.Types as Observation
import qualified Bot.Agent.Types as Agent
import Bot.AgentAudit.Types
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude

agentAuditObserverWith :: (AgentAuditEvent -> Eff es (Maybe Integer)) -> Agent.AgentObserver Observation.ObservationContext es
agentAuditObserverWith recordEvent =
  Agent.AgentObserver{Agent.observe = recordAgentEvent recordEvent}

recordAgentEvent :: (AgentAuditEvent -> Eff es (Maybe Integer)) -> Agent.AgentEvent -> Eff es Observation.ObservationContext
recordAgentEvent recordEvent event =
  observationContext <$> maybe (pure Nothing) recordEvent (agentAuditEvent event)

agentAuditEvent :: Agent.AgentEvent -> Maybe AgentAuditEvent
agentAuditEvent = \case
  Agent.ToolCallStarted{runId, turn, toolCall} ->
    Just ToolCallStarted
      { runId
      , turn
      , toolCall = toolCallTrace toolCall
      }
  Agent.ToolCallFinished{runId, turn, toolCallId, toolName, status, result, resultLength, messageIds} ->
    Just ToolCallFinished{runId, turn, toolCallId, toolName, status, result, resultLength, messageIds}
  Agent.AgentRunInterrupted{runId, reason} ->
    Just AgentRunInterrupted{runId, reason}
  Agent.AgentConversationLinked{runId, linkedMessageId, parentMessageId} ->
    Just AgentConversationLinked{runId, linkedMessageId, parentMessageId}
  Agent.AgentRunStarted{} ->
    Nothing
  Agent.ModelTurnStarted{} ->
    Nothing
  Agent.ModelTurnFinished{} ->
    Nothing
  Agent.AgentRunFinished{} ->
    Nothing

toolCallTrace :: LLM.ToolCall -> ToolCallTrace
toolCallTrace call =
  ToolCallTrace
    { id = call.id
    , name = call.name
    , arguments = call.arguments
    }

observationContext :: Maybe Integer -> Observation.ObservationContext
observationContext auditId =
  Observation.ObservationContext{Observation.auditToolUseId = auditId}
