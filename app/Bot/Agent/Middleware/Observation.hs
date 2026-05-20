{-|
Module      : Bot.Agent.Middleware.Observation
Description : Agent lifecycle observation wrappers
Stability   : experimental
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Bot.Agent.Middleware.Observation
  ( ObservedConversationLink (..)
  , ObservedModelTurn (..)
  , ObservedToolCall (..)
  , ObservationContext (..)
  , emptyObservationContext
  , observeConversationLinked
  , withObservation
  , withObservedModelTurn
  , withObservedAgentRun
  , withObservedToolCall
  )
where

import Bot.Agent.Core
import Bot.Agent.Middleware.Observation.Types
import Bot.Agent.Middleware.Tools (ToolLimitContext (..))
import Bot.Agent.Types
import Bot.Core.Conversation
import Bot.Core.Message (IncomingMessage (..), MessageId)
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Streaming
import qualified Streaming.Prelude as S

data ObservedModelTurn result = ObservedModelTurn
  { runId :: !Text
  , turn :: !Int
  , messageCount :: !Int
  , exposedTools :: ![Text]
  , finished :: result -> AgentEvent
  }

data ObservedToolCall = ObservedToolCall
  { runId :: !Text
  , turn :: !Int
  , toolCall :: !LLM.ToolCall
  }

data ObservedConversationLink = ObservedConversationLink
  { runId :: !Text
  , parentMessageId :: !(Maybe MessageId)
  , linkedMessageId :: !MessageId
  }

withObservation :: (HList.Has ToolLimitContext context) => AgentObserver ObservationContext es -> AgentProgram (ObservationContext ': context) es -> AgentProgram context es
withObservation observer program =
  program
    { aroundAgentRun = \context action ->
        withObservedAgentRun observer (HList.get @ToolLimitContext context) program.agentRun (map (.name) program.agentRun.exposedTools) do
          program.aroundAgentRun (emptyObservationContext HList.:& context) action
    , aroundModelTurn = \context agentState action ->
        let turnInfo = ObservedModelTurn
              { runId = program.agentRun.runId
              , turn = agentState.turn
              , messageCount = conversationMessageCount agentState
              , exposedTools = map (.name) program.agentRun.exposedTools
              , finished = modelDecisionFinished program.agentRun.runId agentState.turn
              }
        in withObservedModelTurn observer turnInfo (program.aroundModelTurn (emptyObservationContext HList.:& context) agentState action)
    , aroundToolTurn = \context toolState action ->
        program.aroundToolTurn (emptyObservationContext HList.:& context) toolState action
    , aroundToolCall = \turn toolCall context action ->
        let observedCall = ObservedToolCall
              { runId = program.agentRun.runId
              , turn = turn
              , toolCall = toolCall
              }
        in withObservedToolCall observer observedCall \observation ->
             program.aroundToolCall turn toolCall (observation HList.:& context) action
    }
  where
    modelDecisionFinished runId turn = \case
      ModelAnswered AgentCompletion{finalText} ->
        ModelTurnFinished
          { runId = runId
          , turn = turn
          , answerKind = "final"
          , contentLength = Text.length finalText
          , toolCalls = []
          }
      ModelNeedsTools ToolTurnState{toolContent, toolCalls} ->
        ModelTurnFinished
          { runId = runId
          , turn = turn
          , answerKind = "tool_request"
          , contentLength = Text.length toolContent
          , toolCalls = toList toolCalls
          }

conversationMessageCount :: AgentState -> Int
conversationMessageCount AgentState{conversation = Conversation{messages}} =
  Foldable.length messages

withObservedAgentRun
  :: AgentObserver ObservationContext es
  -> ToolLimitContext
  -> AgentRun es
  -> [Text]
  -> Stream (Of AgentStreamOutput) (Eff es) AgentCompletion
  -> Stream (Of AgentStreamOutput) (Eff es) AgentCompletion
withObservedAgentRun observer toolLimit agentRun exposedTools action =
  catchStream
    ( do
        lift $ void $ observer.observe AgentRunStarted
          { runId = agentRun.runId
          , messageId = agentRun.context.message.messageId
          , maxTurns = toolLimit.maxToolTurns
          , exposedTools
          }
        result <- action
        let AgentCompletion{status, finalText, turnsUsed} = result
        lift $ void $ observer.observe AgentRunFinished
          { runId = agentRun.runId
          , status
          , finalLength = Text.length finalText
          , turnsUsed
          }
        pure result
    )
    \err -> do
      lift $ void $ observer.observe AgentRunInterrupted{runId = agentRun.runId, reason = interruptedReason err}
      lift $ throwIO err

withObservedModelTurn
  :: AgentObserver ObservationContext es
  -> ObservedModelTurn result
  -> Stream (Of AgentStreamOutput) (Eff es) result
  -> Stream (Of AgentStreamOutput) (Eff es) result
withObservedModelTurn observer turnInfo action = do
  lift $ void $ observer.observe ModelTurnStarted
    { runId = turnInfo.runId
    , turn = turnInfo.turn
    , messageCount = turnInfo.messageCount
    , exposedTools = turnInfo.exposedTools
    }
  result <- action
  lift $ void $ observer.observe (turnInfo.finished result)
  pure result

withObservedToolCall
  :: AgentObserver ObservationContext es
  -> ObservedToolCall
  -> (ObservationContext -> Eff es ToolResult)
  -> Eff es ToolResult
withObservedToolCall observer callInfo action = do
  observation <- observer.observe ToolCallStarted
    { runId = callInfo.runId
    , turn = callInfo.turn
    , toolCall = callInfo.toolCall
    }
  (status, result) <-
    statusFromResult <$> action observation
  finishToolCall observer callInfo status result
  pure result

statusFromResult :: ToolResult -> (Text, ToolResult)
statusFromResult result
  | Just failure <- toolResultFailure result =
      (agentFailureStatus failure, result)
  | otherwise =
      ("ok", result)

observeConversationLinked :: AgentObserver ObservationContext es -> ObservedConversationLink -> Eff es ()
observeConversationLinked observer ObservedConversationLink{runId, parentMessageId, linkedMessageId} =
  void $ observer.observe AgentConversationLinked{runId, linkedMessageId, parentMessageId}

finishToolCall :: AgentObserver ObservationContext es -> ObservedToolCall -> Text -> ToolResult -> Eff es ()
finishToolCall observer callInfo status result =
  void $ observer.observe ToolCallFinished
    { runId = callInfo.runId
    , turn = callInfo.turn
    , toolCallId = callInfo.toolCall.id
    , toolName = callInfo.toolCall.name
    , status = status
    , result = toolResultContent result
    , resultLength = Text.length (toolResultContent result)
    , messageIds = toolResultMessageIds result
    }

catchStream
  :: Stream (Of a) (Eff es) r
  -> (SomeException -> Stream (Of a) (Eff es) r)
  -> Stream (Of a) (Eff es) r
catchStream stream handler = do
  inspected <- lift (try (Streaming.inspect stream))
  case inspected of
    Left err ->
      handler err
    Right (Left result) ->
      pure result
    Right (Right (value S.:> rest)) -> do
      S.yield value
      catchStream rest handler

interruptedReason :: SomeException -> Text
interruptedReason _ =
  "failed"
