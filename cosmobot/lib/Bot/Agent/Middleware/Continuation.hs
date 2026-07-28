{-# LANGUAGE DataKinds #-}
{-|
Module      : Bot.Agent.Middleware.Continuation
Description : One-shot delimited agent continuations
Stability   : experimental
-}
module Bot.Agent.Middleware.Continuation
  ( withContinuations
  )
where

import Bot.Agent.Core
import Bot.Agent.Tools.Common (jsonText)
import Bot.Agent.Tools.Continuation
import Bot.Agent.Transcript (appendMessage, appendMessages)
import Bot.Agent.Types
import Bot.Core.Transcript (Transcript)
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map

withContinuations
  :: (HList.Has ContinuationState transient, HList.Put ContinuationState transient)
  => AgentProgram transient context es
  -> AgentProgram transient context es
withContinuations program =
  program
    { aroundToolTurn = \context toolState action ->
        case exposedContinuationCalls program toolState.toolCalls of
          [] ->
            program.aroundToolTurn context toolState action
          [call]
            | [_] <- toList toolState.toolCalls ->
                program.aroundToolTurn context toolState $
                  runContinuation program context toolState call
          _ ->
            program.aroundToolTurn context toolState $
              rejectConcurrentCalls program context toolState
    }

exposedContinuationCalls :: AgentProgram transient context es -> NonEmpty LLM.ToolCall -> [LLM.ToolCall]
exposedContinuationCalls program =
  filter isExposedContinuation . toList
  where
    isExposedContinuation call =
      isContinuationToolName call.name
        && any ((== call.name) . (.name)) program.agentRun.exposedTools

runContinuation
  :: (HList.Has ContinuationState transient, HList.Put ContinuationState transient)
  => AgentProgram transient context es
  -> MiddlewareContext context
  -> ToolTurnState transient
  -> LLM.ToolCall
  -> Eff es (AgentState transient)
runContinuation program context toolState call =
  case continuationRequest call of
    Nothing ->
      rejectCall program context toolState call "Unknown continuation operation."
    Just (Left err) ->
      rejectCall program context toolState call [i|Invalid continuation arguments: #{err}|]
    Just (Right (CaptureContinuation label)) ->
      capture program context toolState call label
    Just (Right (ResumeContinuation continuationId value)) ->
      resume program context toolState call continuationId value

capture
  :: (HList.Has ContinuationState transient, HList.Put ContinuationState transient)
  => AgentProgram transient context es
  -> MiddlewareContext context
  -> ToolTurnState transient
  -> LLM.ToolCall
  -> Maybe Text
  -> Eff es (AgentState transient)
capture program context toolState call label = do
  let current = HList.get @ContinuationState toolState.agentState.transient
  case Map.lookup call.id current.saved of
    Just _ ->
      rejectCall program context toolState call "Continuation id already exists."
    Nothing -> do
      let savedContinuation =
            SavedContinuation
              { ordinal = current.nextOrdinal
              , answered = toolState.answered
              , captureCall = call
              }
          next =
            current
              { nextOrdinal = current.nextOrdinal + 1
              , saved = Map.insert call.id savedContinuation current.saved
              }
          result =
            toolText . jsonText $
              Aeson.object
                [ "state" Aeson..= ("captured" :: Text)
                , "continuation_id" Aeson..= call.id
                , "label" Aeson..= label
                , "scope" Aeson..= ("current_agent_run" :: Text)
                , "one_shot" Aeson..= True
                ]
      observed <- program.aroundToolCall toolState.agentState.turn call context (pure result)
      let continuationState
            | isNothing (toolResultFailure observed) = next
            | otherwise = current
      pure (advance toolState.agentState (appendMessage (toolResultMessage call observed) toolState.answered) continuationState)

resume
  :: (HList.Has ContinuationState transient, HList.Put ContinuationState transient)
  => AgentProgram transient context es
  -> MiddlewareContext context
  -> ToolTurnState transient
  -> LLM.ToolCall
  -> Text
  -> Aeson.Value
  -> Eff es (AgentState transient)
resume program context toolState call continuationId value = do
  let current = HList.get @ContinuationState toolState.agentState.transient
  case Map.lookup continuationId current.saved of
    Nothing ->
      rejectCall program context toolState call [i|Continuation not found in this agent run: #{continuationId}|]
    Just savedContinuation -> do
      let resumedValue =
            Aeson.object
              [ "state" Aeson..= ("resumed" :: Text)
              , "continuation_id" Aeson..= continuationId
              , "value" Aeson..= value
              ]
          resumeResult = toolText (jsonText resumedValue)
          remaining =
            current
              { saved = Map.filter ((< savedContinuation.ordinal) . (.ordinal)) current.saved
              }
      observed <- program.aroundToolCall toolState.agentState.turn call context (pure resumeResult)
      if isJust (toolResultFailure observed)
        then
          pure (advance toolState.agentState (appendMessage (toolResultMessage call observed) toolState.answered) current)
        else
          pure $
            advance
              toolState.agentState
              (appendMessage (LLM.toolResult savedContinuation.captureCall (jsonText resumedValue)) savedContinuation.answered)
              remaining

rejectConcurrentCalls
  :: AgentProgram transient context es
  -> MiddlewareContext context
  -> ToolTurnState transient
  -> Eff es (AgentState transient)
rejectConcurrentCalls program context toolState = do
  messages <- forM (toList toolState.toolCalls) \call -> do
    result <- program.aroundToolCall toolState.agentState.turn call context $
      pure (argumentFailure "Continuation control tools must be called alone; no tool call in this turn was executed.")
    pure (toolResultMessage call result)
  pure $
    toolState.agentState
      { transcript = appendMessages messages toolState.answered
      , turn = toolState.agentState.turn + 1
      }

rejectCall
  :: (HList.Has ContinuationState transient, HList.Put ContinuationState transient)
  => AgentProgram transient context es
  -> MiddlewareContext context
  -> ToolTurnState transient
  -> LLM.ToolCall
  -> Text
  -> Eff es (AgentState transient)
rejectCall program context toolState call message = do
  result <- program.aroundToolCall toolState.agentState.turn call context (pure (argumentFailure message))
  let current = HList.get @ContinuationState toolState.agentState.transient
  pure (advance toolState.agentState (appendMessage (toolResultMessage call result) toolState.answered) current)

advance
  :: HList.Put ContinuationState transient
  => AgentState transient
  -> Transcript
  -> ContinuationState
  -> AgentState transient
advance agentState transcript continuationState =
  agentState
    { transcript
    , turn = agentState.turn + 1
    , transient = HList.put continuationState agentState.transient
    }

toolResultMessage :: LLM.ToolCall -> ToolResult -> LLM.ChatMessage
toolResultMessage call =
  LLM.toolResult call . toolResultContent

argumentFailure :: Text -> ToolResult
argumentFailure message =
  toolFailure (permanentArgumentFailure message message).failure
