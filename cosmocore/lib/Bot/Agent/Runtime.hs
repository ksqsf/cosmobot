{-# LANGUAGE DataKinds #-}
module Bot.Agent.Runtime (startRuntime, initialState, runProgram) where

import Bot.Agent.Core
import Bot.Agent.Control (finishToolTurn)
import Bot.Agent.Program (runProgramWith)
import Bot.Agent.Transcript (closeInterruptedToolCalls)
import Bot.Agent.Result
import Bot.Agent.Tool (Tool, toolAllowed)
import Bot.Agent.ToolRegistry (resolveToolSchemas, startToolRun)
import qualified Bot.Agent.ToolRegistry as ToolRegistry
import Bot.Core.Transcript
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Effectful.Concurrent.Async as Async
import qualified Streaming.Prelude as S

startRuntime
  :: Concurrent :> es
  => Text -> Int -> host -> metadata -> [Tool host metadata (Eff es)]
  -> Eff es (Runtime host metadata context (Eff es))
startRuntime runId maxTurns context toolCallMetadata tools = do
  let exposedTools = filter (`toolAllowed` context) tools
  runningTools <- traverse (startToolRun context) exposedTools
  let dispatchToolCall metadata turn transcript =
        ToolRegistry.runToolCallWithTranscript context metadata turn transcript tools runningTools
  pure Runtime
    { runId
    , toolCallMetadata
    , context
    , tools
    , exposedTools
    , runningTools
    , dispatchToolCall
    , maxTurns = max 1 maxTurns
    , modelInputTranscript = \_ agentState -> pure agentState.transcript
    , aroundProgram = \_ program -> program
    , aroundAgentRun = \_ action -> action
    , aroundModelTurn = \_ agentState action -> action agentState
    , aroundToolTurn = \_ _ action -> action
    , aroundControlCall = \_ _ _ action -> action
    , aroundToolCall = \_ _ _ action -> action
    }

initialState :: Transcript -> TurnState
initialState transcript =
  TurnState
    { transcript = closeInterruptedToolCalls transcript
    , nextModelTranscript = Nothing
    , turn = 1
    , modelTokenUsage = Nothing
    }

runProgram
  :: (LLM.LLM :> es, Concurrent :> es, KatipE :> es)
  => (host -> Transcript -> [LLM.ChatMessage])
  -> Runtime host metadata '[] (Eff es)
  -> Program (Eff es) Result
  -> Stream (Of Output) (Eff es) Result
runProgram projectMessages runtime =
  runProgramWith
    (\Result{status, turnsUsed} -> logAgentState runtime.runId [i|step=finished status=#{status} turns=#{turnsUsed}|])
    (logAgentState runtime.runId "step=continues")
    (interpretAgentEvent projectMessages runtime)

interpretAgentEvent
  :: (LLM.LLM :> es, Concurrent :> es, KatipE :> es)
  => (host -> Transcript -> [LLM.ChatMessage])
  -> Runtime host metadata '[] (Eff es)
  -> AgentEvent response
  -> Stream (Of Output) (Eff es) response
interpretAgentEvent projectMessages runtime = \case
  RunModel agentState@TurnState{turn, transcript} -> do
    lift $ logAgentState runtime.runId
      [i|step=visible event=RunModel turn=#{turn} messages=#{transcriptMessageCount transcript}|]
    runtime.aroundModelTurn HList.HNil agentState \modelState@TurnState{turn = modelTurn} -> do
      inputTranscript <- lift (runtime.modelInputTranscript HList.HNil modelState)
      answer <- askNext projectMessages runtime modelState inputTranscript
      lift $ logAgentState runtime.runId
        [i|event=RunModel completed turn=#{modelTurn} answer=#{answerKind answer}|]
      pure (modelState, answer)
  RunTools request@ToolRequest{agentState = TurnState{turn}, toolCalls} ->
    lift do
      logAgentState runtime.runId
        [i|step=visible event=RunTools turn=#{turn} calls=#{toolCallSummary toolCalls}|]
      continuedState@TurnState{turn = nextTurn, transcript} <-
        fst <$> toolPhase runtime request
      logAgentState runtime.runId
        [i|event=RunTools completed next_turn=#{nextTurn} messages=#{transcriptMessageCount transcript}|]
      pure continuedState

logAgentState :: KatipE :> es => Text -> Text -> Eff es ()
logAgentState runId message =
  $(logDebug) [i|Agent state: run=#{runId} #{message}|]

answerKind :: LLM.ChatAnswer -> Text
answerKind = \case
  LLM.ChatFinalAnswer{} ->
    "final"
  LLM.ChatToolRequest{toolCalls} ->
    "tools:" <> toolCallSummary toolCalls

toolCallSummary :: NonEmpty LLM.ToolCall -> Text
toolCallSummary =
  Text.intercalate "," . map (\call -> call.id <> ":" <> call.name) . toList

transcriptMessageCount :: Transcript -> Int
transcriptMessageCount =
  Foldable.length . (.messages)

-- | Interpret one tool phase and advance to the next model phase.
toolPhase
  :: (Concurrent :> es, KatipE :> es)
  => Runtime host metadata '[] (Eff es)
  -> ToolRequest
  -> Eff es (TurnState, NonEmpty ToolResult)
toolPhase runtime@Runtime{aroundToolTurn = toolTurn} request =
  finishToolTurn toolTurn request $
    Async.mapConcurrently (executeToolCall runtime request.agentState) request.toolCalls

-----------------------------------------------------------------------------------------
-- * Model helpers
-----------------------------------------------------------------------------------------

-- | Ask the LLM for the next assistant message.
askNext
  :: (LLM.LLM :> es, Concurrent :> es)
  => (host -> Transcript -> [LLM.ChatMessage])
  -> Runtime host metadata context (Eff es)
  -> TurnState
  -> Transcript
  -> Stream (Of Output) (Eff es) LLM.ChatAnswer
askNext projectMessages runtime agentState transcript = do
  schemas <- lift (resolveToolSchemas agentState.transcript agentState.turn runtime.runningTools)
  S.map ContentDelta $
    LLM.askWithToolsStreaming
      schemas
      (projectMessages runtime.context transcript)

executeToolCall :: (Concurrent :> es, KatipE :> es) => Runtime host metadata '[] (Eff es) -> TurnState -> LLM.ToolCall -> Eff es ToolResult
executeToolCall runtime@Runtime{runId} agentState call@LLM.ToolCall{id = callId, name, arguments} = do
  let turn = agentState.turn
  $(logDebug)
    [i|Agent tool: run=#{runId} turn=#{turn} id=#{callId} name=#{name} state=started argument_chars=#{Text.length arguments}|]
  result <-
    runtime.aroundToolCall turn call HList.HNil
      (runtime.dispatchToolCall runtime.toolCallMetadata turn agentState.transcript call)
      `onException` $(logDebug)
        [i|Agent tool: run=#{runId} turn=#{turn} id=#{callId} name=#{name} state=interrupted|]
  $(logDebug)
    [i|Agent tool: run=#{runId} turn=#{turn} id=#{callId} name=#{name} state=finished status=#{toolResultStatus result} result_chars=#{Text.length (toolResultContent result)} images=#{length (toolResultImageUrls result)}|]
  pure result

toolResultStatus :: ToolResult -> Text
toolResultStatus =
  maybe "ok" failureStatus . toolResultFailure
