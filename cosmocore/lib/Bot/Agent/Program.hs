{-# LANGUAGE RankNTypes #-}
module Bot.Agent.Program (agentProgram, agentCompletion, runProgramWith, limitProgram) where

import Bot.Core.Transcript (Transcript)
import Bot.Agent.Core
import Bot.Agent.Transcript (appendMessage)
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Text as Text
import qualified Streaming.Prelude as S

agentProgram
  :: Monad m => Text
  -> TurnState
  -> Program m Result
agentProgram runId agentState = do
  (modelState, answer) <- runModel agentState
  interpretModelAnswer runId modelState answer

interpretModelAnswer
  :: Monad m => Text
  -> TurnState
  -> LLM.ChatAnswer
  -> Program m Result
interpretModelAnswer runId agentState answer =
  case answer of
    LLM.ChatFinalAnswer{content} ->
      pure (agentCompletion runId "answered" content agentState.turn (LLM.chatAnswerTokenUsage answer) answered)
    LLM.ChatToolRequest{content, toolCalls}
      | Just message <- toolCallIdError toolCalls ->
          modelProtocolError runId agentState answer message
      | otherwise -> do
          Program do
            S.yield (ToolCallNotification toolCalls)
            pure (Finished ())
          nextState <- runTools ToolRequest{agentState = observedState, answered, toolContent = content, toolCalls}
          agentProgram runId nextState
  where
    observedState =
      agentState{modelTokenUsage = LLM.chatAnswerTokenUsage answer}
    answered =
      appendMessage (LLM.assistantAnswer answer) agentState.transcript

modelProtocolError
  :: Monad m => Text
  -> TurnState
  -> LLM.ChatAnswer
  -> Text
  -> Program m Result
modelProtocolError runId agentState answer message =
  Program do
    S.yield (ContentDelta message)
    pure . Finished $
      agentCompletion
        runId
        "model_protocol_error"
        message
        agentState.turn
        (LLM.chatAnswerTokenUsage answer)
        agentState.transcript

toolCallIdError :: NonEmpty LLM.ToolCall -> Maybe Text
toolCallIdError calls
  | any (Text.null . Text.strip . (.id)) callList =
      Just "Model returned a tool call with an empty id."
  | duplicateIds@(_ : _) <-
      mapMaybe listToMaybe
        . filter ((> 1) . length)
        . group
        . sort
        $ map (.id) callList =
      Just [i|Model returned duplicate tool-call ids: #{Text.intercalate ", " duplicateIds}|]
  | otherwise =
      Nothing
  where
    callList = toList calls

agentCompletion :: Text -> Text -> Text -> Int -> Maybe LLM.TokenUsage -> Transcript -> Result
agentCompletion runId status answer turnsUsed tokenUsage transcript =
  Result
    { runId
    , transcript
    , status
    , finalText = answer
    , turnsUsed
    , tokenUsage
    }

-- | Interpret the calculus while leaving effects and lifecycle policy to the host.
runProgramWith
  :: Monad m
  => (result -> m ())
  -> m ()
  -> (forall response. AgentEvent response -> Stream (Of Output) m response)
  -> Program m result
  -> Stream (Of Output) m result
runProgramWith finished continued interpretEvent = go
  where
    go program = program.observe >>= \case
      Finished result -> lift (finished result) *> pure result
      Continues next -> lift continued *> go next
      Visible event continue -> interpretEvent event >>= go . continue

limitProgram
  :: Monad m
  => Int
  -> (NonEmpty LLM.ToolCall -> Bool)
  -> (ToolRequest -> Stream (Of Output) m (Step m Result))
  -> Program m Result
  -> Program m Result
limitProgram maxTurns mayTransfer finish (Program action) =
  Program $ action >>= \case
    Finished result ->
      pure (Finished result)
    Continues next ->
      pure (Continues (limitProgram maxTurns mayTransfer finish next))
    Visible (RunTools request) continue
      | request.agentState.turn >= maxTurns
      , not (mayTransfer request.toolCalls) ->
          finish request
      | otherwise ->
          pure (Visible (RunTools request) (limitProgram maxTurns mayTransfer finish . continue))
    Visible event continue ->
      pure (Visible event (limitProgram maxTurns mayTransfer finish . continue))
