{-|
Module      : Bot.Agent.Middleware.Observation
Description : Agent lifecycle observation wrappers
Stability   : experimental
-}

module Bot.Agent.Middleware.Observation
  ( ObservedConversationLink (..)
  , ObservedModelTurn (..)
  , ObservedToolCall (..)
  , observeConversationLinked
  , withObservation
  , withObservedModelTurn
  , withObservedRun
  , withObservedToolCall
  )
where

import Bot.Agent.Core
import Bot.Agent.Types
import Bot.Core.Conversation
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
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
  , parentMessageId :: !(Maybe Integer)
  , linkedMessageId :: !Integer
  }

withObservation :: IOE :> es => AgentObserver es -> AgentProgram es -> AgentProgram es
withObservation observer program =
  program
    { aroundModelTurn = \agentState action ->
        let turnInfo = ObservedModelTurn
              { runId = program.agentRun.runId
              , turn = agentState.turn
              , messageCount = conversationMessageCount agentState
              , exposedTools = map (.name) program.agentRun.exposedTools
              , finished = modelDecisionFinished program.agentRun.runId agentState.turn
              }
        in withObservedModelTurn observer turnInfo (program.aroundModelTurn agentState action)
    , aroundToolCall = \turn call action ->
        let callInfo = ObservedToolCall
              { runId = program.agentRun.runId
              , turn = turn
              , toolCall = call
              }
        in withObservedToolCall observer callInfo do
             program.aroundToolCall turn call action
    }
  where
    modelDecisionFinished runId turn = \case
      ModelAnswered AgentCompletion{result} ->
        ModelTurnFinished
          { runId = runId
          , turn = turn
          , answerKind = "final"
          , contentLength = Text.length result.answer
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

withObservedRun
  :: IOE :> es
  => AgentObserver es
  -> Text
  -> Maybe Integer
  -> Int
  -> [Text]
  -> (result -> (Text, Text, Int))
  -> Stream (Of a) (Eff es) result
  -> Stream (Of a) (Eff es) result
withObservedRun observer runId messageId maxTurns exposedTools finish action =
  catchStream
    ( do
        lift $ observer.observe AgentRunStarted{runId, messageId, maxTurns, exposedTools}
        result <- action
        let (status, finalText, turnsUsed) = finish result
        lift $ observer.observe AgentRunFinished
          { runId
          , status
          , finalLength = Text.length finalText
          , turnsUsed
          }
        pure result
    )
    \err -> do
      lift $ observer.observe AgentRunInterrupted{runId, reason = interruptedReason err}
      lift $ throwIO err

withObservedModelTurn
  :: IOE :> es
  => AgentObserver es
  -> ObservedModelTurn result
  -> Stream (Of Text) (Eff es) result
  -> Stream (Of Text) (Eff es) result
withObservedModelTurn observer turnInfo action = do
  lift $ observer.observe ModelTurnStarted
    { runId = turnInfo.runId
    , turn = turnInfo.turn
    , messageCount = turnInfo.messageCount
    , exposedTools = turnInfo.exposedTools
    }
  result <- action
  lift $ observer.observe (turnInfo.finished result)
  pure result

withObservedToolCall
  :: IOE :> es
  => AgentObserver es
  -> ObservedToolCall
  -> Eff es ToolResult
  -> Eff es ToolResult
withObservedToolCall observer callInfo action = do
  observer.observe ToolCallStarted
    { runId = callInfo.runId
    , turn = callInfo.turn
    , toolCall = callInfo.toolCall
    }
  (status, result) <-
    statusFromResult <$> action `catch` \(err :: SomeException) ->
      if isAsyncException err
        then do
          finishToolCall observer callInfo "interrupted" (toolText "")
          throwIO err
        else
          throwIO err
  finishToolCall observer callInfo status result
  pure result

statusFromResult :: ToolResult -> (Text, ToolResult)
statusFromResult result
  | "Tool " `Text.isPrefixOf` result.content && " failed: " `Text.isInfixOf` result.content =
      ("failed", result)
  | otherwise =
      ("ok", result)

observeConversationLinked :: AgentObserver es -> ObservedConversationLink -> Eff es ()
observeConversationLinked observer ObservedConversationLink{runId, parentMessageId, linkedMessageId} =
  observer.observe AgentConversationLinked{runId, linkedMessageId, parentMessageId}

finishToolCall :: AgentObserver es -> ObservedToolCall -> Text -> ToolResult -> Eff es ()
finishToolCall observer callInfo status result =
  observer.observe ToolCallFinished
    { runId = callInfo.runId
    , turn = callInfo.turn
    , toolCallId = callInfo.toolCall.id
    , toolName = callInfo.toolCall.name
    , status = status
    , result = result.content
    , resultLength = Text.length result.content
    , messageIds = result.messageIds
    }

catchStream
  :: IOE :> es
  => Stream (Of a) (Eff es) r
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
interruptedReason err
  | isAsyncException err = "interrupted"
  | otherwise = "failed"
