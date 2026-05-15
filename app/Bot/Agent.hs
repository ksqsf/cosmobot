{-|
Module      : Bot.Agent
Description : Agent loop and extensible tool framework
Stability   : experimental
-}

module Bot.Agent
  ( Tool (..)
  , AgentContext (..)
  , AgentEvent (..)
  , AgentObserver (..)
  , AgentRun
  , AgentResult (..)
  , ToolConfig (..)
  , WebSearchApi (..)
  , defaultToolConfig
  , ignoreAgentObserver
  , startAgentRun
  , agentRunId
  , runPreparedAgentStreaming
  , ToolResult (..)
  , toolText
  , toolTextWithImages
  , toolMessage
  , toolMessageWithImages
  , runAgent
  , runAgentStreaming
  , runAgentWith
  , runAgentStreamingWith
  , defaultTools
  )
where

import Bot.Core.Conversation
import Bot.Agent.Conversation
  ( appendMessage
  , appendMessages
  , closeInterruptedToolCalls
  )
import Bot.Agent.Core
import Bot.Agent.Middleware.Observation
  ( withObservation
  , withObservedRun
  )
import Bot.Agent.Middleware.Tools
  ( withToolFailureRecovery
  , withToolLimit
  )
import Bot.Agent.ToolRegistry
  ( startToolRun
  , toolAllowed
  , toolSchema
  )
import qualified Bot.Agent.ToolRegistry as ToolRegistry
import Bot.Agent.Tools (defaultTools)
import Bot.Agent.Types
import Bot.Core.Message (IncomingMessage (..))
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Unique (hashUnique, newUnique)
import qualified Streaming.Prelude as S

-----------------------------------------------------------------------------------------
-- * Public runners
-----------------------------------------------------------------------------------------

-- | Run an LLM/tool loop until the model answers or the tool turn limit is hit.
runAgent
  :: (LLM.LLM :> es, Log :> es, IOE :> es)
  => Int
  -> AgentContext es
  -> [Tool es]
  -> Conversation
  -> Eff es (Text, Conversation)
runAgent maxTurns context tools conversation =
  S.mapM_ (\_ -> pure ()) (runAgentStreaming maxTurns context tools conversation)

-- | Run an LLM/tool loop, streaming assistant text chunks from the final model turn.
runAgentStreaming
  :: (LLM.LLM :> es, Log :> es, IOE :> es)
  => Int
  -> AgentContext es
  -> [Tool es]
  -> Conversation
  -> Stream (Of Text) (Eff es) (Text, Conversation)
runAgentStreaming maxTurns context tools conversation = do
  result <- runAgentStreamingWith ignoreAgentObserver maxTurns context tools conversation
  pure (result.answer, result.conversation)

runAgentWith
  :: (LLM.LLM :> es, Log :> es, IOE :> es)
  => AgentObserver es
  -> Int
  -> AgentContext es
  -> [Tool es]
  -> Conversation
  -> Eff es AgentResult
runAgentWith observer maxTurns context tools conversation = do
  agentRun <- startAgentRun context tools
  S.mapM_ (\_ -> pure ()) (runPreparedAgentStreaming observer maxTurns agentRun conversation)

runAgentStreamingWith
  :: (LLM.LLM :> es, Log :> es, IOE :> es)
  => AgentObserver es
  -> Int
  -> AgentContext es
  -> [Tool es]
  -> Conversation
  -> Stream (Of Text) (Eff es) AgentResult
runAgentStreamingWith observer maxTurns context tools conversation = do
  agentRun <- lift (startAgentRun context tools)
  runPreparedAgentStreaming observer maxTurns agentRun conversation

runPreparedAgentStreaming
  :: (LLM.LLM :> es, Log :> es, IOE :> es)
  => AgentObserver es
  -> Int
  -> AgentRun es
  -> Conversation
  -> Stream (Of Text) (Eff es) AgentResult
runPreparedAgentStreaming observer maxTurns agentRun conversation = do
  let normalizedMaxTurns = max 1 maxTurns
      program = defaultAgentProgram observer normalizedMaxTurns agentRun
  withAgentRun observer normalizedMaxTurns agentRun do
    runAgentLoop program (runModelPhase program) (runToolTurn program) (initialAgentState conversation)

agentRunId :: AgentRun es -> Text
agentRunId =
  (.runId)

-----------------------------------------------------------------------------------------
-- * Run setup
-----------------------------------------------------------------------------------------

-- | Select tools visible to this request and start their per-run runners.
startAgentRun :: IOE :> es => AgentContext es -> [Tool es] -> Eff es (AgentRun es)
startAgentRun context tools = do
  unique <- liftIO newUnique
  let exposedTools = filter (`toolAllowed` context) tools
      runId = [i|agent-#{hashUnique unique}|]
  runningTools <- traverse (startToolRun context) exposedTools
  pure AgentRun{runId, context, tools, exposedTools, runningTools}

initialAgentState :: Conversation -> AgentState
initialAgentState conversation =
  AgentState
    { conversation = closeInterruptedToolCalls conversation
    , turn = 1
    }

defaultAgentProgram :: (Log :> es, IOE :> es) => AgentObserver es -> Int -> AgentRun es -> AgentProgram es
defaultAgentProgram observer maxTurns agentRun =
  emptyAgentProgram agentRun
    & withToolFailureRecovery
    & withToolLimit maxTurns
    & withObservation observer

-----------------------------------------------------------------------------------------
-- * Phases
-----------------------------------------------------------------------------------------

runModelPhase
  :: (LLM.LLM :> es, IOE :> es)
  => AgentProgram es
  -> AgentState
  -> Stream (Of Text) (Eff es) ModelDecision
runModelPhase program agentState = do
  answer <- askNext program.agentRun agentState
  modelDecision program.agentRun agentState answer

runToolTurn :: AgentProgram es -> ToolTurn es
runToolTurn program toolState =
  program.aroundToolTurn toolState (toolPhase program toolState)

modelDecision
  :: AgentRun es
  -> AgentState
  -> LLM.ChatAnswer
  -> Stream (Of Text) (Eff es) ModelDecision
modelDecision agentRun agentState answer =
  case answer of
    LLM.ChatFinalAnswer{content} ->
      pure (ModelAnswered (agentCompletion agentRun "answered" content agentState.turn answered))
    LLM.ChatToolRequest{content, toolCalls} ->
      pure (ModelNeedsTools ToolTurnState{agentState, answered, toolContent = content, toolCalls})
  where
    answered =
      appendMessage (LLM.assistantAnswer answer) agentState.conversation

-- | Interpret one tool phase and advance to the next model phase.
toolPhase
  :: AgentProgram es
  -> ToolTurnState
  -> Eff es AgentState
toolPhase program ToolTurnState{agentState, answered, toolCalls} = do
  nextConversation <- continueWithToolCalls program agentState.turn answered toolCalls
  pure (advanceAfterTools agentState nextConversation)

advanceAfterTools :: AgentState -> Conversation -> AgentState
advanceAfterTools agentState conversation =
  agentState
    { conversation = conversation
    , turn = agentState.turn + 1
    }

-----------------------------------------------------------------------------------------
-- * Model helpers
-----------------------------------------------------------------------------------------

-- | Ask the LLM for the next assistant message.
askNext
  :: (LLM.LLM :> es, IOE :> es)
  => AgentRun es
  -> AgentState
  -> Stream (Of Text) (Eff es) LLM.ChatAnswer
askNext agentRun agentState =
  LLM.askWithToolsStreaming
    (map toolSchema agentRun.exposedTools)
    (agentRequestMessages agentRun.context agentState.conversation)

agentRequestMessages :: AgentContext es -> Conversation -> [LLM.ChatMessage]
agentRequestMessages context (Conversation messages)
  | Text.null (Text.strip context.systemContext) =
      Foldable.toList messages
  | otherwise =
      Foldable.toList (LLM.systemText context.systemContext Seq.<| messages)

-----------------------------------------------------------------------------------------
-- * Tool execution
-----------------------------------------------------------------------------------------

-- | Execute requested tools, append their tool-result messages, and persist
-- aliases for any chat messages emitted by tools.
continueWithToolCalls
  :: AgentProgram es
  -> Int
  -> Conversation
  -> NonEmpty LLM.ToolCall
  -> Eff es Conversation
continueWithToolCalls program turn answered calls = do
  executions <- traverse (executeToolCall program turn) calls
  let executionList = toList executions
      next = appendMessages (map (\(resultMessage, _, _) -> resultMessage) executionList <> concatMap (\(_, imageMessages, _) -> imageMessages) executionList) answered
  traverse_ (\messageId -> program.agentRun.context.remember messageId next) (concatMap (\(_, _, messageIds) -> messageIds) executionList)
  pure next

-- | Run one tool call and convert failures into tool-visible text.
--
-- Tool failures must still produce a tool result message; otherwise the next
-- LLM request would contain an assistant tool call without its required result.
executeToolCall :: AgentProgram es -> Int -> LLM.ToolCall -> Eff es (LLM.ChatMessage, [LLM.ChatMessage], [Maybe Integer])
executeToolCall program turn call = do
  result <- program.aroundToolCall turn call do
    ToolRegistry.runToolCall program.agentRun.context program.agentRun.tools program.agentRun.runningTools call
  pure (LLM.toolResult call result.content, toolImageContextMessages call result, result.messageIds)

toolImageContextMessages :: LLM.ToolCall -> ToolResult -> [LLM.ChatMessage]
toolImageContextMessages call result =
  [ LLM.userWithImages (toolImageContextText call result) result.imageUrls
  | not (null result.imageUrls)
  ]

toolImageContextText :: LLM.ToolCall -> ToolResult -> Text
toolImageContextText call result =
  Text.strip [i|Image context returned by tool #{toolName}:
#{toolContent}|]
  where
    toolName = call.name
    toolContent = result.content

-----------------------------------------------------------------------------------------
-- * Completion
-----------------------------------------------------------------------------------------

agentCompletion :: AgentRun es -> Text -> Text -> Int -> Conversation -> AgentCompletion
agentCompletion agentRun status answer turnsUsed conversation =
  AgentCompletion
    { result = AgentResult{runId = agentRun.runId, answer, conversation}
    , status
    , finalText = answer
    , turnsUsed
    }

withAgentRun
  :: IOE :> es
  => AgentObserver es
  -> Int
  -> AgentRun es
  -> Stream (Of Text) (Eff es) AgentCompletion
  -> Stream (Of Text) (Eff es) AgentResult
withAgentRun observer maxTurns agentRun action =
  fmap (.result) $
  withObservedRun
    observer
    agentRun.runId
    agentRun.context.message.messageId
    maxTurns
    (map (.name) agentRun.exposedTools)
    (\completion -> (completion.status, completion.finalText, completion.turnsUsed))
    action
