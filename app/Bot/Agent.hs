{-|
Module      : Bot.Agent
Description : Agent loop and extensible tool framework
Stability   : experimental
-}
{-# LANGUAGE DataKinds #-}

module Bot.Agent
  ( Tool (..)
  , AgentContext (..)
  , AgentEvent (..)
  , AgentObserver (..)
  , AgentProgram
  , AgentRun
  , AgentResult (..)
  , AgentStreamOutput (..)
  , AgentFailureCategory (..)
  , AgentFailure (..)
  , AgentException (..)
  , ToolConfig (..)
  , WebSearchApi (..)
  , defaultToolConfig
  , startAgentRun
  , agentRunId
  , defaultAgentProgram
  , runAgentProgramStreaming
  , runPreparedAgentStreaming
  , ToolResult (..)
  , toolText
  , toolTextWithImages
  , toolFailure
  , toolMessage
  , toolMessageWithImages
  , runAgent
  , runAgentStreaming
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
import Bot.Agent.Middleware.ContextCompaction
  ( withContextCompaction
  , withContextCompactionNotice
  )
import Bot.Agent.Middleware.Observation
  ( ObservationContext
  , withObservation
  )
import Bot.Agent.Middleware.Tools
  ( withToolFailureRecovery
  , withToolLimit
  , withToolMessage
  )
import Bot.Agent.ToolRegistry
  ( startToolRun
  , toolAllowed
  , toolSchema
  )
import qualified Bot.Agent.ToolRegistry as ToolRegistry
import Bot.Agent.Tools (defaultTools)
import Bot.Agent.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Data.Foldable as Foldable
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
  -> Stream (Of AgentStreamOutput) (Eff es) (Text, Conversation)
runAgentStreaming maxTurns context tools conversation = do
  agentRun <- lift (startAgentRun context tools)
  result <- runPreparedAgentStreaming maxTurns agentRun conversation
  pure (result.answer, result.conversation)

runPreparedAgentStreaming
  :: (LLM.LLM :> es, Log :> es, IOE :> es)
  => Int
  -> AgentRun es
  -> Conversation
  -> Stream (Of AgentStreamOutput) (Eff es) AgentResult
runPreparedAgentStreaming maxTurns agentRun conversation =
  runAgentProgramStreaming (plainAgentProgram maxTurns agentRun) conversation

runAgentProgramStreaming
  :: (LLM.LLM :> es, IOE :> es)
  => AgentProgram '[] es
  -> Conversation
  -> Stream (Of AgentStreamOutput) (Eff es) AgentResult
runAgentProgramStreaming program conversation =
  fmap (.result) $
    program.aroundAgentRun HList.HNil do
      runAgentLoop program HList.HNil (runModelPhase program) (runToolTurn program) (initialAgentState conversation)

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

defaultAgentProgram :: (Chat.Chat :> es, LLM.LLM :> es, Log :> es, IOE :> es) => AgentObserver ObservationContext es -> Int -> AgentRun es -> AgentProgram '[] es
defaultAgentProgram observer maxTurns agentRun =
  ( withToolLimit maxTurns
  . withObservation observer
  . withToolMessage
  . withContextCompactionNotice
  . withToolFailureRecovery
  )
    (emptyAgentProgram agentRun)

plainAgentProgram :: (LLM.LLM :> es, Log :> es, IOE :> es) => Int -> AgentRun es -> AgentProgram '[] es
plainAgentProgram maxTurns agentRun =
  ( withToolLimit maxTurns
  . withContextCompaction
  . withToolFailureRecovery
  )
    (emptyAgentProgram agentRun)

-----------------------------------------------------------------------------------------
-- * Phases
-----------------------------------------------------------------------------------------

runModelPhase
  :: (LLM.LLM :> es, IOE :> es)
  => AgentProgram context es
  -> AgentState
  -> Stream (Of AgentStreamOutput) (Eff es) ModelDecision
runModelPhase program agentState = do
  answer <- askNext program.agentRun agentState
  modelDecision program.agentRun agentState answer

runToolTurn :: AgentProgram '[] es -> ToolTurn es
runToolTurn program toolState =
  program.aroundToolTurn HList.HNil toolState (toolPhase program toolState)

modelDecision
  :: AgentRun es
  -> AgentState
  -> LLM.ChatAnswer
  -> Stream (Of AgentStreamOutput) (Eff es) ModelDecision
modelDecision agentRun agentState answer =
  case answer of
    LLM.ChatFinalAnswer{content} ->
      pure (ModelAnswered (agentCompletion agentRun "answered" content agentState.turn answered))
    LLM.ChatToolRequest{content, toolCalls} -> do
      let strippedContent = Text.strip content
      unless (Text.null strippedContent) do
        S.yield (AgentIntermediateMessage strippedContent answered)
      pure (ModelNeedsTools ToolTurnState{agentState, answered, toolContent = content, toolCalls})
  where
    answered =
      appendMessage (LLM.assistantAnswer answer) agentState.conversation

-- | Interpret one tool phase and advance to the next model phase.
toolPhase
  :: AgentProgram '[] es
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
  -> Stream (Of AgentStreamOutput) (Eff es) LLM.ChatAnswer
askNext agentRun agentState = do
  translateLLMStream $
    LLM.askWithToolsStreaming
      (map toolSchema agentRun.exposedTools)
      (agentRequestMessages agentRun.context agentState.conversation)

translateLLMStream :: Stream (Of Text) (Eff es) LLM.ChatAnswer -> Stream (Of AgentStreamOutput) (Eff es) LLM.ChatAnswer
translateLLMStream stream = do
  next <- lift (S.next stream)
  case next of
    Left answer ->
      pure answer
    Right (chunk, rest) -> do
      S.yield (AgentAnswerDelta chunk)
      translateLLMStream rest

agentRequestMessages :: AgentContext es -> Conversation -> [LLM.ChatMessage]
agentRequestMessages context (Conversation messages) =
  mergeSystemContext context.systemContext (Foldable.toList messages)

mergeSystemContext :: Text -> [LLM.ChatMessage] -> [LLM.ChatMessage]
mergeSystemContext context messages
  | Text.null strippedContext = messages
  | otherwise =
      case messages of
        firstMessage : rest
          | firstMessage.role == "system"
          , Just (LLM.TextContent systemPrompt) <- firstMessage.content ->
              replaceMessageContent (Just (LLM.TextContent (joinSystemPrompts systemPrompt strippedContext))) firstMessage : rest
        _ ->
          LLM.systemText strippedContext : messages
  where
    strippedContext = Text.strip context

joinSystemPrompts :: Text -> Text -> Text
joinSystemPrompts systemPrompt context =
  Text.strip $ Text.intercalate "\n\n" [systemPrompt, context]

replaceMessageContent :: Maybe LLM.MessageContent -> LLM.ChatMessage -> LLM.ChatMessage
replaceMessageContent content LLM.ChatMessage{role, toolCalls, toolCallId} =
  LLM.ChatMessage role content toolCalls toolCallId

-----------------------------------------------------------------------------------------
-- * Tool execution
-----------------------------------------------------------------------------------------

-- | Execute requested tools, append their tool-result messages, and persist
-- aliases for any chat messages emitted by tools.
continueWithToolCalls
  :: AgentProgram '[] es
  -> Int
  -> Conversation
  -> NonEmpty LLM.ToolCall
  -> Eff es Conversation
continueWithToolCalls program turn answered calls = do
  executions <- traverse (executeToolCall program turn) calls
  let executionList = toList executions
      next = appendMessages (map (\(resultMessage, _, _) -> resultMessage) executionList <> concatMap (\(_, imageMessages, _) -> imageMessages) executionList) answered
  traverse_ (\messageId -> program.agentRun.context.remember messageId next) (concatMap (\(_, _, result) -> toolResultMessageIds result) executionList)
  pure next

-- | Run one tool call and convert failures into tool-visible text.
--
-- Tool failures must still produce a tool result message; otherwise the next
-- LLM request would contain an assistant tool call without its required result.
executeToolCall :: AgentProgram '[] es -> Int -> LLM.ToolCall -> Eff es (LLM.ChatMessage, [LLM.ChatMessage], ToolResult)
executeToolCall program turn call = do
  result <- program.aroundToolCall turn call HList.HNil do
    ToolRegistry.runToolCall program.agentRun.context program.agentRun.tools program.agentRun.runningTools call
  pure (LLM.toolResult call (toolResultContent result), toolImageContextMessages call result, result)

toolImageContextMessages :: LLM.ToolCall -> ToolResult -> [LLM.ChatMessage]
toolImageContextMessages call result =
  [ LLM.userWithImages (toolImageContextText call result) imageUrls
  | let imageUrls = toolResultImageUrls result
  , not (null imageUrls)
  ]

toolImageContextText :: LLM.ToolCall -> ToolResult -> Text
toolImageContextText call result =
  Text.strip [i|Image context returned by tool #{toolName}:
#{toolContent}|]
  where
    toolName = call.name
    toolContent = toolResultContent result

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
