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
  , ToolEmittedMessageSink (..)
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
  , withLinkingToolEmittedMessagesToConversation
  , withRecordingToolSelfMessages
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
import Bot.Agent.Middleware.ToolResultCompaction
  ( NextModelInput (..)
  , withToolResultCompaction
  )
import Bot.Agent.Middleware.ToolEmittedMessage
  ( ToolEmittedMessageSink (..)
  , withLinkingToolEmittedMessagesToConversation
  , withRecordingToolSelfMessages
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
import qualified Bot.Effect.Media as Media
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
  :: (Chat.Chat :> es, LLM.LLM :> es, Media.Media :> es, KatipE :> es, IOE :> es)
  => Int
  -> AgentContext es
  -> [Tool es]
  -> Conversation
  -> Eff es (Text, Conversation)
runAgent maxTurns context tools conversation = do
  outputs S.:> result <- S.toList (runAgentStreaming maxTurns context tools conversation)
  pure (agentStreamAnswer outputs, result)

-- | Run an LLM/tool loop, streaming assistant content chunks.
runAgentStreaming
  :: (Chat.Chat :> es, LLM.LLM :> es, Media.Media :> es, KatipE :> es, IOE :> es)
  => Int
  -> AgentContext es
  -> [Tool es]
  -> Conversation
  -> Stream (Of AgentStreamOutput) (Eff es) Conversation
runAgentStreaming maxTurns context =
  runAgentStreamingWith maxTurns context

runAgentStreamingWith
  :: (Chat.Chat :> es, LLM.LLM :> es, Media.Media :> es, KatipE :> es, IOE :> es)
  => Int
  -> AgentContext es
  -> [Tool es]
  -> Conversation
  -> Stream (Of AgentStreamOutput) (Eff es) Conversation
runAgentStreamingWith maxTurns context tools conversation = do
  agentRun <- lift (startAgentRun context tools)
  result <- runPreparedAgentStreaming maxTurns agentRun conversation
  pure result.conversation

runPreparedAgentStreaming
  :: (LLM.LLM :> es, Media.Media :> es, KatipE :> es, IOE :> es)
  => Int
  -> AgentRun es
  -> Conversation
  -> Stream (Of AgentStreamOutput) (Eff es) AgentResult
runPreparedAgentStreaming maxTurns agentRun conversation =
  runAgentProgramStreaming (plainAgentProgram maxTurns agentRun) conversation

runAgentProgramStreaming
  :: (LLM.LLM :> es)
  => AgentProgram transient '[] es
  -> Conversation
  -> Stream (Of AgentStreamOutput) (Eff es) AgentResult
runAgentProgramStreaming program conversation =
  fmap (.result) $
    program.aroundAgentRun HList.HNil do
      runAgentLoop program HList.HNil (runModelPhase program HList.HNil) (runToolTurn program) (initialAgentState program.initialTransient conversation)

agentRunId :: AgentRun es -> Text
agentRunId =
  (.runId)

-----------------------------------------------------------------------------------------
-- * Run setup
-----------------------------------------------------------------------------------------

-- | Select tools visible to this request and start their per-run runners.
startAgentRun :: (Chat.Chat :> es, IOE :> es) => AgentContext es -> [Tool es] -> Eff es (AgentRun es)
startAgentRun context tools = do
  unique <- liftIO newUnique
  let exposedTools = filter (`toolAllowed` context) tools
      runId = [i|agent-#{hashUnique unique}|]
  runningTools <- traverse (startToolRun context) exposedTools
  pure AgentRun{runId, context, tools, exposedTools, runningTools}

initialAgentState :: HList.HList transient -> Conversation -> AgentState transient
initialAgentState transient conversation =
  AgentState
    { conversation = closeInterruptedToolCalls conversation
    , turn = 1
    , transient
    }

defaultAgentProgram :: (Chat.Chat :> es, LLM.LLM :> es, Media.Media :> es, KatipE :> es, Prim :> es) => AgentObserver ObservationContext es -> Int -> AgentRun es -> AgentProgram '[NextModelInput] '[] es
defaultAgentProgram observer maxTurns agentRun =
  ( withToolLimit maxTurns
  . withToolResultCompaction
  . withObservation observer
  . withToolMessage
  . withContextCompactionNotice
  . withToolFailureRecovery
  )
    (emptyAgentProgram (NextModelInput Nothing HList.:& HList.HNil) agentRun)

plainAgentProgram :: (LLM.LLM :> es, Media.Media :> es, KatipE :> es, IOE :> es) => Int -> AgentRun es -> AgentProgram '[NextModelInput] '[] es
plainAgentProgram maxTurns agentRun =
  ( withToolLimit maxTurns
  . withToolResultCompaction
  . withContextCompaction
  . withToolFailureRecovery
  )
    (emptyAgentProgram (NextModelInput Nothing HList.:& HList.HNil) agentRun)

-----------------------------------------------------------------------------------------
-- * Phases
-----------------------------------------------------------------------------------------

runModelPhase
  :: (LLM.LLM :> es)
  => AgentProgram transient context es
  -> MiddlewareContext context
  -> AgentState transient
  -> Stream (Of AgentStreamOutput) (Eff es) (ModelDecision transient)
runModelPhase program context agentState = do
  conversation <- lift (program.modelInputConversation context agentState)
  answer <- askNext program.agentRun conversation
  modelDecision program.agentRun agentState answer

runToolTurn :: AgentProgram transient '[] es -> ToolTurn transient es
runToolTurn program toolState =
  program.aroundToolTurn HList.HNil toolState (toolPhase program toolState)

modelDecision
  :: AgentRun es
  -> AgentState transient
  -> LLM.ChatAnswer
  -> Stream (Of AgentStreamOutput) (Eff es) (ModelDecision transient)
modelDecision agentRun agentState answer =
  case answer of
    LLM.ChatFinalAnswer{content} ->
      pure (ModelAnswered (agentCompletion agentRun "answered" content agentState.turn answered))
    LLM.ChatToolRequest{content, toolCalls} -> do
      S.yield (AgentToolCallNotification toolCalls)
      pure (ModelNeedsTools ToolTurnState{agentState, answered, toolContent = content, toolCalls})
  where
    answered =
      appendMessage (LLM.assistantAnswer answer) agentState.conversation

-- | Interpret one tool phase and advance to the next model phase.
toolPhase
  :: AgentProgram transient '[] es
  -> ToolTurnState transient
  -> Eff es (AgentState transient)
toolPhase program ToolTurnState{agentState, answered, toolCalls} = do
  nextConversation <- continueWithToolCalls program agentState.turn answered toolCalls
  pure (advanceAfterTools agentState nextConversation)

advanceAfterTools :: AgentState transient -> Conversation -> AgentState transient
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
  :: (LLM.LLM :> es)
  => AgentRun es
  -> Conversation
  -> Stream (Of AgentStreamOutput) (Eff es) LLM.ChatAnswer
askNext agentRun conversation = do
  S.map AgentContentDelta $
    LLM.askWithToolsStreaming
      (map toolSchema agentRun.exposedTools)
      (agentRequestMessages agentRun.context conversation)

agentStreamAnswer :: [AgentStreamOutput] -> Text
agentStreamAnswer =
  Text.strip . foldMap \case
    AgentContentDelta chunk ->
      chunk
    AgentToolCallNotification{} ->
      ""

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

-- | Execute requested tools and append their tool-result messages.
continueWithToolCalls
  :: AgentProgram transient '[] es
  -> Int
  -> Conversation
  -> NonEmpty LLM.ToolCall
  -> Eff es Conversation
continueWithToolCalls program turn answered calls = do
  executions <- traverse (executeToolCall program turn) calls
  let executionList = toList executions
      next = appendMessages (map (\(resultMessage, _, _) -> resultMessage) executionList <> concatMap (\(_, imageMessages, _) -> imageMessages) executionList) answered
  pure next

-- | Run one tool call and convert failures into tool-visible text.
--
-- Tool failures must still produce a tool result message; otherwise the next
-- LLM request would contain an assistant tool call without its required result.
executeToolCall :: AgentProgram transient '[] es -> Int -> LLM.ToolCall -> Eff es (LLM.ChatMessage, [LLM.ChatMessage], ToolResult)
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
    { result = AgentResult{runId = agentRun.runId, conversation}
    , status
    , finalText = answer
    , turnsUsed
    }
