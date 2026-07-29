{-# LANGUAGE DataKinds #-}
{-|
Module      : Bot.Agent
Description : Agent loop and extensible tool framework
Stability   : experimental
-}

module Bot.Agent
  ( ToolCallMetadata (..)
  , Context (..)
  , Event (..)
  , Observer
  , Program
  , Runtime
  , Result (..)
  , Output (..)
  , ToolEmittedMessageSink (..)
  , SteeringControl (..)
  , FailureCategory (..)
  , Failure (..)
  , ToolConfig (..)
  , WebSearchApi (..)
  , defaultToolConfig
  , startRuntime
  , startRuntimeWithParent
  , runIdOf
  , defaultRuntime
  , agentStream
  , ToolResult (..)
  , toolText
  , toolTextWithImages
  , toolFailure
  , withLinkingToolEmittedMessagesToThread
  , withNormalizingToolReplies
  , withRecordingToolSelfMessages
  , withSteering
  , withTypingNotification
  , runAgent
  , runAgentWithParent
  , runObservedChildAgent
  , runAgentStreaming
  )
where

import Bot.Core.Transcript
import Bot.Agent.Transcript
  ( appendMessage
  , appendMessages
  , closeInterruptedToolCalls
  )
import Bot.Agent.Core
import Bot.Agent.Middleware.ContextCompaction
  ( withContextCompaction
  , withContextCompactionNotice
  )
import Bot.Agent.Middleware.Continuation
  ( withContinuations
  )
import Bot.Agent.Middleware.Observation
  ( ObservationContext
  , withObservation
  )
import Bot.Agent.Middleware.Observation.Types
  ( EventObservation
  , ToolResultObservation
  )
import Bot.Agent.Middleware.Steering
  ( SteeringControl (..)
  , withSteering
  )
import Bot.Agent.Middleware.Tools
  ( withToolFailureRecovery
  , withToolLimit
  , withToolMessage
  )
import Bot.Agent.Middleware.ToolResultCompaction
  ( withToolResultCompaction
  )
import Bot.Agent.Middleware.ToolEmittedMessage
  ( ToolEmittedMessageSink (..)
  , withLinkingToolEmittedMessagesToThread
  , withRecordingToolSelfMessages
  )
import Bot.Agent.Middleware.ToolReplyNormalization
  ( withNormalizingToolReplies
  )
import Bot.Agent.Middleware.Typing
  ( withTypingNotification
  )
import Bot.Agent.ToolRegistry
  ( resolveToolSchemas
  , startToolRun
  )
import qualified Bot.Agent.ToolRegistry as ToolRegistry
import Bot.Agent.Tool (Tool, toolAllowed, toolName)
import Bot.Agent.Tools.Continuation (resumeContinuationTool)
import Bot.Agent.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Crypto.Random as CryptoRandom
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.Async as Async
import qualified Streaming.Prelude as S

-----------------------------------------------------------------------------------------
-- * Public runners
-----------------------------------------------------------------------------------------

-- | Run an LLM/tool loop until the model answers or the tool turn limit is hit.
runAgent
  :: (Chat.Chat :> es, LLM.LLM :> es, Media.Media :> es, KatipE :> es, Concurrent :> es, IOE :> es)
  => Int
  -> Context
  -> [Tool es]
  -> Transcript
  -> Eff es (Text, Transcript)
runAgent maxTurns = runAgentWithParent Nothing maxTurns

runAgentWithParent
  :: (Chat.Chat :> es, LLM.LLM :> es, Media.Media :> es, KatipE :> es, Concurrent :> es, IOE :> es)
  => Maybe Concurrency.Handle
  -> Int
  -> Context
  -> [Tool es]
  -> Transcript
  -> Eff es (Text, Transcript)
runAgentWithParent parent maxTurns context tools transcript = do
  runtime <- startRuntimeWithParent parent maxTurns context tools
  outputs S.:> result <-
    S.toList $
      agentStream
        (plainRuntime defaultCompactionTokenThreshold runtime)
        transcript
  pure (agentStreamAnswer outputs, result.transcript)

runObservedChildAgent
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, LLM.LLM :> es, Media.Media :> es, KatipE :> es, Prim :> es, Concurrent :> es, IOE :> es)
  => Observer ObservationContext es
  -> ToolCallMetadata
  -> Text
  -> Concurrency.Handle
  -> Int
  -> Context
  -> [Tool es]
  -> Transcript
  -> Eff es (Text, Transcript)
runObservedChildAgent observer parentMetadata childLabel parent maxTurns context tools transcript = do
  childRun <- startRuntimeWithOrigin (Just parent) (Just parentMetadata.originRunId) maxTurns context tools
  void $ observer SubAgentRunStarted
    { runId = parentMetadata.agentRunId
    , childRunId = childRun.runId
    , subagentId = childLabel
    }
  outputs S.:> result <-
    S.toList $
      agentStream
        (defaultRuntime observer defaultCompactionTokenThreshold childRun)
        transcript
  pure (agentStreamAnswer outputs, result.transcript)

-- | Run an LLM/tool loop, streaming assistant content chunks.
runAgentStreaming
  :: (Chat.Chat :> es, LLM.LLM :> es, Media.Media :> es, KatipE :> es, Concurrent :> es, IOE :> es)
  => Int
  -> Context
  -> [Tool es]
  -> Transcript
  -> Stream (Of Output) (Eff es) Transcript
runAgentStreaming maxTurns context =
  runAgentStreamingWith maxTurns context

runAgentStreamingWith
  :: (Chat.Chat :> es, LLM.LLM :> es, Media.Media :> es, KatipE :> es, Concurrent :> es, IOE :> es)
  => Int
  -> Context
  -> [Tool es]
  -> Transcript
  -> Stream (Of Output) (Eff es) Transcript
runAgentStreamingWith maxTurns context tools transcript = do
  runtime <- lift (startRuntime maxTurns context tools)
  result <-
    agentStream
      (plainRuntime defaultCompactionTokenThreshold runtime)
      transcript
  pure result.transcript

defaultCompactionTokenThreshold :: Int
defaultCompactionTokenThreshold =
  1000000

agentStream
  :: (LLM.LLM :> es, Concurrent :> es, KatipE :> es)
  => Runtime '[] es
  -> Transcript
  -> Stream (Of Output) (Eff es) Result
agentStream runtime transcript =
  runtime.aroundAgentRun HList.HNil $
    runProgram runtime (restart (initialState transcript))
  where
    restart agentState =
      runtime.aroundProgram runtime (nextModel runtime HList.HNil restart agentState)

runIdOf :: Runtime context es -> Text
runIdOf =
  (.runId)

-----------------------------------------------------------------------------------------
-- * Run setup
-----------------------------------------------------------------------------------------

-- | Select visible tools and construct an identity-middleware runtime.
startRuntime :: (Chat.Chat :> es, Concurrent :> es, IOE :> es) => Int -> Context -> [Tool es] -> Eff es (Runtime context es)
startRuntime = startRuntimeWithParent Nothing

startRuntimeWithParent :: (Chat.Chat :> es, Concurrent :> es, IOE :> es) => Maybe Concurrency.Handle -> Int -> Context -> [Tool es] -> Eff es (Runtime context es)
startRuntimeWithParent parent =
  startRuntimeWithOrigin parent Nothing

startRuntimeWithOrigin :: (Chat.Chat :> es, Concurrent :> es, IOE :> es) => Maybe Concurrency.Handle -> Maybe Text -> Int -> Context -> [Tool es] -> Eff es (Runtime context es)
startRuntimeWithOrigin parent requestedOriginRunId maxTurns context tools = do
  runId <- newAgentRunId
  let exposedTools = filter (`toolAllowed` context) tools
      originRunId = fromMaybe runId requestedOriginRunId
      toolCallMetadata = ToolCallMetadata{agentRunId = runId, originRunId, parent}
  runningTools <- traverse (startToolRun context) exposedTools
  pure Runtime
    { runId
    , toolCallMetadata
    , context
    , tools
    , exposedTools
    , runningTools
    , maxTurns = max 1 maxTurns
    , modelInputTranscript = \_ agentState -> pure agentState.transcript
    , aroundProgram = \_ program -> program
    , aroundAgentRun = \_ action -> action
    , aroundModelTurn = \_ _ agentState action -> action agentState
    , aroundToolTurn = \_ _ action -> action
    , aroundToolCall = \_ _ _ action -> action
    }

newAgentRunId :: IOE :> es => Eff es Text
newAgentRunId = do
  bytes <- liftIO (CryptoRandom.getRandomBytes 16 :: IO StrictByteString.ByteString)
  pure ("agent-" <> TextEncoding.decodeUtf8 (Base64URL.encodeUnpadded bytes))

initialState :: Transcript -> TurnState
initialState transcript =
  TurnState
    { transcript = closeInterruptedToolCalls transcript
    , nextModelTranscript = Nothing
    , turn = 1
    , modelTokenUsage = Nothing
    }

defaultRuntime
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, LLM.LLM :> es, Media.Media :> es, KatipE :> es, Prim :> es)
  => Observer ObservationContext es
  -> Int
  -> Runtime '[ObservationContext, EventObservation es, ToolResultObservation es] es
  -> Runtime '[] es
defaultRuntime observer compactionTokenThreshold =
  ( withContinuations
  . withToolLimit isResumeTransfer
  . withTypingNotification
  . withToolResultCompaction
  . withObservation observer
  . withToolMessage
  . withContextCompactionNotice compactionTokenThreshold
  . withToolFailureRecovery
  )

plainRuntime
  :: (LLM.LLM :> es, Media.Media :> es, KatipE :> es, IOE :> es)
  => Int
  -> Runtime '[ToolResultObservation es] es
  -> Runtime '[] es
plainRuntime compactionTokenThreshold =
  ( withContinuations
  . withToolLimit isResumeTransfer
  . withToolResultCompaction
  . withContextCompaction compactionTokenThreshold
  . withToolFailureRecovery
  )

-----------------------------------------------------------------------------------------
-- * Phases
-----------------------------------------------------------------------------------------

runModelPhase
  :: (LLM.LLM :> es, Concurrent :> es)
  => Runtime context es
  -> HList.HList context
  -> (TurnState -> Program es Result)
  -> TurnState
  -> Stream (Of Output) (Eff es) (Step es Result)
runModelPhase runtime context continue agentState = do
  transcript <- lift (runtime.modelInputTranscript context agentState)
  answer <- askNext runtime agentState transcript
  modelDecision runtime continue agentState answer

isResumeTransfer :: Runtime context es -> NonEmpty LLM.ToolCall -> Bool
isResumeTransfer runtime calls =
  case toList calls of
    [call] ->
      call.name == toolName resumeContinuationTool
        && any ((== call.name) . toolName) runtime.exposedTools
    _ ->
      False

nextModel
  :: (LLM.LLM :> es, Concurrent :> es)
  => Runtime context es
  -> HList.HList context
  -> (TurnState -> Program es Result)
  -> TurnState
  -> Program es Result
nextModel runtime context restart agentState =
  Program $
    runtime.aroundModelTurn
      context
      restart
      agentState
      (runModelPhase runtime context (nextModel runtime context restart))

runProgram
  :: Concurrent :> es
  => Runtime '[] es
  -> Program es result
  -> Stream (Of Output) (Eff es) result
runProgram runtime@Runtime{aroundToolTurn = toolTurn} program =
  program.observe >>= \case
    Finished result ->
      pure result
    NeedsTools request continue -> do
      (continuedState, ()) <- lift $ toolTurn HList.HNil request ((, ()) <$> toolPhase runtime request)
      runProgram runtime (continue continuedState)
    Continues next ->
      runProgram runtime next

modelDecision
  :: Runtime context es
  -> (TurnState -> Program es Result)
  -> TurnState
  -> LLM.ChatAnswer
  -> Stream (Of Output) (Eff es) (Step es Result)
modelDecision runtime continue agentState answer =
  case answer of
    LLM.ChatFinalAnswer{content} ->
      pure (Finished (agentCompletion runtime "answered" content agentState.turn (LLM.chatAnswerTokenUsage answer) answered))
    LLM.ChatToolRequest{content, toolCalls} -> do
      S.yield (ToolCallNotification toolCalls)
      pure (NeedsTools ToolRequest{agentState = observedState, answered, toolContent = content, toolCalls} continue)
  where
    observedState =
      agentState{modelTokenUsage = LLM.chatAnswerTokenUsage answer}
    answered =
      appendMessage (LLM.assistantAnswer answer) agentState.transcript

-- | Interpret one tool phase and advance to the next model phase.
toolPhase
  :: Concurrent :> es
  => Runtime '[] es
  -> ToolRequest
  -> Eff es TurnState
toolPhase runtime ToolRequest{agentState, answered, toolCalls} = do
  nextTranscript <- continueWithToolCalls runtime agentState.turn answered toolCalls
  pure (advanceAfterTools agentState nextTranscript)

advanceAfterTools :: TurnState -> Transcript -> TurnState
advanceAfterTools agentState transcript =
  agentState
    { transcript = transcript
    , turn = agentState.turn + 1
    }

-----------------------------------------------------------------------------------------
-- * Model helpers
-----------------------------------------------------------------------------------------

-- | Ask the LLM for the next assistant message.
askNext
  :: (LLM.LLM :> es, Concurrent :> es)
  => Runtime context es
  -> TurnState
  -> Transcript
  -> Stream (Of Output) (Eff es) LLM.ChatAnswer
askNext runtime agentState transcript = do
  schemas <- lift (resolveToolSchemas agentState.transcript agentState.turn runtime.runningTools)
  S.map ContentDelta $
    LLM.askWithToolsStreaming
      schemas
      (agentRequestMessages runtime.context transcript)

agentStreamAnswer :: [Output] -> Text
agentStreamAnswer =
  Text.strip . foldMap \case
    ContentDelta chunk ->
      chunk
    ToolCallNotification{} ->
      ""
    ReplyBoundary ->
      ""

agentRequestMessages :: Context -> Transcript -> [LLM.ChatMessage]
agentRequestMessages context (Transcript messages) =
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
  :: Concurrent :> es
  => Runtime '[] es
  -> Int
  -> Transcript
  -> NonEmpty LLM.ToolCall
  -> Eff es Transcript
continueWithToolCalls runtime turn answered calls = do
  executions <- Async.mapConcurrently (executeToolCall runtime turn) calls
  let executionList = toList executions
      next = appendMessages (map (\(resultMessage, _, _) -> resultMessage) executionList <> concatMap (\(_, imageMessages, _) -> imageMessages) executionList) answered
  pure next

-- | Run one tool call and convert failures into tool-visible text.
--
-- Tool failures must still produce a tool result message; otherwise the next
-- LLM request would contain an assistant tool call without its required result.
executeToolCall :: Concurrent :> es => Runtime '[] es -> Int -> LLM.ToolCall -> Eff es (LLM.ChatMessage, [LLM.ChatMessage], ToolResult)
executeToolCall runtime turn call = do
  result <- runtime.aroundToolCall turn call HList.HNil do
    ToolRegistry.runToolCall runtime.context runtime.toolCallMetadata runtime.tools runtime.runningTools call
  pure (LLM.toolResult call (toolResultContent result), toolImageContextMessages call result, result)

toolImageContextMessages :: LLM.ToolCall -> ToolResult -> [LLM.ChatMessage]
toolImageContextMessages call result =
  [ LLM.userWithImages (toolImageContextText call result) imageUrls
  | let imageUrls = toolResultImageUrls result
  , not (null imageUrls)
  ]

toolImageContextText :: LLM.ToolCall -> ToolResult -> Text
toolImageContextText call result =
  Text.strip [i|Image context returned by tool #{calledToolName}:
#{toolContent}|]
  where
    calledToolName = call.name
    toolContent = toolResultContent result

-----------------------------------------------------------------------------------------
-- * Completion
-----------------------------------------------------------------------------------------

agentCompletion :: Runtime context es -> Text -> Text -> Int -> Maybe LLM.TokenUsage -> Transcript -> Result
agentCompletion runtime status answer turnsUsed tokenUsage transcript =
  Result
    { runId = runtime.runId
    , transcript
    , status
    , finalText = answer
    , turnsUsed
    , tokenUsage
    }
