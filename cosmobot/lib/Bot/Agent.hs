{-# LANGUAGE DataKinds #-}
{-|
Module      : Bot.Agent
Description : Agent program and extensible tool framework
Stability   : experimental
-}

module Bot.Agent
  ( ToolCallMetadata (..)
  , Context (..)
  , Event (..)
  , Observer
  , Program
  , BotRuntime
  , Result (..)
  , Output (..)
  , ToolEmittedMessageSink (..)
  , SteeringControl (..)
  , FailureCategory (..)
  , Failure (..)
  , ToolConfig (..)
  , WebSearchApi (..)
  , defaultToolConfig
  , withRun
  , withRunObserved
  , runAgent
  , withAgentMetadata
  , startRuntime
  , runIdOf
  , defaultRuntime
  , defaultRuntimeWithStrategy
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
  )
where

import Bot.Agent.Children (withStructuredChildren)
import Bot.Core.Transcript
import Bot.Agent.Program (agentProgram)
import qualified Bot.Agent.Runtime as AgentRuntime
import Bot.Agent.Core
import Bot.Agent.Middleware.ContextCompaction
  ( withContextCompactionNotice
  )
import Bot.Agent.Middleware.LogContext
  ( withLogContext
  )
import Bot.Agent.Middleware.RecursiveTranscript
  ( withRecursiveTranscript
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
import Bot.Agent.Tool (toolName)
import Bot.Agent.Tools.Continuation (resumeContinuationTool)
import Bot.Agent.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Agent as AgentEffect
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import qualified Bot.Util.HList as HList
import Bot.Prelude
import qualified Crypto.Random as CryptoRandom
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Resource as EffectfulResource

-----------------------------------------------------------------------------------------
-- * Public runners
-----------------------------------------------------------------------------------------

runAgent :: Eff (AgentEffect.Agent : es) a -> Eff es a
runAgent =
  interpret (runAgentOperation \runId -> ToolCallMetadata
    { agentRunId = runId
    , originRunId = runId
    , resourceOwner = Nothing
    })

withAgentMetadata
  :: AgentEffect.Agent :> es
  => (Text -> ToolCallMetadata)
  -> Eff es a
  -> Eff es a
withAgentMetadata metadataFor action =
  interpose (passthroughAgentMetadata metadataFor) action

passthroughAgentMetadata
  :: AgentEffect.Agent :> es
  => (Text -> ToolCallMetadata)
  -> EffectHandler AgentEffect.Agent es
passthroughAgentMetadata metadataFor localEnv = \case
  AgentEffect.RunAgent runtime use ->
    passthrough localEnv $ AgentEffect.RunAgent runtime \configured ->
      use configured{toolCallMetadata = metadataFor configured.runId}
  operation -> passthrough localEnv operation

runAgentOperation
  :: (Text -> ToolCallMetadata)
  -> EffectHandler AgentEffect.Agent es
runAgentOperation metadataFor localEnv = \case
    AgentEffect.RunAgent runtime use ->
      localUnlift localEnv (ConcUnlift Persistent Unlimited) \unlift ->
        unlift (use runtime
          { toolCallMetadata = metadataFor runtime.runId
          })
    AgentEffect.RegisterChild{} -> pure ()
    AgentEffect.ConsumeChild{} -> pure ()

withRun
  :: ( AgentEffect.Agent :> es
     , Chat.Chat :> es
     , AgentAudit.AgentAudit :> es
     , Concurrency.Concurrency :> es
     , LLM.LLM :> es
     , Media.Media :> es
     , KatipE :> es
     , Prim :> es
     , Concurrent :> es
     , IOE :> es
     , EffectfulResource.Resource :> es
     )
  => Int
  -> ContextStrategy
  -> Int
  -> Context
  -> [BotTool (Eff es)]
  -> (BotRuntime '[] (Eff es) -> Eff es a)
  -> Eff es a
withRun maxTurns contextStrategy contextTokenThreshold context tools use = do
  withRunObserved (const (pure ())) maxTurns contextStrategy contextTokenThreshold context tools use

withRunObserved
  :: ( AgentEffect.Agent :> es
     , Chat.Chat :> es
     , AgentAudit.AgentAudit :> es
     , Concurrency.Concurrency :> es
     , LLM.LLM :> es
     , Media.Media :> es
     , KatipE :> es
     , Prim :> es
     , Concurrent :> es
     , IOE :> es
     , EffectfulResource.Resource :> es
     )
  => (Event -> Eff es ())
  -> Int
  -> ContextStrategy
  -> Int
  -> Context
  -> [BotTool (Eff es)]
  -> (BotRuntime '[] (Eff es) -> Eff es a)
  -> Eff es a
withRunObserved observe maxTurns contextStrategy contextTokenThreshold context tools use = do
  baseRuntime <- startRuntime maxTurns context tools
  let observer event = AgentAudit.agentAuditObserver event <* observe event
  AgentEffect.withRun
    (defaultRuntimeWithStrategy observer contextStrategy contextTokenThreshold baseRuntime)
    (\runtime -> withStructuredChildren runtime use)

agentStream
  :: (LLM.LLM :> es, Concurrent :> es, KatipE :> es)
  => BotRuntime '[] (Eff es)
  -> Transcript
  -> Stream (Of Output) (Eff es) Result
agentStream runtime transcript =
  runtime.aroundAgentRun HList.HNil
    . AgentRuntime.runProgram agentRequestMessages runtime
    . runtime.aroundProgram runtime
    $ agentProgram runtime.runId (AgentRuntime.initialState transcript)

runIdOf :: BotRuntime context (Eff es) -> Text
runIdOf =
  (.runId)

-----------------------------------------------------------------------------------------
-- * Run setup
-----------------------------------------------------------------------------------------

startRuntime :: (Chat.Chat :> es, Concurrent :> es, IOE :> es) => Int -> Context -> [BotTool (Eff es)] -> Eff es (BotRuntime context (Eff es))
startRuntime maxTurns context tools = do
  runId <- newAgentRunId
  AgentRuntime.startRuntime runId maxTurns context
    ToolCallMetadata{agentRunId = runId, originRunId = runId, resourceOwner = Nothing}
    tools

newAgentRunId :: IOE :> es => Eff es AgentRunId
newAgentRunId = do
  bytes <- liftIO (CryptoRandom.getRandomBytes 16 :: IO StrictByteString.ByteString)
  pure ("agent-" <> TextEncoding.decodeUtf8 (Base64URL.encodeUnpadded bytes))

defaultRuntime
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, LLM.LLM :> es, Media.Media :> es, KatipE :> es, Prim :> es, EffectfulResource.Resource :> es)
  => Observer ObservationContext (Eff es)
  -> Int
  -> BotRuntime '[ObservationContext, EventObservation es, ToolResultObservation es] (Eff es)
  -> BotRuntime '[] (Eff es)
defaultRuntime observer compactionTokenThreshold =
  defaultRuntimeWithStrategy observer ContextCompaction compactionTokenThreshold

defaultRuntimeWithStrategy
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, LLM.LLM :> es, Media.Media :> es, KatipE :> es, Prim :> es, EffectfulResource.Resource :> es)
  => Observer ObservationContext (Eff es)
  -> ContextStrategy
  -> Int
  -> BotRuntime '[ObservationContext, EventObservation es, ToolResultObservation es] (Eff es)
  -> BotRuntime '[] (Eff es)
defaultRuntimeWithStrategy observer contextStrategy contextTokenThreshold =
  ( withLogContext
  . withContinuations
  . withToolLimit isResumeTransfer
  . withTypingNotification
  . withToolResultCompaction
  . withObservation observer contextStrategy
  . withToolMessage
  . contextMiddleware
  . withToolFailureRecovery
  )
  where
    contextMiddleware = case contextStrategy of
      ContextCompaction -> withContextCompactionNotice contextTokenThreshold
      RecursiveTranscript -> withRecursiveTranscript contextTokenThreshold

isResumeTransfer :: BotRuntime context (Eff es) -> NonEmpty LLM.ToolCall -> Bool
isResumeTransfer runtime calls =
  case toList calls of
    [call] ->
      call.name == toolName resumeContinuationTool
        && any ((== call.name) . toolName) runtime.exposedTools
    _ ->
      False

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
  LLM.ChatMessage role content toolCalls toolCallId Nothing

-----------------------------------------------------------------------------------------
-- * Tool execution
-----------------------------------------------------------------------------------------

-- | Run one tool call and convert failures into tool-visible text.
--
-- Tool failures must still produce a tool result message; otherwise the next
-- LLM request would contain an assistant tool call without its required result.
-- * Completion
-----------------------------------------------------------------------------------------
