{-# LANGUAGE DataKinds #-}
{-|
Module      : Bot.Handler.Console
Description : Continuous RPC and ACP console sessions
Stability   : experimental
-}

module Bot.Handler.Console
  ( consoleHandlers
  )
where

import qualified Bot.Agent as Agent
import qualified Bot.Agent.Middleware.Observation as AgentObservation
import qualified Bot.Agent.Tool as AgentTool
import Bot.Core.Message
import Bot.Core.Route
import Bot.Core.Thread
import Bot.Core.Transcript
import qualified Bot.Effect.Agent as AgentEffect
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Plugin as Plugin
import qualified Bot.Effect.Resource as Resource
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Skills as Skills
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Effect.Typst as Typst
import Bot.Chat.AgentReply (AgentReply (..), streamAgentReplyWith)
import Bot.Handler.Console.Config
import Bot.Prelude
import qualified Bot.Plugin.Tool as PluginTool
import qualified Bot.Session as Session
import Bot.Storage.Thread
import qualified Data.Text as Text
import Effectful.FileSystem
import Effectful.Process
import Effectful.Timeout
import qualified Effectful.Resource as EffectfulResource

type HandlerEffects es =
  ( Chat.Chat :> es
  , AgentEffect.Agent :> es
  , ChatLog.ChatLog :> es
  , AgentAudit.AgentAudit :> es
  , Concurrency.Concurrency :> es
  , HTTP.HTTP :> es
  , LLM.LLM :> es
  , Media.Media :> es
  , Memory.Memory :> es
  , Plugin.Plugin :> es
  , Resource.Resource :> es
  , Concurrent :> es
  , Skills.Skills :> es
  , Scheduler.Scheduler :> es
  , Storage.Storage :> es
  , Typst.Typst :> es
  , KatipE :> es
  , Prim :> es
  , Process :> es
  , FileSystem :> es
  , Fail :> es
  , Timeout :> es
  , IOE :> es
  )

consoleHandlers
  :: HandlerEffects es
  => Agent.ToolConfig
  -> [AgentTool.Tool (Eff es)]
  -> ConsoleHandlerConfig
  -> ThreadStore
  -> [RouteHandler es]
consoleHandlers toolCfg tools cfg threads =
  [ stopOn (matching isConsoleMessage) \message _ ->
      handleConsoleMessage toolCfg tools cfg threads message
  ]

isConsoleMessage :: IncomingMessage -> Bool
isConsoleMessage message =
  message.eventKind == IncomingMessageCreated
    && message.kind == ChatPrivate
    && (message.platform == PlatformRPC || message.platform == PlatformACP)

handleConsoleMessage
  :: HandlerEffects es
  => Agent.ToolConfig
  -> [AgentTool.Tool (Eff es)]
  -> ConsoleHandlerConfig
  -> ThreadStore
  -> IncomingMessage
  -> Eff es ()
handleConsoleMessage toolCfg tools cfg threads message =
  case listToMaybe message.chatAliases >>= nonBlank of
    Nothing -> do
      $(logWarning) [i|Rejected console message without a session id: #{incomingMessageLog message}|]
      void $ Chat.replyTo message "Console session id is missing."
    Just sessionIdText -> do
      let sessionId = Session.SessionId sessionIdText
          canonical = canonicalConsoleMessage sessionIdText message
          input = inputWithAttachments message.text message.imageUrls message.files
      if Text.strip message.text == "!halt"
        then haltConsoleSession threads canonical
        else do
          steered <- enqueueActiveSessionThreadSteer threads sessionId canonical input
          unless steered $
            Concurrency.fireWithHandle "console.session" \resource ->
              runConsoleAgent toolCfg tools cfg threads resource sessionId canonical message input
  where
    nonBlank value = value <$ guard (not (Text.null (Text.strip value)))

haltConsoleSession
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, KatipE :> es, Prim :> es, Concurrent :> es)
  => ThreadStore
  -> IncomingMessage
  -> Eff es ()
haltConsoleSession threads message = do
  active <- listActiveThreadsForMessage threads message
  halted <- haltActiveThreadsForMessage threads Concurrency.cancel message (map (.id) active)
  when (null halted) $
    void (Chat.replyTo message "No active console run.")

runConsoleAgent
  :: HandlerEffects es
  => Agent.ToolConfig
  -> [AgentTool.Tool (Eff es)]
  -> ConsoleHandlerConfig
  -> ThreadStore
  -> Concurrency.Handle
  -> Session.SessionId
  -> IncomingMessage
  -> IncomingMessage
  -> MessageInput
  -> Eff es ()
runConsoleAgent toolCfg tools cfg threads resource sessionId canonical message input = do
  (parentMessageKey, previous) <- latestConsoleTranscript threads sessionId
  let transcript = maybe (startWithUserInput input) (appendUserInput input) previous
  systemPrompt <- consoleSystemPrompt cfg
  EffectfulResource.runResource do
    dynamicTools <- PluginTool.definitions <$> Plugin.toolSnapshot
    Agent.withAgentMetadata
      (\runId -> Agent.ToolCallMetadata
        { agentRunId = runId
        , originRunId = runId
        , resourceOwner = Just resource
        }) $
      Agent.withRunObserved
        (publishConsoleActivity message)
        cfg.agentMaxTurns
        cfg.contextStrategy
        (cfg.contextCompactionThresholdKTokens * 1000)
        (consoleAgentContext toolCfg message input systemPrompt)
        (map (AgentTool.hoistTool raise) tools <> dynamicTools)
        \runtime -> do
          used <- withActiveConsoleThread
            threads
            sessionId
            (Agent.runIdOf runtime)
            parentMessageKey
            (consoleThreadMessageKey sessionId <$> canonical.messageId)
            canonical
            input.text
            resource
            transcript
            \activeHandle -> do
              traverse_ (addActiveThreadMessage threads activeHandle . consoleThreadMessageKey sessionId) canonical.messageId
              runActiveConsoleAgent threads activeHandle parentMessageKey sessionId message runtime transcript
          when (isNothing used) $
            void (enqueueActiveSessionThreadSteer threads sessionId canonical input)

publishConsoleActivity :: Chat.Chat :> es => IncomingMessage -> Agent.Event -> Eff es ()
publishConsoleActivity message = \case
  Agent.ModelTurnStarted{runId, turn} ->
    Chat.publishActivity message (Chat.ReasoningStarted runId turn)
  Agent.ModelTurnFinished{runId, turn, answerKind} ->
    Chat.publishActivity message (Chat.ReasoningFinished runId turn answerKind)
  Agent.ToolCallStarted{runId, turn, toolCall} ->
    Chat.publishActivity message (Chat.ToolCallStarted runId turn toolCall.id toolCall.name)
  Agent.ToolCallFinished{runId, turn, toolCallId, toolName, status} ->
    Chat.publishActivity message (Chat.ToolCallFinished runId turn toolCallId toolName status)
  _ ->
    pure ()

withActiveConsoleThread
  :: (Prim :> es, Concurrent :> es, Storage.Storage :> es, KatipE :> es, IOE :> es)
  => ThreadStore
  -> Session.SessionId
  -> Text
  -> Maybe ThreadMessageKey
  -> Maybe ThreadMessageKey
  -> IncomingMessage
  -> Text
  -> Concurrency.Handle
  -> Transcript
  -> (ActiveThreadHandle -> Eff es a)
  -> Eff es (Maybe a)
withActiveConsoleThread threads sessionId runId parentMessageKey messageKey message prompt resource transcript use =
  mask \restore -> do
    active <- rememberActiveSessionThread threads sessionId runId parentMessageKey messageKey message prompt resource transcript
    forM active \activeHandle ->
      restore (use activeHandle) `onException` finishActiveThreadCurrent threads activeHandle

runActiveConsoleAgent
  :: ( Chat.Chat :> es
     , ChatLog.ChatLog :> es
     , AgentAudit.AgentAudit :> es
     , Concurrency.Concurrency :> es
     , LLM.LLM :> es
     , Media.Media :> es
     , Storage.Storage :> es
     , KatipE :> es
     , Prim :> es
     , Concurrent :> es
     )
  => ThreadStore
  -> ActiveThreadHandle
  -> Maybe ThreadMessageKey
  -> Session.SessionId
  -> IncomingMessage
  -> Agent.Runtime '[] (Eff es)
  -> Transcript
  -> Eff es ()
runActiveConsoleAgent threads active parentMessageKey sessionId message runtime transcript = do
  let rememberMessage messageId =
        traverse_ (addActiveThreadMessage threads active . consoleThreadMessageKey sessionId) messageId
      recordUpdate update = do
        traverse_ (addActiveThreadMessage threads active . consoleThreadMessageKey sessionId) $
          ordNub (maybeToList update.responseId <> rights update.sentMessageResults)
        updateActiveThread active (appendAssistant update.answer transcript)
      steering = Agent.SteeringControl
        { drain = drainActiveThreadSteers active
        , complete = completeActiveThreadSteering active
        }
  reply <- streamAgentReplyWith
    runtime
    steering
    (Agent.ToolEmittedMessageSink rememberMessage)
    recordUpdate
    message
    transcript
  commitConsoleReply threads active parentMessageKey sessionId message reply

commitConsoleReply
  :: (ChatLog.ChatLog :> es, AgentAudit.AgentAudit :> es, Storage.Storage :> es, KatipE :> es, Prim :> es, Concurrent :> es)
  => ThreadStore
  -> ActiveThreadHandle
  -> Maybe ThreadMessageKey
  -> Session.SessionId
  -> IncomingMessage
  -> AgentReply
  -> Eff es ()
commitConsoleReply threads active parentMessageKey sessionId message reply = do
  for_ reply.responseId \messageId -> do
    let linkedKey = consoleThreadMessageKey sessionId messageId
    addActiveThreadMessage threads active linkedKey
    AgentObservation.observeThreadLinked AgentAudit.agentAuditObserver $
      AgentObservation.ObservedThreadLink
        { runId = reply.result.runId
        , parentMessageId = (.messageId) <$> parentMessageKey
        , linkedMessageKey = linkedKey
        }
  ChatLog.recordSelfMessage message reply.answer
  finishActiveThread threads active reply.result.transcript

latestConsoleTranscript
  :: (Storage.Storage :> es, Prim :> es, Concurrent :> es)
  => ThreadStore
  -> Session.SessionId
  -> Eff es (Maybe ThreadMessageKey, Maybe Transcript)
latestConsoleTranscript threads sessionId = do
  history <- Session.sessionHistory sessionId
  firstStored (reverse history)
  where
    firstStored [] =
      pure (Nothing, Nothing)
    firstStored (message : rest) = do
      let key = consoleThreadMessageKey message.sessionId message.messageId
      lookupCommittedThreadTranscript threads key >>= \case
        Just transcript -> pure (Just key, Just transcript)
        Nothing -> firstStored rest

consoleSystemPrompt :: Skills.Skills :> es => ConsoleHandlerConfig -> Eff es Text
consoleSystemPrompt cfg = do
  skillsPrompt <- Skills.skillsSystemPrompt
  pure (LLM.contextSystemPrompt cfg.systemPrompt skillsPrompt Nothing Nothing)

consoleAgentContext :: Agent.ToolConfig -> IncomingMessage -> MessageInput -> Text -> Agent.Context
consoleAgentContext toolCfg message input systemPrompt =
  Agent.Context
    { message
    , input
    , superuser = isSuperuser message
    , systemContext = systemPrompt
    , askCommand = ""
    , toolConfig = toolCfg
    }

canonicalConsoleMessage :: Text -> IncomingMessage -> IncomingMessage
canonicalConsoleMessage sessionId message =
  message
    { platform = PlatformRPC
    , chatId = Nothing
    , chatAliases = [sessionId]
    , senderId = Just sessionId
    }

consoleThreadMessageKey :: Session.SessionId -> MessageId -> ThreadMessageKey
consoleThreadMessageKey sessionId messageId =
  ThreadMessageKey PlatformRPC Nothing $
    textMessageId (Session.sessionIdText sessionId <> ":" <> messageIdText messageId)
