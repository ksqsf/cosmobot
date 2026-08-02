{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Handler.Ask.AgentRun
Description : Ask handler agent run and reply lifecycle
Stability   : experimental
-}

module Bot.Handler.Ask.AgentRun
  ( runAskAgentThread
  , askSystemPrompt
  )
where

import qualified Bot.Agent as Agent
import qualified Bot.Agent.Tool as AgentTool
import qualified Bot.Agent.Failure as Failure
import qualified Bot.Agent.Middleware.Observation as AgentObservation
import Bot.Core.Thread
import Bot.Core.Transcript
import Bot.Core.Message
import Bot.Core.Route (isSuperuser)
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Agent as AgentEffect
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Resource as Resource
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Skills as Skills
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Effect.Typst as Typst
import Bot.Handler.Ask.Config
import qualified Bot.Memory as MemoryStore
import Bot.Prelude
import Bot.Storage.Thread
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Effectful.Prim.IORef as IORef
import qualified Streaming.Prelude as S
import Effectful.FileSystem
import Effectful.Process
import Effectful.Timeout

runAskAgentThread
  :: ( Chat.Chat :> es
     , ChatLog.ChatLog :> es
     , AgentAudit.AgentAudit :> es
     , AgentEffect.Agent :> es
     , Concurrency.Concurrency :> es
     , HTTP.HTTP :> es
     , LLM.LLM :> es
     , Media.Media :> es
     , Memory.Memory :> es
     , Resource.Resource :> es
     , Scheduler.Scheduler :> es
     , Skills.Skills :> es
     , Storage.Storage :> es
     , Typst.Typst :> es
     , KatipE :> es
     , Prim :> es
     , Concurrent :> es
     , Fail :> es
     , Timeout :> es
     , Process :> es
     , FileSystem :> es
     , IOE :> es
     )
  => Agent.ToolConfig
  -> [AgentTool.Tool (Eff es)]
  -> AskHandlerConfig
  -> ThreadStore
  -> Concurrency.Handle
  -> Maybe ThreadMessageKey
  -> IncomingMessage
  -> MessageInput
  -> Transcript
  -> Eff es (Text, Transcript)
runAskAgentThread toolCfg tools cfg threads resource parentMessageKey message input transcript = do
  let observer = AgentAudit.agentAuditObserver
  systemPrompt <- askSystemPrompt cfg message
  Agent.withAgentMetadata
    (\runId -> Agent.ToolCallMetadata
      { agentRunId = runId
      , originRunId = runId
      , resourceOwner = Just resource
      }) $
    Agent.withRun
      cfg.agentMaxTurns
      (compactionThresholdTokens cfg)
      (agentContext toolCfg cfg message input systemPrompt)
      tools
      \runtime ->
        withActiveReply threads (Agent.runIdOf runtime) resource parentMessageKey message input.text transcript \activeReply -> do
          reply <- streamAgentReply runtime activeReply message transcript
          commitAgentReply observer activeReply message reply

data AgentReply = AgentReply
  { responseId :: !(Maybe MessageId)
  , answer :: !Text
  , result :: !Agent.Result
  }

agentContext
  :: Agent.ToolConfig
  -> AskHandlerConfig
  -> IncomingMessage
  -> MessageInput
  -> Text
  -> Agent.Context
agentContext toolCfg cfg message input systemPrompt =
  Agent.Context
    { message = message
    , input = input
    , superuser = isSuperuser message
    , systemContext = systemPrompt
    , askCommand = cfg.command
    , toolConfig = toolCfg
    }

askSystemPrompt :: (Memory.Memory :> es, Skills.Skills :> es) => AskHandlerConfig -> IncomingMessage -> Eff es Text
askSystemPrompt cfg message = do
  skillsPrompt <- Skills.skillsSystemPrompt
  senderMemory <- loadScopedMemory (MemoryStore.senderMemoryScope message)
  chatMemory <- loadScopedMemory (MemoryStore.chatMemoryScope message)
  pure . Text.intercalate "\n\n" $
    [ LLM.contextSystemPrompt cfg.systemPrompt skillsPrompt senderMemory chatMemory
    , currentMessageSystemPrompt cfg message
    ]

loadScopedMemory :: Memory.Memory :> es => Either Text MemoryStore.MemoryScope -> Eff es (Maybe Text)
loadScopedMemory =
  either (const (pure Nothing)) Memory.loadMemory

currentMessageSystemPrompt :: AskHandlerConfig -> IncomingMessage -> Text
currentMessageSystemPrompt cfg message =
  Text.unlines
    [ "Current message:"
    , [i|- platform: #{platformText}|]
    , [i|- bot_id: #{botIdText} (cosmobot's own platform user id)|]
    , [i|- chat_kind: #{kindText}|]
    , [i|- chat_id: #{chatIdText}|]
    , [i|- sender_id: #{senderIdText} (the platform user id of the user who sent this message)|]
    , [i|- sender_username: #{senderUsernameText}|]
    ]
  where
    platformText = show message.platform :: String
    botIdText = maybe "unavailable" Text.unpack (message.digest.botId <|> configuredBotId)
    kindText = show message.kind :: String
    chatIdText = maybe "unavailable" show message.chatId :: String
    senderIdText = maybe "unavailable" Text.unpack message.senderId
    senderUsernameText = fromMaybe "unavailable" message.senderUsername
    configuredBotId = listToMaybe [botId | (platform, botId) <- cfg.botIds, platform == message.platform]

streamAgentReply
  :: ( Chat.Chat :> es
     , ChatLog.ChatLog :> es
     , Concurrency.Concurrency :> es
     , LLM.LLM :> es
     , Media.Media :> es
     , Storage.Storage :> es
     , KatipE :> es
     , Prim :> es
     , Concurrent :> es
  )
  => Agent.Runtime '[] (Eff es)
  -> ActiveReplyState
  -> IncomingMessage
  -> Transcript
  -> Eff es AgentReply
streamAgentReply runtime activeReply message transcript =
  do
    let sink = Agent.ToolEmittedMessageSink (rememberToolEmittedMessage activeReply)
        program =
            ( Agent.withSteering (activeSteeringControl activeReply)
          . Agent.withRecordingToolSelfMessages (ChatLog.recordSelfMessage message)
          . Agent.withLinkingToolEmittedMessagesToThread sink
          . Agent.withNormalizingToolReplies
          )
            runtime
    (lastReply, replyResult) <-
      S.mapM_
        (recordReplyUpdate activeReply)
        (Chat.streamMultipleRepliesTo message (agentReplyTextSegments (Agent.agentStream program transcript)))
    let responseId = lastReply.responseId
        (answer, result) = replyResult
    pure AgentReply{responseId, answer, result}
  `catchSync` \err ->
    case fromException err of
      Just ThreadKilled ->
        throwIO err
      _ -> do
        logWarning [i|LLM request failed: #{show err :: String}|]
        let failureMessage = llmFailureMessage err
        responseId <- listToMaybe . rights <$> Chat.replyTo message failureMessage
        pure AgentReply
          { responseId
          , answer = failureMessage
          , result = Agent.Result
              { runId = Agent.runIdOf runtime
              , transcript = transcript
              , status = "failed"
              , finalText = failureMessage
              , turnsUsed = 0
              , tokenUsage = Nothing
              }
          }

-- Project flat agent events into visible chat reply segments. A tool-call
-- notification closes the current reply before tool progress messages appear.
agentReplyTextSegments
  :: Prim :> es
  => Stream (Of Agent.Output) (Eff es) Agent.Result
  -> Stream (Stream (Of Text) (Eff es)) (Eff es) (Text, Agent.Result)
agentReplyTextSegments =
  S.maps (S.mapMaybe id) . S.breaks isNothing . agentReplyTextEvents

agentReplyTextEvents
  :: Prim :> es
  => Stream (Of Agent.Output) (Eff es) Agent.Result
  -> Stream (Of (Maybe Text)) (Eff es) (Text, Agent.Result)
agentReplyTextEvents =
  go mempty
  where
    go answer stream = do
      next <- lift (S.next stream)
      case next of
        Left result ->
          pure (renderReplyText answer, result)
        Right (Agent.ContentDelta chunk, rest) -> do
          S.yield (Just chunk)
          go (appendReplyText chunk answer) rest
        Right (Agent.ToolCallNotification{}, rest) -> do
          S.yield Nothing
          go answer rest
        Right (Agent.ReplyBoundary, rest) -> do
          S.yield Nothing
          go answer rest

appendReplyText :: Text -> TextBuilder.Builder -> TextBuilder.Builder
appendReplyText chunk answer =
  answer <> TextBuilder.fromText chunk

renderReplyText :: TextBuilder.Builder -> Text
renderReplyText =
  Text.strip . LazyText.toStrict . TextBuilder.toLazyText

commitAgentReply
  :: (ChatLog.ChatLog :> es, Storage.Storage :> es, KatipE :> es, Prim :> es, Concurrent :> es)
  => Agent.Observer AgentObservation.ObservationContext (Eff es)
  -> ActiveReplyState
  -> IncomingMessage
  -> AgentReply
  -> Eff es (Text, Transcript)
commitAgentReply observer activeReply message AgentReply{responseId, answer, result} = do
  traverse_ (AgentObservation.observeThreadLinked observer . threadLink message result (activeReply.parentMessageKey <&> (.messageId))) responseId
  ChatLog.recordSelfMessage message answer
  active <- IORef.readIORef activeReply.activeRef
  case active of
    Just activeHandle -> do
      traverse_ (addActiveThreadMessage activeReply.threads activeHandle . threadMessageKey message) responseId
      finishActiveThread activeReply.threads activeHandle result.transcript
    Nothing ->
      rememberThreadTranscriptFrom activeReply.threads activeReply.parentMessageKey (threadMessageKey message <$> responseId) result.transcript
  pure (answer, result.transcript)

threadLink :: IncomingMessage -> Agent.Result -> Maybe MessageId -> MessageId -> AgentObservation.ObservedThreadLink
threadLink message result parentMessageId linkedMessageId =
  AgentObservation.ObservedThreadLink
    { runId = result.runId
    , parentMessageId
    , linkedMessageKey = threadMessageKey message linkedMessageId
    }

compactionThresholdTokens :: AskHandlerConfig -> Int
compactionThresholdTokens cfg =
  cfg.contextCompactionThresholdKTokens * 1000

rememberToolEmittedMessage
  :: (Prim :> es, Concurrent :> es)
  => ActiveReplyState
  -> Maybe MessageId
  -> Eff es ()
rememberToolEmittedMessage activeReply messageId = do
  active <- ensureActiveReply activeReply messageId activeReply.baseTranscript
  traverse_ (\activeHandle -> traverse_ (addActiveThreadMessage activeReply.threads activeHandle . threadMessageKey activeReply.message) messageId) active

discardActiveReply :: (Storage.Storage :> es, KatipE :> es, Prim :> es, Concurrent :> es) => ActiveReplyState -> Eff es ()
discardActiveReply activeReply =
  IORef.readIORef activeReply.activeRef
    >>= traverse_ (finishActiveThreadCurrent activeReply.threads)

data ActiveReplyState = ActiveReplyState
  { threads :: !ThreadStore
  , runId :: !Text
  , resource :: !Concurrency.Handle
  , parentMessageKey :: !(Maybe ThreadMessageKey)
  , message :: !IncomingMessage
  , prompt :: !Text
  , baseTranscript :: !Transcript
  , activeRef :: !(IORef.IORef (Maybe ActiveThreadHandle))
  }

activeSteeringControl
  :: (Prim :> es, Concurrent :> es)
  => ActiveReplyState
  -> Agent.SteeringControl es
activeSteeringControl activeReply =
  Agent.SteeringControl
    { drain =
        IORef.readIORef activeReply.activeRef
          >>= maybe (pure []) drainActiveThreadSteers
    , complete =
        IORef.readIORef activeReply.activeRef
          >>= maybe (pure Nothing) completeActiveThreadSteering
    }

withActiveReply
  :: (Storage.Storage :> es, KatipE :> es, Prim :> es, Concurrent :> es, IOE :> es)
  => ThreadStore
  -> Text
  -> Concurrency.Handle
  -> Maybe ThreadMessageKey
  -> IncomingMessage
  -> Text
  -> Transcript
  -> (ActiveReplyState -> Eff es a)
  -> Eff es a
withActiveReply threads runId resource parentMessageKey message prompt baseTranscript use = mask \restore -> do
  active <- rememberActiveThread threads runId parentMessageKey (threadMessageKey message <$> message.messageId) message prompt resource baseTranscript
  activeRef <- IORef.newIORef active
  let activeReply =
        ActiveReplyState
          { threads
          , runId
          , resource
          , parentMessageKey
          , message
          , prompt
          , baseTranscript
          , activeRef
          }
  restore (use activeReply) `onException` discardActiveReply activeReply

recordReplyUpdate
  :: (Prim :> es, Concurrent :> es)
  => ActiveReplyState
  -> Chat.MessageOutResult
  -> Eff es ()
recordReplyUpdate activeState update = do
  let sentIds = rights update.sentMessageResults
      transcript = appendAssistant update.answer activeState.baseTranscript
  active <- ensureActiveReply activeState (update.responseId <|> listToMaybe sentIds) transcript
  traverse_ (`updateActiveThread` transcript) active
  traverse_ (\activeHandle -> traverse_ (addActiveThreadMessage activeState.threads activeHandle . threadMessageKey activeState.message) sentIds) active

ensureActiveReply
  :: (Prim :> es, Concurrent :> es)
  => ActiveReplyState
  -> Maybe MessageId
  -> Transcript
  -> Eff es (Maybe ActiveThreadHandle)
ensureActiveReply activeState messageId transcript = do
  existing <- IORef.readIORef activeState.activeRef
  case existing of
    Just{} ->
      pure existing
    Nothing -> do
      active <- rememberActiveThread activeState.threads activeState.runId activeState.parentMessageKey (threadMessageKey activeState.message <$> messageId) activeState.message activeState.prompt activeState.resource transcript
      IORef.writeIORef activeState.activeRef active
      pure active

llmFailureMessage :: SomeException -> Text
llmFailureMessage err =
  "LLM request failed: " <> (Failure.failureFromException err).userMessage
