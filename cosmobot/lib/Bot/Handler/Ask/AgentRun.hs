{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Handler.Ask.AgentRun
Description : Ask handler agent run and reply lifecycle
Stability   : experimental
-}

module Bot.Handler.Ask.AgentRun
  ( runAskAgentThread
  )
where

import qualified Bot.Agent as Agent
import qualified Bot.Agent.Failure as AgentFailure
import qualified Bot.Agent.Middleware.Observation as AgentObservation
import Bot.Core.Thread
import Bot.Core.Transcript
import Bot.Core.Message
import Bot.Core.Route (isSuperuser)
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Effect.Typst as Typst
import Bot.Handler.Ask.Config
import Bot.Prelude
import Bot.Storage.Thread
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Effectful.Prim.IORef as IORef
import qualified Streaming
import qualified Streaming.Prelude as S
import Effectful.FileSystem
import Effectful.Process
import Effectful.Timeout

runAskAgentThread
  :: ( Chat.Chat :> es
     , ChatLog.ChatLog :> es
     , AgentAudit.AgentAudit :> es
     , HTTP.HTTP :> es
     , LLM.LLM :> es
     , Media.Media :> es
     , Memory.Memory :> es
     , Scheduler.Scheduler :> es
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
  -> AskHandlerConfig
  -> ThreadStore
  -> Maybe ThreadMessageKey
  -> IncomingMessage
  -> MessageInput
  -> Transcript
  -> Eff es (Text, Transcript)
runAskAgentThread toolCfg cfg threads parentMessageKey message input transcript = do
  threadId <- myThreadId
  activeReply <- newActiveReply threads parentMessageKey message threadId transcript
  let observer = AgentAudit.agentAuditObserver
  agentRun <- Agent.startAgentRun (agentContext toolCfg cfg message input) Agent.defaultTools
  reply <-
    streamAgentReply cfg observer agentRun activeReply message transcript
      `onException` discardActiveReply activeReply
  commitAgentReply observer activeReply message reply
    `onException` discardActiveReply activeReply

data AgentReply = AgentReply
  { responseId :: !(Maybe MessageId)
  , answer :: !Text
  , result :: !Agent.AgentResult
  }

data AgentReplyResult = AgentReplyResult
  { replyAnswer :: !Text
  , agentResult :: !Agent.AgentResult
  }

agentContext
  :: Agent.ToolConfig
  -> AskHandlerConfig
  -> IncomingMessage
  -> MessageInput
  -> Agent.AgentContext es
agentContext toolCfg cfg message input =
  Agent.AgentContext
    { message = message
    , input = input
    , superuser = isSuperuser message
    , systemContext = currentMessageSystemPrompt cfg message
    , askCommand = cfg.command
    , toolConfig = toolCfg
    }

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
     , LLM.LLM :> es
     , Media.Media :> es
     , Storage.Storage :> es
     , KatipE :> es
     , Prim :> es
     , Concurrent :> es
     )
  => AskHandlerConfig
  -> Agent.AgentObserver AgentObservation.ObservationContext es
  -> Agent.AgentRun es
  -> ActiveReplyState
  -> IncomingMessage
  -> Transcript
  -> Eff es AgentReply
streamAgentReply cfg observer agentRun activeReply message transcript =
  do
    let sink = Agent.ToolEmittedMessageSink (rememberToolEmittedMessage activeReply)
        program =
          ( Agent.withRecordingToolSelfMessages (ChatLog.recordSelfMessage message)
          . Agent.withLinkingToolEmittedMessagesToThread sink
          . Agent.withNormalizingToolReplies
          )
            (Agent.defaultAgentProgram observer cfg.agentMaxTurns agentRun)
    (responseId, replyResult) <-
      S.mapM_
        (recordReplyUpdate activeReply)
        (Chat.streamReplySegmentsTo message (.replyAnswer) (agentReplyTextSegments (Agent.runAgentProgramStreaming program transcript)))
    pure AgentReply{responseId, answer = replyResult.replyAnswer, result = replyResult.agentResult}
  `catchSync` \err ->
    case fromException err of
      Just ThreadKilled ->
        throwIO err
      _ -> do
        logWarning [i|LLM request failed: #{show err :: String}|]
        let failureMessage = llmFailureMessage err
        responseId <- rightToMaybe <$> Chat.replyTo message failureMessage
        pure AgentReply
          { responseId
          , answer = failureMessage
          , result = Agent.AgentResult
              { runId = Agent.agentRunId agentRun
              , transcript = transcript
              }
          }

-- Project flat agent events into visible chat reply segments. A tool-call
-- notification closes the current reply before tool progress messages appear.
agentReplyTextSegments
  :: Prim :> es
  => Stream (Of Agent.AgentStreamOutput) (Eff es) Agent.AgentResult
  -> Stream (Stream (Of Text) (Eff es)) (Eff es) AgentReplyResult
agentReplyTextSegments =
  go mempty
  where
    go answer stream = do
      next <- lift (S.next stream)
      case next of
        Left result ->
          pure AgentReplyResult
            { replyAnswer = renderReplyText answer
            , agentResult = result
            }
        Right (Agent.AgentContentDelta chunk, rest) ->
          Streaming.wrap (segment (appendReplyText chunk answer) chunk rest)
        Right (Agent.AgentToolCallNotification{}, rest) ->
          go answer rest

    segment answer chunk stream = do
      S.yield chunk
      next <- lift (S.next stream)
      case next of
        Left result ->
          pure (finished answer result)
        Right (Agent.AgentContentDelta nextChunk, rest) ->
          segment (appendReplyText nextChunk answer) nextChunk rest
        Right (Agent.AgentToolCallNotification{}, rest) ->
          pure (go answer rest)

    finished answer result =
      pure AgentReplyResult
        { replyAnswer = renderReplyText answer
        , agentResult = result
        }

appendReplyText :: Text -> TextBuilder.Builder -> TextBuilder.Builder
appendReplyText chunk answer =
  answer <> TextBuilder.fromText chunk

renderReplyText :: TextBuilder.Builder -> Text
renderReplyText =
  Text.strip . LazyText.toStrict . TextBuilder.toLazyText

commitAgentReply
  :: (ChatLog.ChatLog :> es, Storage.Storage :> es, KatipE :> es, Prim :> es, Concurrent :> es)
  => Agent.AgentObserver AgentObservation.ObservationContext es
  -> ActiveReplyState
  -> IncomingMessage
  -> AgentReply
  -> Eff es (Text, Transcript)
commitAgentReply observer activeReply message AgentReply{responseId, answer, result} = do
  traverse_ (AgentObservation.observeThreadLinked observer . threadLink result (activeReply.parentMessageKey <&> (.messageId))) responseId
  ChatLog.recordSelfMessage message answer
  active <- IORef.readIORef activeReply.activeRef
  case active of
    Just activeHandle ->
      finishActiveThread activeReply.threads activeHandle result.transcript
    Nothing ->
      rememberThreadTranscriptFrom activeReply.threads activeReply.parentMessageKey (threadMessageKey message <$> responseId) result.transcript
  pure (answer, result.transcript)

threadLink :: Agent.AgentResult -> Maybe MessageId -> MessageId -> AgentObservation.ObservedThreadLink
threadLink result parentMessageId linkedMessageId =
  AgentObservation.ObservedThreadLink
    { runId = result.runId
    , parentMessageId
    , linkedMessageId
    }

rememberToolEmittedMessage
  :: (Prim :> es, Concurrent :> es)
  => ActiveReplyState
  -> Maybe MessageId
  -> Eff es ()
rememberToolEmittedMessage activeReply messageId = do
  active <- IORef.readIORef activeReply.activeRef
  case active of
    Just activeHandle ->
      traverse_ (addActiveThreadMessage activeReply.threads activeHandle . threadMessageKey activeReply.message) messageId
    Nothing -> do
      activeHandle <- rememberActiveThread activeReply.threads activeReply.parentMessageKey (threadMessageKey activeReply.message <$> messageId) activeReply.threadId activeReply.baseTranscript
      IORef.writeIORef activeReply.activeRef activeHandle

discardActiveReply :: (Storage.Storage :> es, KatipE :> es, Prim :> es, Concurrent :> es) => ActiveReplyState -> Eff es ()
discardActiveReply activeReply =
  IORef.readIORef activeReply.activeRef
    >>= traverse_ (finishActiveThreadCurrent activeReply.threads)

data ActiveReplyState = ActiveReplyState
  { threads :: !ThreadStore
  , parentMessageKey :: !(Maybe ThreadMessageKey)
  , message :: !IncomingMessage
  , threadId :: !ThreadId
  , baseTranscript :: !Transcript
  , activeRef :: !(IORef.IORef (Maybe ActiveThreadHandle))
  }

newActiveReply
  :: Prim :> es
  => ThreadStore
  -> Maybe ThreadMessageKey
  -> IncomingMessage
  -> ThreadId
  -> Transcript
  -> Eff es ActiveReplyState
newActiveReply threads parentMessageKey message threadId baseTranscript = do
  activeRef <- IORef.newIORef Nothing
  pure ActiveReplyState
    { threads = threads
    , parentMessageKey = parentMessageKey
    , message = message
    , threadId = threadId
    , baseTranscript = baseTranscript
    , activeRef = activeRef
    }

recordReplyUpdate
  :: (Prim :> es, Concurrent :> es)
  => ActiveReplyState
  -> Chat.ReplyStreamUpdate
  -> Eff es ()
recordReplyUpdate activeState update = do
  refreshActiveThread activeState update.answer
  registerActiveIfNeeded activeState update.responseId update.answer
  registerActiveAliases activeState update.sentResponseIds

registerActiveIfNeeded
  :: (Prim :> es, Concurrent :> es)
  => ActiveReplyState
  -> Maybe MessageId
  -> Text
  -> Eff es ()
registerActiveIfNeeded activeState responseId current = do
  existing <- IORef.readIORef activeState.activeRef
  when (isNothing existing) do
    activeHandle <- rememberActiveThread activeState.threads activeState.parentMessageKey (threadMessageKey activeState.message <$> responseId) activeState.threadId (appendAssistant current activeState.baseTranscript)
    IORef.writeIORef activeState.activeRef activeHandle

registerActiveAliases
  :: Prim :> es
  => ActiveReplyState
  -> [MessageId]
  -> Eff es ()
registerActiveAliases activeState responseIds = do
  active <- IORef.readIORef activeState.activeRef
  traverse_ (\activeHandle -> traverse_ (addActiveThreadMessage activeState.threads activeHandle . threadMessageKey activeState.message) responseIds) active

refreshActiveThread
  :: Prim :> es
  => ActiveReplyState
  -> Text
  -> Eff es ()
refreshActiveThread activeState current = do
  active <- IORef.readIORef activeState.activeRef
  traverse_ (`updateActiveThread` appendAssistant current activeState.baseTranscript) active

llmFailureMessage :: SomeException -> Text
llmFailureMessage err =
  "LLM request failed: " <> (AgentFailure.agentFailureFromException err).userMessage
