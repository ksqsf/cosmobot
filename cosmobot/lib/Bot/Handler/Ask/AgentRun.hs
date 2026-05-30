{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Handler.Ask.AgentRun
Description : Ask handler agent run and reply lifecycle
Stability   : experimental
-}

module Bot.Handler.Ask.AgentRun
  ( runAskAgentConversation
  )
where

import qualified Bot.Agent as Agent
import qualified Bot.Agent.Failure as AgentFailure
import qualified Bot.Agent.Middleware.Observation as AgentObservation
import Bot.Core.Conversation
import Bot.Core.Message
import qualified Bot.Core.ReplyBody as ReplyBody
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
import Bot.Storage.Conversation
import qualified Data.Text as Text
import qualified Effectful.Prim.IORef as IORef
import qualified Streaming.Prelude as S
import Effectful.FileSystem
import Effectful.Process
import Effectful.Timeout

runAskAgentConversation
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
  -> ConversationStore
  -> Maybe ConversationMessageKey
  -> IncomingMessage
  -> MessageInput
  -> Conversation
  -> Eff es (Text, Conversation)
runAskAgentConversation toolCfg cfg conversations parentMessageKey message input conversation = do
  threadId <- myThreadId
  activeReply <- newActiveReply conversations parentMessageKey message threadId conversation
  let observer = AgentAudit.agentAuditObserver
  agentRun <- Agent.startAgentRun (agentContext toolCfg cfg message input) Agent.defaultTools
  reply <-
    streamAgentReply cfg observer agentRun activeReply message conversation
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
  -> Conversation
  -> Eff es AgentReply
streamAgentReply cfg observer agentRun activeReply message conversation =
  do
    let sink = Agent.ToolEmittedMessageSink (rememberToolEmittedMessage activeReply)
        program =
          ( Agent.withRecordingToolSelfMessages (ChatLog.recordSelfMessage message)
          . Agent.withLinkingToolEmittedMessagesToConversation sink
          . Agent.withNormalizingToolReplies
          )
            (Agent.defaultAgentProgram observer cfg.agentMaxTurns agentRun)
    (responseId, replyResult) <-
      S.mapM_
        (recordReplyUpdate activeReply)
        (Chat.streamReplySegmentsTo message (.replyAnswer) (agentReplySegmentStream (Agent.runAgentProgramStreaming program conversation)))
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
              , conversation = conversation
              }
          }

agentReplySegmentStream
  :: Stream (Of Agent.AgentStreamOutput) (Eff es) Agent.AgentResult
  -> Stream (Of ReplyBody.ReplySegmentEvent) (Eff es) AgentReplyResult
agentReplySegmentStream =
  go ""
  where
    go answer stream = do
      next <- lift (S.next stream)
      case next of
        Left result ->
          pure AgentReplyResult
            { replyAnswer = Text.strip answer
            , agentResult = result
            }
        Right (Agent.AgentContentDelta chunk, rest) -> do
          S.yield (ReplyBody.ReplySegmentDelta chunk)
          go (answer <> chunk) rest
        Right (Agent.AgentToolCallNotification{}, rest) -> do
          S.yield ReplyBody.ReplySegmentBoundary
          go answer rest

commitAgentReply
  :: (ChatLog.ChatLog :> es, Storage.Storage :> es, KatipE :> es, Prim :> es, Concurrent :> es)
  => Agent.AgentObserver AgentObservation.ObservationContext es
  -> ActiveReplyState
  -> IncomingMessage
  -> AgentReply
  -> Eff es (Text, Conversation)
commitAgentReply observer activeReply message AgentReply{responseId, answer, result} = do
  traverse_ (AgentObservation.observeConversationLinked observer . conversationLink result (activeReply.parentMessageKey <&> (.messageId))) responseId
  ChatLog.recordSelfMessage message answer
  active <- IORef.readIORef activeReply.activeRef
  case active of
    Just activeHandle ->
      finishActiveConversation activeReply.conversations activeHandle result.conversation
    Nothing ->
      rememberConversationFrom activeReply.conversations activeReply.parentMessageKey (conversationMessageKey message <$> responseId) result.conversation
  pure (answer, result.conversation)

conversationLink :: Agent.AgentResult -> Maybe MessageId -> MessageId -> AgentObservation.ObservedConversationLink
conversationLink result parentMessageId linkedMessageId =
  AgentObservation.ObservedConversationLink
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
      traverse_ (addActiveConversationMessage activeReply.conversations activeHandle . conversationMessageKey activeReply.message) messageId
    Nothing -> do
      activeHandle <- rememberActiveConversation activeReply.conversations activeReply.parentMessageKey (conversationMessageKey activeReply.message <$> messageId) activeReply.threadId activeReply.baseConversation
      IORef.writeIORef activeReply.activeRef activeHandle

discardActiveReply :: (Storage.Storage :> es, KatipE :> es, Prim :> es, Concurrent :> es) => ActiveReplyState -> Eff es ()
discardActiveReply activeReply =
  IORef.readIORef activeReply.activeRef
    >>= traverse_ (finishActiveConversationCurrent activeReply.conversations)

data ActiveReplyState = ActiveReplyState
  { conversations :: !ConversationStore
  , parentMessageKey :: !(Maybe ConversationMessageKey)
  , message :: !IncomingMessage
  , threadId :: !ThreadId
  , baseConversation :: !Conversation
  , activeRef :: !(IORef.IORef (Maybe ActiveConversationHandle))
  }

newActiveReply
  :: Prim :> es
  => ConversationStore
  -> Maybe ConversationMessageKey
  -> IncomingMessage
  -> ThreadId
  -> Conversation
  -> Eff es ActiveReplyState
newActiveReply conversations parentMessageKey message threadId baseConversation = do
  activeRef <- IORef.newIORef Nothing
  pure ActiveReplyState
    { conversations = conversations
    , parentMessageKey = parentMessageKey
    , message = message
    , threadId = threadId
    , baseConversation = baseConversation
    , activeRef = activeRef
    }

recordReplyUpdate
  :: (Prim :> es, Concurrent :> es)
  => ActiveReplyState
  -> Chat.ReplyStreamUpdate
  -> Eff es ()
recordReplyUpdate activeState update = do
  refreshActiveConversation activeState update.answer
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
    activeHandle <- rememberActiveConversation activeState.conversations activeState.parentMessageKey (conversationMessageKey activeState.message <$> responseId) activeState.threadId (appendAssistant current activeState.baseConversation)
    IORef.writeIORef activeState.activeRef activeHandle

registerActiveAliases
  :: Prim :> es
  => ActiveReplyState
  -> [MessageId]
  -> Eff es ()
registerActiveAliases activeState responseIds = do
  active <- IORef.readIORef activeState.activeRef
  traverse_ (\activeHandle -> traverse_ (addActiveConversationMessage activeState.conversations activeHandle . conversationMessageKey activeState.message) responseIds) active

refreshActiveConversation
  :: Prim :> es
  => ActiveReplyState
  -> Text
  -> Eff es ()
refreshActiveConversation activeState current = do
  active <- IORef.readIORef activeState.activeRef
  traverse_ (`updateActiveConversation` appendAssistant current activeState.baseConversation) active

llmFailureMessage :: SomeException -> Text
llmFailureMessage err =
  "LLM request failed: " <> (AgentFailure.agentFailureFromException err).userMessage
