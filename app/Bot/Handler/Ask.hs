{-|
Module      : Bot.Handler.Ask
Description : Ask command and threaded conversation handler
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Handler.Ask
  ( askHandlers
  )
where

import qualified Bot.Agent as Agent
import qualified Bot.Agent.Failure as AgentFailure
import qualified Bot.Agent.Middleware.Observation as AgentObservation
import Bot.Core.Conversation
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Skills as Skills
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Effect.Typst as Typst
import Bot.Core.Route
import Bot.Handler.Ask.Config
import qualified Bot.Memory as MemoryStore
import Bot.Core.Message
import Bot.Prelude
import Bot.Storage.Conversation
import Control.Concurrent (ThreadId, myThreadId)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Streaming.Prelude as S

-- | Routes for ask, draw, private, mention, and reply continuation flows.
askHandlers
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, AgentAudit.AgentAudit :> es, LLM.LLM :> es, Memory.Memory :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Typst.Typst :> es, Log :> es, IOE :> es)
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> [RouteHandler es]
askHandlers toolCfg cfg conversations =
  [ drawRoute cfg conversations
  , haltRoute conversations
  , askRoute toolCfg cfg conversations
  , privateRoute toolCfg cfg conversations
  , mentionRoute toolCfg cfg conversations
  , continueRoute toolCfg cfg conversations
  ]

drawRoute
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, AgentAudit.AgentAudit :> es, LLM.LLM :> es, Memory.Memory :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Log :> es, IOE :> es)
  => AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
drawRoute cfg conversations =
  requireAuth canStartConversation (\_ -> pure ()) $
    stopOn (command cfg.drawCommand) \message prompt ->
      forkEff (startDrawConversation "matched draw route" cfg conversations message prompt)

askRoute
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, AgentAudit.AgentAudit :> es, LLM.LLM :> es, Memory.Memory :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Typst.Typst :> es, Log :> es, IOE :> es)
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
askRoute toolCfg cfg conversations =
  requireAuth canStartConversation (\_ -> pure ()) $
    stopOn (askPrefix cfg) \message prompt ->
      forkEff (startAskConversation "matched ask route" toolCfg cfg conversations message prompt)

haltRoute
  :: (Chat.Chat :> es, Storage.Storage :> es, Log :> es, IOE :> es)
  => ConversationStore
  -> RouteHandler es
haltRoute conversations =
  stopOn (command "!halt" *> replyToMessage) \message parentId -> do
    halted <- haltConversation conversations (conversationMessageKey message parentId)
    if halted
      then logInfo_ "halted"
      else logInfo_ "couldn't halt active conversation"

privateRoute
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, AgentAudit.AgentAudit :> es, LLM.LLM :> es, Memory.Memory :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Typst.Typst :> es, Log :> es, IOE :> es)
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
privateRoute toolCfg cfg conversations =
  stopOn privateMessage \message prompt ->
    forkEff (startAskConversation "matched private ask route" toolCfg cfg conversations message prompt)
  where
    privateMessage =
      promptOrImages
        <* matching isAllowedPrivate
        <* notReply
        <* notAskPrefix cfg
        <* notCommand cfg.drawCommand

mentionRoute
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, AgentAudit.AgentAudit :> es, LLM.LLM :> es, Memory.Memory :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Typst.Typst :> es, Log :> es, IOE :> es)
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
mentionRoute toolCfg cfg conversations =
  stopOn mentionMessage \message prompt ->
    forkEff (startAskConversation "matched bot mention route" toolCfg cfg conversations message prompt)
  where
    mentionMessage =
      promptOrImages
        <* matching isAllowedGroup
        <* matching mentionsConfiguredBot
        <* notReply
        <* notAskPrefix cfg
        <* notCommand cfg.drawCommand

continueRoute
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, AgentAudit.AgentAudit :> es, LLM.LLM :> es, Memory.Memory :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Typst.Typst :> es, Log :> es, IOE :> es)
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
continueRoute toolCfg cfg conversations =
  stopOn continuedMessage \message parentId ->
    forkEff do
      let parentKey = conversationMessageKey message parentId
      parent <- lookupConversation conversations parentKey
      case parent of
        Nothing
          | not (canStartFromReply message) -> do
              logTrace "Ignoring reply to unknown conversation message" parentId
              logInfo_ [i|Ignoring unknown conversation reply: #{parentId}|]
          | otherwise ->
              startConversationFromReply toolCfg cfg conversations message parentId
        Just conversation ->
          continueConversation toolCfg cfg conversations message parentKey conversation
  where
    continuedMessage =
      replyToMessage <* notAskPrefix cfg <* notCommand cfg.drawCommand

askPrefix :: AskHandlerConfig -> MessageFilter Text
askPrefix cfg =
  command cfg.command <|> maybe empty prefixedText cfg.name

notAskPrefix :: AskHandlerConfig -> MessageFilter IncomingMessage
notAskPrefix cfg =
  let MessageFilter matches = askPrefix cfg
  in
  rejecting \message ->
    isJust (matches message)

startAskConversation
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, AgentAudit.AgentAudit :> es, LLM.LLM :> es, Memory.Memory :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Typst.Typst :> es, Log :> es, IOE :> es)
  => Text
  -> Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
startAskConversation label toolCfg cfg conversations message prompt = do
  logTrace label message
  logInfo_ [i|#{label}: #{incomingMessageLogLine message}|]
  referenced <- fetchReferencedMessage message
  let contextImages = maybe [] (.imageUrls) referenced <> message.imageUrls
  let contextPrompt = promptWithReferencedContext prompt referenced contextImages
  conversation <- startConversation cfg message contextPrompt contextImages
  threadId <- liftIO myThreadId
  void $ askConversation toolCfg cfg conversations Nothing threadId message conversation

startDrawConversation
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, AgentAudit.AgentAudit :> es, LLM.LLM :> es, Memory.Memory :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Log :> es, IOE :> es)
  => Text
  -> AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
startDrawConversation label cfg conversations message prompt = do
  logTrace label message
  logInfo_ [i|#{label}: #{incomingMessageLogLine message}|]
  referenced <- fetchReferencedMessage message
  let contextImages = maybe [] (.imageUrls) referenced <> message.imageUrls
  let contextPrompt = promptWithReferencedContext prompt referenced contextImages
  conversation <- startConversation cfg message contextPrompt contextImages
  answer <- drawConversation conversation
  responseId <- Chat.replyTo message answer
  ChatLog.recordBotMessage message responseId answer
  rememberConversation conversations (conversationMessageKey message <$> responseId) (appendAssistant answer conversation)

fetchReferencedMessage
  :: Chat.Chat :> es
  => IncomingMessage
  -> Eff es (Maybe ReferencedMessage)
fetchReferencedMessage message =
  traverse (Chat.getMessageContent message) message.replyToMessageId <&> join

startConversationFromReply
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, AgentAudit.AgentAudit :> es, LLM.LLM :> es, Memory.Memory :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Typst.Typst :> es, Log :> es, IOE :> es)
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> Integer
  -> Eff es ()
startConversationFromReply toolCfg cfg conversations message parentId = do
  logTrace "starting conversation from mentioned reply" message
  logInfo_ [i|starting conversation from mentioned reply: #{incomingMessageLogLine message}|]
  referenced <- Chat.getMessageContent message parentId
  let contextImages = maybe [] (.imageUrls) referenced <> message.imageUrls
  let prompt = promptWithReferencedContext message.text referenced contextImages
  unless (Text.null prompt && null contextImages) do
    conversation <- startConversation cfg message prompt contextImages
    threadId <- liftIO myThreadId
    void $ askConversation toolCfg cfg conversations (Just (conversationMessageKey message parentId)) threadId message conversation

continueConversation
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, AgentAudit.AgentAudit :> es, LLM.LLM :> es, Memory.Memory :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Typst.Typst :> es, Log :> es, IOE :> es)
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> ConversationMessageKey
  -> Conversation
  -> Eff es ()
continueConversation toolCfg cfg conversations message parentKey conversation = do
  logTrace "continuing conversation" message
  logInfo_ [i|continuing conversation: #{incomingMessageLogLine message}|]
  let nextConversation =
        appendUserContext (promptOrImageDefault message.text message.imageUrls) message.imageUrls conversation
  threadId <- liftIO myThreadId
  void $ askConversation toolCfg cfg conversations (Just parentKey) threadId message nextConversation

askConversation
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, AgentAudit.AgentAudit :> es, LLM.LLM :> es, Memory.Memory :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Typst.Typst :> es, Log :> es, IOE :> es)
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> Maybe ConversationMessageKey
  -> ThreadId
  -> IncomingMessage
  -> Conversation
  -> Eff es (Text, Conversation)
askConversation toolCfg cfg conversations parentMessageKey threadId message conversation = do
  activeReply <- newActiveReply conversations parentMessageKey message threadId conversation
  let observer = AgentAudit.agentAuditObserver
  agentRun <- Agent.startAgentRun (agentContext toolCfg cfg conversations parentMessageKey message) Agent.defaultTools
  reply <-
    streamAgentReply cfg observer agentRun activeReply message conversation
      `onException` discardActiveReply activeReply
  commitAgentReply observer activeReply message reply
    `onException` discardActiveReply activeReply

data AgentReply = AgentReply
  { responseId :: !(Maybe Integer)
  , result :: !Agent.AgentResult
  }

agentContext
  :: (ChatLog.ChatLog :> es, Storage.Storage :> es, Log :> es, IOE :> es)
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> Maybe ConversationMessageKey
  -> IncomingMessage
  -> Agent.AgentContext es
agentContext toolCfg cfg conversations parentMessageKey message =
  Agent.AgentContext
    { message = message
    , superuser = isSuperuser message
    , systemContext = currentMessageSystemPrompt cfg message
    , askCommand = cfg.command
    , toolConfig = toolCfg
    , remember = \messageId -> rememberConversationFrom conversations parentMessageKey (conversationMessageKey message <$> messageId)
    , recordBotMessage = ChatLog.recordBotMessage message
    }

streamAgentReply
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Storage.Storage :> es, Log :> es, IOE :> es)
  => AskHandlerConfig
  -> Agent.AgentObserver AgentObservation.ObservationContext es
  -> Agent.AgentRun es
  -> ActiveReplyState
  -> IncomingMessage
  -> Conversation
  -> Eff es AgentReply
streamAgentReply cfg observer agentRun activeReply message conversation =
  do
    let program = Agent.defaultAgentProgram observer cfg.agentMaxTurns agentRun
    (responseId, result) <-
      S.mapM_
        (recordReplyUpdate activeReply)
        (Chat.streamReplySegmentsTo message (.answer) (agentReplySegmentStream observer activeReply message agentRun (Agent.runAgentProgramStreaming program conversation)))
    pure AgentReply{responseId, result}
  `catch` \(err :: SomeException) ->
    case Exception.fromException err of
      Just Exception.ThreadKilled ->
        throwIO err
      _ -> do
        logAttention_ [i|LLM request failed: #{show err :: String}|]
        let failureMessage = llmFailureMessage err
        responseId <- Chat.replyTo message failureMessage
        pure AgentReply
          { responseId
          , result = Agent.AgentResult
              { runId = Agent.agentRunId agentRun
              , answer = failureMessage
              , conversation = conversation
              }
          }

agentReplySegmentStream
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, Storage.Storage :> es, Log :> es, IOE :> es)
  => Agent.AgentObserver AgentObservation.ObservationContext es
  -> ActiveReplyState
  -> IncomingMessage
  -> Agent.AgentRun es
  -> Stream (Of Agent.AgentStreamOutput) (Eff es) Agent.AgentResult
  -> Stream (Of ReplyBody.ReplySegmentEvent) (Eff es) Agent.AgentResult
agentReplySegmentStream observer activeReply message agentRun stream = do
  next <- lift (S.next stream)
  case next of
    Left result ->
      pure result
    Right (Agent.AgentAnswerDelta chunk, rest) -> do
      S.yield (ReplyBody.ReplySegmentDelta chunk)
      agentReplySegmentStream observer activeReply message agentRun rest
    Right (Agent.AgentIntermediateMessage body snapshot, rest) -> do
      let content = ReplyBody.replyContentFromBody body
      S.yield (ReplyBody.ReplySegmentMessage content)
      lift (sendIntermediateAgentMessage observer activeReply message agentRun (ReplyBody.replyContentToBody content) snapshot)
      agentReplySegmentStream observer activeReply message agentRun rest

sendIntermediateAgentMessage
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, Storage.Storage :> es, Log :> es, IOE :> es)
  => Agent.AgentObserver AgentObservation.ObservationContext es
  -> ActiveReplyState
  -> IncomingMessage
  -> Agent.AgentRun es
  -> Text
  -> Conversation
  -> Eff es ()
sendIntermediateAgentMessage observer activeReply message agentRun body conversation = do
  responseId <- Chat.replyTo message body
  ChatLog.recordBotMessage message responseId body
  rememberConversationFrom activeReply.conversations activeReply.parentMessageKey (conversationMessageKey message <$> responseId) conversation
  traverse_ (AgentObservation.observeConversationLinked observer . intermediateConversationLink agentRun activeReply) responseId

intermediateConversationLink :: Agent.AgentRun es -> ActiveReplyState -> Integer -> AgentObservation.ObservedConversationLink
intermediateConversationLink agentRun activeReply linkedMessageId =
  AgentObservation.ObservedConversationLink
    { runId = Agent.agentRunId agentRun
    , parentMessageId = activeReply.parentMessageKey <&> (.messageId)
    , linkedMessageId
    }

commitAgentReply
  :: (ChatLog.ChatLog :> es, Storage.Storage :> es, Log :> es, IOE :> es)
  => Agent.AgentObserver AgentObservation.ObservationContext es
  -> ActiveReplyState
  -> IncomingMessage
  -> AgentReply
  -> Eff es (Text, Conversation)
commitAgentReply observer activeReply message AgentReply{responseId, result} = do
  traverse_ (AgentObservation.observeConversationLinked observer . conversationLink result (activeReply.parentMessageKey <&> (.messageId))) responseId
  ChatLog.recordBotMessage message responseId result.answer
  active <- liftIO (IORef.readIORef activeReply.activeRef)
  case active of
    Just activeHandle ->
      finishActiveConversation activeReply.conversations activeHandle result.conversation
    Nothing ->
      rememberConversationFrom activeReply.conversations activeReply.parentMessageKey (conversationMessageKey message <$> responseId) result.conversation
  pure (result.answer, result.conversation)

conversationLink :: Agent.AgentResult -> Maybe Integer -> Integer -> AgentObservation.ObservedConversationLink
conversationLink result parentMessageId linkedMessageId =
  AgentObservation.ObservedConversationLink
    { runId = result.runId
    , parentMessageId
    , linkedMessageId
    }

discardActiveReply :: (Storage.Storage :> es, Log :> es, IOE :> es) => ActiveReplyState -> Eff es ()
discardActiveReply activeReply =
  liftIO (IORef.readIORef activeReply.activeRef)
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
  :: IOE :> es
  => ConversationStore
  -> Maybe ConversationMessageKey
  -> IncomingMessage
  -> ThreadId
  -> Conversation
  -> Eff es ActiveReplyState
newActiveReply conversations parentMessageKey message threadId baseConversation = do
  activeRef <- liftIO (IORef.newIORef Nothing)
  pure ActiveReplyState
    { conversations = conversations
    , parentMessageKey = parentMessageKey
    , message = message
    , threadId = threadId
    , baseConversation = baseConversation
    , activeRef = activeRef
    }

recordReplyUpdate
  :: IOE :> es
  => ActiveReplyState
  -> Chat.ReplyStreamUpdate
  -> Eff es ()
recordReplyUpdate activeState update = do
  refreshActiveConversation activeState update.answer
  registerActiveIfNeeded activeState update.responseId update.answer
  registerActiveAliases activeState update.sentResponseIds

registerActiveIfNeeded
  :: IOE :> es
  => ActiveReplyState
  -> Maybe Integer
  -> Text
  -> Eff es ()
registerActiveIfNeeded activeState responseId current = do
  existing <- liftIO (IORef.readIORef activeState.activeRef)
  when (isNothing existing) do
    activeHandle <- rememberActiveConversation activeState.conversations activeState.parentMessageKey (conversationMessageKey activeState.message <$> responseId) activeState.threadId (appendAssistant current activeState.baseConversation)
    liftIO $ IORef.writeIORef activeState.activeRef activeHandle

registerActiveAliases
  :: IOE :> es
  => ActiveReplyState
  -> [Integer]
  -> Eff es ()
registerActiveAliases activeState responseIds = do
  active <- liftIO (IORef.readIORef activeState.activeRef)
  traverse_ (\activeHandle -> traverse_ (addActiveConversationMessage activeState.conversations activeHandle . conversationMessageKey activeState.message) responseIds) active

refreshActiveConversation
  :: IOE :> es
  => ActiveReplyState
  -> Text
  -> Eff es ()
refreshActiveConversation activeState current = do
  active <- liftIO (IORef.readIORef activeState.activeRef)
  traverse_ (`updateActiveConversation` appendAssistant current activeState.baseConversation) active

llmFailureMessage :: SomeException -> Text
llmFailureMessage err =
  "LLM request failed: " <> (AgentFailure.agentFailureFromException err).userMessage

drawConversation
  :: (LLM.LLM :> es, Log :> es)
  => Conversation
  -> Eff es Text
drawConversation conversation =
  LLM.askImageWithHistory (Foldable.toList conversation.messages) `catch` \(err :: SomeException) -> do
    logInfo_ [i|LLM image request failed: #{show err :: String}|]
    pure ("Image generation failed: " <> (AgentFailure.agentFailureFromException err).userMessage)


promptOrImageDefault :: Text -> [Text] -> Text
promptOrImageDefault prompt imageUrls
  | not (Text.null stripped) = stripped
  | null imageUrls = ""
  | otherwise = "请根据图片回答。"
  where
    stripped = Text.strip prompt

startConversation :: (Memory.Memory :> es, Skills.Skills :> es) => AskHandlerConfig -> IncomingMessage -> Text -> [Text] -> Eff es Conversation
startConversation cfg message prompt imageUrls = do
  skillsPrompt <- Skills.skillsSystemPrompt
  senderMemory <- loadScopedMemory (MemoryStore.senderMemoryScope message)
  chatMemory <- loadScopedMemory (MemoryStore.chatMemoryScope message)
  let systemPrompt = LLM.contextSystemPrompt cfg.systemPrompt skillsPrompt senderMemory chatMemory
  pure (startWithSystemAndUserContext systemPrompt prompt imageUrls)

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

loadScopedMemory :: Memory.Memory :> es => Either Text MemoryStore.MemoryScope -> Eff es (Maybe Text)
loadScopedMemory =
  either (const (pure Nothing)) Memory.loadMemory

promptWithReferencedContext :: Text -> Maybe ReferencedMessage -> [Text] -> Text
promptWithReferencedContext prompt referenced imageUrls =
  case (promptOrImageDefault prompt imageUrls, referenced >>= referencedMessageContext) of
    ("", Just quotedContext) ->
      [i|请根据被回复消息回答。

被回复消息：
#{quotedContext}|]
    (userPrompt, Just quotedContext) ->
      [i|#{userPrompt}

被回复消息：
#{quotedContext}|]
    (userPrompt, _) ->
      userPrompt

referencedMessageContext :: ReferencedMessage -> Maybe Text
referencedMessageContext referenced =
  if null contextLines
    then Nothing
    else Just (Text.unlines contextLines)
  where
    contextLines =
      referencedSenderLine referenced <> referencedTextLines referenced

referencedSenderLine :: ReferencedMessage -> [Text]
referencedSenderLine referenced =
  [ "被回复用户：" <> Text.intercalate " " (catMaybes [referenced.senderDisplayName, parenthesized <$> referenced.senderIdentifier])
  | isJust referenced.senderDisplayName || isJust referenced.senderIdentifier
  ]
  where
    parenthesized value =
      "(" <> value <> ")"

referencedTextLines :: ReferencedMessage -> [Text]
referencedTextLines referenced =
  [ text | let text = Text.strip referenced.text, not (Text.null text) ]
