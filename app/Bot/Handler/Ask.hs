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
import qualified Bot.Effect.Media as Media
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
import qualified Data.Foldable as Foldable
import qualified Effectful.Prim.IORef as IORef
import qualified Data.Text as Text
import qualified Streaming.Prelude as S
import Effectful.Timeout
import Effectful.Process
import Effectful.FileSystem

type HandlerEffects es =
  ( Chat.Chat :> es
  , ChatLog.ChatLog :> es
  , AgentAudit.AgentAudit :> es
  , LLM.LLM :> es
  , Media.Media :> es
  , Memory.Memory :> es
  , Concurrent :> es
  , Skills.Skills :> es
  , Scheduler.Scheduler :> es
  , Storage.Storage :> es
  , Typst.Typst :> es
  , KatipE :> es
  , Prim :> es
  , Process :> es
  , FileSystem :> es
  , Concurrent :> es
  , Fail :> es
  , Timeout :> es
  , IOE :> es
  )

-- | Routes for ask, draw, private, mention, and reply continuation flows.
askHandlers
  :: HandlerEffects es
  => ChatLog.ChatLog :> es
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
  :: HandlerEffects es
  => ChatLog.ChatLog :> es
  => AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
drawRoute cfg conversations =
  requireAuth canStartConversation (\_ -> pure ()) $
    stopOn (command cfg.drawCommand) \message prompt ->
      spawnTask (startDrawConversation "matched draw route" cfg conversations message prompt)

askRoute
  :: HandlerEffects es
  => IOE :> es
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
askRoute toolCfg cfg conversations =
  requireAuth canStartConversation (\_ -> pure ()) $
    stopOn (askPrefix cfg) \message prompt ->
      spawnTask (startAskConversation "matched ask route" toolCfg cfg conversations message prompt)

haltRoute
  :: Chat.Chat :> es
  => Storage.Storage :> es
  => KatipE :> es
  => Prim :> es
  => Concurrent :> es
  => IOE :> es
  => ConversationStore
  -> RouteHandler es
haltRoute conversations =
  stopOn (command "!halt" *> replyToMessage) \message parentId -> do
    halted <- haltConversation conversations (conversationMessageKey message parentId)
    if halted
      then logInfo "halted"
      else logInfo "couldn't halt active conversation"

privateRoute
  :: HandlerEffects es
  => Concurrent :> es
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
privateRoute toolCfg cfg conversations =
  stopOn privateMessage \message prompt ->
    spawnTask (startAskConversation "matched private ask route" toolCfg cfg conversations message prompt)
  where
    privateMessage =
      promptOrImages
        <* matching isAllowedPrivate
        <* notReply
        <* notAskPrefix cfg
        <* notCommand cfg.drawCommand

mentionRoute
  :: HandlerEffects es
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
mentionRoute toolCfg cfg conversations =
  stopOn mentionMessage \message prompt ->
    spawnTask (startAskConversation "matched bot mention route" toolCfg cfg conversations message prompt)
  where
    mentionMessage =
      promptOrImages
        <* matching mentionsConfiguredBot
        <* notReply
        <* notAskPrefix cfg
        <* notCommand cfg.drawCommand

continueRoute
  :: HandlerEffects es
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
continueRoute toolCfg cfg conversations =
  stopOn continuedMessage \message parentId ->
    spawnTask do
      let parentKey = conversationMessageKey message parentId
      parent <- lookupConversation conversations parentKey
      case parent of
        Nothing
          | not (canStartFromReply message) -> do
              logDebug [i|Ignoring reply to unknown conversation message: #{show parentId :: String}|]
              logInfo [i|Ignoring unknown conversation reply: #{messageIdText parentId}|]
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
  :: HandlerEffects es
  => Text
  -> Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
startAskConversation label toolCfg cfg conversations message prompt = do
  logDebug [i|#{label}: #{show message :: String}|]
  logInfo [i|#{label}: #{incomingMessageLogLine message}|]
  referenced <- fetchReferencedMessage message
  let contextImages = maybe [] (.imageUrls) referenced <> message.imageUrls
  let contextPrompt = promptWithReferencedContext prompt referenced contextImages
  let input = inputWithImages contextPrompt contextImages
  conversation <- startConversation cfg message input
  threadId <- myThreadId
  void $ askConversation toolCfg cfg conversations Nothing threadId message input conversation

startDrawConversation
  :: HandlerEffects es
  => ChatLog.ChatLog :> es
  => Text
  -> AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
startDrawConversation label cfg conversations message prompt = do
  logDebug [i|#{label}: #{show message :: String}|]
  logInfo [i|#{label}: #{incomingMessageLogLine message}|]
  referenced <- fetchReferencedMessage message
  let contextImages = maybe [] (.imageUrls) referenced <> message.imageUrls
  let contextPrompt = promptWithReferencedContext prompt referenced contextImages
  let input = inputWithImages contextPrompt contextImages
  conversation <- startConversation cfg message input
  answer <- drawConversation conversation
  responseId <- Chat.replyTo message answer
  ChatLog.recordSelfMessage message answer
  rememberConversation conversations (conversationMessageKey message <$> responseId) (appendAssistant answer conversation)

fetchReferencedMessage
  :: Chat.Chat :> es
  => IncomingMessage
  -> Eff es (Maybe ReferencedMessage)
fetchReferencedMessage message =
  traverse (Chat.getMessageContent message) message.replyToMessageId <&> join

startConversationFromReply
  :: HandlerEffects es
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> MessageId
  -> Eff es ()
startConversationFromReply toolCfg cfg conversations message parentId = do
  logDebug [i|starting conversation from mentioned reply: #{show message :: String}|]
  logInfo [i|starting conversation from mentioned reply: #{incomingMessageLogLine message}|]
  referenced <- Chat.getMessageContent message parentId
  let contextImages = maybe [] (.imageUrls) referenced <> message.imageUrls
  let prompt = promptWithReferencedContext message.text referenced contextImages
  unless (Text.null prompt && null contextImages) do
    let input = inputWithImages prompt contextImages
    conversation <- startConversation cfg message input
    threadId <- myThreadId
    void $ askConversation toolCfg cfg conversations (Just (conversationMessageKey message parentId)) threadId message input conversation

continueConversation
  :: HandlerEffects es
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> ConversationMessageKey
  -> Conversation
  -> Eff es ()
continueConversation toolCfg cfg conversations message parentKey conversation = do
  logDebug [i|continuing conversation: #{show message :: String}|]
  logInfo [i|continuing conversation: #{incomingMessageLogLine message}|]
  let input = inputWithImages (promptOrImageDefault message.text message.imageUrls) message.imageUrls
  let nextConversation =
        appendUserInput input conversation
  threadId <- myThreadId
  void $ askConversation toolCfg cfg conversations (Just parentKey) threadId message input nextConversation

askConversation
  :: HandlerEffects es
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> Maybe ConversationMessageKey
  -> ThreadId
  -> IncomingMessage
  -> MessageInput
  -> Conversation
  -> Eff es (Text, Conversation)
askConversation toolCfg cfg conversations parentMessageKey threadId message input conversation = do
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

streamAgentReply
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Media.Media :> es, Storage.Storage :> es, KatipE :> es, Prim :> es, Concurrent :> es)
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
        responseId <- Chat.replyTo message failureMessage
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
  active <- (IORef.readIORef activeReply.activeRef)
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

drawConversation
  :: (LLM.LLM :> es, KatipE :> es)
  => Conversation
  -> Eff es Text
drawConversation conversation =
  LLM.askImageWithHistory (Foldable.toList conversation.messages) `catchSync` \err -> do
    logInfo [i|LLM image request failed: #{show err :: String}|]
    pure ("Image generation failed: " <> (AgentFailure.agentFailureFromException err).userMessage)


promptOrImageDefault :: Text -> [Text] -> Text
promptOrImageDefault prompt imageUrls
  | not (Text.null stripped) = stripped
  | null imageUrls = ""
  | otherwise = "请根据图片回答。"
  where
    stripped = Text.strip prompt

startConversation :: (Memory.Memory :> es, Skills.Skills :> es) => AskHandlerConfig -> IncomingMessage -> MessageInput -> Eff es Conversation
startConversation cfg message input = do
  skillsPrompt <- Skills.skillsSystemPrompt
  senderMemory <- loadScopedMemory (MemoryStore.senderMemoryScope message)
  chatMemory <- loadScopedMemory (MemoryStore.chatMemoryScope message)
  let systemPrompt = LLM.contextSystemPrompt cfg.systemPrompt skillsPrompt senderMemory chatMemory
  pure (startWithSystemAndUserInput systemPrompt input)

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
