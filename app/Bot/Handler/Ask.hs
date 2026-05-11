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
import Bot.Config
import Bot.Conversation
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Scheduler as Scheduler
import Bot.Filter
import qualified Bot.Memory as Memory
import Bot.Message
import Bot.Prelude
import Control.Concurrent (ThreadId, forkIO, myThreadId)
import qualified Control.Exception as Exception
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Streaming.Prelude as S

-- | Routes for ask, draw, private, mention, and reply continuation flows.
askHandlers
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => Memory.MemoryConfig
  -> Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> [RouteHandler es]
askHandlers memoryCfg toolCfg cfg conversations =
  [ drawRoute memoryCfg cfg conversations
  , haltRoute conversations
  , askRoute memoryCfg toolCfg cfg conversations
  , privateRoute memoryCfg toolCfg cfg conversations
  , mentionRoute memoryCfg toolCfg cfg conversations
  , continueRoute memoryCfg toolCfg cfg conversations
  ]

drawRoute
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => Memory.MemoryConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
drawRoute memoryCfg cfg conversations =
  routeStop (command cfg.drawCommand <* matching (canStartConversation cfg)) $ \message prompt ->
    forkEff (startDrawConversation "matched draw route" memoryCfg cfg conversations message prompt)

askRoute
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => Memory.MemoryConfig
  -> Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
askRoute memoryCfg toolCfg cfg conversations =
  routeStop (askPrefix cfg <* matching (canStartConversation cfg)) $ \message prompt ->
    forkEff (startAskConversation "matched ask route" memoryCfg toolCfg cfg conversations message prompt)

forkEff :: IOE :> es => Eff es () -> Eff es ()
forkEff action =
  withEffToIO (ConcUnlift Persistent Unlimited) $ \runInIO ->
    void $ liftIO $ forkIO (runInIO action)

haltRoute
  :: (Chat.Chat :> es, Log :> es, IOE :> es)
  => ConversationStore
  -> RouteHandler es
haltRoute conversations =
  routeStop (command "!halt" *> replyToMessage) \_message parentId -> do
    halted <- haltConversation conversations parentId
    if halted
      then logInfo_ "halted"
      else logInfo_ "couldn't halt active conversation"

privateRoute
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => Memory.MemoryConfig
  -> Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
privateRoute memoryCfg toolCfg cfg conversations =
  routeStop privateMessage $ \message prompt ->
    forkEff (startAskConversation "matched private ask route" memoryCfg toolCfg cfg conversations message prompt)
  where
    privateMessage =
      promptOrImages
        <* matching (isAllowedPrivate cfg)
        <* notReply
        <* notAskPrefix cfg
        <* notCommand cfg.drawCommand

mentionRoute
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => Memory.MemoryConfig
  -> Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
mentionRoute memoryCfg toolCfg cfg conversations =
  routeStop mentionMessage $ \message prompt ->
    forkEff (startAskConversation "matched bot mention route" memoryCfg toolCfg cfg conversations message prompt)
  where
    mentionMessage =
      promptOrImages
        <* matching (isAllowedGroup cfg)
        <* matching (mentionsBot cfg)
        <* notReply
        <* notAskPrefix cfg
        <* notCommand cfg.drawCommand

continueRoute
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => Memory.MemoryConfig
  -> Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
continueRoute memoryCfg toolCfg cfg conversations =
  routeStop continuedMessage \message parentId ->
    forkEff do
      parent <- lookupConversation conversations parentId
      case parent of
        Nothing
          | not (canStartFromReply cfg message) -> do
              logTrace "Ignoring reply to unknown conversation message" parentId
              logInfo "Ignoring unknown conversation reply" parentId
          | otherwise ->
              startConversationFromReply memoryCfg toolCfg cfg conversations message parentId
        Just conversation ->
          continueConversation memoryCfg toolCfg cfg conversations message parentId conversation
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
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => Text
  -> Memory.MemoryConfig
  -> Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
startAskConversation label memoryCfg toolCfg cfg conversations message prompt = do
  logTrace label message
  logInfo label (incomingMessageLogLine message)
  referenced <- fetchReferencedMessage message
  let contextImages = maybe [] (.imageUrls) referenced <> message.imageUrls
  let contextPrompt = promptWithReferencedContext prompt referenced contextImages
  conversation <- startConversation memoryCfg cfg message contextPrompt contextImages
  threadId <- liftIO myThreadId
  void $ askConversation memoryCfg toolCfg cfg conversations Nothing threadId message conversation

startDrawConversation
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => Text
  -> Memory.MemoryConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
startDrawConversation label memoryCfg cfg conversations message prompt = do
  logTrace label message
  logInfo label (incomingMessageLogLine message)
  referenced <- fetchReferencedMessage message
  let contextImages = maybe [] (.imageUrls) referenced <> message.imageUrls
  let contextPrompt = promptWithReferencedContext prompt referenced contextImages
  conversation <- startConversation memoryCfg cfg message contextPrompt contextImages
  answer <- drawConversation conversation
  responseId <- Chat.replyTo message answer
  ChatLog.recordBotMessage message responseId answer
  rememberConversation conversations responseId (appendAssistant answer conversation)

fetchReferencedMessage
  :: Chat.Chat :> es
  => IncomingMessage
  -> Eff es (Maybe ReferencedMessage)
fetchReferencedMessage message =
  traverse (Chat.getMessageContent message) message.replyToMessageId <&> join

startConversationFromReply
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => Memory.MemoryConfig
  -> Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> Integer
  -> Eff es ()
startConversationFromReply memoryCfg toolCfg cfg conversations message parentId = do
  logTrace "starting conversation from mentioned reply" message
  logInfo "starting conversation from mentioned reply" (incomingMessageLogLine message)
  referenced <- Chat.getMessageContent message parentId
  let contextImages = maybe [] (.imageUrls) referenced <> message.imageUrls
  let prompt = promptWithReferencedContext message.text referenced contextImages
  unless (Text.null prompt && null contextImages) do
    conversation <- startConversation memoryCfg cfg message prompt contextImages
    threadId <- liftIO myThreadId
    void $ askConversation memoryCfg toolCfg cfg conversations (Just parentId) threadId message conversation

continueConversation
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => Memory.MemoryConfig
  -> Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> Integer
  -> Conversation
  -> Eff es ()
continueConversation memoryCfg toolCfg cfg conversations message parentId conversation = do
  logTrace "continuing conversation" message
  logInfo "continuing conversation" (incomingMessageLogLine message)
  let nextConversation =
        appendUserContext (promptOrImageDefault message.text message.imageUrls) message.imageUrls conversation
  threadId <- liftIO myThreadId
  void $ askConversation memoryCfg toolCfg cfg conversations (Just parentId) threadId message nextConversation

askConversation
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => Memory.MemoryConfig
  -> Agent.ToolConfig
  -> AskHandlerConfig
  -> ConversationStore
  -> Maybe Integer
  -> ThreadId
  -> IncomingMessage
  -> Conversation
  -> Eff es (Text, Conversation)
askConversation memoryCfg toolCfg cfg conversations parentMessageId threadId message conversation = do
  activeReply <- newActiveReply conversations parentMessageId threadId conversation
  (responseId, (answer, answeredConversation)) <-
    S.mapM_
      (recordReplyUpdate activeReply)
      (Chat.streamReplyTo message fst (Agent.runAgentStreaming cfg.agentMaxTurns context Agent.defaultTools conversation))
      `catch` \(err :: SomeException) -> do
        case Exception.fromException err of
          Just Exception.ThreadKilled ->
            throwIO err
          _ -> do
            logAttention "LLM request failed" (show err :: String)
            responseId <- Chat.replyTo message "LLM request failed."
            pure (responseId, ("LLM request failed.", conversation))
  ChatLog.recordBotMessage message responseId answer
  active <- liftIO (IORef.readIORef activeReply.activeRef)
  case active of
    Just handle ->
      finishActiveConversation conversations handle answeredConversation
    Nothing ->
      rememberConversationFrom conversations parentMessageId responseId answeredConversation
  pure (answer, answeredConversation)
  where
    context =
      Agent.AgentContext
        { message = message
        , superuser = isSuperuser cfg message
        , askCommand = cfg.command
        , toolConfig = toolCfg
        , memoryConfig = Just memoryCfg
        , remember = rememberConversationFrom conversations parentMessageId
        , recordBotMessage = ChatLog.recordBotMessage message
        }

data ActiveReplyState = ActiveReplyState
  { conversations :: !ConversationStore
  , parentMessageId :: !(Maybe Integer)
  , threadId :: !ThreadId
  , baseConversation :: !Conversation
  , activeRef :: !(IORef.IORef (Maybe ActiveConversationHandle))
  }

newActiveReply
  :: IOE :> es
  => ConversationStore
  -> Maybe Integer
  -> ThreadId
  -> Conversation
  -> Eff es ActiveReplyState
newActiveReply conversations parentMessageId threadId baseConversation = do
  activeRef <- liftIO (IORef.newIORef Nothing)
  pure ActiveReplyState
    { conversations = conversations
    , parentMessageId = parentMessageId
    , threadId = threadId
    , baseConversation = baseConversation
    , activeRef = activeRef
    }

recordReplyUpdate
  :: IOE :> es
  => ActiveReplyState
  -> Chat.ReplyStreamUpdate
  -> Eff es ()
recordReplyUpdate state update = do
  refreshActiveConversation state update.answer
  registerActiveIfNeeded state update.responseId update.answer

registerActiveIfNeeded
  :: IOE :> es
  => ActiveReplyState
  -> Maybe Integer
  -> Text
  -> Eff es ()
registerActiveIfNeeded state responseId current = do
  existing <- liftIO (IORef.readIORef state.activeRef)
  when (isNothing existing) do
    handle <- rememberActiveConversation state.conversations state.parentMessageId responseId state.threadId (appendAssistant current state.baseConversation)
    liftIO $ IORef.writeIORef state.activeRef handle

refreshActiveConversation
  :: IOE :> es
  => ActiveReplyState
  -> Text
  -> Eff es ()
refreshActiveConversation state current = do
  active <- liftIO (IORef.readIORef state.activeRef)
  traverse_ (`updateActiveConversation` appendAssistant current state.baseConversation) active

drawConversation
  :: (LLM.LLM :> es, Log :> es)
  => Conversation
  -> Eff es Text
drawConversation conversation =
  LLM.askImageWithHistory conversation.messages `catch` \(err :: SomeException) -> do
    logInfo "LLM image request failed" (show err :: String)
    pure "Image generation failed."

mentionsBot :: AskHandlerConfig -> IncomingMessage -> Bool
mentionsBot cfg message =
  mentionsConfiguredBot cfg message

promptOrImageDefault :: Text -> [Text] -> Text
promptOrImageDefault prompt imageUrls
  | not (Text.null stripped) = stripped
  | null imageUrls = ""
  | otherwise = "请根据图片回答。"
  where
    stripped = Text.strip prompt

startConversation :: IOE :> es => Memory.MemoryConfig -> AskHandlerConfig -> IncomingMessage -> Text -> [Text] -> Eff es Conversation
startConversation memoryCfg cfg message prompt imageUrls = do
  senderMemory <- Memory.loadSenderMemory memoryCfg message
  chatMemory <- Memory.loadChatMemory memoryCfg message
  let systemPrompt = Memory.memorySystemPrompt cfg.systemPrompt senderMemory chatMemory
  pure (startWithSystemAndUserContext systemPrompt prompt imageUrls)

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
