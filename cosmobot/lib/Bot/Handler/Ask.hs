{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Handler.Ask
Description : Ask command and threaded conversation handler
Stability   : experimental
-}

module Bot.Handler.Ask
  ( askHandlers
  )
where

import qualified Bot.Agent as Agent
import qualified Bot.Agent.Failure as AgentFailure
import Bot.Core.Conversation
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Skills as Skills
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Effect.Typst as Typst
import Bot.Core.Route
import Bot.Handler.Ask.AgentRun (runAskAgentConversation)
import Bot.Handler.Ask.Config
import qualified Bot.Memory as MemoryStore
import Bot.Core.Message
import Bot.Prelude
import Bot.Storage.Conversation
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import Effectful.Timeout
import Effectful.Process
import Effectful.FileSystem

type HandlerEffects es =
  ( Chat.Chat :> es
  , ChatLog.ChatLog :> es
  , AgentAudit.AgentAudit :> es
  , HTTP.HTTP :> es
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
  void $ runAskAgentConversation toolCfg cfg conversations Nothing message input conversation

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
  responseId <- rightToMaybe <$> Chat.replyTo message answer
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
    void $ runAskAgentConversation toolCfg cfg conversations (Just (conversationMessageKey message parentId)) message input conversation

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
  void $ runAskAgentConversation toolCfg cfg conversations (Just parentKey) message input nextConversation

drawConversation
  :: (LLM.LLM :> es, KatipE :> es)
  => Conversation
  -> Eff es Text
drawConversation conversation =
  LLM.askImageWithHistory (Foldable.toList conversation.messages) `catchSync` \err -> do
    logError [i|LLM image request failed: #{show err :: String}|]
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
      referencedSenderLine referenced <> referencedTextLines referenced <> referencedImageLines referenced

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

referencedImageLines :: ReferencedMessage -> [Text]
referencedImageLines referenced =
  [ "被回复图片：" <> Text.intercalate ", " imageUrls
  | let imageUrls = filter (not . Text.null) (map Text.strip referenced.imageUrls)
  , not (null imageUrls)
  ]
