{-
Module      : Bot.Handler.Ask
Description : Ask command and threaded conversation handler
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Handler.Ask where

import qualified Bot.Agent as Agent
import Bot.Config
import Bot.Conversation
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Scheduler as Scheduler
import Bot.Filter
import Bot.Message
import Bot.Prelude
import Control.Concurrent (forkIO)
import qualified Data.Text as Text

askHandlers
  :: (Chat.Chat :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => AskHandlerConfig
  -> ConversationStore
  -> [RouteHandler es]
askHandlers cfg conversations =
  [ drawRoute cfg conversations
  , askRoute cfg conversations
  , privateRoute cfg conversations
  , mentionRoute cfg conversations
  , continueRoute cfg conversations
  ]

drawRoute
  :: (Chat.Chat :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
drawRoute cfg conversations =
  route (command cfg.drawCommand <* matching (canStartConversation cfg)) $ \message prompt ->
    forkEff (startDrawConversation "matched draw route" cfg conversations message prompt)

askRoute
  :: (Chat.Chat :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
askRoute cfg conversations =
  route (command cfg.command <* matching (canStartConversation cfg)) $ \message prompt ->
    forkEff (startAskConversation "matched ask route" cfg conversations message prompt)

forkEff :: IOE :> es => Eff es () -> Eff es ()
forkEff action =
  withEffToIO (ConcUnlift Persistent Unlimited) $ \runInIO ->
    void $ liftIO $ forkIO (runInIO action)

privateRoute
  :: (Chat.Chat :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
privateRoute cfg conversations =
  route privateMessage $ \message prompt ->
    startAskConversation "matched private ask route" cfg conversations message prompt
  where
    privateMessage =
      promptOrImages
        <* matching (isAllowedPrivate cfg)
        <* notReply
        <* notCommand cfg.command
        <* notCommand cfg.drawCommand

mentionRoute
  :: (Chat.Chat :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
mentionRoute cfg conversations =
  route mentionMessage $ \message prompt ->
    startAskConversation "matched bot mention route" cfg conversations message prompt
  where
    mentionMessage =
      promptOrImages
        <* matching (isAllowedGroup cfg)
        <* matching (mentionsBot cfg)
        <* notReply
        <* notCommand cfg.command
        <* notCommand cfg.drawCommand

continueRoute
  :: (Chat.Chat :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => AskHandlerConfig
  -> ConversationStore
  -> RouteHandler es
continueRoute cfg conversations =
  route continuedMessage \message parentId -> do
    parent <- lookupConversation conversations parentId
    case parent of
      Nothing
        | not (canStartFromReply cfg message) -> do
            logTrace "Ignoring reply to unknown conversation message" parentId
            logInfo "Ignoring unknown conversation reply" parentId
        | otherwise ->
            startConversationFromReply cfg conversations message parentId
      Just conversation ->
        continueConversation cfg conversations message conversation
  where
    continuedMessage =
      replyToMessage <* notCommand cfg.command <* notCommand cfg.drawCommand

startAskConversation
  :: (Chat.Chat :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => Text
  -> AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
startAskConversation label cfg conversations message prompt = do
  logTrace label message
  logInfo label (incomingMessageLog message)
  let conversation = startConversation cfg (promptOrImageDefault prompt message.imageUrls) message.imageUrls
  (answer, answeredConversation) <- askConversation cfg conversations message conversation
  responseId <- Chat.replyTo message answer
  rememberConversation conversations responseId answeredConversation

startDrawConversation
  :: (Chat.Chat :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => Text
  -> AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
startDrawConversation label cfg conversations message prompt = do
  logTrace label message
  logInfo label (incomingMessageLog message)
  referenced <- fetchReferencedMessage message
  let contextImages = maybe [] (.imageUrls) referenced <> message.imageUrls
  let contextPrompt = promptWithReferencedContext prompt referenced contextImages
  let conversation = startConversation cfg contextPrompt contextImages
  answer <- drawConversation conversation
  responseId <- Chat.replyTo message answer
  rememberConversation conversations responseId (appendAssistant answer conversation)

fetchReferencedMessage
  :: Chat.Chat :> es
  => IncomingMessage
  -> Eff es (Maybe ReferencedMessage)
fetchReferencedMessage message =
  traverse (Chat.getMessageContent message) message.replyToMessageId <&> join

startConversationFromReply
  :: (Chat.Chat :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> Integer
  -> Eff es ()
startConversationFromReply cfg conversations message parentId = do
  logTrace "starting conversation from mentioned reply" message
  logInfo "starting conversation from mentioned reply" (incomingMessageLog message)
  referenced <- Chat.getMessageContent message parentId
  let contextImages = maybe [] (.imageUrls) referenced <> message.imageUrls
  let prompt = promptWithReferencedContext message.text referenced contextImages
  unless (Text.null prompt && null contextImages) do
    let conversation = startConversation cfg prompt contextImages
    (answer, answeredConversation) <- askConversation cfg conversations message conversation
    responseId <- Chat.replyTo message answer
    rememberConversation conversations responseId answeredConversation

continueConversation
  :: (Chat.Chat :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> Conversation
  -> Eff es ()
continueConversation cfg conversations message conversation = do
  logTrace "continuing conversation" message
  logInfo "continuing conversation" (incomingMessageLog message)
  let nextConversation =
        appendUserContext (promptOrImageDefault message.text message.imageUrls) message.imageUrls conversation
  (answer, answeredConversation) <- askConversation cfg conversations message nextConversation
  responseId <- Chat.replyTo message answer
  rememberConversation conversations responseId answeredConversation

askConversation
  :: (Chat.Chat :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => AskHandlerConfig
  -> ConversationStore
  -> IncomingMessage
  -> Conversation
  -> Eff es (Text, Conversation)
askConversation cfg conversations message conversation =
  Agent.runAgent cfg.agentMaxTurns context Agent.defaultTools conversation `catch` \(err :: SomeException) -> do
    logInfo "LLM request failed" (show err :: String)
    pure ("LLM request failed.", conversation)
  where
    context =
      Agent.AgentContext
        { message = message
        , superuser = isSuperuser cfg message
        , askCommand = cfg.command
        , remember = rememberConversation conversations
        }

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

startConversation :: AskHandlerConfig -> Text -> [Text] -> Conversation
startConversation cfg prompt imageUrls =
  startWithSystemAndUserContext cfg.systemPrompt prompt imageUrls

promptWithReferencedContext :: Text -> Maybe ReferencedMessage -> [Text] -> Text
promptWithReferencedContext prompt referenced imageUrls =
  case (promptOrImageDefault prompt imageUrls, Text.strip . (.text) <$> referenced) of
    ("", Just quotedText) | not (Text.null quotedText) ->
      [i|请根据被回复消息回答。

被回复消息：
#{quotedText}|]
    (userPrompt, Just quotedText) | not (Text.null quotedText) ->
      [i|#{userPrompt}

被回复消息：
#{quotedText}|]
    (userPrompt, _) ->
      userPrompt
