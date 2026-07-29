{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Handler.Ask
Description : Ask command and threaded ask handler
Stability   : experimental
-}

module Bot.Handler.Ask
  ( askHandlers
  )
where

import qualified Bot.Agent as Agent
import qualified Bot.Agent.Failure as Failure
import qualified Bot.Core.ReplyBody as ReplyBody
import Bot.Core.Thread
import Bot.Core.Transcript
import qualified Bot.Effect.ACP as ACP
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.AgentAudit as AgentAudit
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
import Bot.Core.Route
import Bot.Handler.Ask.AgentRun (runAskAgentThread)
import Bot.Handler.Ask.Config
import qualified Bot.Memory as MemoryStore
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Session as Session
import Bot.Storage.Thread
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Effectful.Timeout
import Effectful.Process
import Effectful.FileSystem

type HandlerEffects es =
  ( ACP.ACP :> es
  , Chat.Chat :> es
  , ChatLog.ChatLog :> es
  , AgentAudit.AgentAudit :> es
  , Concurrency.Concurrency :> es
  , HTTP.HTTP :> es
  , LLM.LLM :> es
  , Media.Media :> es
  , Memory.Memory :> es
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

-- | Routes for ask, draw, private, mention, and reply continuation flows.
askHandlers
  :: HandlerEffects es
  => ChatLog.ChatLog :> es
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ThreadStore
  -> [RouteHandler es]
askHandlers toolCfg cfg threads =
  [ haltRoute threads
  , steerRoute threads
  , drawRoute cfg threads
  , askRoute toolCfg cfg threads
  , privateRoute toolCfg cfg threads
  , mentionRoute toolCfg cfg threads
  , continueRoute toolCfg cfg threads
  ]

drawRoute
  :: HandlerEffects es
  => ChatLog.ChatLog :> es
  => AskHandlerConfig
  -> ThreadStore
  -> RouteHandler es
drawRoute cfg threads =
  withHelp (RouteHelp (cfg.drawCommand <> " <prompt>") "Generate an image from a prompt.") $
  requireAuth canStartThread (\_ -> pure ()) $
    stopOn (command cfg.drawCommand) \message prompt ->
      Concurrency.fire "ask.draw" $
        startDrawThread "matched draw route" cfg threads message prompt

askRoute
  :: HandlerEffects es
  => IOE :> es
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ThreadStore
  -> RouteHandler es
askRoute toolCfg cfg threads =
  withHelp (RouteHelp (cfg.command <> " <prompt>") "Start an agent conversation.") $
  requireAuth canStartThread (\_ -> pure ()) $
    stopOn (askPrefix cfg) \message prompt ->
      Concurrency.fireWithHandle "ask.command" \resource ->
        startAskThread "matched ask route" toolCfg cfg threads resource message prompt

haltRoute
  :: Chat.Chat :> es
  => Storage.Storage :> es
  => Concurrency.Concurrency :> es
  => KatipE :> es
  => Prim :> es
  => Concurrent :> es
  => IOE :> es
  => ThreadStore
  -> RouteHandler es
haltRoute threads =
  withHelp (RouteHelp "!halt [all|<id>...]" "List or stop active agent threads in this chat.") $
  stopOn (command "!halt") (handleHalt threads)

steerRoute
  :: (Prim :> es, Concurrent :> es)
  => ThreadStore
  -> RouteHandler es
steerRoute threads =
  guardRouteM (\message -> enqueueActiveThreadSteer threads message (Text.strip message.text)) $
    stopOn steeringMessage \_ _ -> pure ()
  where
    steeringMessage =
      matching \message ->
        isJust message.replyToMessageId
          && not (Text.null (Text.strip message.text))
          && null message.imageUrls
          && null message.files

handleHalt
  :: (Chat.Chat :> es, Storage.Storage :> es, Concurrency.Concurrency :> es, KatipE :> es, Prim :> es, Concurrent :> es)
  => ThreadStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleHalt threads message args
  | Text.null input, isJust message.replyToMessageId = do
      halted <- haltThreadForMessage threads Concurrency.cancel message
      logHalted halted
  | Text.null input =
      listActiveThreadsForMessage threads message >>= void . Chat.replyTo message . renderActiveThreads
  | input == "all", isNothing message.replyToMessageId = do
      active <- listActiveThreadsForMessage threads message
      halted <- haltActiveThreadsForMessage threads Concurrency.cancel message (map (.id) active)
      logInfo [i|halted #{length halted} active threads|]
  | otherwise =
      case traverse parseThreadId (Text.words input) of
        Nothing ->
          void $ Chat.replyTo message "Usage: !halt, !halt all, or !halt <id>..."
        Just threadIds -> do
          halted <- haltActiveThreadsForMessage threads Concurrency.cancel message (ordNub threadIds)
          logInfo [i|halted #{length halted} requested active threads|]
  where
    input = Text.strip args
    logHalted True = logInfo "halted"
    logHalted False = logInfo "couldn't halt active thread"

parseThreadId :: Text -> Maybe Concurrency.Id
parseThreadId value = do
  threadId <- readMaybe (toString value)
  guard (threadId > 0)
  pure (Concurrency.Id threadId)

renderActiveThreads :: [ActiveThreadInfo] -> Text
renderActiveThreads [] =
  "No active threads."
renderActiveThreads threads =
  Text.unlines
    [ "- " <> show thread.id.unId <> ": " <> Text.take 20 (Text.unwords (Text.words thread.prompt))
    | thread <- threads
    ]

privateRoute
  :: HandlerEffects es
  => Concurrent :> es
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ThreadStore
  -> RouteHandler es
privateRoute toolCfg cfg threads =
  stopOn privateMessage \message prompt ->
    Concurrency.fireWithHandle "ask.private" \resource ->
      startAskThread "matched private ask route" toolCfg cfg threads resource message prompt
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
  -> ThreadStore
  -> RouteHandler es
mentionRoute toolCfg cfg threads =
  stopOn mentionMessage \message prompt ->
    Concurrency.fireWithHandle "ask.mention" \resource ->
      startAskThread "matched bot mention route" toolCfg cfg threads resource message prompt
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
  -> ThreadStore
  -> RouteHandler es
continueRoute toolCfg cfg threads =
  guardRouteM (replyReferencesThread threads) $
  stopOn continuedMessage \message parentId ->
    Concurrency.fireWithHandle "ask.continue" \resource -> do
      let parentKey = threadMessageKey message parentId
      parentTranscript <- lookupThreadTranscript threads parentKey
      case parentTranscript of
        Nothing
          | not (canStartFromReply message) -> do
              logDebug [i|Ignoring reply to unknown thread message: #{show parentId :: String}|]
              logInfo [i|Ignoring unknown thread reply: #{messageIdText parentId}|]
          | otherwise ->
              startThreadFromReply toolCfg cfg threads resource message parentId
        Just transcript ->
          continueThread toolCfg cfg threads resource message parentKey transcript
  where
    continuedMessage =
      replyToMessage <* notAskPrefix cfg <* notCommand cfg.drawCommand

replyReferencesThread
  :: (Chat.Chat :> es, Storage.Storage :> es, Prim :> es, Concurrent :> es)
  => ThreadStore
  -> IncomingMessage
  -> Eff es Bool
replyReferencesThread threads message =
  case threadMessageKey message <$> message.replyToMessageId of
    Nothing ->
      pure False
    Just messageKey -> do
      lookupActiveThreadRunId threads messageKey >>= \case
        Just{} ->
          pure False
        Nothing ->
          lookupThreadTranscript threads messageKey >>= \case
            Just{} ->
              pure True
            Nothing ->
              replyReferencesBot message

replyReferencesBot :: Chat.Chat :> es => IncomingMessage -> Eff es Bool
replyReferencesBot =
  fmap (maybe False (.senderIsBot)) . fetchReferencedMessage

askPrefix :: AskHandlerConfig -> MessageFilter Text
askPrefix cfg =
  command cfg.command <|> maybe empty prefixedText cfg.name

notAskPrefix :: AskHandlerConfig -> MessageFilter IncomingMessage
notAskPrefix cfg =
  let MessageFilter matches = askPrefix cfg
  in
  rejecting \message ->
    isJust (matches message)

startAskThread
  :: HandlerEffects es
  => Text
  -> Agent.ToolConfig
  -> AskHandlerConfig
  -> ThreadStore
  -> Concurrency.Handle
  -> IncomingMessage
  -> Text
  -> Eff es ()
startAskThread label toolCfg cfg threads resource message prompt = do
  logDebug [i|#{label}: #{show message :: String}|]
  logInfo [i|#{label}: #{incomingMessageLogLine message}|]
  referenced <- fetchReferencedMessage message
  let contextImages = maybe [] (.imageUrls) referenced <> message.imageUrls
  let contextFiles = referencedFiles referenced <> message.files
  let contextPrompt = promptWithCurrentFiles (promptWithReferencedContext prompt referenced contextImages) message.files
  let input = inputWithAttachments contextPrompt contextImages contextFiles
  transcript <- startTranscript cfg message input
  void $ runAskAgentThread toolCfg cfg threads resource Nothing message input transcript

startDrawThread
  :: HandlerEffects es
  => ChatLog.ChatLog :> es
  => Text
  -> AskHandlerConfig
  -> ThreadStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
startDrawThread label cfg threads message prompt = do
  logDebug [i|#{label}: #{show message :: String}|]
  logInfo [i|#{label}: #{incomingMessageLogLine message}|]
  referenced <- fetchReferencedMessage message
  let contextImages = maybe [] (.imageUrls) referenced <> message.imageUrls
  let contextFiles = referencedFiles referenced <> message.files
  let contextPrompt = promptWithCurrentFiles (promptWithReferencedContext prompt referenced contextImages) message.files
  let input = inputWithAttachments contextPrompt contextImages contextFiles
  transcript <- startTranscript cfg message input
  answer <- drawTranscript transcript
  responseId <- listToMaybe . rights <$> Chat.replyTo message answer
  ChatLog.recordSelfMessage message answer
  rememberThreadTranscript threads (threadMessageKey message <$> responseId) (appendAssistant answer transcript)

fetchReferencedMessage
  :: Chat.Chat :> es
  => IncomingMessage
  -> Eff es (Maybe ReferencedMessage)
fetchReferencedMessage message =
  traverse (Chat.getMessageContent message) message.replyToMessageId <&> join

startThreadFromReply
  :: HandlerEffects es
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ThreadStore
  -> Concurrency.Handle
  -> IncomingMessage
  -> MessageId
  -> Eff es ()
startThreadFromReply toolCfg cfg threads resource message parentId = do
  logDebug [i|starting thread from mentioned reply: #{show message :: String}|]
  logInfo [i|starting thread from mentioned reply: #{incomingMessageLogLine message}|]
  referenced <- Chat.getMessageContent message parentId
  let contextImages = maybe [] (.imageUrls) referenced <> message.imageUrls
  let contextFiles = referencedFiles referenced <> message.files
  let prompt = promptWithCurrentFiles (promptWithReferencedContext message.text referenced contextImages) message.files
  unless (Text.null prompt && null contextImages) do
    let input = inputWithAttachments prompt contextImages contextFiles
    transcript <- startTranscript cfg message input
    void $ runAskAgentThread toolCfg cfg threads resource (Just (threadMessageKey message parentId)) message input transcript

continueThread
  :: HandlerEffects es
  => Agent.ToolConfig
  -> AskHandlerConfig
  -> ThreadStore
  -> Concurrency.Handle
  -> IncomingMessage
  -> ThreadMessageKey
  -> Transcript
  -> Eff es ()
continueThread toolCfg cfg threads resource message parentKey transcript = do
  logDebug [i|continuing thread: #{show message :: String}|]
  logInfo [i|continuing thread: #{incomingMessageLogLine message}|]
  let prompt = promptWithCurrentFiles (promptOrImageDefault message.text message.imageUrls) message.files
  let input = inputWithAttachments prompt message.imageUrls message.files
  let nextTranscript =
        appendUserInput input transcript
  void $ runAskAgentThread toolCfg cfg threads resource (Just parentKey) message input nextTranscript

drawTranscript
  :: (LLM.LLM :> es, KatipE :> es)
  => Transcript
  -> Eff es Text
drawTranscript transcript =
  LLM.askImageWithHistory (Foldable.toList transcript.messages) `catchSync` \err -> do
    logError [i|LLM image request failed: #{show err :: String}|]
    pure ("Image generation failed: " <> (Failure.failureFromException err).userMessage)


promptOrImageDefault :: Text -> [Text] -> Text
promptOrImageDefault prompt imageUrls
  | not (Text.null stripped) = stripped
  | null imageUrls = ""
  | otherwise = "请根据图片回答。"
  where
    stripped = Text.strip prompt

startTranscript :: (Memory.Memory :> es, Skills.Skills :> es, Storage.Storage :> es) => AskHandlerConfig -> IncomingMessage -> MessageInput -> Eff es Transcript
startTranscript cfg message input = do
  skillsPrompt <- Skills.skillsSystemPrompt
  senderMemory <- loadScopedMemory (MemoryStore.senderMemoryScope message)
  chatMemory <- loadScopedMemory (MemoryStore.chatMemoryScope message)
  let systemPrompt = LLM.contextSystemPrompt cfg.systemPrompt skillsPrompt senderMemory chatMemory
  case acpSessionId message of
    Nothing ->
      pure (startWithSystemAndUserInput systemPrompt input)
    Just sessionId -> do
      history <- Session.sessionHistory sessionId
      pure $
        if null history
          then startWithSystemAndUserInput systemPrompt input
          else sessionHistoryTranscript systemPrompt history

acpSessionId :: IncomingMessage -> Maybe Session.SessionId
acpSessionId message = do
  guard (message.platform == PlatformACP)
  Session.SessionId <$> listToMaybe message.chatAliases

sessionHistoryTranscript :: Text -> [Session.SessionMessage] -> Transcript
sessionHistoryTranscript systemPrompt history =
  Transcript $
    Seq.fromList $
      systemMessages <> concatMap sessionMessageChatMessages history
  where
    systemMessages =
      [LLM.systemText systemPrompt | not (Text.null systemPrompt)]

sessionMessageChatMessages :: Session.SessionMessage -> [LLM.ChatMessage]
sessionMessageChatMessages message
  | message.sender == "user" =
      [LLM.userWithImages message.text (sessionMessageImageRefs message)]
  | otherwise =
      assistantMessages message.text (sessionMessageImageRefs message)

assistantMessages :: Text -> [Text] -> [LLM.ChatMessage]
assistantMessages text imageRefs =
  textMessages <> imageContextMessages
  where
    visibleImageRefs =
      filter (not . ReplyBody.isBase64ImageRef) imageRefs
    textMessages =
      [LLM.assistantText text | not (Text.null (Text.strip text))] <>
      [LLM.assistantText "Generated image." | Text.null (Text.strip text) && not (null imageRefs)]
    imageContextMessages =
      [ LLM.userWithImages "The previous assistant response generated this image. Use it as visual context for follow-up questions." visibleImageRefs
      | not (null visibleImageRefs)
      ]

sessionMessageImageRefs :: Session.SessionMessage -> [Text]
sessionMessageImageRefs message =
  ordNub $
    filter (not . Text.null . Text.strip) $
      message.imageUrls <> [attachment.url | attachment <- message.attachments, attachment.kind == "image"]

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
      referencedSenderLine referenced <> referencedTextLines referenced <> referencedImageLines referenced <> referencedFileLines referenced

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

referencedFileLines :: ReferencedMessage -> [Text]
referencedFileLines referenced =
  [ "被回复文件：" <> renderMessageFile file
  | file <- ordNubOn (.ref) referenced.files
  ]

referencedFiles :: Maybe ReferencedMessage -> [MessageFile]
referencedFiles =
  maybe [] (ordNubOn (.ref) . (.files))

promptWithCurrentFiles :: Text -> [MessageFile] -> Text
promptWithCurrentFiles prompt files =
  Text.intercalate "\n" $
    filter (not . Text.null)
      [ Text.strip prompt
      , Text.unlines ["附件：" <> renderMessageFile file | file <- ordNubOn (.ref) files]
      ]

renderMessageFile :: MessageFile -> Text
renderMessageFile file =
  file.name <> " (" <> file.ref <> ")"
