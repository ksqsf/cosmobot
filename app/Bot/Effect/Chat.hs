{-|
Module      : Bot.Effect.Chat
Description : Unified chat platform effect
Stability   : experimental
-}

module Bot.Effect.Chat
  ( -- * Effect
    Chat
  , replyTo
  , editMessage
  , deleteMessage
  , ReplyStreamStyle (..)
  , ReplyStreamUpdate (..)
  , streamReplyTo
  , getMessageContent
  , getSenderMemberInfo
  , getMemberInfo
  , getUserAvatar
  , listGroupMembers
  , mentionUser
  , ChatHandlers (..)
  , runChatWith

    -- * Reply rendering
  , imageDirective
  , renderReplyBody
  , replyImageUrls
  )
where

import Bot.Core.Message
import Bot.Prelude
import Bot.Core.ReplyBody
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Streaming.Prelude as S

-- | Platform-independent chat operations used by handlers and tools.
data Chat :: Effect where
  ReplyTo
    :: IncomingMessage
    -> Text
    -> Chat m (Maybe Integer)
  EditMessage
    :: IncomingMessage
    -> Integer
    -> Text
    -> Chat m Bool
  DeleteMessage
    :: IncomingMessage
    -> Integer
    -> Chat m Bool
  ReplyStreamStyle
    :: IncomingMessage
    -> Chat m ReplyStreamStyle
  GetMessageContent
    :: IncomingMessage
    -> Integer
    -> Chat m (Maybe ReferencedMessage)
  GetSenderMemberInfo
    :: IncomingMessage
    -> Chat m (Maybe Aeson.Value)
  GetMemberInfo
    :: IncomingMessage
    -> Integer
    -> Chat m (Maybe Aeson.Value)
  GetUserAvatar
    :: IncomingMessage
    -> Text
    -> Chat m (Maybe Aeson.Value)
  ListGroupMembers
    :: IncomingMessage
    -> Chat m (Maybe Aeson.Value)
  MentionUser
    :: IncomingMessage
    -> Integer
    -> Text
    -> Chat m (Maybe Integer)

type instance DispatchOf Chat = Dynamic

-- | Reply to the chat containing the incoming message.
replyTo :: Chat :> es => IncomingMessage -> Text -> Eff es (Maybe Integer)
replyTo message body =
  send (ReplyTo message body)

-- | Edit a previously sent message when the platform supports it.
editMessage :: Chat :> es => IncomingMessage -> Integer -> Text -> Eff es Bool
editMessage message messageId body =
  send (EditMessage message messageId body)

-- | Delete or recall a previously sent message when the platform supports it.
deleteMessage :: Chat :> es => IncomingMessage -> Integer -> Eff es Bool
deleteMessage message messageId =
  send (DeleteMessage message messageId)

data ReplyStreamStyle
  = EditableReply !Int
  | ChunkedReply !Int

data ReplyStreamUpdate = ReplyStreamUpdate
  { responseId :: !(Maybe Integer)
  , sentResponseIds :: ![Integer]
  , answer :: !Text
  }
  deriving (Show)

data ReplyStream = ReplyStream
  { message :: !IncomingMessage
  , style :: !ReplyStreamStyle
  , answerRef :: !(IORef.IORef TextAccumulator)
  , pendingRef :: !(IORef.IORef TextAccumulator)
  , lastEditRef :: !(IORef.IORef Int)
  , lastEditedTextRef :: !(IORef.IORef Text)
  , responseIdRef :: !(IORef.IORef (Maybe Integer))
  , lastChunkResponseIdRef :: !(IORef.IORef (Maybe Integer))
  }

data TextAccumulator = TextAccumulator
  { builder :: !TextBuilder.Builder
  , lengthChars :: !Int
  }

emptyTextAccumulator :: TextAccumulator
emptyTextAccumulator = TextAccumulator mempty 0

singletonTextAccumulator :: Text -> TextAccumulator
singletonTextAccumulator text =
  TextAccumulator (TextBuilder.fromText text) (Text.length text)

appendTextAccumulator :: Text -> TextAccumulator -> TextAccumulator
appendTextAccumulator text accumulator =
  TextAccumulator
    { builder = accumulator.builder <> TextBuilder.fromText text
    , lengthChars = accumulator.lengthChars + Text.length text
    }

textAccumulatorText :: TextAccumulator -> Text
textAccumulatorText =
  LazyText.toStrict . TextBuilder.toLazyText . (.builder)

streamReplyTo
  :: (Chat :> es, IOE :> es)
  => IncomingMessage
  -> (r -> Text)
  -> Stream (Of Text) (Eff es) r
  -> Stream (Of ReplyStreamUpdate) (Eff es) (Maybe Integer, r)
streamReplyTo message finalAnswer input = do
  replyStream <- lift (newReplyStream message)
  result <- go replyStream input
  (responseId, sentResponseIds) <- lift (finishReplyStream replyStream (finalAnswer result))
  answer <- lift (textAccumulatorText <$> liftIO (IORef.readIORef replyStream.answerRef))
  S.yield ReplyStreamUpdate{responseId, sentResponseIds, answer}
  pure (responseId, result)
  where
    go replyStream stream = do
      next <- lift (S.next stream)
      case next of
        Left result ->
          pure result
        Right (chunk, rest) -> do
          update <- lift (pushReplyStreamChunk replyStream chunk)
          S.yield update
          go replyStream rest

newReplyStream :: (Chat :> es, IOE :> es) => IncomingMessage -> Eff es ReplyStream
newReplyStream message = do
  style <- send (ReplyStreamStyle message)
  answerRef <- liftIO (IORef.newIORef emptyTextAccumulator)
  pendingRef <- liftIO (IORef.newIORef emptyTextAccumulator)
  lastEditRef <- liftIO (IORef.newIORef 0)
  lastEditedTextRef <- liftIO (IORef.newIORef "")
  responseIdRef <- liftIO (IORef.newIORef Nothing)
  lastChunkResponseIdRef <- liftIO (IORef.newIORef Nothing)
  pure ReplyStream
    { message = message
    , style = style
    , answerRef = answerRef
    , pendingRef = pendingRef
    , lastEditRef = lastEditRef
    , lastEditedTextRef = lastEditedTextRef
    , responseIdRef = responseIdRef
    , lastChunkResponseIdRef = lastChunkResponseIdRef
    }

pushReplyStreamChunk :: (Chat :> es, IOE :> es) => ReplyStream -> Text -> Eff es ReplyStreamUpdate
pushReplyStreamChunk stream chunk = do
  fullAccumulator <- liftIO $ IORef.atomicModifyIORef' stream.answerRef \old ->
    let new = appendTextAccumulator chunk old in (new, new)
  let full = textAccumulatorText fullAccumulator
  sentResponseIds <- case stream.style of
    EditableReply editChunkChars ->
      pushEditableReplyChunk editChunkChars stream fullAccumulator full $> []
    ChunkedReply messageLimit ->
      pushChunkedReplyChunk messageLimit stream chunk
  responseId <- liftIO (IORef.readIORef stream.responseIdRef)
  pure ReplyStreamUpdate{responseId, sentResponseIds, answer = full}

finishReplyStream :: (Chat :> es, IOE :> es) => ReplyStream -> Text -> Eff es (Maybe Integer, [Integer])
finishReplyStream stream answer = do
  liftIO $ IORef.writeIORef stream.answerRef (singletonTextAccumulator answer)
  sentResponseIds <- case stream.style of
    EditableReply _ -> do
      responseId <- ensureEditableReplyMessage stream answer
      editEditableReply stream responseId (nonEmptyAnswer answer)
      pure []
    ChunkedReply _ ->
      flushChunkedReplyFinal stream answer
  responseId <- liftIO (IORef.readIORef stream.responseIdRef)
  pure (responseId, sentResponseIds)

pushEditableReplyChunk :: (Chat :> es, IOE :> es) => Int -> ReplyStream -> TextAccumulator -> Text -> Eff es ()
pushEditableReplyChunk editChunkChars stream fullAccumulator full = do
  responseId <- ensureEditableReplyMessage stream full
  lastEdit <- liftIO (IORef.readIORef stream.lastEditRef)
  when (fullAccumulator.lengthChars - lastEdit >= editChunkChars) do
    edited <- editMessage stream.message responseId full
    when edited do
      liftIO $ IORef.writeIORef stream.lastEditRef fullAccumulator.lengthChars
      liftIO $ IORef.writeIORef stream.lastEditedTextRef full

editEditableReply :: (Chat :> es, IOE :> es) => ReplyStream -> Integer -> Text -> Eff es ()
editEditableReply stream responseId body = do
  lastEditedText <- liftIO (IORef.readIORef stream.lastEditedTextRef)
  unless (body == lastEditedText) do
    edited <- editMessage stream.message responseId body
    when edited do
      liftIO $ IORef.writeIORef stream.lastEditedTextRef body

ensureEditableReplyMessage :: (Chat :> es, IOE :> es) => ReplyStream -> Text -> Eff es Integer
ensureEditableReplyMessage stream full = do
  responseId <- liftIO (IORef.readIORef stream.responseIdRef)
  case responseId of
    Just messageId ->
      pure messageId
    Nothing -> do
      sent <- replyTo stream.message (initialEditableBody full)
      case sent of
        Nothing ->
          pure 0
        Just messageId -> do
          liftIO $ IORef.writeIORef stream.responseIdRef sent
          liftIO $ IORef.writeIORef stream.lastEditRef (Text.length (initialEditableBody full))
          liftIO $ IORef.writeIORef stream.lastEditedTextRef (initialEditableBody full)
          pure messageId

initialEditableBody :: Text -> Text
initialEditableBody full
  | Text.null full = "..."
  | otherwise = full

pushChunkedReplyChunk :: (Chat :> es, IOE :> es) => Int -> ReplyStream -> Text -> Eff es [Integer]
pushChunkedReplyChunk messageLimit stream chunk = do
  pending <- liftIO $ IORef.atomicModifyIORef' stream.pendingRef \old ->
    let new = appendTextAccumulator chunk old in (new, new)
  flushChunkedReplySegments messageLimit stream pending

flushChunkedReplySegments :: (Chat :> es, IOE :> es) => Int -> ReplyStream -> TextAccumulator -> Eff es [Integer]
flushChunkedReplySegments messageLimit stream pending =
  if pending.lengthChars >= messageLimit
    then do
      let pendingText = textAccumulatorText pending
          (segment, rest) = Text.splitAt messageLimit pendingText
          restAccumulator = singletonTextAccumulator rest
      target <- chunkReplyTarget stream
      sent <- replyTo target segment
      registerChunkReplyMessage stream sent
      liftIO $ IORef.writeIORef stream.pendingRef restAccumulator
      later <- flushChunkedReplySegments messageLimit stream restAccumulator
      pure (maybeToList sent <> later)
    else
      pure []

flushChunkedReplyFinal :: (Chat :> es, IOE :> es) => ReplyStream -> Text -> Eff es [Integer]
flushChunkedReplyFinal stream answer = do
  pending <- liftIO (IORef.readIORef stream.pendingRef)
  responseId <- liftIO (IORef.readIORef stream.responseIdRef)
  case (pending.lengthChars == 0, responseId) of
    (False, _) -> do
      target <- chunkReplyTarget stream
      sent <- replyTo target (textAccumulatorText pending)
      registerChunkReplyMessage stream sent
      liftIO $ IORef.writeIORef stream.pendingRef emptyTextAccumulator
      pure (maybeToList sent)
    (True, Nothing) -> do
      sent <- replyTo stream.message (nonEmptyAnswer answer)
      registerChunkReplyMessage stream sent
      pure (maybeToList sent)
    (True, Just _) ->
      pure []

registerFirstReplyMessage :: IOE :> es => ReplyStream -> Maybe Integer -> Eff es ()
registerFirstReplyMessage stream sent = do
  existing <- liftIO (IORef.readIORef stream.responseIdRef)
  when (isNothing existing) $
    liftIO $ IORef.writeIORef stream.responseIdRef sent

registerChunkReplyMessage :: IOE :> es => ReplyStream -> Maybe Integer -> Eff es ()
registerChunkReplyMessage stream sent = do
  registerFirstReplyMessage stream sent
  traverse_ (liftIO . IORef.writeIORef stream.lastChunkResponseIdRef . Just) sent

chunkReplyTarget :: IOE :> es => ReplyStream -> Eff es IncomingMessage
chunkReplyTarget stream = do
  previous <- liftIO (IORef.readIORef stream.lastChunkResponseIdRef)
  pure (maybe stream.message (replyTargetMessage stream.message) previous)

replyTargetMessage :: IncomingMessage -> Integer -> IncomingMessage
replyTargetMessage IncomingMessage{platform, kind, chatId, chatAliases, digest, senderId, senderUsername, replyToMessageId, mentions, mentionUsernames, imageUrls, text, raw} responseId =
  IncomingMessage
    { platform = platform
    , kind = kind
    , chatId = chatId
    , chatAliases = chatAliases
    , digest = digest
    , senderId = senderId
    , senderUsername = senderUsername
    , messageId = Just responseId
    , replyToMessageId = replyToMessageId
    , mentions = mentions
    , mentionUsernames = mentionUsernames
    , imageUrls = imageUrls
    , text = text
    , raw = raw
    }

nonEmptyAnswer :: Text -> Text
nonEmptyAnswer answer
  | Text.null answer = "LLM response was empty."
  | otherwise = answer

-- | Fetch content for a referenced platform message id.
getMessageContent :: Chat :> es => IncomingMessage -> Integer -> Eff es (Maybe ReferencedMessage)
getMessageContent message messageId =
  send (GetMessageContent message messageId)

-- | Fetch member info for the sender of the current message.
getSenderMemberInfo :: Chat :> es => IncomingMessage -> Eff es (Maybe Aeson.Value)
getSenderMemberInfo message =
  send (GetSenderMemberInfo message)

-- | Fetch member info for a user id in the current chat.
getMemberInfo :: Chat :> es => IncomingMessage -> Integer -> Eff es (Maybe Aeson.Value)
getMemberInfo message userId =
  send (GetMemberInfo message userId)

-- | Fetch avatar information for a platform user id.
getUserAvatar :: Chat :> es => IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
getUserAvatar message userId =
  send (GetUserAvatar message userId)

-- | List group members when the platform exposes such an API.
listGroupMembers :: Chat :> es => IncomingMessage -> Eff es (Maybe Aeson.Value)
listGroupMembers message =
  send (ListGroupMembers message)

-- | Send a reply that mentions a platform user id.
mentionUser :: Chat :> es => IncomingMessage -> Integer -> Text -> Eff es (Maybe Integer)
mentionUser message userId body =
  send (MentionUser message userId body)

-- | Interpret chat operations by delegating each operation to platform code.
data ChatHandlers es = ChatHandlers
  { handleReplyTo :: IncomingMessage -> Text -> Eff es (Maybe Integer)
  , handleEditMessage :: IncomingMessage -> Integer -> Text -> Eff es Bool
  , handleDeleteMessage :: IncomingMessage -> Integer -> Eff es Bool
  , handleReplyStreamStyle :: IncomingMessage -> Eff es ReplyStreamStyle
  , handleGetMessageContent :: IncomingMessage -> Integer -> Eff es (Maybe ReferencedMessage)
  , handleGetSenderMemberInfo :: IncomingMessage -> Eff es (Maybe Aeson.Value)
  , handleGetMemberInfo :: IncomingMessage -> Integer -> Eff es (Maybe Aeson.Value)
  , handleGetUserAvatar :: IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
  , handleListGroupMembers :: IncomingMessage -> Eff es (Maybe Aeson.Value)
  , handleMentionUser :: IncomingMessage -> Integer -> Text -> Eff es (Maybe Integer)
  }

runChatWith
  :: ChatHandlers es
  -> Eff (Chat : es) a
  -> Eff es a
runChatWith handlers = interpret $ \_ -> \case
  ReplyTo message body ->
    handlers.handleReplyTo message body
  EditMessage message messageId body ->
    handlers.handleEditMessage message messageId body
  DeleteMessage message messageId ->
    handlers.handleDeleteMessage message messageId
  ReplyStreamStyle message ->
    handlers.handleReplyStreamStyle message
  GetMessageContent message messageId ->
    handlers.handleGetMessageContent message messageId
  GetSenderMemberInfo message ->
    handlers.handleGetSenderMemberInfo message
  GetMemberInfo message userId ->
    handlers.handleGetMemberInfo message userId
  GetUserAvatar message userId ->
    handlers.handleGetUserAvatar message userId
  ListGroupMembers message ->
    handlers.handleListGroupMembers message
  MentionUser message userId body ->
    handlers.handleMentionUser message userId body
