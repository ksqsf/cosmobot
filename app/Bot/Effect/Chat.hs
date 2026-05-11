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
  , ReplyStreamStyle (..)
  , ReplyStreamUpdate (..)
  , streamReplyTo
  , getMessageContent
  , getSenderMemberInfo
  , getMemberInfo
  , listGroupMembers
  , mentionUser
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
  , answerRef :: !(IORef.IORef Text)
  , pendingRef :: !(IORef.IORef Text)
  , lastEditRef :: !(IORef.IORef Int)
  , responseIdRef :: !(IORef.IORef (Maybe Integer))
  , lastChunkResponseIdRef :: !(IORef.IORef (Maybe Integer))
  }

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
  answer <- lift (liftIO (IORef.readIORef replyStream.answerRef))
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
  answerRef <- liftIO (IORef.newIORef "")
  pendingRef <- liftIO (IORef.newIORef "")
  lastEditRef <- liftIO (IORef.newIORef 0)
  responseIdRef <- liftIO (IORef.newIORef Nothing)
  lastChunkResponseIdRef <- liftIO (IORef.newIORef Nothing)
  pure ReplyStream
    { message = message
    , style = style
    , answerRef = answerRef
    , pendingRef = pendingRef
    , lastEditRef = lastEditRef
    , responseIdRef = responseIdRef
    , lastChunkResponseIdRef = lastChunkResponseIdRef
    }

pushReplyStreamChunk :: (Chat :> es, IOE :> es) => ReplyStream -> Text -> Eff es ReplyStreamUpdate
pushReplyStreamChunk stream chunk = do
  full <- liftIO $ IORef.atomicModifyIORef' stream.answerRef \old ->
    let new = old <> chunk in (new, new)
  sentResponseIds <- case stream.style of
    EditableReply editChunkChars ->
      pushEditableReplyChunk editChunkChars stream full $> []
    ChunkedReply messageLimit ->
      pushChunkedReplyChunk messageLimit stream chunk
  responseId <- liftIO (IORef.readIORef stream.responseIdRef)
  pure ReplyStreamUpdate{responseId, sentResponseIds, answer = full}

finishReplyStream :: (Chat :> es, IOE :> es) => ReplyStream -> Text -> Eff es (Maybe Integer, [Integer])
finishReplyStream stream answer = do
  liftIO $ IORef.writeIORef stream.answerRef answer
  sentResponseIds <- case stream.style of
    EditableReply _ -> do
      responseId <- ensureEditableReplyMessage stream answer
      void $ editMessage stream.message responseId (nonEmptyAnswer answer)
      pure []
    ChunkedReply _ ->
      flushChunkedReplyFinal stream answer
  responseId <- liftIO (IORef.readIORef stream.responseIdRef)
  pure (responseId, sentResponseIds)

pushEditableReplyChunk :: (Chat :> es, IOE :> es) => Int -> ReplyStream -> Text -> Eff es ()
pushEditableReplyChunk editChunkChars stream full = do
  responseId <- ensureEditableReplyMessage stream full
  lastEdit <- liftIO (IORef.readIORef stream.lastEditRef)
  when (Text.length full - lastEdit >= editChunkChars) do
    edited <- editMessage stream.message responseId full
    when edited $ liftIO (IORef.writeIORef stream.lastEditRef (Text.length full))

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
          pure messageId

initialEditableBody :: Text -> Text
initialEditableBody full
  | Text.null full = "..."
  | otherwise = full

pushChunkedReplyChunk :: (Chat :> es, IOE :> es) => Int -> ReplyStream -> Text -> Eff es [Integer]
pushChunkedReplyChunk messageLimit stream chunk = do
  pending <- liftIO $ IORef.atomicModifyIORef' stream.pendingRef \old ->
    let new = old <> chunk in (new, new)
  flushChunkedReplySegments messageLimit stream pending

flushChunkedReplySegments :: (Chat :> es, IOE :> es) => Int -> ReplyStream -> Text -> Eff es [Integer]
flushChunkedReplySegments messageLimit stream pending =
  if Text.length pending >= messageLimit
    then do
      let (segment, rest) = Text.splitAt messageLimit pending
      target <- chunkReplyTarget stream
      sent <- replyTo target segment
      registerChunkReplyMessage stream sent
      liftIO $ IORef.writeIORef stream.pendingRef rest
      later <- flushChunkedReplySegments messageLimit stream rest
      pure (maybeToList sent <> later)
    else
      pure []

flushChunkedReplyFinal :: (Chat :> es, IOE :> es) => ReplyStream -> Text -> Eff es [Integer]
flushChunkedReplyFinal stream answer = do
  pending <- liftIO (IORef.readIORef stream.pendingRef)
  responseId <- liftIO (IORef.readIORef stream.responseIdRef)
  case (Text.null pending, responseId) of
    (False, _) -> do
      target <- chunkReplyTarget stream
      sent <- replyTo target pending
      registerChunkReplyMessage stream sent
      liftIO $ IORef.writeIORef stream.pendingRef ""
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
replyTargetMessage IncomingMessage{platform, kind, chatId, senderId, senderUsername, replyToMessageId, mentions, mentionUsernames, imageUrls, text, raw} responseId =
  IncomingMessage
    { platform = platform
    , kind = kind
    , chatId = chatId
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

-- | List group members when the platform exposes such an API.
listGroupMembers :: Chat :> es => IncomingMessage -> Eff es (Maybe Aeson.Value)
listGroupMembers message =
  send (ListGroupMembers message)

-- | Send a reply that mentions a platform user id.
mentionUser :: Chat :> es => IncomingMessage -> Integer -> Text -> Eff es (Maybe Integer)
mentionUser message userId body =
  send (MentionUser message userId body)

-- | Interpret chat operations by delegating each operation to platform code.
runChatWith
  :: (IncomingMessage -> Text -> Eff es (Maybe Integer))
  -> (IncomingMessage -> Integer -> Text -> Eff es Bool)
  -> (IncomingMessage -> Eff es ReplyStreamStyle)
  -> (IncomingMessage -> Integer -> Eff es (Maybe ReferencedMessage))
  -> (IncomingMessage -> Eff es (Maybe Aeson.Value))
  -> (IncomingMessage -> Integer -> Eff es (Maybe Aeson.Value))
  -> (IncomingMessage -> Eff es (Maybe Aeson.Value))
  -> (IncomingMessage -> Integer -> Text -> Eff es (Maybe Integer))
  -> Eff (Chat : es) a
  -> Eff es a
runChatWith reply edit streamStyle fetch fetchSenderMember fetchMember listMembers mention = interpret $ \_ -> \case
  ReplyTo message body ->
    reply message body
  EditMessage message messageId body ->
    edit message messageId body
  ReplyStreamStyle message ->
    streamStyle message
  GetMessageContent message messageId ->
    fetch message messageId
  GetSenderMemberInfo message ->
    fetchSenderMember message
  GetMemberInfo message userId ->
    fetchMember message userId
  ListGroupMembers message ->
    listMembers message
  MentionUser message userId body ->
    mention message userId body
