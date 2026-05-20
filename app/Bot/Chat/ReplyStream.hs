{-|
Module      : Bot.Chat.ReplyStream
Description : Callback-driven reply streaming state machine
Stability   : experimental
-}

module Bot.Chat.ReplyStream
  ( ReplyStreamCallbacks (..)
  , streamReplyTo
  , streamReplySegmentsTo
  )
where

import Bot.Chat.Types
import Bot.Core.Message
import Bot.Core.ReplyBody
import Bot.Prelude
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Effectful.Prim.IORef as IORef
import qualified Streaming.Prelude as S

data ReplyStreamCallbacks es = ReplyStreamCallbacks
  { replyStreamStyleFor :: IncomingMessage -> Eff es ReplyStreamStyle
  , sendReplyTo :: IncomingMessage -> Text -> Eff es (Maybe MessageId)
  , editReplyMessage :: IncomingMessage -> MessageId -> Text -> Eff es Bool
  }

data ReplyStream es = ReplyStream
  { message :: !IncomingMessage
  , style :: !ReplyStreamStyle
  , callbacks :: !(ReplyStreamCallbacks es)
  , answerRef :: !(IORef.IORef TextAccumulator)
  , pendingRef :: !(IORef.IORef TextAccumulator)
  , lastEditRef :: !(IORef.IORef Int)
  , lastEditedTextRef :: !(IORef.IORef Text)
  , responseIdRef :: !(IORef.IORef (Maybe MessageId))
  , lastChunkResponseIdRef :: !(IORef.IORef (Maybe MessageId))
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
  :: Prim :> es
  => ReplyStreamCallbacks es
  -> IncomingMessage
  -> (r -> Text)
  -> Stream (Of Text) (Eff es) r
  -> Stream (Of ReplyStreamUpdate) (Eff es) (Maybe MessageId, r)
streamReplyTo callbacks message finalAnswer input = do
  replyStream <- lift (newReplyStream callbacks message)
  result <- go replyStream input
  (responseId, sentResponseIds) <- lift (finishReplyStream replyStream (finalAnswer result))
  answer <- lift (textAccumulatorText <$> IORef.readIORef replyStream.answerRef)
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

streamReplySegmentsTo
  :: Prim :> es
  => ReplyStreamCallbacks es
  -> IncomingMessage
  -> (r -> Text)
  -> Stream (Of ReplySegmentEvent) (Eff es) r
  -> Stream (Of ReplyStreamUpdate) (Eff es) (Maybe MessageId, r)
streamReplySegmentsTo callbacks message finalAnswer input =
  go Nothing False input
  where
    go current sawText stream = do
      next <- lift (S.next stream)
      case next of
        Left result -> do
          responseId <- finishAtEnd current sawText result
          pure (responseId, result)
        Right (event, rest) ->
          case event of
            ReplySegmentDelta chunk -> do
              let openedSegment = isNothing current
              replyStream <- lift (maybe (newReplyStream callbacks message) pure current)
              pushed <- lift (pushReplyStreamChunk replyStream chunk)
              let update =
                    if openedSegment
                      then pushed{sentResponseIds = maybeToList pushed.responseId <> pushed.sentResponseIds}
                      else pushed
              S.yield update
              go (Just replyStream) True rest
            ReplySegmentBoundary -> do
              responseId <- closeSegment current
              go Nothing (sawText || isJust responseId) rest
            ReplySegmentMessage{} -> do
              responseId <- closeSegment current
              go Nothing (sawText || isJust responseId) rest

    finishAtEnd current sawText result =
      case current of
        Just replyStream ->
          closeSegmentUpdate replyStream
        Nothing
          | sawText ->
              pure Nothing
          | otherwise -> do
              let answer = finalAnswer result
              if Text.null (Text.strip answer)
                then pure Nothing
                else do
                  replyStream <- lift (newReplyStream callbacks message)
                  closeSegmentUpdateWith replyStream answer

    closeSegment current =
      case current of
        Nothing ->
          pure Nothing
        Just replyStream ->
          closeSegmentUpdate replyStream

    closeSegmentUpdate replyStream = do
      answer <- lift (textAccumulatorText <$> IORef.readIORef replyStream.answerRef)
      closeSegmentUpdateWith replyStream answer

    closeSegmentUpdateWith replyStream answer = do
      (responseId, sentResponseIds) <- lift (finishReplyStream replyStream answer)
      finalText <- lift (textAccumulatorText <$> IORef.readIORef replyStream.answerRef)
      let update = ReplyStreamUpdate{responseId, sentResponseIds, answer = finalText}
      S.yield update
      pure responseId

newReplyStream :: Prim :> es => ReplyStreamCallbacks es -> IncomingMessage -> Eff es (ReplyStream es)
newReplyStream callbacks message = do
  style <- callbacks.replyStreamStyleFor message
  answerRef <- IORef.newIORef emptyTextAccumulator
  pendingRef <- IORef.newIORef emptyTextAccumulator
  lastEditRef <- IORef.newIORef 0
  lastEditedTextRef <- IORef.newIORef ""
  responseIdRef <- IORef.newIORef Nothing
  lastChunkResponseIdRef <- IORef.newIORef Nothing
  pure ReplyStream
    { message = message
    , style = style
    , callbacks = callbacks
    , answerRef = answerRef
    , pendingRef = pendingRef
    , lastEditRef = lastEditRef
    , lastEditedTextRef = lastEditedTextRef
    , responseIdRef = responseIdRef
    , lastChunkResponseIdRef = lastChunkResponseIdRef
    }

pushReplyStreamChunk :: Prim :> es => ReplyStream es -> Text -> Eff es ReplyStreamUpdate
pushReplyStreamChunk stream chunk = do
  fullAccumulator <- IORef.atomicModifyIORef' stream.answerRef \old ->
    let new = appendTextAccumulator chunk old in (new, new)
  let full = textAccumulatorText fullAccumulator
  sentResponseIds <- case stream.style of
    EditableReply editChunkChars messageLimit ->
      pushEditableReplyChunk editChunkChars messageLimit stream fullAccumulator full
    ChunkedReply messageLimit ->
      pushChunkedReplyChunk messageLimit stream chunk
  responseId <- IORef.readIORef stream.responseIdRef
  pure ReplyStreamUpdate{responseId, sentResponseIds, answer = full}

finishReplyStream :: Prim :> es => ReplyStream es -> Text -> Eff es (Maybe MessageId, [MessageId])
finishReplyStream stream answer = do
  IORef.writeIORef stream.answerRef (singletonTextAccumulator answer)
  sentResponseIds <- case stream.style of
    EditableReply _ messageLimit ->
      finishEditableReplyStream messageLimit stream answer
    ChunkedReply _ ->
      flushChunkedReplyFinal stream answer
  responseId <- IORef.readIORef stream.responseIdRef
  pure (responseId, sentResponseIds)

pushEditableReplyChunk :: Prim :> es => Int -> Int -> ReplyStream es -> TextAccumulator -> Text -> Eff es [MessageId]
pushEditableReplyChunk editChunkChars messageLimit stream fullAccumulator full = do
  maybeResponseId <- ensureEditableReplyMessage stream (editableReplyBody messageLimit full)
  for_ maybeResponseId \responseId -> do
    lastEdit <- IORef.readIORef stream.lastEditRef
    when (fullAccumulator.lengthChars - lastEdit >= editChunkChars) do
      let body = editableReplyBody messageLimit full
      void (editEditableReply stream responseId body)
      IORef.writeIORef stream.lastEditRef fullAccumulator.lengthChars
  pure []

finishEditableReplyStream :: Prim :> es => Int -> ReplyStream es -> Text -> Eff es [MessageId]
finishEditableReplyStream messageLimit stream answer = do
  let finalAnswer = nonEmptyAnswer answer
      (editableBody, overflow) = Text.splitAt messageLimit finalAnswer
  maybeResponseId <- ensureEditableReplyMessage stream editableBody
  traverse_ (\responseId -> editEditableReply stream responseId editableBody) maybeResponseId
  if Text.null overflow
    then pure []
    else do
      IORef.writeIORef stream.pendingRef (singletonTextAccumulator overflow)
      flushChunkedReplyFinalFrom (maybe (chunkReplyTarget stream) (editableChunkReplyTarget stream) maybeResponseId) stream finalAnswer

editableReplyBody :: Int -> Text -> Text
editableReplyBody messageLimit =
  Text.take messageLimit . initialEditableBody

editEditableReply :: Prim :> es => ReplyStream es -> MessageId -> Text -> Eff es Bool
editEditableReply stream responseId body = do
  lastEditedText <- IORef.readIORef stream.lastEditedTextRef
  if body == lastEditedText
    then pure False
    else do
      edited <- stream.callbacks.editReplyMessage stream.message responseId body
      when edited do
        IORef.writeIORef stream.lastEditedTextRef body
      pure edited

ensureEditableReplyMessage :: Prim :> es => ReplyStream es -> Text -> Eff es (Maybe MessageId)
ensureEditableReplyMessage stream full = do
  responseId <- IORef.readIORef stream.responseIdRef
  case responseId of
    Just messageId ->
      pure (Just messageId)
    Nothing -> do
      sent <- stream.callbacks.sendReplyTo stream.message (initialEditableBody full)
      for_ sent \_ -> do
        IORef.writeIORef stream.responseIdRef sent
        IORef.writeIORef stream.lastEditRef (Text.length (initialEditableBody full))
        IORef.writeIORef stream.lastEditedTextRef (initialEditableBody full)
      pure sent

initialEditableBody :: Text -> Text
initialEditableBody full
  | Text.null full = "..."
  | otherwise = full

pushChunkedReplyChunk :: Prim :> es => Int -> ReplyStream es -> Text -> Eff es [MessageId]
pushChunkedReplyChunk messageLimit stream chunk = do
  pending <- IORef.atomicModifyIORef' stream.pendingRef \old ->
    let new = appendTextAccumulator chunk old in (new, new)
  flushChunkedReplySegments messageLimit stream pending

flushChunkedReplySegments :: Prim :> es => Int -> ReplyStream es -> TextAccumulator -> Eff es [MessageId]
flushChunkedReplySegments messageLimit stream pending =
  if pending.lengthChars >= messageLimit
    then do
      let pendingText = textAccumulatorText pending
          (segment, rest) = Text.splitAt messageLimit pendingText
          restAccumulator = singletonTextAccumulator rest
      target <- chunkReplyTarget stream
      sent <- stream.callbacks.sendReplyTo target segment
      registerChunkReplyMessage stream sent
      IORef.writeIORef stream.pendingRef restAccumulator
      later <- flushChunkedReplySegments messageLimit stream restAccumulator
      pure (maybeToList sent <> later)
    else
      pure []

flushChunkedReplyFinal :: Prim :> es => ReplyStream es -> Text -> Eff es [MessageId]
flushChunkedReplyFinal stream answer =
  flushChunkedReplyFinalFrom (chunkReplyTarget stream) stream answer

flushChunkedReplyFinalFrom :: Prim :> es => Eff es IncomingMessage -> ReplyStream es -> Text -> Eff es [MessageId]
flushChunkedReplyFinalFrom targetAction stream answer = do
  pending <- IORef.readIORef stream.pendingRef
  responseId <- IORef.readIORef stream.responseIdRef
  case (pending.lengthChars == 0, responseId) of
    (False, _) -> do
      sent <- flushChunkedReplyText targetAction stream pending
      IORef.writeIORef stream.pendingRef emptyTextAccumulator
      pure sent
    (True, Nothing) -> do
      sent <- stream.callbacks.sendReplyTo stream.message (nonEmptyAnswer answer)
      registerChunkReplyMessage stream sent
      pure (maybeToList sent)
    (True, Just _) ->
      pure []

flushChunkedReplyText :: Prim :> es => Eff es IncomingMessage -> ReplyStream es -> TextAccumulator -> Eff es [MessageId]
flushChunkedReplyText targetAction stream pending = do
  case stream.style of
    EditableReply _ messageLimit ->
      flushWithLimit messageLimit
    ChunkedReply messageLimit ->
      flushWithLimit messageLimit
  where
    flushWithLimit messageLimit =
      if pending.lengthChars > messageLimit
        then do
          let pendingText = textAccumulatorText pending
              (segment, rest) = Text.splitAt messageLimit pendingText
          target <- targetAction
          sent <- stream.callbacks.sendReplyTo target segment
          registerChunkReplyMessage stream sent
          later <- flushChunkedReplyText (chunkReplyTarget stream) stream (singletonTextAccumulator rest)
          pure (maybeToList sent <> later)
        else do
          target <- targetAction
          sent <- stream.callbacks.sendReplyTo target (textAccumulatorText pending)
          registerChunkReplyMessage stream sent
          pure (maybeToList sent)

registerFirstReplyMessage :: Prim :> es => ReplyStream es -> Maybe MessageId -> Eff es ()
registerFirstReplyMessage stream sent = do
  existing <- IORef.readIORef stream.responseIdRef
  when (isNothing existing) $
    IORef.writeIORef stream.responseIdRef sent

registerChunkReplyMessage :: Prim :> es => ReplyStream es -> Maybe MessageId -> Eff es ()
registerChunkReplyMessage stream sent = do
  registerFirstReplyMessage stream sent
  traverse_ (IORef.writeIORef stream.lastChunkResponseIdRef . Just) sent

chunkReplyTarget :: Prim :> es => ReplyStream es -> Eff es IncomingMessage
chunkReplyTarget stream = do
  previous <- IORef.readIORef stream.lastChunkResponseIdRef
  pure (maybe stream.message (replyTargetMessage stream.message) previous)

editableChunkReplyTarget :: Prim :> es => ReplyStream es -> MessageId -> Eff es IncomingMessage
editableChunkReplyTarget stream responseId = do
  previous <- IORef.readIORef stream.lastChunkResponseIdRef
  pure (replyTargetMessage stream.message (fromMaybe responseId previous))

replyTargetMessage :: IncomingMessage -> MessageId -> IncomingMessage
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
