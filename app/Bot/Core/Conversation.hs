{-|
Module      : Bot.Core.Conversation
Description : In-memory conversation graph
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Core.Conversation
  ( -- * Store
    ConversationStore
  , ActiveConversationHandle
  , newConversationStore
  , lookupConversation
  , rememberConversation
  , rememberConversationFrom
  , rememberActiveConversation
  , addActiveConversationMessage
  , updateActiveConversation
  , finishActiveConversation
  , finishActiveConversationCurrent
  , haltConversation

    -- * Conversation values
  , Conversation (..)
  , startWithUser
  , startWithUserContext
  , startWithSystemAndUser
  , startWithSystemAndUserContext
  , appendUser
  , appendUserContext
  , appendAssistant
  )
where

import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Storage.SQLite as Storage
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text as Text
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent (ThreadId, killThread)

-- | Mutable conversation index, optionally mirrored into SQLite.
data ConversationStore = ConversationStore
  { unConversationStore :: IORef.IORef ConversationState
  , activeConversationStore :: IORef.IORef (Map Integer ActiveConversation)
  , sqliteStore :: !(Maybe Storage.SQLiteStore)
  }

-- | Ordered chat history sent to the LLM.
newtype Conversation = Conversation
  { messages :: [LLM.ChatMessage]
  }
  deriving (Show, Generic)

data ConversationState = ConversationState
  { nextConversationId :: !Integer
  , conversations :: !(Map Integer ConversationNode)
  }

data ConversationNode = ConversationNode
  { conversationId :: !Integer
  , parentMessageId :: !(Maybe Integer)
  , conversation :: !Conversation
  }

data ActiveConversation = ActiveConversation
  { activeMessageId :: !Integer
  , activeParentMessageId :: !(Maybe Integer)
  , activeMessageIds :: !(IORef.IORef [Integer])
  , activeCurrent :: !(IORef.IORef Conversation)
  , activeDone :: !(MVar.MVar Conversation)
  , activeThreadId :: !ThreadId
  }

newtype ActiveConversationHandle = ActiveConversationHandle ActiveConversation

instance Aeson.ToJSON Conversation where
  toJSON Conversation{messages} =
    Aeson.object
      [ "messages" Aeson..= messages
      ]

instance Aeson.FromJSON Conversation where
  parseJSON = Aeson.withObject "Conversation" $ \o ->
    Conversation <$> o Aeson..: "messages"

-- | Create a store and preload persisted conversations when storage exists.
newConversationStore :: Maybe Storage.SQLiteStore -> IO ConversationStore
newConversationStore sqliteStore = do
  conversationState <- maybe emptyConversationState loadStoredConversations sqliteStore
  ref <- IORef.newIORef conversationState
  activeRef <- IORef.newIORef Map.empty
  pure ConversationStore{unConversationStore = ref, activeConversationStore = activeRef, sqliteStore}

emptyConversationState :: IO ConversationState
emptyConversationState =
  pure ConversationState
    { nextConversationId = 1
    , conversations = Map.empty
    }

loadStoredConversations :: Storage.SQLiteStore -> IO ConversationState
loadStoredConversations store = do
  rows <- Storage.loadConversationRows store
  let decodedRows =
        mapMaybe decodeStoredConversation rows
      assignedRows =
        assignMissingConversationIds decodedRows
      conversations =
        foldl' insertStoredConversation Map.empty assignedRows
      nextConversationId =
        1 + foldl' max 0 (map (.storedConversationId) assignedRows)
  pure ConversationState{nextConversationId, conversations}

insertStoredConversation :: Map Integer ConversationNode -> StoredConversation -> Map Integer ConversationNode
insertStoredConversation acc StoredConversation{storedMessageId, storedConversationId, storedParentMessageId, storedPayload} =
  Map.insert storedMessageId ConversationNode
    { conversationId = storedConversationId
    , parentMessageId = storedParentMessageId
    , conversation = storedConversationFromPayload acc storedParentMessageId storedPayload
    }
    acc

data StoredConversation = StoredConversation
  { storedMessageId :: !Integer
  , storedConversationId :: !Integer
  , storedParentMessageId :: !(Maybe Integer)
  , storedPayload :: !StoredConversationPayload
  }

data StoredConversationPayload
  = StoredConversationSnapshot !Conversation
  | StoredConversationMessages ![LLM.ChatMessage]

decodeStoredConversation :: Storage.ConversationRow -> Maybe StoredConversation
decodeStoredConversation row = do
  payload <- decodeConversationPayload row
  let conversationId = fromMaybe 0 row.conversationId
  pure StoredConversation
    { storedMessageId = row.messageId
    , storedConversationId = conversationId
    , storedParentMessageId = row.parentMessageId
    , storedPayload = payload
    }

decodeConversationPayload :: Storage.ConversationRow -> Maybe StoredConversationPayload
decodeConversationPayload row =
  case row.payloadKind of
    Storage.ConversationPayloadMessages ->
      StoredConversationMessages <$> decodeMessages row.payloadJson
    Storage.ConversationPayloadSnapshot ->
      StoredConversationSnapshot <$> decodeConversation row.payloadJson

storedConversationFromPayload :: Map Integer ConversationNode -> Maybe Integer -> StoredConversationPayload -> Conversation
storedConversationFromPayload _ _ (StoredConversationSnapshot conversation) =
  conversation
storedConversationFromPayload acc parentMessageId (StoredConversationMessages messages) =
  case parentMessageId >>= (`Map.lookup` acc) of
    Nothing ->
      Conversation messages
    Just parent ->
      Conversation (parent.conversation.messages <> messages)

assignMissingConversationIds :: [StoredConversation] -> [StoredConversation]
assignMissingConversationIds rows =
  zipWith assign rows [1..]
  where
    maxExistingId =
      foldl' max 0 [ cid | StoredConversation{storedConversationId = cid} <- rows, cid > 0 ]
    assign row fallbackId
      | row.storedConversationId > 0 = row
      | otherwise = row{storedConversationId = maxExistingId + fallbackId}

decodeConversation :: Text -> Maybe Conversation
decodeConversation =
  either (const Nothing) Just . Aeson.eitherDecodeStrict' . TextEncoding.encodeUtf8

decodeMessages :: Text -> Maybe [LLM.ChatMessage]
decodeMessages =
  either (const Nothing) Just . Aeson.eitherDecodeStrict' . TextEncoding.encodeUtf8

-- | Start a conversation with a single user text message.
startWithUser :: Text -> Conversation
startWithUser prompt =
  startWithSystemAndUserContext "" prompt []

-- | Start a conversation with user text and image context.
startWithUserContext :: Text -> [Text] -> Conversation
startWithUserContext prompt imageUrls =
  startWithSystemAndUserContext "" prompt imageUrls

-- | Start with a system prompt and user text.
startWithSystemAndUser :: Text -> Text -> Conversation
startWithSystemAndUser systemPrompt prompt =
  startWithSystemAndUserContext systemPrompt prompt []

-- | Start with a system prompt plus user text and image context.
startWithSystemAndUserContext :: Text -> Text -> [Text] -> Conversation
startWithSystemAndUserContext systemPrompt prompt imageUrls =
  Conversation (systemMessages <> [LLM.userWithImages prompt imageUrls])
  where
    systemMessages
      | Text.null systemPrompt = []
      | otherwise         = [LLM.systemText systemPrompt]

-- | Append a text-only user turn.
appendUser :: Text -> Conversation -> Conversation
appendUser prompt =
  appendUserContext prompt []

-- | Append a user turn with optional image context.
appendUserContext :: Text -> [Text] -> Conversation -> Conversation
appendUserContext prompt imageUrls (Conversation history) =
  Conversation (history <> [LLM.userWithImages prompt imageUrls])

-- | Append an assistant reply, preserving generated image references as context.
appendAssistant :: Text -> Conversation -> Conversation
appendAssistant answer (Conversation history) =
  Conversation (history <> assistantContext answer)

assistantContext :: Text -> [LLM.ChatMessage]
assistantContext answer =
  assistantTextContext <> imageContext
  where
    answerText = Chat.renderReplyBody answer
    imageUrls = Chat.replyImageUrls answer
    assistantTextContext =
      [ LLM.assistantText answerText | not (Text.null answerText) ] <>
      [ LLM.assistantText "Generated image." | Text.null answerText && not (null imageUrls) ]
    imageContext =
      [ LLM.userWithImages "The previous assistant response generated this image. Use it as visual context for follow-up questions." imageUrls
      | not (null imageUrls)
      ]

-- | Look up the conversation associated with a bot reply message id.
lookupConversation :: IOE :> es => ConversationStore -> Integer -> Eff es (Maybe Conversation)
lookupConversation ConversationStore{unConversationStore = ref, activeConversationStore = activeRef} messageId = do
  finished <- liftIO $ fmap (.conversation) . Map.lookup messageId . (.conversations) <$> IORef.readIORef ref
  case finished of
    Just conversation ->
      pure (Just conversation)
    Nothing -> do
      active <- liftIO $ Map.lookup messageId <$> IORef.readIORef activeRef
      traverse (liftIO . MVar.readMVar . (.activeDone)) active

rememberActiveConversation
  :: IOE :> es
  => ConversationStore
  -> Maybe Integer
  -> Maybe Integer
  -> ThreadId
  -> Conversation
  -> Eff es (Maybe ActiveConversationHandle)
rememberActiveConversation _ _ Nothing _ _ =
  pure Nothing
rememberActiveConversation ConversationStore{activeConversationStore = activeRef} parentMessageId (Just messageId) threadId conversation = do
  messageIds <- liftIO (IORef.newIORef [messageId])
  current <- liftIO (IORef.newIORef conversation)
  done <- liftIO MVar.newEmptyMVar
  let active = ActiveConversation
        { activeMessageId = messageId
        , activeParentMessageId = parentMessageId
        , activeMessageIds = messageIds
        , activeCurrent = current
        , activeDone = done
        , activeThreadId = threadId
        }
  liftIO $ IORef.atomicModifyIORef' activeRef \activeMap ->
    (Map.insert messageId active activeMap, ())
  pure (Just (ActiveConversationHandle active))

addActiveConversationMessage :: IOE :> es => ConversationStore -> ActiveConversationHandle -> Integer -> Eff es ()
addActiveConversationMessage ConversationStore{activeConversationStore = activeRef} (ActiveConversationHandle active) messageId = do
  liftIO $ IORef.atomicModifyIORef' active.activeMessageIds \messageIds ->
    let next =
          if messageId `elem` messageIds
            then messageIds
            else messageId : messageIds
    in (next, ())
  liftIO $ IORef.atomicModifyIORef' activeRef \activeMap ->
    (Map.insert messageId active activeMap, ())

updateActiveConversation :: IOE :> es => ActiveConversationHandle -> Conversation -> Eff es ()
updateActiveConversation (ActiveConversationHandle active) conversation =
  liftIO $ IORef.writeIORef active.activeCurrent conversation

finishActiveConversation
  :: (IOE :> es, Log :> es)
  => ConversationStore
  -> ActiveConversationHandle
  -> Conversation
  -> Eff es ()
finishActiveConversation store@ConversationStore{activeConversationStore = activeRef} (ActiveConversationHandle active) conversation = do
  updateActiveConversation (ActiveConversationHandle active) conversation
  messageIds <- liftIO (IORef.readIORef active.activeMessageIds)
  traverse_ (\messageId -> rememberConversationFrom store active.activeParentMessageId (Just messageId) conversation) messageIds
  void $ liftIO (MVar.tryPutMVar active.activeDone conversation)
  liftIO $ IORef.atomicModifyIORef' activeRef \activeMap ->
    (foldl' (flip Map.delete) activeMap messageIds, ())

finishActiveConversationCurrent
  :: (IOE :> es, Log :> es)
  => ConversationStore
  -> ActiveConversationHandle
  -> Eff es ()
finishActiveConversationCurrent store (ActiveConversationHandle active) = do
  conversation <- liftIO (IORef.readIORef active.activeCurrent)
  finishActiveConversation store (ActiveConversationHandle active) conversation

haltConversation :: (IOE :> es, Log :> es) => ConversationStore -> Integer -> Eff es Bool
haltConversation store@ConversationStore{activeConversationStore = activeRef} messageId = do
  active <- liftIO $ Map.lookup messageId <$> IORef.readIORef activeRef
  case active of
    Nothing ->
      pure False
    Just activeConversation -> do
      conversation <- liftIO (IORef.readIORef activeConversation.activeCurrent)
      messageIds <- liftIO (IORef.readIORef activeConversation.activeMessageIds)
      liftIO (killThread activeConversation.activeThreadId)
      traverse_ (\activeMessageId -> rememberConversationFrom store activeConversation.activeParentMessageId (Just activeMessageId) conversation) messageIds
      void $ liftIO (MVar.tryPutMVar activeConversation.activeDone conversation)
      liftIO $ IORef.atomicModifyIORef' activeRef \activeMap ->
        (foldl' (flip Map.delete) activeMap messageIds, ())
      pure True

-- | Associate a bot reply message id with a conversation and persist it.
rememberConversation :: (IOE :> es, Log :> es) => ConversationStore -> Maybe Integer -> Conversation -> Eff es ()
rememberConversation store =
  rememberConversationFrom store Nothing

-- | Associate a bot reply with the same conversation as a parent bot message when known.
rememberConversationFrom
  :: (IOE :> es, Log :> es)
  => ConversationStore
  -> Maybe Integer
  -> Maybe Integer
  -> Conversation
  -> Eff es ()
rememberConversationFrom _ _ Nothing _ =
  pure ()
rememberConversationFrom ConversationStore{unConversationStore = ref, sqliteStore} parentMessageId (Just messageId) conversation = do
  (conversationId, storageParentMessageId, storedMessages) <- liftIO $ IORef.atomicModifyIORef' ref \conversationState ->
    let conversationId = conversationIdFor conversationState parentMessageId messageId
        node = ConversationNode{conversationId, parentMessageId, conversation}
        (storageParentMessageId, storedMessages) =
          conversationMessagesForStorage conversationState parentMessageId conversation
        nextConversationId =
          if conversationId < conversationState.nextConversationId
            then conversationState.nextConversationId
            else conversationId + 1
        nextState = conversationState
          { nextConversationId = nextConversationId
          , conversations = Map.insert messageId node conversationState.conversations
          }
    in (nextState, (conversationId, storageParentMessageId, storedMessages))
  traverse_
    ( \store ->
        liftIO (Storage.saveConversationMessages store messageId conversationId storageParentMessageId (messagesJson storedMessages))
          `catch` \(err :: SomeException) ->
            logInfo "Failed to persist conversation" (show err :: String)
    )
    sqliteStore

conversationIdFor :: ConversationState -> Maybe Integer -> Integer -> Integer
conversationIdFor conversationState parentMessageId messageId =
  fromMaybe conversationState.nextConversationId (parentConversationId <|> existingConversationId)
  where
    parentConversationId =
      parentMessageId >>= \parentId ->
        (.conversationId) <$> Map.lookup parentId conversationState.conversations
    existingConversationId =
      (.conversationId) <$> Map.lookup messageId conversationState.conversations

messagesJson :: [LLM.ChatMessage] -> Text
messagesJson =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

conversationMessagesForStorage :: ConversationState -> Maybe Integer -> Conversation -> (Maybe Integer, [LLM.ChatMessage])
conversationMessagesForStorage conversationState parentMessageId conversation =
  case parentMessageId >>= (`Map.lookup` conversationState.conversations) of
    Just parent
      | Just suffix <- conversationSuffix parent.conversation conversation ->
          (parentMessageId, suffix)
      | otherwise ->
          (Nothing, conversation.messages)
    Nothing ->
      (parentMessageId, conversation.messages)

conversationSuffix :: Conversation -> Conversation -> Maybe [LLM.ChatMessage]
conversationSuffix parent child
  | parentJson == childPrefixJson =
      Just (drop parentLength child.messages)
  | otherwise =
      Nothing
  where
    parentLength =
      length parent.messages
    parentJson =
      map messageJson parent.messages
    childPrefixJson =
      map messageJson (take parentLength child.messages)

messageJson :: LLM.ChatMessage -> LazyByteString.ByteString
messageJson =
  Aeson.encode
