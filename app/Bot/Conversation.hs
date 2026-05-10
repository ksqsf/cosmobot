{-|
Module      : Bot.Conversation
Description : In-memory conversation graph
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Conversation
  ( -- * Store
    ConversationStore
  , newConversationStore
  , lookupConversation
  , rememberConversation
  , rememberConversationFrom
  , withConversationLock

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

-- | Mutable conversation index, optionally mirrored into SQLite.
data ConversationStore = ConversationStore
  { unConversationStore :: IORef.IORef ConversationState
  , sqliteStore :: !(Maybe Storage.SQLiteStore)
  , conversationLocks :: MVar.MVar (Map Integer (MVar.MVar ()))
  }

-- | Ordered chat history sent to the LLM.
newtype Conversation = Conversation
  { messages :: [LLM.ChatMessage]
  }
  deriving (Show, Generic)

data ConversationEntry = ConversationEntry
  { conversationId :: !Integer
  , conversation :: !Conversation
  }

data ConversationState = ConversationState
  { nextConversationId :: !Integer
  , conversations :: !(Map Integer ConversationEntry)
  }

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
  conversations <- maybe (pure Map.empty) loadStoredConversations sqliteStore
  let entries = Map.fromList
        [ (messageId, ConversationEntry{conversationId, conversation})
        | ((messageId, conversation), conversationId) <- zip (Map.toList conversations) [1..]
        ]
      nextConversationId = fromIntegral (Map.size entries) + 1
  ref <- IORef.newIORef ConversationState{nextConversationId, conversations = entries}
  conversationLocks <- MVar.newMVar Map.empty
  pure ConversationStore{unConversationStore = ref, sqliteStore, conversationLocks}

loadStoredConversations :: Storage.SQLiteStore -> IO (Map Integer Conversation)
loadStoredConversations store =
  Map.mapMaybe decodeConversation <$> Storage.loadConversationRows store

decodeConversation :: Text -> Maybe Conversation
decodeConversation =
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
lookupConversation ConversationStore{unConversationStore = ref} messageId =
  liftIO $ fmap (.conversation) . Map.lookup messageId . (.conversations) <$> IORef.readIORef ref

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
  liftIO $ IORef.atomicModifyIORef' ref \conversationState ->
    let conversationId = conversationIdFor conversationState parentMessageId messageId
        entry = ConversationEntry{conversationId, conversation}
        nextConversationId =
          if conversationId < conversationState.nextConversationId
            then conversationState.nextConversationId
            else conversationId + 1
        updatedConversations =
          Map.map
            ( \existing ->
                if existing.conversationId == conversationId
                  then existing{conversation = conversation}
                  else existing
            )
            conversationState.conversations
        nextState = conversationState
          { nextConversationId = nextConversationId
          , conversations = Map.insert messageId entry updatedConversations
          }
    in (nextState, ())
  traverse_
    ( \store ->
        liftIO (Storage.saveConversationJson store messageId (conversationJson conversation))
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

-- | Run an action under the lock for the conversation containing this bot message.
withConversationLock :: IOE :> es => ConversationStore -> Integer -> Eff es a -> Eff es a
withConversationLock ConversationStore{unConversationStore = ref, conversationLocks} messageId action = do
  conversationId <- liftIO do
    conversationState <- IORef.readIORef ref
    pure ((.conversationId) <$> Map.lookup messageId conversationState.conversations)
  case conversationId of
    Nothing ->
      action
    Just cid -> do
      lock <- liftIO (lockForConversation conversationLocks cid)
      withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
        liftIO (MVar.withMVar lock \_ -> runInIO action)

lockForConversation :: MVar.MVar (Map Integer (MVar.MVar ())) -> Integer -> IO (MVar.MVar ())
lockForConversation locksVar conversationId =
  MVar.modifyMVar locksVar \locks ->
    case Map.lookup conversationId locks of
      Just lock ->
        pure (locks, lock)
      Nothing -> do
        lock <- MVar.newMVar ()
        pure (Map.insert conversationId lock locks, lock)

conversationJson :: Conversation -> Text
conversationJson =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode
