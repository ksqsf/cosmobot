{-|
Module      : Bot.Storage.Conversation
Description : Persistent conversation graph
Stability   : experimental
-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Storage.Conversation
  ( ConversationStore
  , ActiveConversationHandle
  , ConversationRow (..)
  , newConversationStore
  , lookupConversation
  , lookupConversationMessageIds
  , rememberConversation
  , rememberConversationFrom
  , rememberActiveConversation
  , addActiveConversationMessage
  , updateActiveConversation
  , finishActiveConversation
  , finishActiveConversationCurrent
  , haltConversation
  , loadConversationRows
  )
where

import Bot.Core.Conversation
import Bot.Core.Message
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude hiding (newIORef, readIORef, atomicModifyIORef, writeIORef, atomicModifyIORef')
import Bot.Storage.Prelude
import qualified Effectful.Concurrent.MVar as MVar
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.Int as Int
import Effectful.Prim.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text.Encoding as TextEncoding

data ConversationStore = ConversationStore
  { unConversationStore :: IORef ConversationState
  , activeConversationStore :: IORef (Map ConversationMessageKey ActiveConversation)
  }

data ConversationState = ConversationState
  { nextConversationId :: !Integer
  , conversationTree :: !ConversationTree
  , conversationIds :: !(Map ConversationMessageKey Integer)
  , recentConversationIds :: ![ConversationMessageKey]
  }

data StoredConversationNode = StoredConversationNode
  { conversationId :: !Integer
  , treeNode :: !ConversationTreeNode
  }

data ActiveConversation = ActiveConversation
  { activeMessageKey :: !ConversationMessageKey
  , activeParentMessageKey :: !(Maybe ConversationMessageKey)
  , activeMessageKeys :: !(IORef [ConversationMessageKey])
  , activeCurrent :: !(IORef Conversation)
  , activeDone :: !(MVar.MVar Conversation)
  , activeThreadId :: !ThreadId
  }

newtype ActiveConversationHandle = ActiveConversationHandle ActiveConversation

data ConversationRow = ConversationRow
  { messageKey :: !ConversationMessageKey
  , conversationId :: !(Maybe Integer)
  , parentMessageKey :: !(Maybe ConversationMessageKey)
  , messagesJson :: !Text
  }
  deriving (Eq, Show)

data ConversationStorageRow = ConversationStorageRow
  { id :: ID ConversationStorageRow
  , platform_key :: Text
  , chat_id :: Maybe Int.Int64
  , message_id :: Text
  , conversation_id :: Maybe Int.Int64
  , parent_chat_id :: Maybe Int.Int64
  , parent_message_id :: Maybe Text
  , messages_json :: Text
  }
  deriving (Generic)

instance SqlRow ConversationStorageRow

conversationRows :: Table ConversationStorageRow
conversationRows =
  table "conversation_nodes_scoped"
    [ #id :- autoPrimary
    , #platform_key :- index
    , #chat_id :- index
    , #message_id :- index
    , #conversation_id :- index
    , #parent_message_id :- index
    ]

newConversationStore :: Prim :> es => Eff es ConversationStore
newConversationStore = do
  ref <- newIORef ConversationState{nextConversationId = 1, conversationTree = emptyConversationTree, conversationIds = Map.empty, recentConversationIds = []}
  activeRef <- newIORef Map.empty
  pure ConversationStore{unConversationStore = ref, activeConversationStore = activeRef}

lookupConversation :: (Prim :> es, Concurrent :> es, Storage.Storage :> es) => ConversationStore -> ConversationMessageKey -> Eff es (Maybe Conversation)
lookupConversation store@ConversationStore{activeConversationStore = activeRef} messageKey = do
  finished <- fmap (.treeNode.conversation) <$> lookupConversationNode store messageKey
  case finished of
    Just conversation ->
      pure (Just conversation)
    Nothing -> do
      active <- Map.lookup messageKey <$> readIORef activeRef
      traverse (MVar.readMVar . (.activeDone)) active

lookupConversationMessageIds :: (Prim :> es, Storage.Storage :> es) => ConversationStore -> ConversationMessageKey -> Eff es [MessageId]
lookupConversationMessageIds store@ConversationStore{unConversationStore = ref, activeConversationStore = activeRef} messageKey = do
  active <- Map.lookup messageKey <$> readIORef activeRef
  case active of
    Just activeConversation ->
      map (.messageId) <$> readIORef activeConversation.activeMessageKeys
    Nothing -> do
      node <- lookupConversationNode store messageKey
      case node of
        Nothing ->
          loadConversationMessageIds messageKey
        Just target -> do
          cached <- Map.toList . (.conversationIds) <$> readIORef ref
          stored <- loadConversationMessageIds messageKey
          pure (ordNub (stored <> [cachedKey.messageId | (cachedKey, cachedConversationId) <- cached, cachedConversationId == target.conversationId]))

rememberActiveConversation
  :: (Prim :> es, Concurrent :> es)
  => ConversationStore
  -> Maybe ConversationMessageKey
  -> Maybe ConversationMessageKey
  -> ThreadId
  -> Conversation
  -> Eff es (Maybe ActiveConversationHandle)
rememberActiveConversation _ _ Nothing _ _ =
  pure Nothing
rememberActiveConversation ConversationStore{activeConversationStore = activeRef} parentMessageKey (Just messageKey) threadId conversation = do
  messageKeys <- newIORef [messageKey]
  current <- newIORef conversation
  done <- MVar.newEmptyMVar
  let active = ActiveConversation{activeMessageKey = messageKey, activeParentMessageKey = parentMessageKey, activeMessageKeys = messageKeys, activeCurrent = current, activeDone = done, activeThreadId = threadId}
  atomicModifyIORef' activeRef \activeMap ->
    (Map.insert messageKey active activeMap, ())
  pure (Just (ActiveConversationHandle active))

addActiveConversationMessage :: Prim :> es => ConversationStore -> ActiveConversationHandle -> ConversationMessageKey -> Eff es ()
addActiveConversationMessage ConversationStore{activeConversationStore = activeRef} (ActiveConversationHandle active) messageKey = do
  atomicModifyIORef' active.activeMessageKeys \messageKeys ->
    let next = if messageKey `elem` messageKeys then messageKeys else messageKey : messageKeys
    in (next, ())
  atomicModifyIORef' activeRef \activeMap ->
    (Map.insert messageKey active activeMap, ())

updateActiveConversation :: Prim :> es => ActiveConversationHandle -> Conversation -> Eff es ()
updateActiveConversation (ActiveConversationHandle active) conversation =
  writeIORef active.activeCurrent conversation

finishActiveConversation
  :: (Prim :> es, KatipE :> es, Concurrent :> es, Storage.Storage :> es)
  => ConversationStore
  -> ActiveConversationHandle
  -> Conversation
  -> Eff es ()
finishActiveConversation store@ConversationStore{activeConversationStore = activeRef} (ActiveConversationHandle active) conversation = do
  updateActiveConversation (ActiveConversationHandle active) conversation
  messageKeys <- readIORef active.activeMessageKeys
  traverse_ (\messageKey -> rememberConversationFrom store active.activeParentMessageKey (Just messageKey) conversation) messageKeys
  void $ MVar.tryPutMVar active.activeDone conversation
  atomicModifyIORef' activeRef \activeMap ->
    (foldl' (flip Map.delete) activeMap messageKeys, ())

finishActiveConversationCurrent
  :: (Prim :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
  => ConversationStore
  -> ActiveConversationHandle
  -> Eff es ()
finishActiveConversationCurrent store (ActiveConversationHandle active) = do
  conversation <- readIORef active.activeCurrent
  finishActiveConversation store (ActiveConversationHandle active) conversation

haltConversation :: (Prim :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es) => ConversationStore -> ConversationMessageKey -> Eff es Bool
haltConversation store@ConversationStore{activeConversationStore = activeRef} messageKey = do
  active <- Map.lookup messageKey <$> readIORef activeRef
  case active of
    Nothing ->
      pure False
    Just activeConversation -> do
      conversation <- readIORef activeConversation.activeCurrent
      messageKeys <- readIORef activeConversation.activeMessageKeys
      killThread activeConversation.activeThreadId
      traverse_ (\activeMessageKey -> rememberConversationFrom store activeConversation.activeParentMessageKey (Just activeMessageKey) conversation) messageKeys
      void $ MVar.tryPutMVar activeConversation.activeDone conversation
      atomicModifyIORef' activeRef \activeMap ->
        (foldl' (flip Map.delete) activeMap messageKeys, ())
      pure True

rememberConversation :: (Prim :> es, KatipE :> es, Storage.Storage :> es) => ConversationStore -> Maybe ConversationMessageKey -> Conversation -> Eff es ()
rememberConversation store =
  rememberConversationFrom store Nothing

rememberConversationFrom
  :: (Prim :> es, KatipE :> es, Storage.Storage :> es)
  => ConversationStore
  -> Maybe ConversationMessageKey
  -> Maybe ConversationMessageKey
  -> Conversation
  -> Eff es ()
rememberConversationFrom _ _ Nothing _ =
  pure ()
rememberConversationFrom store@ConversationStore{unConversationStore = ref} parentMessageKey (Just messageKey) conversation = do
  ensureConversationTable
  parentNode <- lookupConversationNodeMaybe store parentMessageKey
  existingNode <- lookupConversationNodeMaybe store (Just messageKey)
  nextStoredConversationId <- loadNextConversationId
  (conversationId, storageParentMessageKey, storedMessages) <- atomicModifyIORef' ref \conversationState ->
    let conversationId = conversationIdFor conversationState parentNode existingNode
        node = StoredConversationNode
          { conversationId
          , treeNode = ConversationTreeNode{messageKey, parentMessageKey, conversation}
          }
        (storageParentMessageKey, storedMessages) = conversationMessagesForStorage parentMessageKey parentNode conversation
        nextConversationId = max nextStoredConversationId (if conversationId < conversationState.nextConversationId then conversationState.nextConversationId else conversationId + 1)
        nextState = conversationState{nextConversationId = nextConversationId}
        cachedState = cacheConversationNode messageKey node nextState
    in (cachedState, (conversationId, storageParentMessageKey, storedMessages))
  saveConversationMessages messageKey conversationId storageParentMessageKey (messagesJson storedMessages)
    `catchSync` \err ->
      logInfo [i|Failed to persist conversation: #{show err :: String}|]

lookupConversationNode :: (Prim :> es, Storage.Storage :> es) => ConversationStore -> ConversationMessageKey -> Eff es (Maybe StoredConversationNode)
lookupConversationNode store messageKey =
  lookupConversationNodeMaybe store (Just messageKey)

lookupConversationNodeMaybe :: (Prim :> es, Storage.Storage :> es) => ConversationStore -> Maybe ConversationMessageKey -> Eff es (Maybe StoredConversationNode)
lookupConversationNodeMaybe _ Nothing =
  pure Nothing
lookupConversationNodeMaybe store@ConversationStore{unConversationStore = ref} (Just messageKey) = do
  cached <- do
    conversationState <- readIORef ref
    pure do
      treeNode <- lookupConversationTreeNode messageKey conversationState.conversationTree
      conversationId <- Map.lookup messageKey conversationState.conversationIds
      pure StoredConversationNode{conversationId, treeNode}
  case cached of
    Just node ->
      pure (Just node)
    Nothing ->
      loadConversationNodeFromStorage store [] messageKey

loadConversationNodeFromStorage :: (Prim :> es, Storage.Storage :> es) => ConversationStore -> [ConversationMessageKey] -> ConversationMessageKey -> Eff es (Maybe StoredConversationNode)
loadConversationNodeFromStorage store@ConversationStore{unConversationStore = ref} visited messageKey
  | messageKey `elem` visited =
      pure Nothing
  | otherwise = do
      row <- loadConversationRow messageKey
      case row >>= decodeStoredConversation of
        Nothing ->
          pure Nothing
        Just stored -> do
          parentNode <- case stored.storedParentMessageKey of
            Nothing ->
              pure Nothing
            Just parentMessageKey ->
              lookupConversationNodeMaybe store (Just parentMessageKey)
                >>= maybe (loadConversationNodeFromStorage store (messageKey : visited) parentMessageKey) (pure . Just)
          let node = StoredConversationNode
                { conversationId = stored.storedConversationId
                , treeNode = ConversationTreeNode
                    { messageKey = messageKey
                    , parentMessageKey = stored.storedParentMessageKey
                    , conversation = storedConversationFromMessages parentNode stored.storedMessages
                    }
                }
          atomicModifyIORef' ref \conversationState ->
            (cacheConversationNode messageKey node conversationState, ())
          pure (Just node)

data StoredConversation = StoredConversation
  { storedConversationId :: !Integer
  , storedParentMessageKey :: !(Maybe ConversationMessageKey)
  , storedMessages :: ![LLM.ChatMessage]
  }

decodeStoredConversation :: ConversationRow -> Maybe StoredConversation
decodeStoredConversation row = do
  messages <- decodeMessages row.messagesJson
  let conversationId = fromMaybe 0 row.conversationId
  pure StoredConversation{storedConversationId = conversationId, storedParentMessageKey = row.parentMessageKey, storedMessages = messages}

storedConversationFromMessages :: Maybe StoredConversationNode -> [LLM.ChatMessage] -> Conversation
storedConversationFromMessages parentNode messages =
  case parentNode of
    Nothing ->
      Conversation (Seq.fromList messages)
    Just parent ->
      Conversation (parent.treeNode.conversation.messages <> Seq.fromList messages)

decodeMessages :: Text -> Maybe [LLM.ChatMessage]
decodeMessages =
  either (const Nothing) Just . Aeson.eitherDecodeStrict' . TextEncoding.encodeUtf8

ensureConversationTable :: Storage.Storage :> es => Eff es ()
ensureConversationTable =
  runSelda (tryCreateTable conversationRows)

loadConversationRows :: Storage.Storage :> es => Eff es [ConversationRow]
loadConversationRows = do
  ensureConversationTable
  rows <- runSelda $
    query do
      row <- select conversationRows
      order (row ! #id) ascending
      pure row
  pure (map conversationRowFromStorage rows)

loadConversationRow :: Storage.Storage :> es => ConversationMessageKey -> Eff es (Maybe ConversationRow)
loadConversationRow targetMessageKey = do
  ensureConversationTable
  rows <- runSelda $
    query $
      queryLimit 0 1 do
        row <- select conversationRows
        restrict (conversationKeyMatches targetMessageKey row)
        pure row
  pure (conversationRowFromStorage <$> viaNonEmpty head rows)

loadConversationMessageIds :: Storage.Storage :> es => ConversationMessageKey -> Eff es [MessageId]
loadConversationMessageIds messageKey = do
  target <- loadConversationRow messageKey
  case target >>= (.conversationId) of
    Nothing ->
      pure []
    Just targetConversationId -> do
      rows <- runSelda $
        query do
          row <- select conversationRows
          restrict (row ! #conversation_id .== literal (Just (fromIntegral targetConversationId :: Int.Int64)))
          order (row ! #id) ascending
          pure (row ! #message_id)
      pure (map textMessageId rows)

loadNextConversationId :: Storage.Storage :> es => Eff es Integer
loadNextConversationId = do
  rows <- loadConversationRows
  pure (Foldable.maximum (1 : [fromMaybe 0 row.conversationId + 1 | row <- rows]))

saveConversationMessages :: Storage.Storage :> es => ConversationMessageKey -> Integer -> Maybe ConversationMessageKey -> Text -> Eff es ()
saveConversationMessages messageKey conversationId parentMessageKey storedMessagesJson = do
  ensureConversationTable
  runSelda do
    deleteFrom_ conversationRows \row ->
      conversationKeyMatches messageKey row
    insert_
      conversationRows
      [ ConversationStorageRow
          { id = def
          , platform_key = chatPlatformKey messageKey.platform
          , chat_id = fromIntegral <$> messageKey.chatId
          , message_id = messageIdText messageKey.messageId
          , conversation_id = Just (fromIntegral conversationId)
          , parent_chat_id = fromIntegral <$> (parentMessageKey >>= (.chatId))
          , parent_message_id = messageIdText <$> (parentMessageKey <&> (.messageId))
          , messages_json = storedMessagesJson
          }
      ]

conversationRowFromStorage :: ConversationStorageRow -> ConversationRow
conversationRowFromStorage row =
  let messageKey = ConversationMessageKey{platform = platformFromKey row.platform_key, chatId = fromIntegral <$> row.chat_id, messageId = textMessageId row.message_id}
  in ConversationRow
    { messageKey = messageKey
    , conversationId = fromIntegral <$> row.conversation_id
    , parentMessageKey = do
        parentMessageId <- textMessageId <$> row.parent_message_id
        pure ConversationMessageKey
          { platform = messageKey.platform
          , chatId = fromIntegral <$> row.parent_chat_id
          , messageId = parentMessageId
          }
    , messagesJson = row.messages_json
    }

conversationKeyMatches :: forall (backend :: Type). ConversationMessageKey -> Row backend ConversationStorageRow -> Col backend Bool
conversationKeyMatches key row =
  row ! #platform_key .== literal (chatPlatformKey key.platform)
    .&& nullableIntegerMatches key.chatId (row ! #chat_id)
    .&& row ! #message_id .== literal (messageIdText key.messageId)

nullableIntegerMatches :: forall (backend :: Type). Maybe Integer -> Col backend (Maybe Int.Int64) -> Col backend Bool
nullableIntegerMatches Nothing column =
  isNull column
nullableIntegerMatches (Just value) column =
  column .== literal (Just (fromIntegral value :: Int.Int64))

platformFromKey :: Text -> ChatPlatform
platformFromKey = \case
  "telegram" ->
    PlatformTelegram
  "matrix" ->
    PlatformMatrix
  "discord" ->
    PlatformDiscord
  _ ->
    PlatformQQ

cacheConversationNode :: ConversationMessageKey -> StoredConversationNode -> ConversationState -> ConversationState
cacheConversationNode messageKey node conversationState =
  conversationState
    { conversationTree = ConversationTree (Map.restrictKeys insertedTree retainedIds)
    , conversationIds = Map.restrictKeys insertedIds retainedIds
    , recentConversationIds = retainedOrder
    }
  where
    insertedTree = (insertConversationTreeNode node.treeNode conversationState.conversationTree).nodes
    insertedIds = Map.insert messageKey node.conversationId conversationState.conversationIds
    nextOrder = messageKey : filter (/= messageKey) conversationState.recentConversationIds
    retainedOrder = take maxCachedConversations nextOrder
    retainedIds = Map.keysSet (Map.fromList [(key, ()) | key <- retainedOrder])

maxCachedConversations :: Int
maxCachedConversations =
  32

conversationIdFor :: ConversationState -> Maybe StoredConversationNode -> Maybe StoredConversationNode -> Integer
conversationIdFor conversationState parentNode existingNode =
  fromMaybe conversationState.nextConversationId (parentConversationId <|> existingConversationId)
  where
    parentConversationId = (.conversationId) <$> parentNode
    existingConversationId = (.conversationId) <$> existingNode

messagesJson :: [LLM.ChatMessage] -> Text
messagesJson =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

conversationMessagesForStorage :: Maybe ConversationMessageKey -> Maybe StoredConversationNode -> Conversation -> (Maybe ConversationMessageKey, [LLM.ChatMessage])
conversationMessagesForStorage parentMessageKey parentNode conversation =
  case parentNode of
    Just parent
      | Just suffix <- conversationSuffix parent.treeNode.conversation conversation ->
          (parentMessageKey, suffix)
      | otherwise ->
          (Nothing, conversationMessagesList conversation)
    Nothing ->
      (parentMessageKey, conversationMessagesList conversation)

conversationSuffix :: Conversation -> Conversation -> Maybe [LLM.ChatMessage]
conversationSuffix parent child
  | parentJson == childPrefixJson =
      Just (drop parentLength childMessages)
  | otherwise =
      Nothing
  where
    parentMessages = conversationMessagesList parent
    childMessages = conversationMessagesList child
    parentLength = length parentMessages
    parentJson = map messageJson parentMessages
    childPrefixJson = map messageJson (take parentLength childMessages)

conversationMessagesList :: Conversation -> [LLM.ChatMessage]
conversationMessagesList =
  Foldable.toList . (.messages)

messageJson :: LLM.ChatMessage -> LazyByteString.ByteString
messageJson =
  Aeson.encode
