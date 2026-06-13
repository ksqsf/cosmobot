{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Storage.Thread
Description : Persistent platform thread graph
Stability   : experimental
-}

module Bot.Storage.Thread
  ( ThreadStore
  , ActiveThreadHandle
  , ThreadRow (..)
  , newThreadStore
  , lookupThreadTranscript
  , lookupThreadMessageIds
  , rememberThreadTranscript
  , rememberThreadTranscriptFrom
  , rememberActiveThread
  , addActiveThreadMessage
  , updateActiveThread
  , finishActiveThread
  , finishActiveThreadCurrent
  , haltThread
  , haltThreadForMessage
  , loadThreadRows
  )
where

import Bot.Core.Message
import Bot.Core.Thread
import Bot.Core.Transcript
import Bot.Effect.Concurrency (Handle (..), Id)
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude hiding (Handle, newIORef, readIORef, atomicModifyIORef, writeIORef, atomicModifyIORef')
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

data ThreadStore = ThreadStore
  { unThreadStore :: IORef ThreadState
  , activeThreadStore :: IORef (Map ThreadMessageKey ActiveThread)
  }

data ThreadState = ThreadState
  { nextThreadStorageId :: !Integer
  , threadTree :: !ThreadTree
  , threadIds :: !(Map ThreadMessageKey Integer)
  , recentThreadIds :: ![ThreadMessageKey]
  }

data StoredThreadNode = StoredThreadNode
  { threadStorageId :: !Integer
  , treeNode :: !ThreadNode
  }

data ActiveThread = ActiveThread
  { activeMessageKey :: !ThreadMessageKey
  , activeParentMessageKey :: !(Maybe ThreadMessageKey)
  , activeMessageKeys :: !(IORef [ThreadMessageKey])
  , activeCurrent :: !(IORef Transcript)
  , activeDone :: !(MVar.MVar Transcript)
  , activeHandle :: !Handle
  }

newtype ActiveThreadHandle = ActiveThreadHandle ActiveThread

data ThreadRow = ThreadRow
  { messageKey :: !ThreadMessageKey
  , threadStorageId :: !(Maybe Integer)
  , parentMessageKey :: !(Maybe ThreadMessageKey)
  , messagesJson :: !Text
  }
  deriving (Eq, Show)

data ThreadStorageRow = ThreadStorageRow
  { id :: ID ThreadStorageRow
  , platform_key :: Text
  , chat_id :: Maybe Int.Int64
  , message_id :: Text
  , thread_id :: Maybe Int.Int64
  , parent_chat_id :: Maybe Int.Int64
  , parent_message_id :: Maybe Text
  , messages_json :: Text
  }
  deriving (Generic)

instance SqlRow ThreadStorageRow

threadRows :: Table ThreadStorageRow
threadRows =
  table "threads"
    [ #id :- autoPrimary
    , #platform_key :- index
    , #chat_id :- index
    , #message_id :- index
    , #thread_id :- index
    , #parent_message_id :- index
    ]

newThreadStore :: Prim :> es => Eff es ThreadStore
newThreadStore = do
  ref <- newIORef ThreadState{nextThreadStorageId = 1, threadTree = emptyThreadTree, threadIds = Map.empty, recentThreadIds = []}
  activeRef <- newIORef Map.empty
  pure ThreadStore{unThreadStore = ref, activeThreadStore = activeRef}

lookupThreadTranscript :: (Prim :> es, Concurrent :> es, Storage.Storage :> es) => ThreadStore -> ThreadMessageKey -> Eff es (Maybe Transcript)
lookupThreadTranscript store@ThreadStore{activeThreadStore = activeRef} messageKey = do
  finished <- fmap (.treeNode.transcript) <$> lookupStoredThreadNode store messageKey
  case finished of
    Just transcript ->
      pure (Just transcript)
    Nothing -> do
      active <- Map.lookup messageKey <$> readIORef activeRef
      traverse (MVar.readMVar . (.activeDone)) active

lookupThreadMessageIds :: (Prim :> es, Storage.Storage :> es) => ThreadStore -> ThreadMessageKey -> Eff es [MessageId]
lookupThreadMessageIds store@ThreadStore{unThreadStore = ref, activeThreadStore = activeRef} messageKey = do
  active <- Map.lookup messageKey <$> readIORef activeRef
  case active of
    Just activeThread ->
      map (.messageId) <$> readIORef activeThread.activeMessageKeys
    Nothing -> do
      node <- lookupStoredThreadNode store messageKey
      case node of
        Nothing ->
          loadThreadMessageIdsFromStorage messageKey
        Just target -> do
          cached <- Map.toList . (.threadIds) <$> readIORef ref
          stored <- loadThreadMessageIdsFromStorage messageKey
          pure (ordNub (stored <> [cachedKey.messageId | (cachedKey, cachedThreadStorageId) <- cached, cachedThreadStorageId == target.threadStorageId]))

rememberActiveThread
  :: (Prim :> es, Concurrent :> es)
  => ThreadStore
  -> Maybe ThreadMessageKey
  -> Maybe ThreadMessageKey
  -> Handle
  -> Transcript
  -> Eff es (Maybe ActiveThreadHandle)
rememberActiveThread _ _ Nothing _ _ =
  pure Nothing
rememberActiveThread ThreadStore{activeThreadStore = activeRef} parentMessageKey (Just messageKey) activeHandle transcript = do
  messageKeys <- newIORef [messageKey]
  current <- newIORef transcript
  done <- MVar.newEmptyMVar
  let active = ActiveThread{activeMessageKey = messageKey, activeParentMessageKey = parentMessageKey, activeMessageKeys = messageKeys, activeCurrent = current, activeDone = done, activeHandle}
  atomicModifyIORef' activeRef \activeMap ->
    (Map.insert messageKey active activeMap, ())
  pure (Just (ActiveThreadHandle active))

addActiveThreadMessage :: Prim :> es => ThreadStore -> ActiveThreadHandle -> ThreadMessageKey -> Eff es ()
addActiveThreadMessage ThreadStore{activeThreadStore = activeRef} (ActiveThreadHandle active) messageKey = do
  atomicModifyIORef' active.activeMessageKeys \messageKeys ->
    let next = if messageKey `elem` messageKeys then messageKeys else messageKey : messageKeys
    in (next, ())
  atomicModifyIORef' activeRef \activeMap ->
    (Map.insert messageKey active activeMap, ())

updateActiveThread :: Prim :> es => ActiveThreadHandle -> Transcript -> Eff es ()
updateActiveThread (ActiveThreadHandle active) transcript =
  writeIORef active.activeCurrent transcript

finishActiveThread
  :: (Prim :> es, KatipE :> es, Concurrent :> es, Storage.Storage :> es)
  => ThreadStore
  -> ActiveThreadHandle
  -> Transcript
  -> Eff es ()
finishActiveThread store@ThreadStore{activeThreadStore = activeRef} (ActiveThreadHandle active) transcript = do
  updateActiveThread (ActiveThreadHandle active) transcript
  messageKeys <- readIORef active.activeMessageKeys
  traverse_ (\messageKey -> rememberThreadTranscriptFrom store active.activeParentMessageKey (Just messageKey) transcript) messageKeys
  void $ MVar.tryPutMVar active.activeDone transcript
  atomicModifyIORef' activeRef \activeMap ->
    (foldl' (flip Map.delete) activeMap messageKeys, ())

finishActiveThreadCurrent
  :: (Prim :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
  => ThreadStore
  -> ActiveThreadHandle
  -> Eff es ()
finishActiveThreadCurrent store (ActiveThreadHandle active) = do
  transcript <- readIORef active.activeCurrent
  finishActiveThread store (ActiveThreadHandle active) transcript

haltThread
  :: (Prim :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
  => ThreadStore
  -> (Id -> Eff es Bool)
  -> ThreadMessageKey
  -> Eff es Bool
haltThread store@ThreadStore{activeThreadStore = activeRef} cancel messageKey = do
  active <- Map.lookup messageKey <$> readIORef activeRef
  case active of
    Nothing ->
      pure False
    Just activeThread -> do
      transcript <- readIORef activeThread.activeCurrent
      messageKeys <- readIORef activeThread.activeMessageKeys
      void $ cancel activeThread.activeHandle.handleId
      traverse_ (\activeMessageKey -> rememberThreadTranscriptFrom store activeThread.activeParentMessageKey (Just activeMessageKey) transcript) messageKeys
      void $ MVar.tryPutMVar activeThread.activeDone transcript
      atomicModifyIORef' activeRef \activeMap ->
        (foldl' (flip Map.delete) activeMap messageKeys, ())
      pure True

haltThreadForMessage
  :: (Prim :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
  => ThreadStore
  -> (Id -> Eff es Bool)
  -> IncomingMessage
  -> Eff es Bool
haltThreadForMessage store cancel message =
  haltFirst (haltCandidateKeys message)
  where
    haltFirst [] =
      pure False
    haltFirst (messageKey : rest) =
      haltThread store cancel messageKey >>= \case
        True ->
          pure True
        False ->
          haltFirst rest

haltCandidateKeys :: IncomingMessage -> [ThreadMessageKey]
haltCandidateKeys message =
  ordNub (catMaybes [replyKey, currentKey])
  where
    replyKey =
      threadMessageKey message <$> message.replyToMessageId
    currentKey =
      threadMessageKey message <$> message.messageId

rememberThreadTranscript :: (Prim :> es, KatipE :> es, Storage.Storage :> es) => ThreadStore -> Maybe ThreadMessageKey -> Transcript -> Eff es ()
rememberThreadTranscript store =
  rememberThreadTranscriptFrom store Nothing

rememberThreadTranscriptFrom
  :: (Prim :> es, KatipE :> es, Storage.Storage :> es)
  => ThreadStore
  -> Maybe ThreadMessageKey
  -> Maybe ThreadMessageKey
  -> Transcript
  -> Eff es ()
rememberThreadTranscriptFrom _ _ Nothing _ =
  pure ()
rememberThreadTranscriptFrom store@ThreadStore{unThreadStore = ref} parentMessageKey (Just messageKey) transcript = do
  ensureThreadTable
  parentNode <- lookupStoredThreadNodeMaybe store parentMessageKey
  existingNode <- lookupStoredThreadNodeMaybe store (Just messageKey)
  nextStoredThreadId <- loadNextThreadStorageId
  (threadStorageId, storageParentMessageKey, storedMessages) <- atomicModifyIORef' ref \threadState ->
    let threadStorageId = threadStorageIdFor threadState parentNode existingNode
        node = StoredThreadNode
          { threadStorageId
          , treeNode = ThreadNode{messageKey, parentMessageKey, transcript}
          }
        (storageParentMessageKey, storedMessages) = transcriptMessagesForStorage parentMessageKey parentNode transcript
        nextThreadStorageId = max nextStoredThreadId (if threadStorageId < threadState.nextThreadStorageId then threadState.nextThreadStorageId else threadStorageId + 1)
        nextState = threadState{nextThreadStorageId = nextThreadStorageId}
        cachedState = cacheThreadNode messageKey node nextState
    in (cachedState, (threadStorageId, storageParentMessageKey, storedMessages))
  saveThreadMessages messageKey threadStorageId storageParentMessageKey (messagesJson storedMessages)
    `catchSync` \err ->
      logError [i|Failed to persist thread: #{show err :: String}|]

lookupStoredThreadNode :: (Prim :> es, Storage.Storage :> es) => ThreadStore -> ThreadMessageKey -> Eff es (Maybe StoredThreadNode)
lookupStoredThreadNode store messageKey =
  lookupStoredThreadNodeMaybe store (Just messageKey)

lookupStoredThreadNodeMaybe :: (Prim :> es, Storage.Storage :> es) => ThreadStore -> Maybe ThreadMessageKey -> Eff es (Maybe StoredThreadNode)
lookupStoredThreadNodeMaybe _ Nothing =
  pure Nothing
lookupStoredThreadNodeMaybe store@ThreadStore{unThreadStore = ref} (Just messageKey) = do
  cached <- do
    threadState <- readIORef ref
    pure do
      treeNode <- lookupThreadNode messageKey threadState.threadTree
      threadStorageId <- Map.lookup messageKey threadState.threadIds
      pure StoredThreadNode{threadStorageId, treeNode}
  case cached of
    Just node ->
      pure (Just node)
    Nothing ->
      loadThreadNodeFromStorage store [] messageKey

loadThreadNodeFromStorage :: (Prim :> es, Storage.Storage :> es) => ThreadStore -> [ThreadMessageKey] -> ThreadMessageKey -> Eff es (Maybe StoredThreadNode)
loadThreadNodeFromStorage store@ThreadStore{unThreadStore = ref} visited messageKey
  | messageKey `elem` visited =
      pure Nothing
  | otherwise = do
      row <- loadThreadRow messageKey
      case row >>= decodeStoredThread of
        Nothing ->
          pure Nothing
        Just stored -> do
          parentNode <- case stored.storedParentMessageKey of
            Nothing ->
              pure Nothing
            Just parentMessageKey ->
              lookupStoredThreadNodeMaybe store (Just parentMessageKey)
                >>= maybe (loadThreadNodeFromStorage store (messageKey : visited) parentMessageKey) (pure . Just)
          let node = StoredThreadNode
                { threadStorageId = stored.storedThreadStorageId
                , treeNode = ThreadNode
                    { messageKey = messageKey
                    , parentMessageKey = stored.storedParentMessageKey
                    , transcript = storedTranscriptFromMessages parentNode stored.storedMessages
                    }
                }
          atomicModifyIORef' ref \threadState ->
            (cacheThreadNode messageKey node threadState, ())
          pure (Just node)

data StoredThread = StoredThread
  { storedThreadStorageId :: !Integer
  , storedParentMessageKey :: !(Maybe ThreadMessageKey)
  , storedMessages :: ![LLM.ChatMessage]
  }

decodeStoredThread :: ThreadRow -> Maybe StoredThread
decodeStoredThread row = do
  messages <- decodeMessages row.messagesJson
  let threadStorageId = fromMaybe 0 row.threadStorageId
  pure StoredThread{storedThreadStorageId = threadStorageId, storedParentMessageKey = row.parentMessageKey, storedMessages = messages}

storedTranscriptFromMessages :: Maybe StoredThreadNode -> [LLM.ChatMessage] -> Transcript
storedTranscriptFromMessages parentNode messages =
  case parentNode of
    Nothing ->
      Transcript (Seq.fromList messages)
    Just parent ->
      Transcript (parent.treeNode.transcript.messages <> Seq.fromList messages)

decodeMessages :: Text -> Maybe [LLM.ChatMessage]
decodeMessages =
  either (const Nothing) Just . Aeson.eitherDecodeStrict' . TextEncoding.encodeUtf8

ensureThreadTable :: Storage.Storage :> es => Eff es ()
ensureThreadTable =
  runSelda (tryCreateTable threadRows)

loadThreadRows :: Storage.Storage :> es => Eff es [ThreadRow]
loadThreadRows = do
  ensureThreadTable
  rows <- runSelda $
    query do
      row <- select threadRows
      order (row ! #id) ascending
      pure row
  pure (map threadRowFromStorage rows)

loadThreadRow :: Storage.Storage :> es => ThreadMessageKey -> Eff es (Maybe ThreadRow)
loadThreadRow targetMessageKey = do
  ensureThreadTable
  rows <- runSelda $
    query $
      queryLimit 0 1 do
        row <- select threadRows
        restrict (threadKeyMatches targetMessageKey row)
        pure row
  pure (threadRowFromStorage <$> viaNonEmpty head rows)

loadThreadMessageIdsFromStorage :: Storage.Storage :> es => ThreadMessageKey -> Eff es [MessageId]
loadThreadMessageIdsFromStorage messageKey = do
  target <- loadThreadRow messageKey
  case target >>= (.threadStorageId) of
    Nothing ->
      pure []
    Just targetThreadStorageId -> do
      rows <- runSelda $
        query do
          row <- select threadRows
          restrict (row ! #thread_id .== literal (Just (fromIntegral targetThreadStorageId :: Int.Int64)))
          order (row ! #id) ascending
          pure (row ! #message_id)
      pure (map textMessageId rows)

loadNextThreadStorageId :: Storage.Storage :> es => Eff es Integer
loadNextThreadStorageId = do
  rows <- loadThreadRows
  pure (Foldable.maximum (1 : [fromMaybe 0 row.threadStorageId + 1 | row <- rows]))

saveThreadMessages :: Storage.Storage :> es => ThreadMessageKey -> Integer -> Maybe ThreadMessageKey -> Text -> Eff es ()
saveThreadMessages messageKey threadStorageId parentMessageKey storedMessagesJson = do
  ensureThreadTable
  runSelda do
    deleteFrom_ threadRows \row ->
      threadKeyMatches messageKey row
    insert_
      threadRows
      [ ThreadStorageRow
          { id = def
          , platform_key = chatPlatformKey messageKey.platform
          , chat_id = fromIntegral <$> messageKey.chatId
          , message_id = messageIdText messageKey.messageId
          , thread_id = Just (fromIntegral threadStorageId)
          , parent_chat_id = fromIntegral <$> (parentMessageKey >>= (.chatId))
          , parent_message_id = messageIdText <$> (parentMessageKey <&> (.messageId))
          , messages_json = storedMessagesJson
          }
      ]

threadRowFromStorage :: ThreadStorageRow -> ThreadRow
threadRowFromStorage row =
  let messageKey = ThreadMessageKey{platform = platformFromKey row.platform_key, chatId = fromIntegral <$> row.chat_id, messageId = textMessageId row.message_id}
  in ThreadRow
    { messageKey = messageKey
    , threadStorageId = fromIntegral <$> row.thread_id
    , parentMessageKey = do
        parentMessageId <- textMessageId <$> row.parent_message_id
        pure ThreadMessageKey
          { platform = messageKey.platform
          , chatId = fromIntegral <$> row.parent_chat_id
          , messageId = parentMessageId
          }
    , messagesJson = row.messages_json
    }

threadKeyMatches :: forall (backend :: Type). ThreadMessageKey -> Row backend ThreadStorageRow -> Col backend Bool
threadKeyMatches key row =
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

cacheThreadNode :: ThreadMessageKey -> StoredThreadNode -> ThreadState -> ThreadState
cacheThreadNode messageKey node threadState =
  threadState
    { threadTree = ThreadTree (Map.restrictKeys insertedTree retainedIds)
    , threadIds = Map.restrictKeys insertedIds retainedIds
    , recentThreadIds = retainedOrder
    }
  where
    insertedTree = (insertThreadNode node.treeNode threadState.threadTree).nodes
    insertedIds = Map.insert messageKey node.threadStorageId threadState.threadIds
    nextOrder = messageKey : filter (/= messageKey) threadState.recentThreadIds
    retainedOrder = take maxCachedThreads nextOrder
    retainedIds = Map.keysSet (Map.fromList [(key, ()) | key <- retainedOrder])

maxCachedThreads :: Int
maxCachedThreads =
  4

threadStorageIdFor :: ThreadState -> Maybe StoredThreadNode -> Maybe StoredThreadNode -> Integer
threadStorageIdFor threadState parentNode existingNode =
  fromMaybe threadState.nextThreadStorageId (parentThreadStorageId <|> existingThreadStorageId)
  where
    parentThreadStorageId = (.threadStorageId) <$> parentNode
    existingThreadStorageId = (.threadStorageId) <$> existingNode

messagesJson :: [LLM.ChatMessage] -> Text
messagesJson =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

transcriptMessagesForStorage :: Maybe ThreadMessageKey -> Maybe StoredThreadNode -> Transcript -> (Maybe ThreadMessageKey, [LLM.ChatMessage])
transcriptMessagesForStorage parentMessageKey parentNode transcript =
  case parentNode of
    Just parent
      | Just suffix <- transcriptSuffix parent.treeNode.transcript transcript ->
          (parentMessageKey, suffix)
      | otherwise ->
          (Nothing, transcriptMessagesList transcript)
    Nothing ->
      (parentMessageKey, transcriptMessagesList transcript)

transcriptSuffix :: Transcript -> Transcript -> Maybe [LLM.ChatMessage]
transcriptSuffix parent child
  | parentJson == childPrefixJson =
      Just (drop parentLength childMessages)
  | otherwise =
      Nothing
  where
    parentMessages = transcriptMessagesList parent
    childMessages = transcriptMessagesList child
    parentLength = length parentMessages
    parentJson = map messageJson parentMessages
    childPrefixJson = map messageJson (take parentLength childMessages)

transcriptMessagesList :: Transcript -> [LLM.ChatMessage]
transcriptMessagesList =
  Foldable.toList . (.messages)

messageJson :: LLM.ChatMessage -> LazyByteString.ByteString
messageJson =
  Aeson.encode
