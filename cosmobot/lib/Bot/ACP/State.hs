{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.ACP.State
Description : Runtime state for ACP sessions and notifications
Stability   : experimental
-}

module Bot.ACP.State
  ( AcpState
  , AcpClientId
  , AcpClientQueue
  , AcpClientEvent (..)
  , AcpSessionId
  , newAcpState
  , registerClient
  , unregisterClient
  , readClient
  , writeClient
  , broadcast
  , openSession
  , deleteSession
  , enqueueUserMessage
  , enqueuePromptMessage
  , incomingMessages
  , sessionIdFromMessage
  , acpSessionIdText
  , PromptCompletion (..)
  , completeSessionPrompt
  , cancelSessionPrompts
  , notifyPromptComplete
  , withPromptWaiter
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import qualified Bot.Session as Session
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Effectful.Concurrent.STM as STM
import qualified Effectful.FileSystem as FileSystem
import qualified Streaming as S
import qualified Streaming.Prelude as S

type AcpClientId = Integer

newtype AcpClientQueue = AcpClientQueue (STM.TBQueue AcpClientEvent)

data AcpClientEvent
  = AcpClientSend !Aeson.Value
  | AcpClientDisconnect !Text
  deriving (Eq, Show)

type AcpSessionId = Session.SessionId

data PromptCompletion
  = PromptCompleted !MessageId
  | PromptCancelled
  deriving (Eq, Show)

data AcpState = AcpState
  { clients :: !(STM.TVar (Map AcpClientId AcpClientQueue))
  , nextClientId :: !(STM.TVar AcpClientId)
  , inbound :: !(STM.TChan IncomingMessage)
  , promptWaiters :: !(STM.TVar (Map AcpSessionId [STM.TMVar PromptCompletion]))
  , activePromptMessages :: !(STM.TVar (Map AcpSessionId [MessageId]))
  }

newAcpState :: Concurrent :> es => Eff es AcpState
newAcpState = STM.atomically do
  clients <- STM.newTVar Map.empty
  nextClientId <- STM.newTVar 1
  inbound <- STM.newTChan
  promptWaiters <- STM.newTVar Map.empty
  activePromptMessages <- STM.newTVar Map.empty
  pure AcpState{clients, nextClientId, inbound, promptWaiters, activePromptMessages}

registerClient :: Concurrent :> es => AcpState -> Eff es (AcpClientId, AcpClientQueue)
registerClient acpState =
  STM.atomically do
    clientId <- STM.readTVar acpState.nextClientId
    STM.writeTVar acpState.nextClientId (clientId + 1)
    queue <- AcpClientQueue <$> STM.newTBQueue acpClientQueueCapacity
    STM.modifyTVar' acpState.clients (Map.insert clientId queue)
    pure (clientId, queue)

unregisterClient :: Concurrent :> es => AcpState -> AcpClientId -> Eff es ()
unregisterClient acpState clientId =
  STM.atomically $
    STM.modifyTVar' acpState.clients (Map.delete clientId)

readClient :: Concurrent :> es => AcpClientQueue -> Eff es AcpClientEvent
readClient (AcpClientQueue queue) =
  STM.atomically (STM.readTBQueue queue)

writeClient :: Concurrent :> es => AcpClientQueue -> Aeson.Value -> Eff es ()
writeClient (AcpClientQueue queue) value =
  STM.atomically (STM.writeTBQueue queue (AcpClientSend value))

broadcast :: Concurrent :> es => AcpState -> Aeson.Value -> Eff es ()
broadcast acpState value =
  STM.atomically do
    clients <- STM.readTVar acpState.clients
    clients' <- Map.traverseMaybeWithKey (broadcastClient value) clients
    STM.writeTVar acpState.clients clients'

openSession :: Storage.Storage :> es => Maybe Text -> Eff es Session.Session
openSession =
  Session.openSession

deleteSession :: (Storage.Storage :> es, FileSystem.FileSystem :> es) => AcpSessionId -> Eff es Bool
deleteSession =
  Session.deleteSession

enqueueUserMessage
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => AcpState
  -> Session.SessionSend
  -> Eff es (Either Text (Maybe IncomingMessage))
enqueueUserMessage acpState sessionSend =
  Session.appendUserMessage sessionSend >>= \case
    Left err ->
      pure (Left err)
    Right Nothing ->
      pure (Right Nothing)
    Right (Just sessionMessage) -> do
      message <- acpIncomingMessage sessionSend sessionMessage
      STM.atomically (STM.writeTChan acpState.inbound message)
      pure (Right (Just message))

enqueuePromptMessage
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => AcpState
  -> Session.SessionSend
  -> Eff es (Either Text (Maybe IncomingMessage))
enqueuePromptMessage acpState sessionSend =
  Session.appendUserMessage sessionSend >>= \case
    Left err ->
      pure (Left err)
    Right Nothing ->
      pure (Right Nothing)
    Right (Just sessionMessage) -> do
      message <- acpIncomingMessage sessionSend sessionMessage
      STM.atomically do
        for_ message.messageId \messageId ->
          registerPromptMessageSTM acpState sessionSend.sessionId messageId
        STM.writeTChan acpState.inbound message
      pure (Right (Just message))

incomingMessages :: Concurrent :> es => AcpState -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages acpState =
  forever do
    message <- S.lift (STM.atomically (STM.readTChan acpState.inbound))
    S.yield message

sessionIdFromMessage :: IncomingMessage -> AcpSessionId
sessionIdFromMessage message =
  Session.SessionId (fromMaybe "session" (listToMaybe message.chatAliases))

acpSessionIdText :: AcpSessionId -> Text
acpSessionIdText =
  Session.sessionIdText

notifyPromptComplete :: Concurrent :> es => AcpState -> AcpSessionId -> MessageId -> Eff es ()
notifyPromptComplete acpState sessionId messageId =
  STM.atomically do
    waiters <- STM.readTVar acpState.promptWaiters
    case Map.lookup sessionId waiters of
      Nothing ->
        pure ()
      Just sessionWaiters ->
        traverse_ (`STM.tryPutTMVar` PromptCompleted messageId) sessionWaiters

completeSessionPrompt :: Concurrent :> es => AcpState -> AcpSessionId -> Eff es ()
completeSessionPrompt acpState sessionId =
  STM.atomically $
    STM.modifyTVar' acpState.activePromptMessages (Map.delete sessionId)

cancelSessionPrompts :: Concurrent :> es => AcpState -> AcpSessionId -> Eff es [MessageId]
cancelSessionPrompts acpState sessionId = do
  STM.atomically do
    messageIds <- Map.findWithDefault [] sessionId <$> STM.readTVar acpState.activePromptMessages
    waiters <- Map.findWithDefault [] sessionId <$> STM.readTVar acpState.promptWaiters
    STM.modifyTVar' acpState.activePromptMessages (Map.delete sessionId)
    traverse_ (`STM.tryPutTMVar` PromptCancelled) waiters
    pure messageIds

withPromptWaiter
  :: (Concurrent :> es, IOE :> es)
  => AcpState
  -> AcpSessionId
  -> Eff es a
  -> Eff es PromptCompletion
withPromptWaiter acpState sessionId action =
  bracket acquire release use
  where
    acquire =
      STM.atomically do
        waiter <- STM.newEmptyTMVar
        STM.modifyTVar' acpState.promptWaiters (Map.insertWith (<>) sessionId [waiter])
        pure waiter
    release waiter =
      STM.atomically $
        STM.modifyTVar' acpState.promptWaiters (Map.update (removeWaiter waiter) sessionId)
    use waiter = do
      _ <- action
      STM.atomically (STM.readTMVar waiter)

removeWaiter :: Eq a => a -> [a] -> Maybe [a]
removeWaiter waiter waiters =
  case filter (/= waiter) waiters of
    [] ->
      Nothing
    remaining ->
      Just remaining

registerPromptMessageSTM :: AcpState -> AcpSessionId -> MessageId -> STM.STM ()
registerPromptMessageSTM acpState sessionId messageId =
  STM.modifyTVar' acpState.activePromptMessages (Map.insertWith (<>) sessionId [messageId])

broadcastClient :: Aeson.Value -> AcpClientId -> AcpClientQueue -> STM.STM (Maybe AcpClientQueue)
broadcastClient value _clientId (AcpClientQueue queue) = do
  full <- STM.isFullTBQueue queue
  if full
    then do
      drainTBQueue queue
      STM.writeTBQueue queue (AcpClientDisconnect "ACP notification queue overflow")
      pure Nothing
    else do
      STM.writeTBQueue queue (AcpClientSend value)
      pure (Just (AcpClientQueue queue))

drainTBQueue :: STM.TBQueue a -> STM.STM ()
drainTBQueue queue =
  STM.tryReadTBQueue queue >>= \case
    Nothing ->
      pure ()
    Just _ ->
      drainTBQueue queue

acpClientQueueCapacity :: Natural
acpClientQueueCapacity =
  256

acpIncomingMessage
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => Session.SessionSend
  -> Session.SessionMessage
  -> Eff es IncomingMessage
acpIncomingMessage sessionSend messageRow = do
  llmImageUrls <- Session.sessionSendLlmImageUrls sessionSend
  pure IncomingMessage
    { platform = PlatformACP
    , kind = ChatPrivate
    , chatId = Nothing
    , chatAliases = [Session.sessionIdText sessionSend.sessionId]
    , digest = MessageDigest
        { chatIsAllowed = True
        , senderIsAllowed = True
        , senderIsSuperuser = True
        , mentionsBot = True
        , botId = Just "acp"
        }
    , senderId = Just "acp-user"
    , senderUsername = Just "ACP"
    , messageId = Just messageRow.messageId
    , replyToMessageId = sessionSend.replyToMessageId
    , mentions = []
    , mentionUsernames = []
    , imageUrls = llmImageUrls
    , text = Session.sessionSendContextText sessionSend
    , raw = Aeson.toJSON sessionSend
    }
