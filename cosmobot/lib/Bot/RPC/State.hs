{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.RPC.State
Description : Shared runtime state for RPC sessions and notifications
Stability   : experimental
-}

module Bot.RPC.State
  ( RpcState
  , RpcClientId
  , RpcClient
  , RpcClientEvent (..)
  , RpcTopic (..)
  , RpcSessionId
  , RpcOutbound (..)
  , RpcChatMessage
  , RpcChatSession
  , RpcChatAttachmentRef (..)
  , RpcChatSend (..)
  , unRpcSessionId
  , newRpcState
  , registerClient
  , unregisterClient
  , readClient
  , writeClient
  , writeClientAndWait
  , subscribe
  , unsubscribe
  , publish
  , broadcastAuditRecord
  , openChatSession
  , countChatSessions
  , listChatSessions
  , getChatSession
  , chatHistoryPage
  , forkChatSession
  , renameChatSession
  , deleteChatSession
  , enqueueChatMessage
  , incomingMessages
  , sessionIdFromMessage
  , storedMessageToRpc
  , storedMediaRef
  , parseMediaId
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Media as Media
import Bot.Prelude
import qualified Bot.JSONRPC as RPC
import qualified Bot.Session as Session
import qualified Bot.Effect.Storage as StorageEffect
import qualified Bot.Storage.Session as Storage
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Effectful.Concurrent.STM as STM
import qualified Effectful.FileSystem as FileSystem
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Prelude (Show (showsPrec))
import qualified Prelude

type RpcClientId = Integer

data RpcClient = RpcClient
  { events :: !(STM.TBQueue RpcClientEvent)
  , subscriptions :: !(STM.TVar (Set RpcTopic))
  }

data RpcClientEvent
  = RpcClientSend !Aeson.Value
  | RpcClientSendAndAck !Aeson.Value !(STM.TMVar ())
  | RpcClientDisconnect !Text

instance Eq RpcClientEvent where
  RpcClientSend left == RpcClientSend right = left == right
  RpcClientSendAndAck left _ == RpcClientSendAndAck right _ = left == right
  RpcClientDisconnect left == RpcClientDisconnect right = left == right
  _ == _ = False

instance Show RpcClientEvent where
  showsPrec _ = \case
    RpcClientSend value -> Prelude.showString "RpcClientSend " . showsPrec 11 value
    RpcClientSendAndAck value _ -> Prelude.showString "RpcClientSendAndAck " . showsPrec 11 value
    RpcClientDisconnect reason -> Prelude.showString "RpcClientDisconnect " . showsPrec 11 reason

type RpcSessionId = Session.SessionId

data RpcTopic
  = ChatEvents !RpcSessionId
  | AuditEvents
  | SystemEvents
  deriving (Eq, Ord, Show)

unRpcSessionId :: RpcSessionId -> Text
unRpcSessionId =
  Session.sessionIdText

data RpcState = RpcState
  { clients :: !(STM.TVar (Map RpcClientId RpcClient))
  , nextClientId :: !(STM.TVar RpcClientId)
  , inbound :: !(STM.TChan IncomingMessage)
  }

data RpcOutbound = RpcOutbound
  { sessionId :: !RpcSessionId
  , messageId :: !(Maybe MessageId)
  , text :: !Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

type RpcChatMessage = Session.SessionMessage

type RpcChatSession = Session.Session

data RpcChatAttachmentRef = RpcChatAttachmentRef
  { attachmentId :: !Text
  , name :: !Text
  , mediaType :: !Text
  , kind :: !Text
  , size :: !Int
  , url :: !Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

instance Aeson.FromJSON RpcChatAttachmentRef where
  parseJSON =
    Aeson.withObject "RPC chat attachment" \o -> do
      attachmentId <- o Aeson..: "attachmentId" <|> o Aeson..: "attachment_id" <|> o Aeson..: "id"
      name <- fromMaybe attachmentId <$> o Aeson..:? "name"
      mediaType <- fromMaybe "application/octet-stream" <$> (o Aeson..:? "mediaType" >>= \case
        Just value -> pure (Just value)
        Nothing -> o Aeson..:? "media_type")
      kind <- fromMaybe (kindFromMediaType mediaType) <$> o Aeson..:? "kind"
      size <- fromMaybe 0 <$> o Aeson..:? "size"
      url <- fromMaybe (defaultMediaUrl attachmentId) <$> o Aeson..:? "url"
      pure RpcChatAttachmentRef{attachmentId, name, mediaType, kind, size, url}

data RpcChatSend = RpcChatSend
  { sessionId :: !RpcSessionId
  , text :: !Text
  , imageUrls :: ![Text]
  , attachments :: ![RpcChatAttachmentRef]
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

newRpcState :: Concurrent :> es => Eff es RpcState
newRpcState = STM.atomically do
  clients <- STM.newTVar Map.empty
  nextClientId <- STM.newTVar 1
  inbound <- STM.newTChan
  pure RpcState{clients, nextClientId, inbound}

registerClient :: Concurrent :> es => RpcState -> Eff es (RpcClientId, RpcClient)
registerClient rpcState =
  STM.atomically do
    clientId <- STM.readTVar rpcState.nextClientId
    STM.writeTVar rpcState.nextClientId (clientId + 1)
    events <- STM.newTBQueue rpcClientQueueCapacity
    subscriptions <- STM.newTVar Set.empty
    let client = RpcClient{events, subscriptions}
    STM.modifyTVar' rpcState.clients (Map.insert clientId client)
    pure (clientId, client)

unregisterClient :: Concurrent :> es => RpcState -> RpcClientId -> Eff es ()
unregisterClient rpcState clientId =
  STM.atomically $
    STM.modifyTVar' rpcState.clients (Map.delete clientId)

readClient :: Concurrent :> es => RpcClient -> Eff es RpcClientEvent
readClient client =
  STM.atomically (STM.readTBQueue client.events)

writeClient :: Concurrent :> es => RpcClient -> Aeson.Value -> Eff es ()
writeClient client value =
  STM.atomically (STM.writeTBQueue client.events (RpcClientSend value))

writeClientAndWait :: Concurrent :> es => RpcClient -> Aeson.Value -> Eff es ()
writeClientAndWait client value = do
  acknowledgement <- STM.atomically STM.newEmptyTMVar
  STM.atomically (STM.writeTBQueue client.events (RpcClientSendAndAck value acknowledgement))
  STM.atomically (STM.takeTMVar acknowledgement)

subscribe :: Concurrent :> es => RpcClient -> RpcTopic -> Eff es ()
subscribe client topic =
  STM.atomically (STM.modifyTVar' client.subscriptions (Set.insert topic))

unsubscribe :: Concurrent :> es => RpcClient -> RpcTopic -> Eff es ()
unsubscribe client topic =
  STM.atomically (STM.modifyTVar' client.subscriptions (Set.delete topic))

publish :: Concurrent :> es => RpcState -> RpcTopic -> Aeson.Value -> Eff es ()
publish rpcState topic value = do
  STM.atomically do
    clients <- STM.readTVar rpcState.clients
    clients' <- Map.traverseMaybeWithKey (publishClient topic value) clients
    STM.writeTVar rpcState.clients clients'

broadcastAuditRecord :: Concurrent :> es => RpcState -> Aeson.Value -> Eff es ()
broadcastAuditRecord rpcState recordValue =
  publish rpcState AuditEvents (Aeson.toJSON (RPC.notification "audit.event" recordValue))

openChatSession :: StorageEffect.Storage :> es => Maybe Text -> Eff es RpcChatSession
openChatSession =
  Session.openSession

listChatSessions :: StorageEffect.Storage :> es => Eff es [RpcChatSession]
listChatSessions =
  Session.listSessions

countChatSessions :: StorageEffect.Storage :> es => Eff es Int
countChatSessions =
  Session.countSessions

getChatSession :: StorageEffect.Storage :> es => RpcSessionId -> Eff es (Maybe RpcChatSession)
getChatSession sessionId =
  Session.getSession sessionId

chatHistoryPage
  :: StorageEffect.Storage :> es
  => RpcSessionId
  -> Maybe MessageId
  -> Int
  -> Eff es (Either Text ([RpcChatMessage], Bool))
chatHistoryPage =
  Session.sessionHistoryPage

forkChatSession :: StorageEffect.Storage :> es => RpcSessionId -> MessageId -> Maybe Text -> Eff es (Maybe RpcChatSession)
forkChatSession =
  Session.forkSession

renameChatSession :: StorageEffect.Storage :> es => RpcSessionId -> Text -> Eff es (Maybe RpcChatSession)
renameChatSession sessionId label =
  Session.renameSession sessionId label

deleteChatSession :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es) => RpcSessionId -> Eff es Bool
deleteChatSession sessionId =
  Session.deleteSession sessionId

enqueueChatMessage
  :: (Concurrent :> es, StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => RpcState
  -> RpcChatSend
  -> Eff es (Either Text (Maybe IncomingMessage))
enqueueChatMessage rpcState chatSend = do
  appended <- Session.appendUserMessage (rpcChatSendToSession chatSend)
  case appended of
    Left err ->
      pure (Left err)
    Right Nothing ->
      pure (Right Nothing)
    Right (Just sessionMessage) -> do
      message <- rpcIncomingMessage chatSend sessionMessage
      STM.atomically (STM.writeTChan rpcState.inbound message)
      publish rpcState (ChatEvents chatSend.sessionId) (Aeson.toJSON (RPC.notification "chat.message" sessionMessage))
      pure (Right (Just message))

incomingMessages :: Concurrent :> es => RpcState -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages rpcState = forever do
  message <- S.lift (STM.atomically (STM.readTChan rpcState.inbound))
  S.yield message

rpcIncomingMessage :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es) => RpcChatSend -> RpcChatMessage -> Eff es IncomingMessage
rpcIncomingMessage chatSend messageRow = do
  let sessionText = Session.sessionIdText chatSend.sessionId
      canonicalSend :: Session.SessionSend
      canonicalSend =
        Session.SessionSend
          { sessionId = chatSend.sessionId
          , text = chatSend.text
          , imageUrls = messageRow.imageUrls
          , attachments = messageRow.attachments
          , replyToMessageId = Nothing
          }
  llmImageUrls <- Session.sessionSendLlmImageUrls canonicalSend
  pure IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformRPC
    , kind = ChatPrivate
    , chatId = Just (textChatId sessionText)
    , chatAliases = [sessionText]
    , chatDisplayName = Nothing
    , digest = MessageDigest
        { chatIsAllowed = True
        , senderIsAllowed = True
        , senderIsSuperuser = True
        , mentionsBot = True
        , botId = Just "rpc"
        }
    , senderId = Just sessionText
    , senderUsername = Just "RPC"
    , senderDisplayName = Just "RPC"
    , senderGlobalDisplayName = Just "RPC"
    , messageId = Just messageRow.messageId
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = llmImageUrls
    , files = []
    , text = Session.sessionSendContextText canonicalSend
    , raw = Aeson.toJSON canonicalSend
    }

sessionIdFromMessage :: IncomingMessage -> RpcSessionId
sessionIdFromMessage message =
  Session.SessionId (fromMaybe "session" (listToMaybe message.chatAliases))

storedMessageToRpc :: Storage.StoredChatMessage -> RpcChatMessage
storedMessageToRpc =
  Session.storedMessageToSession

publishClient :: RpcTopic -> Aeson.Value -> RpcClientId -> RpcClient -> STM.STM (Maybe RpcClient)
publishClient topic value _clientId client = do
  subscriptions <- STM.readTVar client.subscriptions
  if not (subscribedTo topic subscriptions)
    then pure (Just client)
    else do
      full <- STM.isFullTBQueue client.events
      if full
        then do
          drainTBQueue client.events
          STM.writeTBQueue client.events (RpcClientDisconnect "RPC notification queue overflow")
          pure Nothing
        else do
          STM.writeTBQueue client.events (RpcClientSend value)
          pure (Just client)

subscribedTo :: RpcTopic -> Set RpcTopic -> Bool
subscribedTo topic subscriptions =
  Set.member topic subscriptions
    || (isChatTopic topic && Set.member SystemEvents subscriptions)
  where
    isChatTopic ChatEvents{} = True
    isChatTopic _ = False

drainTBQueue :: STM.TBQueue a -> STM.STM ()
drainTBQueue queue =
  STM.tryReadTBQueue queue >>= \case
    Nothing ->
      pure ()
    Just _ ->
      drainTBQueue queue

rpcClientQueueCapacity :: Natural
rpcClientQueueCapacity = 256

storedMediaRef :: Media.MediaFileInfo -> Text -> Storage.StoredMediaRef
storedMediaRef =
  Session.storedMediaRef

parseMediaId :: Text -> Maybe Text
parseMediaId =
  Session.parseMediaId

rpcChatSendToSession :: RpcChatSend -> Session.SessionSend
rpcChatSendToSession chatSend =
  Session.SessionSend
    { sessionId = chatSend.sessionId
    , text = chatSend.text
    , imageUrls = chatSend.imageUrls
    , attachments = map rpcAttachmentToSession chatSend.attachments
    , replyToMessageId = Nothing
    }

rpcAttachmentToSession :: RpcChatAttachmentRef -> Session.SessionAttachmentRef
rpcAttachmentToSession attachment =
  Session.SessionAttachmentRef
    { attachmentId = attachment.attachmentId
    , name = attachment.name
    , mediaType = attachment.mediaType
    , kind = attachment.kind
    , size = attachment.size
    , url = attachment.url
    }

defaultMediaUrl :: Text -> Text
defaultMediaUrl attachmentId =
  attachmentId

kindFromMediaType :: Text -> Text
kindFromMediaType mediaType
  | "image/" `Text.isPrefixOf` media = "image"
  | "audio/" `Text.isPrefixOf` media = "audio"
  | otherwise = "file"
  where
    media = Text.toLower mediaType
