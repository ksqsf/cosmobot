{-|
Module      : Bot.RPC.State
Description : Shared runtime state for RPC sessions and notifications
Stability   : experimental
-}

module Bot.RPC.State
  ( RpcState
  , RpcClientId
  , RpcSessionId (..)
  , RpcOutbound (..)
  , RpcChatMessage (..)
  , RpcChatSession (..)
  , RpcChatAttachmentRef (..)
  , RpcChatSend (..)
  , newRpcState
  , registerClient
  , unregisterClient
  , writeClient
  , broadcast
  , broadcastAuditRecord
  , openChatSession
  , listChatSessions
  , getChatSession
  , chatHistory
  , forkChatSession
  , renameChatSession
  , deleteChatSession
  , enqueueChatMessage
  , incomingMessages
  , rpcChatDriver
  )
where

import qualified Bot.Chat.Types as Chat
import Bot.Chat.Driver.Types
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.RPC.Protocol as Protocol
import qualified Bot.Effect.Storage as StorageEffect
import qualified Bot.Storage.RPC as Storage
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Effectful.Concurrent.STM as STM
import qualified Streaming as S
import qualified Streaming.Prelude as S

type RpcClientId = Integer

newtype RpcSessionId = RpcSessionId { unRpcSessionId :: Text }
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON RpcSessionId where
  toJSON =
    Aeson.String . (.unRpcSessionId)

instance Aeson.FromJSON RpcSessionId where
  parseJSON value =
    RpcSessionId <$> Aeson.parseJSON value

data RpcState = RpcState
  { clients :: !(STM.TVar (Map RpcClientId (STM.TChan Aeson.Value)))
  , nextClientId :: !(STM.TVar RpcClientId)
  , nextSessionNumber :: !(STM.TVar Integer)
  , nextMessageNumber :: !(STM.TVar Integer)
  , inbound :: !(STM.TChan IncomingMessage)
  }

data RpcOutbound = RpcOutbound
  { sessionId :: !RpcSessionId
  , messageId :: !(Maybe MessageId)
  , text :: !Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data RpcChatMessage = RpcChatMessage
  { sessionId :: !RpcSessionId
  , messageId :: !MessageId
  , sender :: !Text
  , text :: !Text
  , imageUrls :: ![Text]
  , replyToMessageId :: !(Maybe MessageId)
  , parentMessageId :: !(Maybe MessageId)
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data RpcChatSession = RpcChatSession
  { sessionId :: !RpcSessionId
  , label :: !(Maybe Text)
  , parentSessionId :: !(Maybe RpcSessionId)
  , parentMessageId :: !(Maybe MessageId)
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data RpcChatAttachmentRef = RpcChatAttachmentRef
  { attachmentId :: !Text
  , kind :: !Text
  , name :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data RpcChatSend = RpcChatSend
  { sessionId :: !RpcSessionId
  , text :: !Text
  , imageUrls :: ![Text]
  , attachments :: ![RpcChatAttachmentRef]
  , replyToMessageId :: !(Maybe MessageId)
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

newRpcState :: Concurrent :> es => Eff es RpcState
newRpcState = STM.atomically do
  clients <- STM.newTVar Map.empty
  nextClientId <- STM.newTVar 1
  nextSessionNumber <- STM.newTVar 1
  nextMessageNumber <- STM.newTVar 1
  inbound <- STM.newTChan
  pure RpcState{clients, nextClientId, nextSessionNumber, nextMessageNumber, inbound}

registerClient :: Concurrent :> es => RpcState -> Eff es (RpcClientId, STM.TChan Aeson.Value)
registerClient rpcState =
  STM.atomically do
    clientId <- STM.readTVar rpcState.nextClientId
    STM.writeTVar rpcState.nextClientId (clientId + 1)
    queue <- STM.newTChan
    STM.modifyTVar' rpcState.clients (Map.insert clientId queue)
    pure (clientId, queue)

unregisterClient :: Concurrent :> es => RpcState -> RpcClientId -> Eff es ()
unregisterClient rpcState clientId =
  STM.atomically $
    STM.modifyTVar' rpcState.clients (Map.delete clientId)

writeClient :: Concurrent :> es => STM.TChan Aeson.Value -> Aeson.Value -> Eff es ()
writeClient queue value =
  STM.atomically (STM.writeTChan queue value)

broadcast :: Concurrent :> es => RpcState -> Aeson.Value -> Eff es ()
broadcast rpcState value = do
  queues <- STM.atomically (Map.elems <$> STM.readTVar rpcState.clients)
  traverse_ (`writeClient` value) queues

broadcastAuditRecord :: Concurrent :> es => RpcState -> Aeson.Value -> Eff es ()
broadcastAuditRecord rpcState recordValue =
  broadcast rpcState (Aeson.toJSON (Protocol.notification "audit.event" recordValue))

openChatSession :: (Concurrent :> es, StorageEffect.Storage :> es) => RpcState -> Maybe Text -> Eff es RpcChatSession
openChatSession rpcState label = do
  session <- Storage.createSession label
  rememberSessionNumber rpcState session.sessionId
  pure (storedSessionToRpc session)

listChatSessions :: StorageEffect.Storage :> es => Eff es [RpcChatSession]
listChatSessions =
  map storedSessionToRpc <$> Storage.listSessions

getChatSession :: StorageEffect.Storage :> es => RpcSessionId -> Eff es (Maybe RpcChatSession)
getChatSession sessionId =
  fmap storedSessionToRpc <$> Storage.loadSession sessionId.unRpcSessionId

chatHistory :: StorageEffect.Storage :> es => RpcSessionId -> Eff es [RpcChatMessage]
chatHistory sessionId =
  map storedMessageToRpc <$> Storage.loadSessionHistory sessionId.unRpcSessionId

forkChatSession :: (Concurrent :> es, StorageEffect.Storage :> es) => RpcState -> RpcSessionId -> MessageId -> Maybe Text -> Eff es (Maybe RpcChatSession)
forkChatSession rpcState sourceSessionId messageId label = do
  session <- Storage.forkSession sourceSessionId.unRpcSessionId messageId label
  traverse_ (rememberSessionNumber rpcState . (.sessionId)) session
  pure (storedSessionToRpc <$> session)

renameChatSession :: StorageEffect.Storage :> es => RpcSessionId -> Text -> Eff es (Maybe RpcChatSession)
renameChatSession sessionId label =
  fmap storedSessionToRpc <$> Storage.renameSession sessionId.unRpcSessionId label

deleteChatSession :: StorageEffect.Storage :> es => RpcSessionId -> Eff es Bool
deleteChatSession sessionId =
  Storage.deleteSession sessionId.unRpcSessionId

enqueueChatMessage :: (Concurrent :> es, StorageEffect.Storage :> es) => RpcState -> RpcChatSend -> Eff es (Maybe IncomingMessage)
enqueueChatMessage rpcState chatSend = do
  stored <- Storage.appendMessage
    chatSend.sessionId.unRpcSessionId
    "user"
    chatSend.text
    chatSend.imageUrls
    chatSend.replyToMessageId
    chatSend.replyToMessageId
  case stored of
    Nothing ->
      pure Nothing
    Just messageRow -> do
      rememberMessageNumber rpcState messageRow.messageId
      let message = rpcIncomingMessage chatSend messageRow.messageId
          chatMessage = storedMessageToRpc messageRow
      STM.atomically (STM.writeTChan rpcState.inbound message)
      broadcast rpcState (Aeson.toJSON (Protocol.notification "chat.message" chatMessage))
      pure (Just message)

incomingMessages :: Concurrent :> es => RpcState -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages rpcState = forever do
  message <- S.lift (STM.atomically (STM.readTChan rpcState.inbound))
  S.yield message

rpcChatDriver :: (Concurrent :> es, IOE :> es, StorageEffect.Storage :> es) => RpcState -> ChatPlatformDriver es
rpcChatDriver rpcState = driver
  where
    driver = ChatPlatformDriver
      { platform = PlatformRPC
      , replyTo = \message body -> do
          let sessionId = sessionIdFromMessage message
              parentMessageId = message.messageId
          stored <- Storage.appendMessage
            sessionId.unRpcSessionId
            "assistant"
            body
            []
            parentMessageId
            parentMessageId
          case stored of
            Nothing ->
              pure Nothing
            Just storedReply -> do
              rememberMessageNumber rpcState storedReply.messageId
              broadcast rpcState (Aeson.toJSON (Protocol.notification "chat.message" (storedMessageToRpc storedReply)))
              pure (Just storedReply.messageId)
      , replyAudio = \message audioRef caption -> do
          let body = maybe audioRef (\c -> c <> "\n" <> audioRef) caption
          Right <$> driver.replyTo message body
      , uploadFile = \message path -> do
          sent <- driver.replyTo message ("Uploaded file: " <> Text.pack path)
          pure (Right sent)
      , editMessage = \message messageId body -> do
          let sessionId = sessionIdFromMessage message
              payload = RpcOutbound sessionId (Just messageId) body
          updated <- Storage.updateMessageText sessionId.unRpcSessionId messageId body
          broadcast rpcState (Aeson.toJSON (Protocol.notification "chat.message_update" payload))
          pure updated
      , deleteMessage = \_ _ -> pure False
      , replyStreamStyle = \_ -> pure (Chat.EditableReply 1200 4000)
      , getMessageContent = \_ _ -> pure Nothing
      , getSenderMemberInfo = \_ -> pure Nothing
      , getMemberInfo = \_ _ -> pure Nothing
      , getUserAvatar = \_ _ -> pure Nothing
      , listGroupMembers = \_ -> pure Nothing
      , mentionUser = \message _ body -> driver.replyTo message body
      , setMemberTitle = \_ _ _ -> pure False
      }

rpcIncomingMessage :: RpcChatSend -> MessageId -> IncomingMessage
rpcIncomingMessage chatSend messageId =
  let sessionText = chatSend.sessionId.unRpcSessionId
  in IncomingMessage
    { platform = PlatformRPC
    , kind = ChatPrivate
    , chatId = Nothing
    , chatAliases = [sessionText]
    , digest = MessageDigest
        { chatIsAllowed = True
        , senderIsAllowed = True
        , senderIsSuperuser = True
        , mentionsBot = True
        , botId = Just "rpc"
        }
    , senderId = Just "rpc-user"
    , senderUsername = Just "RPC"
    , messageId = Just messageId
    , replyToMessageId = chatSend.replyToMessageId
    , mentions = []
    , mentionUsernames = []
    , imageUrls = chatSend.imageUrls
    , text = chatSend.text
    , raw = Aeson.toJSON chatSend
    }

rememberMessageNumber :: Concurrent :> es => RpcState -> MessageId -> Eff es ()
rememberMessageNumber rpcState messageId =
  case Text.stripPrefix "rpc-" (messageIdText messageId) >>= readMaybe . Text.unpack of
    Nothing ->
      pure ()
    Just number ->
      STM.atomically $
        STM.modifyTVar' rpcState.nextMessageNumber (max (number + 1))

rememberSessionNumber :: Concurrent :> es => RpcState -> Text -> Eff es ()
rememberSessionNumber rpcState sessionId =
  case readMaybe . Text.unpack =<< viaNonEmpty last (Text.splitOn "-" sessionId) of
    Nothing ->
      pure ()
    Just number ->
      STM.atomically $
        STM.modifyTVar' rpcState.nextSessionNumber (max (number + 1))

sessionIdFromMessage :: IncomingMessage -> RpcSessionId
sessionIdFromMessage message =
  RpcSessionId (fromMaybe "session" (listToMaybe message.chatAliases))

storedSessionToRpc :: Storage.StoredChatSession -> RpcChatSession
storedSessionToRpc session =
  RpcChatSession
    { sessionId = RpcSessionId session.sessionId
    , label = session.label
    , parentSessionId = RpcSessionId <$> session.parentSessionId
    , parentMessageId = session.parentMessageId
    }

storedMessageToRpc :: Storage.StoredChatMessage -> RpcChatMessage
storedMessageToRpc message =
  RpcChatMessage
    { sessionId = RpcSessionId message.sessionId
    , messageId = message.messageId
    , sender = message.sender
    , text = message.text
    , imageUrls = message.imageUrls
    , replyToMessageId = message.replyToMessageId
    , parentMessageId = message.parentMessageId
    }
