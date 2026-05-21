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
  , RpcChatSend (..)
  , newRpcState
  , registerClient
  , unregisterClient
  , writeClient
  , broadcast
  , broadcastAuditRecord
  , openChatSession
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
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Effectful.Concurrent.STM as STM
import qualified Streaming as S
import qualified Streaming.Prelude as S

type RpcClientId = Integer

newtype RpcSessionId = RpcSessionId { unRpcSessionId :: Text }
  deriving (Eq, Ord, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

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
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data RpcChatSend = RpcChatSend
  { sessionId :: !RpcSessionId
  , text :: !Text
  , imageUrls :: ![Text]
  , replyToMessageId :: !(Maybe MessageId)
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

newRpcState :: Concurrent :> es => Eff es RpcState
newRpcState = STM.atomically do
  clients <- STM.newTVar Map.empty
  nextClientId <- STM.newTVar 1
  nextSessionNumber <- STM.newTVar 1
  nextMessageNumber <- STM.newTVar 1
  inbound <- STM.newTChan
  pure RpcState{clients, nextClientId, nextSessionNumber, nextMessageNumber, inbound}

registerClient :: Concurrent :> es => RpcState -> Eff es (RpcClientId, STM.TChan Aeson.Value)
registerClient state =
  STM.atomically do
    clientId <- STM.readTVar state.nextClientId
    STM.writeTVar state.nextClientId (clientId + 1)
    queue <- STM.newTChan
    STM.modifyTVar' state.clients (Map.insert clientId queue)
    pure (clientId, queue)

unregisterClient :: Concurrent :> es => RpcState -> RpcClientId -> Eff es ()
unregisterClient state clientId =
  STM.atomically $
    STM.modifyTVar' state.clients (Map.delete clientId)

writeClient :: Concurrent :> es => STM.TChan Aeson.Value -> Aeson.Value -> Eff es ()
writeClient queue value =
  STM.atomically (STM.writeTChan queue value)

broadcast :: Concurrent :> es => RpcState -> Aeson.Value -> Eff es ()
broadcast state value = do
  queues <- STM.atomically (Map.elems <$> STM.readTVar state.clients)
  traverse_ (`writeClient` value) queues

broadcastAuditRecord :: Concurrent :> es => RpcState -> Aeson.Value -> Eff es ()
broadcastAuditRecord state recordValue =
  broadcast state (Aeson.toJSON (Protocol.notification "audit.event" recordValue))

openChatSession :: Concurrent :> es => RpcState -> Maybe Text -> Eff es RpcSessionId
openChatSession state label =
  STM.atomically do
    number <- STM.readTVar state.nextSessionNumber
    STM.writeTVar state.nextSessionNumber (number + 1)
    let base = fromMaybe "session" (label >>= nonEmptyText)
    pure (RpcSessionId (base <> "-" <> show number))

enqueueChatMessage :: Concurrent :> es => RpcState -> RpcChatSend -> Eff es IncomingMessage
enqueueChatMessage state send = do
  message <- rpcIncomingMessage state send
  STM.atomically (STM.writeTChan state.inbound message)
  broadcast state (Aeson.toJSON (Protocol.notification "chat.message" (incomingToChatMessage "user" message send.sessionId)))
  pure message

incomingMessages :: Concurrent :> es => RpcState -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages state = forever do
  message <- S.lift (STM.atomically (STM.readTChan state.inbound))
  S.yield message

rpcChatDriver :: (Concurrent :> es, IOE :> es) => RpcState -> ChatPlatformDriver es
rpcChatDriver state = driver
  where
    driver = ChatPlatformDriver
      { platform = PlatformRPC
      , replyTo = \message body -> do
          messageId <- nextRpcMessageId state
          let sessionId = sessionIdFromMessage message
              payload = RpcOutbound sessionId (Just messageId) body
          broadcast state (Aeson.toJSON (Protocol.notification "chat.message" payload))
          pure (Just messageId)
      , replyAudio = \message audioRef caption -> do
          let body = maybe audioRef (\c -> c <> "\n" <> audioRef) caption
          Right <$> driver.replyTo message body
      , uploadFile = \message path -> do
          sent <- driver.replyTo message ("Uploaded file: " <> Text.pack path)
          pure (Right sent)
      , editMessage = \message messageId body -> do
          let payload = RpcOutbound (sessionIdFromMessage message) (Just messageId) body
          broadcast state (Aeson.toJSON (Protocol.notification "chat.message_update" payload))
          pure True
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

rpcIncomingMessage :: Concurrent :> es => RpcState -> RpcChatSend -> Eff es IncomingMessage
rpcIncomingMessage state send = do
  messageId <- nextRpcMessageId state
  let sessionText = send.sessionId.unRpcSessionId
  pure IncomingMessage
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
    , replyToMessageId = send.replyToMessageId
    , mentions = []
    , mentionUsernames = []
    , imageUrls = send.imageUrls
    , text = send.text
    , raw = Aeson.toJSON send
    }

incomingToChatMessage :: Text -> IncomingMessage -> RpcSessionId -> RpcChatMessage
incomingToChatMessage sender message sessionId =
  RpcChatMessage
    { sessionId
    , messageId = fromMaybe "unknown" message.messageId
    , sender
    , text = message.text
    , imageUrls = message.imageUrls
    , replyToMessageId = message.replyToMessageId
    }

nextRpcMessageId :: Concurrent :> es => RpcState -> Eff es MessageId
nextRpcMessageId state =
  STM.atomically do
    number <- STM.readTVar state.nextMessageNumber
    STM.writeTVar state.nextMessageNumber (number + 1)
    pure (textMessageId ("rpc-" <> show number))

sessionIdFromMessage :: IncomingMessage -> RpcSessionId
sessionIdFromMessage message =
  RpcSessionId (fromMaybe "session" (listToMaybe message.chatAliases))

nonEmptyText :: Text -> Maybe Text
nonEmptyText value =
  let stripped = Text.strip value
  in stripped <$ guard (not (Text.null stripped))
