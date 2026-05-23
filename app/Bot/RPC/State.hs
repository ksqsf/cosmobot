{-|
Module      : Bot.RPC.State
Description : Shared runtime state for RPC sessions and notifications
Stability   : experimental
-}

module Bot.RPC.State
  ( RpcState
  , RpcClientId
  , RpcClientQueue
  , RpcClientEvent (..)
  , RpcSessionId (..)
  , RpcOutbound (..)
  , RpcChatMessage (..)
  , RpcChatSession (..)
  , RpcChatAttachmentRef (..)
  , RpcChatSend (..)
  , newRpcState
  , registerClient
  , unregisterClient
  , readClient
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
import qualified Bot.Effect.Media as Media
import Bot.Effect.Media (MediaObject (..))
import qualified Bot.Core.ReplyBody as ReplyBody
import Bot.Prelude
import qualified Bot.RPC.Config as Config
import qualified Bot.RPC.Protocol as Protocol
import qualified Bot.Effect.Storage as StorageEffect
import qualified Bot.Storage.RPC as Storage
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.STM as STM
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.FilePath (takeExtension, takeFileName)

type RpcClientId = Integer

newtype RpcClientQueue = RpcClientQueue (STM.TBQueue RpcClientEvent)

data RpcClientEvent
  = RpcClientSend !Aeson.Value
  | RpcClientDisconnect !Text
  deriving (Eq, Show)

newtype RpcSessionId = RpcSessionId { unRpcSessionId :: Text }
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON RpcSessionId where
  toJSON =
    Aeson.String . (.unRpcSessionId)

instance Aeson.FromJSON RpcSessionId where
  parseJSON value =
    RpcSessionId <$> Aeson.parseJSON value

data RpcState = RpcState
  { clients :: !(STM.TVar (Map RpcClientId RpcClientQueue))
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
  , attachments :: ![RpcChatAttachmentRef]
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

registerClient :: Concurrent :> es => RpcState -> Eff es (RpcClientId, RpcClientQueue)
registerClient rpcState =
  STM.atomically do
    clientId <- STM.readTVar rpcState.nextClientId
    STM.writeTVar rpcState.nextClientId (clientId + 1)
    queue <- RpcClientQueue <$> STM.newTBQueue rpcClientQueueCapacity
    STM.modifyTVar' rpcState.clients (Map.insert clientId queue)
    pure (clientId, queue)

unregisterClient :: Concurrent :> es => RpcState -> RpcClientId -> Eff es ()
unregisterClient rpcState clientId =
  STM.atomically $
    STM.modifyTVar' rpcState.clients (Map.delete clientId)

readClient :: Concurrent :> es => RpcClientQueue -> Eff es RpcClientEvent
readClient (RpcClientQueue queue) =
  STM.atomically (STM.readTBQueue queue)

writeClient :: Concurrent :> es => RpcClientQueue -> Aeson.Value -> Eff es ()
writeClient (RpcClientQueue queue) value =
  STM.atomically (STM.writeTBQueue queue (RpcClientSend value))

broadcast :: Concurrent :> es => RpcState -> Aeson.Value -> Eff es ()
broadcast rpcState value = do
  STM.atomically do
    clients <- STM.readTVar rpcState.clients
    clients' <- Map.traverseMaybeWithKey (broadcastClient value) clients
    STM.writeTVar rpcState.clients clients'

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

deleteChatSession :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es) => RpcSessionId -> Eff es Bool
deleteChatSession sessionId =
  Storage.deleteSession sessionId.unRpcSessionId

enqueueChatMessage
  :: (Concurrent :> es, StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => RpcState
  -> RpcChatSend
  -> Eff es (Either Text (Maybe IncomingMessage))
enqueueChatMessage rpcState chatSend = do
  attachments <- resolveMediaRefs (map (.attachmentId) chatSend.attachments)
  case attachments of
    Left err ->
      pure (Left err)
    Right mediaRefs -> do
      stored <- Storage.appendMessage
        chatSend.sessionId.unRpcSessionId
        "user"
        chatSend.text
        chatSend.imageUrls
        mediaRefs
        chatSend.replyToMessageId
        chatSend.replyToMessageId
      case stored of
        Left err ->
          pure (Left err)
        Right Nothing ->
          pure (Right Nothing)
        Right (Just messageRow) -> do
          rememberMessageNumber rpcState messageRow.messageId
          message <- rpcIncomingMessage chatSend messageRow
          let chatMessage = storedMessageToRpc messageRow
          STM.atomically (STM.writeTChan rpcState.inbound message)
          broadcast rpcState (Aeson.toJSON (Protocol.notification "chat.message" chatMessage))
          pure (Right (Just message))

incomingMessages :: Concurrent :> es => RpcState -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages rpcState = forever do
  message <- S.lift (STM.atomically (STM.readTChan rpcState.inbound))
  S.yield message

rpcChatDriver :: (Concurrent :> es, IOE :> es, StorageEffect.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es) => Config.Config -> RpcState -> ChatPlatformDriver es
rpcChatDriver cfg rpcState = driver
  where
    driver = ChatPlatformDriver
      { platform = PlatformRPC
      , replyTo = \message body -> do
          let sessionId = sessionIdFromMessage message
              parentMessageId = message.messageId
          reply <- rpcReplyContent cfg body
          stored <- Storage.appendMessage
            sessionId.unRpcSessionId
            "assistant"
            reply.text
            reply.imageUrls
            reply.attachments
            parentMessageId
            parentMessageId
          case stored of
            Left _ ->
              pure Nothing
            Right Nothing ->
              pure Nothing
            Right (Just storedReply) -> do
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
              text = ReplyBody.renderReplyBody body
              payload = RpcOutbound sessionId (Just messageId) text
          updated <- Storage.updateMessageText sessionId.unRpcSessionId messageId text
          broadcast rpcState (Aeson.toJSON (Protocol.notification "chat.message_update" payload))
          pure updated
      , deleteMessage = \_ _ -> pure False
      , replyStreamStyle = \_ -> pure (Chat.EditableReply 1200 4000)
      , getMessageContent = \_ _ -> pure Nothing
      , getSenderMemberInfo = \_ -> pure Nothing
      , getMemberInfo = \_ _ -> pure Nothing
      , getUserAvatar = \_ _ -> pure Nothing
      , listGroupMembers = \_ -> pure Nothing
      , normalizeMediaRef = pure
      , mentionUser = \message _ body -> driver.replyTo message body
      , setMemberTitle = \_ _ _ -> pure False
      }

data RpcReplyContent = RpcReplyContent
  { text :: !Text
  , imageUrls :: ![Text]
  , attachments :: ![Storage.StoredMediaRef]
  }

rpcReplyContent
  :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => Config.Config
  -> Text
  -> Eff es RpcReplyContent
rpcReplyContent cfg body = do
  converted <- traverse (rpcReplyImage cfg) (ReplyBody.replyImageUrls body)
  pure RpcReplyContent
    { text = ReplyBody.renderReplyBody body
    , imageUrls = [url | Left url <- converted]
    , attachments = [attachment | Right attachment <- converted]
    }

rpcReplyImage
  :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => Config.Config
  -> Text
  -> Eff es (Either Text Storage.StoredMediaRef)
rpcReplyImage _cfg ref =
  case Text.stripPrefix "file://" (Text.strip ref) of
    Nothing ->
      pure (Left ref)
    Just pathText -> do
      let path = Text.unpack pathText
      exists <- FileSystem.doesFileExist path
      if exists
        then do
          bytes <- FileSystemByteString.readFile path
          mediaRef <- Media.storeMediaObject $
            MediaObject
              { bytes
              , mimeType = imageMediaType path
              , sourceName = Just (Text.pack (takeFileName path))
              }
          case mediaRef >>= parseMediaId of
            Nothing ->
              pure (Left ref)
            Just fileId ->
              Media.mediaFileInfo fileId >>= \case
                Nothing -> pure (Left ref)
                Just info -> do
                  url <- Media.publicMediaRef info.ref
                  pure (Right (storedMediaRef info url))
        else
          pure (Left ref)

imageMediaType :: FilePath -> Text
imageMediaType path =
  case Text.toLower (Text.pack (takeExtension path)) of
    ".avif" -> "image/avif"
    ".gif" -> "image/gif"
    ".jpeg" -> "image/jpeg"
    ".jpg" -> "image/jpeg"
    ".png" -> "image/png"
    ".webp" -> "image/webp"
    _ -> "application/octet-stream"

rpcIncomingMessage :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es) => RpcChatSend -> Storage.StoredChatMessage -> Eff es IncomingMessage
rpcIncomingMessage chatSend messageRow = do
  let sessionText = chatSend.sessionId.unRpcSessionId
      canonicalSend :: RpcChatSend
      canonicalSend =
        RpcChatSend
          { sessionId = chatSend.sessionId
          , text = chatSend.text
          , imageUrls = messageRow.imageUrls
          , attachments = map storedAttachmentToRpc messageRow.attachments
          , replyToMessageId = chatSend.replyToMessageId
          }
  llmImageUrls <- rpcChatSendLlmImageUrls canonicalSend
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
    , messageId = Just messageRow.messageId
    , replyToMessageId = chatSend.replyToMessageId
    , mentions = []
    , mentionUsernames = []
    , imageUrls = llmImageUrls
    , text = chatSendContextText canonicalSend
    , raw = Aeson.toJSON canonicalSend
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
    , attachments = map storedAttachmentToRpc message.attachments
    , replyToMessageId = message.replyToMessageId
    , parentMessageId = message.parentMessageId
    }

broadcastClient :: Aeson.Value -> RpcClientId -> RpcClientQueue -> STM.STM (Maybe RpcClientQueue)
broadcastClient value _clientId (RpcClientQueue queue) = do
  full <- STM.isFullTBQueue queue
  if full
    then do
      drainTBQueue queue
      STM.writeTBQueue queue (RpcClientDisconnect "RPC notification queue overflow")
      pure Nothing
    else do
      STM.writeTBQueue queue (RpcClientSend value)
      pure (Just (RpcClientQueue queue))

drainTBQueue :: STM.TBQueue a -> STM.STM ()
drainTBQueue queue =
  STM.tryReadTBQueue queue >>= \case
    Nothing ->
      pure ()
    Just _ ->
      drainTBQueue queue

rpcClientQueueCapacity :: Natural
rpcClientQueueCapacity = 256

rpcChatSendLlmImageUrls :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es) => RpcChatSend -> Eff es [Text]
rpcChatSendLlmImageUrls chatSend = do
  attachmentRefs <- catMaybes <$> traverse attachmentDataImageRef (filter ((== "image") . (.kind)) chatSend.attachments)
  pure (ordNub (filter isDirectLlmImageRef chatSend.imageUrls <> attachmentRefs))

attachmentDataImageRef :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es) => RpcChatAttachmentRef -> Eff es (Maybe Text)
attachmentDataImageRef attachment =
  case parseMediaId attachment.attachmentId of
    Nothing ->
      pure Nothing
    Just fileId ->
      Media.mediaFileInfo fileId >>= \case
        Nothing ->
          pure Nothing
        Just stored ->
          if stored.exists && "image/" `Text.isPrefixOf` Text.toLower stored.mimeType
            then do
              bytes <- FileSystemByteString.readFile stored.path
              pure (Just (dataImageRef stored.mimeType bytes))
            else
              pure Nothing

dataImageRef :: Text -> ByteString -> Text
dataImageRef mediaType bytes =
  "data:" <> mediaType <> ";base64," <> TextEncoding.decodeUtf8 (Base64.encode bytes)

isDirectLlmImageRef :: Text -> Bool
isDirectLlmImageRef ref =
  let stripped = Text.toLower (Text.strip ref)
  in "https://" `Text.isPrefixOf` stripped || "data:image/" `Text.isPrefixOf` stripped

chatSendContextText :: RpcChatSend -> Text
chatSendContextText chatSend
  | null nonImageAttachments =
      chatSend.text
  | Text.null (Text.strip chatSend.text) =
      attachmentContext
  | otherwise =
      chatSend.text <> "\n\n" <> attachmentContext
  where
    nonImageAttachments =
      filter ((/= "image") . (.kind)) chatSend.attachments
    attachmentContext =
      Text.unlines $
        "Attachments:" :
        [ "- " <> attachment.name <> " (" <> attachment.mediaType <> ", " <> attachment.url <> ")"
        | attachment <- nonImageAttachments
        ]

storedAttachmentToRpc :: Storage.StoredMediaRef -> RpcChatAttachmentRef
storedAttachmentToRpc attachment =
  RpcChatAttachmentRef
    { attachmentId = attachment.attachmentId
    , name = attachment.name
    , mediaType = attachment.mediaType
    , kind = attachment.kind
    , size = attachment.size
    , url = attachment.url
    }

storedMediaRef :: Media.MediaFileInfo -> Text -> Storage.StoredMediaRef
storedMediaRef media url =
  Storage.StoredMediaRef
    { attachmentId = media.ref
    , name = fromMaybe media.fileId media.sourceName
    , mediaType = media.mimeType
    , kind = kindFromMediaType media.mimeType
    , size = media.size
    , url
    }

resolveMediaRefs
  :: Media.Media :> es
  => [Text]
  -> Eff es (Either Text [Storage.StoredMediaRef])
resolveMediaRefs =
  fmap sequence . traverse resolveMediaRef . ordNub
  where
    resolveMediaRef ref =
      case parseMediaId ref of
        Nothing ->
          pure (Left [i|Unknown media ref: #{ref}|])
        Just _ ->
          Media.mediaFileInfoByRef ref >>= \case
            Nothing ->
              pure (Left [i|Unknown media ref: #{ref}|])
            Just media -> do
              url <- Media.publicMediaRef media.ref
              pure (Right (storedMediaRef media url))

defaultMediaUrl :: Text -> Text
defaultMediaUrl attachmentId =
  attachmentId

parseMediaId :: Text -> Maybe Text
parseMediaId ref = do
  fileId <- Text.stripPrefix "media:" (Text.strip ref)
  guard (not (Text.null fileId))
  pure fileId

kindFromMediaType :: Text -> Text
kindFromMediaType mediaType
  | "image/" `Text.isPrefixOf` media = "image"
  | "audio/" `Text.isPrefixOf` media = "audio"
  | otherwise = "file"
  where
    media = Text.toLower mediaType
