{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.RPC.Server
Description : Local JSON-RPC websocket server
Stability   : experimental
-}

module Bot.RPC.Server
  ( RpcServerCallbacks (..)
  , noRpcServerCallbacks
  , runRpcServer
  , rpcServerApplication
  , rpcServerApp
  , dispatchRpcRequest
  , dispatchRpcRequestWithConfig
  )
where

import Bot.Prelude
import Bot.Core.Message (IncomingMessage (..), MessageId)
import qualified Bot.Effect.Media as Media
import Bot.Effect.Media (MediaObject (..))
import qualified Bot.Effect.Storage as Storage
import qualified Bot.RPC.Config as Config
import qualified Bot.RPC.Protocol as Protocol
import qualified Bot.RPC.State as State
import qualified Bot.Storage.RPC as RpcStorage
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Streaming.ByteString as Q
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.FileSystem as FileSystem
import qualified JSONRPC
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

data RpcServerCallbacks es = RpcServerCallbacks
  { auditMethod :: Protocol.RpcRequest -> Eff es (Maybe (Either Protocol.RpcError Aeson.Value))
  }

data RpcAttachmentUpload = RpcAttachmentUpload
  { name :: !Text
  , mediaType :: !Text
  , kind :: !Text
  , bytes :: !ByteString
  }
  deriving (Eq, Show)

newtype MediaStatsParams = MediaStatsParams
  { limit :: Int
  }

newtype MediaGcParams = MediaGcParams
  { maxAgeSeconds :: Int
  }

newtype RpcClientDisconnected = RpcClientDisconnected Text
  deriving (Show)

instance Exception RpcClientDisconnected

noRpcServerCallbacks :: RpcServerCallbacks es
noRpcServerCallbacks = RpcServerCallbacks
  { auditMethod = \_ -> pure Nothing
  }

runRpcServer
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> Eff es ()
runRpcServer cfg@Config.Config{enabled} rpcState callbacks = do
  if enabled
    then runRpcServer' cfg rpcState callbacks
    else pure ()

runRpcServer'
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> Eff es ()
runRpcServer' cfg rpcState callbacks = do
  let Config.Config{host, port} = cfg
      settings =
        Warp.setHost (fromString host) $
          Warp.setPort port Warp.defaultSettings
  logInfo [i|RPC server listening on #{host}:#{port}; websocket endpoint /rpc|]
  withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
    liftIO $
      Warp.runSettings settings (rpcServerApplication runInIO cfg rpcState callbacks)

rpcServerApplication
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => (forall a. Eff es a -> IO a)
  -> Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> Wai.Application
rpcServerApplication runInIO cfg rpcState callbacks =
  WaiWS.websocketsOr WS.defaultConnectionOptions websocketApp (httpApp runInIO cfg)
  where
    websocketApp pending =
      runInIO (rpcServerApp cfg rpcState callbacks pending)

rpcServerApp
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> WS.PendingConnection
  -> Eff es ()
rpcServerApp cfg rpcState callbacks pending
  | not (requestIsRpcPath (WS.pendingRequest pending)) =
      liftIO $
        WS.rejectRequestWith pending $
          WS.defaultRejectRequest
            { WS.rejectCode = 404
            , WS.rejectMessage = "Not Found"
            , WS.rejectBody = "not found"
            }
  | requestIsAuthorized cfg (WS.pendingRequest pending) = do
      conn <- liftIO (WS.acceptRequest pending)
      serveAcceptedClient cfg rpcState callbacks conn
  | otherwise =
      liftIO $
        WS.rejectRequestWith pending $
          WS.defaultRejectRequest
            { WS.rejectCode = 401
            , WS.rejectMessage = "Unauthorized"
            , WS.rejectBody = "unauthorized"
            }

serveAcceptedClient
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> WS.Connection
  -> Eff es ()
serveAcceptedClient cfg rpcState callbacks conn = do
  (clientId, queue) <- State.registerClient rpcState
  logDebug [i|RPC client #{clientId} connected|]
  (race_
      (writeQueuedFrames queue conn)
      (readRequestFrames cfg rpcState callbacks queue conn)
    `catchSync` \err ->
      logDebug [i|RPC client #{clientId} disconnected: #{displayException err}|])
    `finally` do
      State.unregisterClient rpcState clientId
      logDebug [i|RPC client #{clientId} unregistered|]

writeQueuedFrames
  :: (IOE :> es, Concurrent :> es)
  => State.RpcClientQueue
  -> WS.Connection
  -> Eff es ()
writeQueuedFrames queue conn =
  forever do
    State.readClient queue >>= \case
      State.RpcClientSend value ->
        liftIO (WS.sendTextData conn (Aeson.encode value))
      State.RpcClientDisconnect reason -> do
        liftIO (WS.sendClose conn reason)
        throwIO (RpcClientDisconnected reason)

readRequestFrames
  :: (IOE :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> State.RpcClientQueue
  -> WS.Connection
  -> Eff es ()
readRequestFrames cfg rpcState callbacks queue conn =
  forever do
    bytes <- liftIO (WS.receiveData conn :: IO ByteString)
    response <- case Aeson.eitherDecodeStrict bytes of
      Left err ->
        pure (Just (Protocol.parseErrorResponse (Text.pack err)))
      Right value ->
        case Aeson.fromJSON value of
          Aeson.Success (JSONRPC.RequestMessage request) ->
            Just <$> dispatchRpcRequestWithConfig rpcState cfg callbacks request
          Aeson.Success (JSONRPC.NotificationMessage notification_) -> do
            _ <- dispatchRpcRequestWithConfig rpcState cfg callbacks (notificationToRequest notification_)
            pure Nothing
          Aeson.Error err ->
            pure (Just (Protocol.invalidRequestResponse (Text.pack err)))
          Aeson.Success _ ->
            pure (Just (Protocol.invalidRequestResponse "Expected request or notification"))
    traverse_ (State.writeClient queue . Aeson.toJSON) response

dispatchRpcRequest
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => State.RpcState
  -> RpcServerCallbacks es
  -> Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchRpcRequest rpcState callbacks request =
  dispatchRpcRequestWithConfig rpcState defaultDispatchConfig callbacks request

dispatchRpcRequestWithConfig
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => State.RpcState
  -> Config.Config
  -> RpcServerCallbacks es
  -> Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchRpcRequestWithConfig rpcState cfg callbacks request =
  dispatchRpcRequestUnsafe rpcState cfg callbacks request
    `catchSync` \err ->
      pure $
        Protocol.errorResponse
          (Protocol.requestId request)
          "internal_error"
          [i|RPC request failed: #{exceptionFirstLine err}|]

exceptionFirstLine :: Exception err => err -> Text
exceptionFirstLine =
  Text.takeWhile (/= '\n') . toText . displayException

dispatchRpcRequestUnsafe
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => State.RpcState
  -> Config.Config
  -> RpcServerCallbacks es
  -> Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchRpcRequestUnsafe rpcState _cfg callbacks request =
  case Protocol.requestMethod request of
    "chat.open_session" ->
      dispatchOpenSession rpcState request
    "chat.list_sessions" ->
      dispatchListSessions request
    "chat.get_session" ->
      dispatchGetSession request
    "chat.history" ->
      dispatchHistory request
    "chat.fork" ->
      dispatchFork rpcState request
    "chat.rename_session" ->
      dispatchRenameSession request
    "chat.delete_session" ->
      dispatchDeleteSession request
    "chat.upload_attachment" ->
      dispatchUploadAttachment request
    "chat.send" ->
      dispatchChatSend rpcState request
    "media.resolve_source" ->
      dispatchMediaResolveSource request
    "media.get" ->
      dispatchMediaGet request
    "media.delete" ->
      dispatchMediaDelete request
    "media.stats" ->
      dispatchMediaStats request
    "media.gc" ->
      dispatchMediaGc request
    method
      | "audit." `Text.isPrefixOf` method ->
          dispatchAudit callbacks request
      | otherwise ->
          pure (methodNotFound (Protocol.requestId request) method)

dispatchOpenSession
  :: (Concurrent :> es, Storage.Storage :> es)
  => State.RpcState
  -> Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchOpenSession rpcState request =
  case AesonTypes.parseEither parseOpenSessionParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right label -> do
      session <- State.openChatSession rpcState label
      pure $
        Protocol.successResponse (Protocol.requestId request) $
          Aeson.object
            [ "sessionId" Aeson..= rpcSessionIdText session.sessionId
            , "session" Aeson..= session
            ]

dispatchListSessions
  :: Storage.Storage :> es
  => Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchListSessions request = do
  sessions <- State.listChatSessions
  pure $
    Protocol.successResponse (Protocol.requestId request) $
      Aeson.object ["sessions" Aeson..= sessions]

dispatchGetSession
  :: Storage.Storage :> es
  => Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchGetSession request =
  case AesonTypes.parseEither parseSessionIdParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right sessionId -> do
      session <- State.getChatSession sessionId
      history <- maybe (pure []) (const (State.chatHistory sessionId)) session
      pure $
        Protocol.successResponse (Protocol.requestId request) $
          Aeson.object
            [ "session" Aeson..= session
            , "messages" Aeson..= history
            ]

dispatchHistory
  :: Storage.Storage :> es
  => Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchHistory request =
  case AesonTypes.parseEither parseSessionIdParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right sessionId -> do
      messages <- State.chatHistory sessionId
      pure $
        Protocol.successResponse (Protocol.requestId request) $
          Aeson.object
            [ "sessionId" Aeson..= rpcSessionIdText sessionId
            , "messages" Aeson..= messages
            ]

dispatchFork
  :: (Concurrent :> es, Storage.Storage :> es)
  => State.RpcState
  -> Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchFork rpcState request =
  case AesonTypes.parseEither parseForkParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right (sessionId, messageId, label) -> do
      forked <- State.forkChatSession rpcState sessionId messageId label
      case forked of
        Nothing ->
          pure (Protocol.errorResponse (Protocol.requestId request) "not_found" "Session or message not found")
        Just session ->
          pure $
            Protocol.successResponse (Protocol.requestId request) $
              Aeson.object
                [ "sessionId" Aeson..= rpcSessionIdText session.sessionId
                , "session" Aeson..= session
                ]

dispatchRenameSession
  :: Storage.Storage :> es
  => Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchRenameSession request =
  case AesonTypes.parseEither parseRenameSessionParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right (sessionId, label) -> do
      renamed <- State.renameChatSession sessionId label
      case renamed of
        Nothing ->
          pure (Protocol.errorResponse (Protocol.requestId request) "not_found" "Session not found")
        Just session ->
          pure $
            Protocol.successResponse (Protocol.requestId request) $
              Aeson.object ["session" Aeson..= session]

dispatchDeleteSession
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es)
  => Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchDeleteSession request =
  case AesonTypes.parseEither parseSessionIdParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right sessionId -> do
      deleted <- State.deleteChatSession sessionId
      pure $
        Protocol.successResponse (Protocol.requestId request) $
          Aeson.object
            [ "sessionId" Aeson..= rpcSessionIdText sessionId
            , "deleted" Aeson..= deleted
            ]

dispatchUploadAttachment
  :: Media.Media :> es
  => Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchUploadAttachment request =
  case AesonTypes.parseEither (parseAttachmentUploadParams defaultUploadMaxBytes) (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right upload -> do
      storedRef <- Media.storeMediaObject $
        MediaObject
          { bytes = Q.fromStrict upload.bytes
          , mimeType = upload.mediaType
          , sourceName = Just upload.name
          }
      case storedRef of
        Nothing ->
          pure (Protocol.errorResponse (Protocol.requestId request) "internal_error" "Media storage did not return a media ref")
        Just mediaRef ->
          case parseMediaRef mediaRef of
            Nothing ->
              pure (Protocol.errorResponse (Protocol.requestId request) "internal_error" "Media storage did not return a media ref")
            Just fileId ->
              Media.mediaFileInfo fileId >>= \case
                Nothing ->
                  pure (Protocol.errorResponse (Protocol.requestId request) "internal_error" "Stored media file could not be loaded")
                Just media -> do
                  url <- Media.publicMediaRef mediaRef
                  pure $
                    Protocol.successResponse (Protocol.requestId request) $
                      attachmentResponse upload media url

dispatchChatSend
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => State.RpcState
  -> Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchChatSend rpcState request =
  case AesonTypes.parseEither parseChatSendParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right chatSend -> do
      message <- State.enqueueChatMessage rpcState chatSend
      case message of
        Left err ->
          pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" err)
        Right Nothing ->
          pure (Protocol.errorResponse (Protocol.requestId request) "not_found" "Session not found")
        Right (Just IncomingMessage{messageId}) ->
          pure $
            Protocol.successResponse (Protocol.requestId request) $
              Aeson.object
                [ "sessionId" Aeson..= rpcSessionIdText chatSend.sessionId
                , "messageId" Aeson..= messageId
                ]

dispatchMediaStats
  :: Media.Media :> es
  => Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchMediaStats request = do
  case AesonTypes.parseEither parseMediaStatsParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right params ->
      do
        stats <- Media.mediaCacheStats
        files <- Media.listMediaFiles
        pure $
          Protocol.successResponse (Protocol.requestId request) $
            Aeson.object
              [ "stats" Aeson..= stats
              , "files" Aeson..= take params.limit files
              ]

dispatchMediaResolveSource
  :: Media.Media :> es
  => Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchMediaResolveSource request =
  case AesonTypes.parseEither parseMediaSourceParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right sourceRef -> do
      mediaRef <- Media.mediaRefForSource sourceRef
      pure $
        Protocol.successResponse (Protocol.requestId request) $
          Aeson.object
            [ "sourceRef" Aeson..= sourceRef
            , "mediaId" Aeson..= mediaRef
            , "fileId" Aeson..= (mediaRef >>= parseMediaRef)
            ]

dispatchMediaGet
  :: Media.Media :> es
  => Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchMediaGet request =
  case AesonTypes.parseEither parseMediaIdParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right fileId -> do
      Media.mediaCacheEntry fileId >>= \case
        Nothing ->
          pure (Protocol.errorResponse (Protocol.requestId request) "not_found" [i|Media file not found: #{fileId}|])
        Just entry -> do
          let mediaRef = entry.file.ref
          publicUrl <- Media.publicMediaRef mediaRef
          localPath <- Media.localMediaPath mediaRef
          pure $
            Protocol.successResponse (Protocol.requestId request) $
              mediaEntryResponse entry publicUrl localPath

dispatchMediaDelete
  :: Media.Media :> es
  => Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchMediaDelete request =
  case AesonTypes.parseEither parseMediaIdParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right fileId -> do
      deleted <- Media.deleteMediaFile fileId
      pure $
        Protocol.successResponse (Protocol.requestId request) $
          Aeson.object
            [ "fileId" Aeson..= fileId
            , "mediaId" Aeson..= ("media:" <> fileId)
            , "deleted" Aeson..= deleted
            ]

dispatchMediaGc
  :: (Storage.Storage :> es, Media.Media :> es)
  => Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchMediaGc request =
  case AesonTypes.parseEither parseMediaGcParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right params -> do
      retained <- Set.fromList <$> RpcStorage.referencedMediaFileIds
      deleted <- Media.gcMediaCache params.maxAgeSeconds retained
      pure $
        Protocol.successResponse (Protocol.requestId request) $
          Aeson.object
            [ "deleted" Aeson..= deleted
            , "retainedReferencedFiles" Aeson..= Set.size retained
            ]

dispatchAudit
  :: RpcServerCallbacks es
  -> Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchAudit callbacks request =
  callbacks.auditMethod request >>= \case
    Nothing ->
      pure (methodNotFound (Protocol.requestId request) (Protocol.requestMethod request))
    Just (Left err) ->
      pure (JSONRPC.ErrorMessage (JSONRPC.JSONRPCError JSONRPC.rPC_VERSION (Protocol.requestId request) err))
    Just (Right value) ->
      pure (Protocol.successResponse (Protocol.requestId request) value)

parseOpenSessionParams :: Aeson.Value -> AesonTypes.Parser (Maybe Text)
parseOpenSessionParams =
  Aeson.withObject "chat.open_session params" \o ->
    o Aeson..:? "label"

parseChatSendParams :: Aeson.Value -> AesonTypes.Parser State.RpcChatSend
parseChatSendParams =
  Aeson.withObject "chat.send params" \o -> do
    sessionText <- o Aeson..: "sessionId" <|> o Aeson..: "session_id"
    text <- o Aeson..: "text"
    imageUrls <-
      o Aeson..:? "imageUrls" >>= \case
        Just value -> pure value
        Nothing -> fromMaybe [] <$> o Aeson..:? "image_urls"
    replyToMessageId <-
      o Aeson..:? "replyToMessageId" >>= \case
        Just value -> pure (Just value)
        Nothing -> o Aeson..:? "reply_to_message_id"
    attachments <- fromMaybe [] <$> o Aeson..:? "attachments"
    pure State.RpcChatSend
      { sessionId = State.RpcSessionId sessionText
      , text
      , imageUrls
      , attachments
      , replyToMessageId
      }

parseAttachmentUploadParams :: Int -> Aeson.Value -> AesonTypes.Parser RpcAttachmentUpload
parseAttachmentUploadParams maxBytes =
  Aeson.withObject "chat.upload_attachment params" \o -> do
    name <- fromMaybe "attachment" <$> o Aeson..:? "name"
    mediaType <-
      o Aeson..:? "mediaType" >>= \case
        Just value -> pure value
        Nothing -> fromMaybe "application/octet-stream" <$> o Aeson..:? "media_type"
    let cleanType = cleanMediaType mediaType
    kind <- fromMaybe (kindFromMediaType cleanType) <$> o Aeson..:? "kind"
    expectedSize <- o Aeson..:? "size"
    encodedText <- o Aeson..: "data"
    traverse_ (\size -> when (size > maxBytes) (fail "attachment size exceeds configured limit")) expectedSize
    when (Text.length encodedText > maxBase64Length maxBytes) $
      fail "encoded attachment exceeds configured limit"
    bytes <-
      case Base64.decode (TextEncoding.encodeUtf8 encodedText) of
        Left err -> fail err
        Right decoded -> pure decoded
    when (ByteString.length bytes > maxBytes) $
      fail "attachment size exceeds configured limit"
    traverse_ (\size -> when (size /= ByteString.length bytes) (fail "size does not match decoded attachment bytes")) expectedSize
    pure RpcAttachmentUpload{name, mediaType = cleanType, kind, bytes}

maxBase64Length :: Int -> Int
maxBase64Length maxBytes =
  ((maxBytes + 2) `div` 3) * 4

parseSessionIdParams :: Aeson.Value -> AesonTypes.Parser State.RpcSessionId
parseSessionIdParams =
  Aeson.withObject "session params" \o ->
    State.RpcSessionId <$> (o Aeson..: "sessionId" <|> o Aeson..: "session_id")

parseForkParams :: Aeson.Value -> AesonTypes.Parser (State.RpcSessionId, MessageId, Maybe Text)
parseForkParams =
  Aeson.withObject "chat.fork params" \o -> do
    sessionId <- State.RpcSessionId <$> (o Aeson..: "sessionId" <|> o Aeson..: "session_id")
    messageId <- o Aeson..: "messageId" <|> o Aeson..: "message_id"
    label <- o Aeson..:? "label"
    pure (sessionId, messageId, label)

parseRenameSessionParams :: Aeson.Value -> AesonTypes.Parser (State.RpcSessionId, Text)
parseRenameSessionParams =
  Aeson.withObject "chat.rename_session params" \o -> do
    sessionId <- State.RpcSessionId <$> (o Aeson..: "sessionId" <|> o Aeson..: "session_id")
    label <- o Aeson..: "label"
    pure (sessionId, label)

parseMediaStatsParams :: Aeson.Value -> AesonTypes.Parser MediaStatsParams
parseMediaStatsParams = \case
  Aeson.Null ->
    pure MediaStatsParams{limit = 50}
  value ->
    Aeson.withObject "media.stats params" parse value
  where
    parse o = do
      limit <- fromMaybe 50 <$> o Aeson..:? "limit"
      when (limit < 0) $
        fail "limit must be non-negative"
      pure MediaStatsParams{limit}

parseMediaSourceParams :: Aeson.Value -> AesonTypes.Parser Text
parseMediaSourceParams =
  Aeson.withObject "media.resolve_source params" \o -> do
    sourceRef <-
      o Aeson..:? "sourceRef" >>= \case
        Just value -> pure value
        Nothing ->
          o Aeson..:? "source_ref" >>= \case
            Just value -> pure value
            Nothing -> o Aeson..: "source"
    let clean = Text.strip sourceRef
    when (Text.null clean) $
      fail "sourceRef must be non-empty"
    pure clean

parseMediaIdParams :: Aeson.Value -> AesonTypes.Parser Text
parseMediaIdParams =
  Aeson.withObject "media id params" \o -> do
    ref <-
      o Aeson..:? "mediaId" >>= \case
        Just value -> pure value
        Nothing ->
          o Aeson..:? "media_id" >>= \case
            Just value -> pure value
            Nothing ->
              o Aeson..:? "fileId" >>= \case
                Just value -> pure value
                Nothing -> o Aeson..: "file_id"
    case parseMediaIdOrFileId ref of
      Nothing ->
        fail "mediaId must be media:<file_id> or a non-empty fileId"
      Just fileId ->
        pure fileId

parseMediaGcParams :: Aeson.Value -> AesonTypes.Parser MediaGcParams
parseMediaGcParams = \case
  Aeson.Null ->
    pure MediaGcParams{maxAgeSeconds = 0}
  value ->
    Aeson.withObject "media.gc params" parse value
  where
    parse o = do
      maxAgeSeconds <-
        o Aeson..:? "maxAgeSeconds" >>= \case
          Just value -> pure value
          Nothing -> fromMaybe 0 <$> o Aeson..:? "max_age_seconds"
      when (maxAgeSeconds < 0) $
        fail "maxAgeSeconds must be non-negative"
      pure MediaGcParams{maxAgeSeconds}

methodNotFound :: Protocol.RequestId -> Text -> Protocol.RpcResponse
methodNotFound requestId method =
  Protocol.errorResponse requestId "method_not_found" [i|Unknown RPC method: #{method}|]

notificationToRequest :: Protocol.RpcNotification -> Protocol.RpcRequest
notificationToRequest notification_ =
  JSONRPC.JSONRPCRequest JSONRPC.rPC_VERSION (JSONRPC.RequestId Aeson.Null) notification_.method notification_.params

rpcSessionIdText :: State.RpcSessionId -> Text
rpcSessionIdText (State.RpcSessionId value) =
  value

requestIsAuthorized :: Config.Config -> WS.RequestHead -> Bool
requestIsAuthorized cfg request =
  authorizationBearer request == Just expectedToken
  where
    Config.Config{token} = cfg
    expectedToken = TextEncoding.encodeUtf8 token

requestIsRpcPath :: WS.RequestHead -> Bool
requestIsRpcPath request =
  path == "/rpc"
  where
    (path, _) = ByteString.break (== questionMark) request.requestPath

httpApp :: (forall a. Eff es a -> IO a) -> Config.Config -> Wai.Application
httpApp _runInIO _cfg request respond =
  case (Wai.requestMethod request, Wai.pathInfo request) of
    ("GET", _) ->
      respond $
        textResponse Http.status404 "not found"
    ("HEAD", _) ->
      respond $
        Wai.responseLBS Http.status404 (baseSecurityHeaders []) ""
    _ ->
      respond $
        textResponse Http.status405 "method not allowed"

authorizationBearer :: WS.RequestHead -> Maybe ByteString
authorizationBearer request =
  ByteString.stripPrefix bearerPrefix =<< (snd <$> find ((== "Authorization") . fst) request.requestHeaders)

textResponse :: Http.Status -> LazyByteString.ByteString -> Wai.Response
textResponse status body =
  Wai.responseLBS status (baseSecurityHeaders [("Content-Type", "text/plain; charset=utf-8")]) body

baseSecurityHeaders :: Http.ResponseHeaders -> Http.ResponseHeaders
baseSecurityHeaders headers =
  headers
    <> [ ("Referrer-Policy", "no-referrer")
       , ("X-Content-Type-Options", "nosniff")
       , ("X-Frame-Options", "DENY")
       ]

questionMark :: Word8
questionMark = 63

bearerPrefix :: ByteString
bearerPrefix = "Bearer "

attachmentResponse :: RpcAttachmentUpload -> Media.MediaFileInfo -> Text -> Aeson.Value
attachmentResponse upload media url =
  let mediaRef = media.ref
  in
  Aeson.object
    [ "id" Aeson..= mediaRef
    , "attachmentId" Aeson..= mediaRef
    , "mediaRef" Aeson..= mediaRef
    , "fileId" Aeson..= media.fileId
    , "name" Aeson..= upload.name
    , "mediaType" Aeson..= media.mimeType
    , "media_type" Aeson..= media.mimeType
    , "kind" Aeson..= upload.kind
    , "size" Aeson..= media.size
    , "url" Aeson..= url
    ]

kindFromMediaType :: Text -> Text
kindFromMediaType mediaType
  | "image/" `Text.isPrefixOf` media = "image"
  | "audio/" `Text.isPrefixOf` media = "audio"
  | otherwise = "file"
  where
    media = Text.toLower mediaType

cleanMediaType :: Text -> Text
cleanMediaType value =
  case Text.strip value of
    stripped
      | validMediaType stripped -> stripped
      | otherwise -> "application/octet-stream"

validMediaType :: Text -> Bool
validMediaType value =
  case Text.splitOn "/" value of
    [mainType, subtype] ->
      validToken mainType && validToken subtype
    _ ->
      False

validToken :: Text -> Bool
validToken value =
  not (Text.null value) && Text.all validTokenChar value

validTokenChar :: Char -> Bool
validTokenChar char =
  (char >= 'a' && char <= 'z')
    || (char >= 'A' && char <= 'Z')
    || (char >= '0' && char <= '9')
    || char `elem` ("!#$&^_.+-" :: String)

defaultDispatchConfig :: Config.Config
defaultDispatchConfig =
  Config.toRuntimeConfig Config.defaultFileConfig

defaultUploadMaxBytes :: Int
defaultUploadMaxBytes =
  25 * 1024 * 1024

parseMediaRef :: Text -> Maybe Text
parseMediaRef ref = do
  fileId <- Text.stripPrefix "media:" (Text.strip ref)
  guard (not (Text.null fileId))
  pure fileId

parseMediaIdOrFileId :: Text -> Maybe Text
parseMediaIdOrFileId ref =
  case parseMediaRef ref of
    Just fileId ->
      Just fileId
    Nothing ->
      let fileId = Text.strip ref
      in if Text.null fileId then Nothing else Just fileId

mediaEntryResponse :: Media.MediaCacheEntry -> Text -> Maybe FilePath -> Aeson.Value
mediaEntryResponse entry publicUrl localPath =
  Aeson.object
    [ "mediaId" Aeson..= entry.file.ref
    , "fileId" Aeson..= entry.file.fileId
    , "file" Aeson..= entry.file
    , "sourceRefs" Aeson..= entry.sourceRefs
    , "source_refs" Aeson..= entry.sourceRefs
    , "platformRefs" Aeson..= entry.platformRefs
    , "platform_refs" Aeson..= entry.platformRefs
    , "publicUrl" Aeson..= publicUrl
    , "public_url" Aeson..= publicUrl
    , "localPath" Aeson..= localPath
    , "local_path" Aeson..= localPath
    ]
