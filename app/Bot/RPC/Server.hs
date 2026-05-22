{-|
Module      : Bot.RPC.Server
Description : Local JSON-RPC websocket server
Stability   : experimental
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Bot.Effect.Storage as Storage
import qualified Bot.RPC.Config as Config
import qualified Bot.RPC.Protocol as Protocol
import qualified Bot.RPC.State as State
import qualified Bot.Storage.Attachment as Attachment
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as ByteStringChar8
import Data.Char (isSpace)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
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

newtype RpcClientDisconnected = RpcClientDisconnected Text
  deriving (Show)

instance Exception RpcClientDisconnected

noRpcServerCallbacks :: RpcServerCallbacks es
noRpcServerCallbacks = RpcServerCallbacks
  { auditMethod = \_ -> pure Nothing
  }

runRpcServer
  :: (IOE :> es, Log :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> Eff es ()
runRpcServer cfg@Config.Config{enabled} rpcState callbacks = do
  if enabled
    then runRpcServer' cfg rpcState callbacks
    else pure ()

runRpcServer'
  :: (IOE :> es, Log :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> Eff es ()
runRpcServer' cfg rpcState callbacks = do
  let Config.Config{host, port} = cfg
      settings =
        Warp.setHost (fromString host) $
          Warp.setPort port Warp.defaultSettings
  logInfo_ [i|RPC server listening on #{host}:#{port}; websocket endpoint /rpc; attachment endpoint /attachments/<id>|]
  withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
    liftIO $
      Warp.runSettings settings (rpcServerApplication runInIO cfg rpcState callbacks)

rpcServerApplication
  :: (IOE :> es, Log :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es)
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
  :: (IOE :> es, Log :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es)
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
      conn <- liftIO (acceptRpcRequest pending)
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
  :: (IOE :> es, Log :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> WS.Connection
  -> Eff es ()
serveAcceptedClient cfg rpcState callbacks conn = do
  (clientId, queue) <- State.registerClient rpcState
  logTrace_ [i|RPC client #{clientId} connected|]
  (race_
      (writeQueuedFrames queue conn)
      (readRequestFrames cfg rpcState callbacks queue conn)
    `catchSync` \err ->
      logTrace_ [i|RPC client #{clientId} disconnected: #{displayException err}|])
    `finally` do
      State.unregisterClient rpcState clientId
      logTrace_ [i|RPC client #{clientId} unregistered|]

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
  :: (IOE :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es)
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
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es)
  => State.RpcState
  -> RpcServerCallbacks es
  -> Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchRpcRequest rpcState callbacks request =
  dispatchRpcRequestWithConfig rpcState defaultDispatchConfig callbacks request

dispatchRpcRequestWithConfig
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es)
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
          [i|RPC request failed: #{displayException err}|]

dispatchRpcRequestUnsafe
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es)
  => State.RpcState
  -> Config.Config
  -> RpcServerCallbacks es
  -> Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchRpcRequestUnsafe rpcState cfg callbacks request =
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
      dispatchUploadAttachment cfg request
    "chat.delete_attachment" ->
      dispatchDeleteAttachment request
    "chat.send" ->
      dispatchChatSend rpcState request
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
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es)
  => Config.Config
  -> Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchUploadAttachment cfg request =
  case AesonTypes.parseEither (parseAttachmentUploadParams cfg.attachmentMaxBytes) (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right upload -> do
      stored <- Attachment.storeAttachment (attachmentConfig cfg) upload
      case stored of
        Left err ->
          pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" err)
        Right attachment ->
          pure $
            Protocol.successResponse (Protocol.requestId request) $
              attachmentResponse attachment

dispatchDeleteAttachment
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es)
  => Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchDeleteAttachment request =
  case AesonTypes.parseEither parseAttachmentIdParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right attachmentId -> do
      deleted <- Attachment.deleteUnreferencedAttachment attachmentId
      pure $
        Protocol.successResponse (Protocol.requestId request) $
          Aeson.object
            [ "attachmentId" Aeson..= attachmentId
            , "id" Aeson..= attachmentId
            , "deleted" Aeson..= deleted
            ]

dispatchChatSend
  :: (Concurrent :> es, Storage.Storage :> es)
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

parseAttachmentUploadParams :: Int -> Aeson.Value -> AesonTypes.Parser Attachment.AttachmentUpload
parseAttachmentUploadParams maxBytes =
  Aeson.withObject "chat.upload_attachment params" \o -> do
    name <- fromMaybe "attachment" <$> o Aeson..:? "name"
    mediaType <-
      o Aeson..:? "mediaType" >>= \case
        Just value -> pure value
        Nothing -> fromMaybe "application/octet-stream" <$> o Aeson..:? "media_type"
    kind <- fromMaybe (kindFromMediaType mediaType) <$> o Aeson..:? "kind"
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
    pure Attachment.AttachmentUpload{name, mediaType, kind, bytes}

maxBase64Length :: Int -> Int
maxBase64Length maxBytes =
  ((maxBytes + 2) `div` 3) * 4

parseAttachmentIdParams :: Aeson.Value -> AesonTypes.Parser Text
parseAttachmentIdParams =
  Aeson.withObject "chat.delete_attachment params" \o ->
    o Aeson..: "attachmentId" <|> o Aeson..: "attachment_id" <|> o Aeson..: "id"

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
    || subprotocolAccessToken request == Just expectedToken
  where
    Config.Config{token} = cfg
    expectedToken = TextEncoding.encodeUtf8 token

acceptRpcRequest :: WS.PendingConnection -> IO WS.Connection
acceptRpcRequest pending
  | "cosmobot-rpc" `elem` requestedSubprotocols (WS.pendingRequest pending) =
      WS.acceptRequestWith pending $
        WS.AcceptRequest
          { WS.acceptSubprotocol = Just "cosmobot-rpc"
          , WS.acceptHeaders = []
          }
  | otherwise =
      WS.acceptRequest pending

requestIsRpcPath :: WS.RequestHead -> Bool
requestIsRpcPath request =
  path == "/rpc"
  where
    (path, _) = ByteString.break (== questionMark) request.requestPath

httpApp :: (Storage.Storage :> es, FileSystem.FileSystem :> es) => (forall a. Eff es a -> IO a) -> Config.Config -> Wai.Application
httpApp runInIO cfg request respond =
  case (Wai.requestMethod request, Wai.pathInfo request) of
    ("OPTIONS", ["attachments", _attachmentId]) ->
      respond attachmentPreflightResponse
    ("GET", ["attachments", attachmentId])
      | httpRequestIsAuthorized cfg request ->
          serveAttachment runInIO attachmentId respond
      | otherwise ->
          respond $
            attachmentTextResponse Http.status401 "unauthorized"
    ("GET", _) ->
      respond $
        textResponse Http.status404 "not found"
    ("HEAD", _) ->
      respond $
        Wai.responseLBS Http.status404 (baseSecurityHeaders []) ""
    _ ->
      respond $
        textResponse Http.status405 "method not allowed"

serveAttachment
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es)
  => (forall a. Eff es a -> IO a)
  -> Text
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
serveAttachment runInIO attachmentId respond = do
  attachment <- runInIO (Attachment.loadAttachment attachmentId)
  case attachment of
    Nothing ->
      respond (attachmentTextResponse Http.status404 "not found")
    Just stored -> do
      exists <- runInIO (FileSystem.doesFileExist stored.path)
      if exists
        then
          respond $
            Wai.responseFile
              Http.status200
              ( corsAttachmentHeaders
                  [ ("Content-Type", TextEncoding.encodeUtf8 stored.mediaType)
                  , ("Content-Disposition", "attachment; filename=\"" <> safeHeaderBytes stored.name <> "\"")
                  ]
              )
              stored.path
              Nothing
        else
          respond (attachmentTextResponse Http.status404 "not found")

httpRequestIsAuthorized :: Config.Config -> Wai.Request -> Bool
httpRequestIsAuthorized cfg request =
  httpAuthorizationBearer request == Just expectedToken
  where
    Config.Config{token} = cfg
    expectedToken = TextEncoding.encodeUtf8 token

authorizationBearer :: WS.RequestHead -> Maybe ByteString
authorizationBearer request =
  ByteString.stripPrefix bearerPrefix =<< (snd <$> find ((== "Authorization") . fst) request.requestHeaders)

subprotocolAccessToken :: WS.RequestHead -> Maybe ByteString
subprotocolAccessToken request =
  listToMaybe (mapMaybe decodeProtocolToken (requestedSubprotocols request))

requestedSubprotocols :: WS.RequestHead -> [ByteString]
requestedSubprotocols request =
  case snd <$> find ((== "Sec-WebSocket-Protocol") . fst) request.requestHeaders of
    Nothing ->
      []
    Just value ->
      map stripAsciiByteString (ByteStringChar8.split ',' value)

decodeProtocolToken :: ByteString -> Maybe ByteString
decodeProtocolToken protocol = do
  encoded <- ByteString.stripPrefix "cosmobot-token." protocol
  either (const Nothing) Just (Base64.decode (padBase64Url (ByteString.map fromUrlChar encoded)))
  where

    fromUrlChar char
      | char == 45 = 43
      | char == 95 = 47
      | otherwise = char

padBase64Url :: ByteString -> ByteString
padBase64Url value =
  value <> ByteString.replicate padding 61
  where
    padding =
      case ByteString.length value `mod` 4 of
        0 -> 0
        rest -> 4 - rest

stripAsciiByteString :: ByteString -> ByteString
stripAsciiByteString =
  ByteStringChar8.dropWhile isSpace . ByteStringChar8.dropWhileEnd isSpace

httpAuthorizationBearer :: Wai.Request -> Maybe ByteString
httpAuthorizationBearer request =
  ByteString.stripPrefix bearerPrefix =<< (snd <$> find ((== "Authorization") . fst) (Wai.requestHeaders request))

textResponse :: Http.Status -> LazyByteString.ByteString -> Wai.Response
textResponse status body =
  Wai.responseLBS status (baseSecurityHeaders [("Content-Type", "text/plain; charset=utf-8")]) body

attachmentTextResponse :: Http.Status -> LazyByteString.ByteString -> Wai.Response
attachmentTextResponse status body =
  Wai.responseLBS status (corsAttachmentHeaders [("Content-Type", "text/plain; charset=utf-8")]) body

attachmentPreflightResponse :: Wai.Response
attachmentPreflightResponse =
  Wai.responseLBS Http.status204 (corsAttachmentHeaders []) ""

baseSecurityHeaders :: Http.ResponseHeaders -> Http.ResponseHeaders
baseSecurityHeaders headers =
  headers
    <> [ ("Referrer-Policy", "no-referrer")
       , ("X-Content-Type-Options", "nosniff")
       , ("X-Frame-Options", "DENY")
       ]

attachmentHeaders :: Http.ResponseHeaders -> Http.ResponseHeaders
attachmentHeaders headers =
  baseSecurityHeaders headers
    <> [ ("Cache-Control", "no-store")
       ]

corsAttachmentHeaders :: Http.ResponseHeaders -> Http.ResponseHeaders
corsAttachmentHeaders headers =
  attachmentHeaders headers
    <> [ ("Access-Control-Allow-Origin", "*")
       , ("Access-Control-Allow-Methods", "GET, HEAD, OPTIONS")
       , ("Access-Control-Allow-Headers", "Authorization, Content-Type")
       , ("Access-Control-Max-Age", "600")
       ]

questionMark :: Word8
questionMark = 63

bearerPrefix :: ByteString
bearerPrefix = "Bearer "

attachmentConfig :: Config.Config -> Attachment.AttachmentConfig
attachmentConfig cfg =
  Attachment.AttachmentConfig
    { directory = cfg.attachmentDir
    , maxBytes = cfg.attachmentMaxBytes
    }

attachmentResponse :: Attachment.StoredAttachmentRef -> Aeson.Value
attachmentResponse attachment =
  Aeson.object
    [ "id" Aeson..= attachment.attachmentId
    , "attachmentId" Aeson..= attachment.attachmentId
    , "name" Aeson..= attachment.name
    , "mediaType" Aeson..= attachment.mediaType
    , "media_type" Aeson..= attachment.mediaType
    , "kind" Aeson..= attachment.kind
    , "size" Aeson..= attachment.size
    , "url" Aeson..= attachment.url
    ]

kindFromMediaType :: Text -> Text
kindFromMediaType mediaType
  | "image/" `Text.isPrefixOf` media = "image"
  | "audio/" `Text.isPrefixOf` media = "audio"
  | otherwise = "file"
  where
    media = Text.toLower mediaType

safeHeaderBytes :: Text -> ByteString
safeHeaderBytes =
  TextEncoding.encodeUtf8 . Text.map safe
  where
    safe char
      | char == '"' || char == '\\' || char == '\r' || char == '\n' = '_'
      | otherwise = char

defaultDispatchConfig :: Config.Config
defaultDispatchConfig =
  Config.toRuntimeConfig Config.defaultFileConfig
