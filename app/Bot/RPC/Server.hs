{-|
Module      : Bot.RPC.Server
Description : Local JSON-RPC websocket server
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.RPC.Server
  ( RpcServerCallbacks (..)
  , noRpcServerCallbacks
  , runRpcServer
  , rpcServerApp
  , dispatchRpcRequest
  )
where

import Bot.Prelude
import Bot.Core.Message (IncomingMessage (..))
import qualified Bot.RPC.Config as Config
import qualified Bot.RPC.Protocol as Protocol
import qualified Bot.RPC.State as State
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.STM as STM
import qualified JSONRPC
import qualified Network.HTTP.Types.URI as URI
import qualified Network.WebSockets as WS

data RpcServerCallbacks es = RpcServerCallbacks
  { auditMethod :: Protocol.RpcRequest -> Eff es (Maybe (Either Protocol.RpcError Aeson.Value))
  }

noRpcServerCallbacks :: RpcServerCallbacks es
noRpcServerCallbacks = RpcServerCallbacks
  { auditMethod = \_ -> pure Nothing
  }

runRpcServer
  :: (IOE :> es, Log :> es, Concurrent :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> Eff es ()
runRpcServer cfg@Config.Config{enabled} rpcState callbacks = do
  if enabled
    then runRpcServer' cfg rpcState callbacks
    else pure ()

runRpcServer'
  :: (IOE :> es, Log :> es, Concurrent :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> Eff es ()
runRpcServer' cfg rpcState callbacks = do
  let Config.Config{host, port} = cfg
  logInfo_ [i|RPC websocket listening on #{host}:#{port}|]
  withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
    liftIO $
      WS.runServer host port \pending ->
        runInIO (rpcServerApp cfg rpcState callbacks pending)

rpcServerApp
  :: (IOE :> es, Log :> es, Concurrent :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> WS.PendingConnection
  -> Eff es ()
rpcServerApp cfg rpcState callbacks pending
  | requestIsAuthorized cfg (WS.pendingRequest pending) = do
      conn <- liftIO (WS.acceptRequest pending)
      serveAcceptedClient rpcState callbacks conn
  | otherwise =
      liftIO $
        WS.rejectRequestWith pending $
          WS.defaultRejectRequest
            { WS.rejectCode = 401
            , WS.rejectMessage = "Unauthorized"
            , WS.rejectBody = "unauthorized"
            }

serveAcceptedClient
  :: (IOE :> es, Log :> es, Concurrent :> es)
  => State.RpcState
  -> RpcServerCallbacks es
  -> WS.Connection
  -> Eff es ()
serveAcceptedClient rpcState callbacks conn = do
  (clientId, queue) <- State.registerClient rpcState
  logTrace_ [i|RPC client #{clientId} connected|]
  (race_
      (writeQueuedFrames queue conn)
      (readRequestFrames rpcState callbacks queue conn)
    `catchSync` \err ->
      logTrace_ [i|RPC client #{clientId} disconnected: #{displayException err}|])
    `finally` do
      State.unregisterClient rpcState clientId
      logTrace_ [i|RPC client #{clientId} unregistered|]

writeQueuedFrames
  :: (IOE :> es, Concurrent :> es)
  => STM.TChan Aeson.Value
  -> WS.Connection
  -> Eff es ()
writeQueuedFrames queue conn =
  forever do
    value <- STM.atomically (STM.readTChan queue)
    liftIO (WS.sendTextData conn (Aeson.encode value))

readRequestFrames
  :: (IOE :> es, Concurrent :> es)
  => State.RpcState
  -> RpcServerCallbacks es
  -> STM.TChan Aeson.Value
  -> WS.Connection
  -> Eff es ()
readRequestFrames rpcState callbacks queue conn =
  forever do
    bytes <- liftIO (WS.receiveData conn :: IO ByteString)
    response <- case Aeson.eitherDecodeStrict bytes of
      Left err ->
        pure (Just (Protocol.parseErrorResponse (Text.pack err)))
      Right value ->
        case Aeson.fromJSON value of
          Aeson.Success (JSONRPC.RequestMessage request) ->
            Just <$> dispatchRpcRequest rpcState callbacks request
          Aeson.Success (JSONRPC.NotificationMessage notification_) -> do
            _ <- dispatchRpcRequest rpcState callbacks (notificationToRequest notification_)
            pure Nothing
          Aeson.Error err ->
            pure (Just (Protocol.invalidRequestResponse (Text.pack err)))
          Aeson.Success _ ->
            pure (Just (Protocol.invalidRequestResponse "Expected request or notification"))
    traverse_ (State.writeClient queue . Aeson.toJSON) response

dispatchRpcRequest
  :: Concurrent :> es
  => State.RpcState
  -> RpcServerCallbacks es
  -> Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchRpcRequest rpcState callbacks request =
  case Protocol.requestMethod request of
    "chat.open_session" ->
      dispatchOpenSession rpcState request
    "chat.send" ->
      dispatchChatSend rpcState request
    method
      | "audit." `Text.isPrefixOf` method ->
          dispatchAudit callbacks request
      | otherwise ->
          pure (methodNotFound (Protocol.requestId request) method)

dispatchOpenSession
  :: Concurrent :> es
  => State.RpcState
  -> Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchOpenSession rpcState request =
  case AesonTypes.parseEither parseOpenSessionParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right label -> do
      sessionId <- State.openChatSession rpcState label
      pure $
        Protocol.successResponse (Protocol.requestId request) $
          Aeson.object
            [ "sessionId" Aeson..= rpcSessionIdText sessionId
            ]

dispatchChatSend
  :: Concurrent :> es
  => State.RpcState
  -> Protocol.RpcRequest
  -> Eff es Protocol.RpcResponse
dispatchChatSend rpcState request =
  case AesonTypes.parseEither parseChatSendParams (Protocol.requestParams request) of
    Left err ->
      pure (Protocol.errorResponse (Protocol.requestId request) "invalid_params" (Text.pack err))
    Right chatSend -> do
      message <- State.enqueueChatMessage rpcState chatSend
      let IncomingMessage{messageId} = message
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
    pure State.RpcChatSend
      { sessionId = State.RpcSessionId sessionText
      , text
      , imageUrls
      , replyToMessageId
      }

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
  queryAccessToken request == Just expectedToken
    || authorizationBearer request == Just expectedToken
  where
    Config.Config{token} = cfg
    expectedToken = TextEncoding.encodeUtf8 token

queryAccessToken :: WS.RequestHead -> Maybe ByteString
queryAccessToken request =
  join (snd <$> find ((== "access_token") . fst) (URI.parseQuery queryBytes))
  where
    (_, queryBytes) = ByteString.break (== questionMark) request.requestPath

authorizationBearer :: WS.RequestHead -> Maybe ByteString
authorizationBearer request =
  ByteString.stripPrefix bearerPrefix =<< (snd <$> find ((== "Authorization") . fst) request.requestHeaders)

questionMark :: Word8
questionMark = 63

bearerPrefix :: ByteString
bearerPrefix = "Bearer "
