{-# LANGUAGE RankNTypes #-}
{-|
Module      : Bot.ACP.Server
Description : Minimal Agent Client Protocol websocket server
Stability   : experimental
-}

module Bot.ACP.Server
  ( runAcpServer
  , acpServerApplication
  , acpServerApp
  , dispatchAcpRequest
  )
where

import qualified Bot.ACP.Config as Config
import qualified Bot.ACP.State as State
import qualified Bot.ACP.Types as ACP
import Bot.Core.Message (IncomingMessage (..), MessageId)
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import qualified Bot.Session as Session
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
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

data AcpClientDisconnected = AcpClientDisconnected Text
  deriving (Show)

instance Exception AcpClientDisconnected

data AcpPromptDispatchError
  = AcpPromptInvalid !Text
  | AcpPromptSessionNotFound
  deriving (Show)

instance Exception AcpPromptDispatchError

data PromptParams = PromptParams
  { sessionId :: !State.AcpSessionId
  , text :: !Text
  }
  deriving (Eq, Show)

runAcpServer
  :: (IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => Config.Config
  -> State.AcpState
  -> Eff es ()
runAcpServer cfg@Config.Config{enabled} acpState =
  when enabled do
    let Config.Config{host, port} = cfg
        settings =
          Warp.setHost (fromString host) $
            Warp.setPort port Warp.defaultSettings
    logInfo [i|ACP server listening on #{host}:#{port}; websocket endpoint /acp|]
    withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
      liftIO $
        Warp.runSettings settings (acpServerApplication runInIO cfg acpState)

acpServerApplication
  :: (IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => (forall a. Eff es a -> IO a)
  -> Config.Config
  -> State.AcpState
  -> Wai.Application
acpServerApplication runInIO cfg acpState =
  WaiWS.websocketsOr WS.defaultConnectionOptions websocketApp httpApp
  where
    websocketApp pending =
      runInIO (acpServerApp cfg acpState pending)

acpServerApp
  :: (IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => Config.Config
  -> State.AcpState
  -> WS.PendingConnection
  -> Eff es ()
acpServerApp cfg acpState pending
  | not (requestIsAcpPath (WS.pendingRequest pending)) =
      liftIO $
        WS.rejectRequestWith pending $
          WS.defaultRejectRequest
            { WS.rejectCode = 404
            , WS.rejectMessage = "Not Found"
            , WS.rejectBody = "not found"
            }
  | requestIsAuthorized cfg (WS.pendingRequest pending) = do
      conn <- liftIO (WS.acceptRequest pending)
      serveAcceptedClient acpState conn
  | otherwise =
      liftIO $
        WS.rejectRequestWith pending $
          WS.defaultRejectRequest
            { WS.rejectCode = 401
            , WS.rejectMessage = "Unauthorized"
            , WS.rejectBody = "unauthorized"
            }

serveAcceptedClient
  :: (IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => State.AcpState
  -> WS.Connection
  -> Eff es ()
serveAcceptedClient acpState conn = do
  (clientId, queue) <- State.registerClient acpState
  logDebug [i|ACP client #{clientId} connected|]
  (Concurrency.raceTasks_
      [i|acp.client.#{clientId}.writer|]
      (writeQueuedFrames queue conn)
      [i|acp.client.#{clientId}.reader|]
      (readRequestFrames acpState queue conn)
    `catchSync` \err ->
      logDebug [i|ACP client #{clientId} disconnected: #{displayException err}|])
    `finally` do
      State.unregisterClient acpState clientId
      logDebug [i|ACP client #{clientId} unregistered|]

writeQueuedFrames
  :: (IOE :> es, Concurrent :> es)
  => State.AcpClientQueue
  -> WS.Connection
  -> Eff es ()
writeQueuedFrames queue conn =
  forever do
    State.readClient queue >>= \case
      State.AcpClientSend value ->
        liftIO (WS.sendTextData conn (Aeson.encode value))
      State.AcpClientDisconnect reason -> do
        liftIO (WS.sendClose conn reason)
        throwIO (AcpClientDisconnected reason)

readRequestFrames
  :: (IOE :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => State.AcpState
  -> State.AcpClientQueue
  -> WS.Connection
  -> Eff es ()
readRequestFrames acpState queue conn =
  forever do
    bytes <- liftIO (WS.receiveData conn :: IO ByteString)
    response <- case Aeson.eitherDecodeStrict bytes of
      Left err ->
        pure (Just (ACP.parseErrorResponse (Text.pack err)))
      Right value ->
        case Aeson.fromJSON value of
          Aeson.Success (JSONRPC.RequestMessage request) ->
            Just <$> dispatchAcpRequest acpState queue request
          Aeson.Success (JSONRPC.NotificationMessage notification_) -> do
            _ <- dispatchAcpRequest acpState queue (notificationToRequest notification_)
            pure Nothing
          Aeson.Error err ->
            pure (Just (ACP.invalidRequestResponse (Text.pack err)))
          Aeson.Success _ ->
            pure (Just (ACP.invalidRequestResponse "Expected request or notification"))
    traverse_ (State.writeClient queue . Aeson.toJSON) response

dispatchAcpRequest
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => State.AcpState
  -> State.AcpClientQueue
  -> ACP.AcpRequest
  -> Eff es ACP.AcpResponse
dispatchAcpRequest acpState queue request =
  case ACP.requestMethod request of
    "initialize" ->
      pure (ACP.successResponse (ACP.requestId request) initializeResponse)
    "authenticate" ->
      pure (ACP.successResponse (ACP.requestId request) (Aeson.object []))
    "session/new" ->
      dispatchNewSession request
    "session/delete" ->
      dispatchDeleteSession request
    "session/prompt" ->
      dispatchPrompt acpState queue request
    method ->
      pure (ACP.errorResponse (ACP.requestId request) "method_not_found" [i|Unknown ACP method: #{method}|])

initializeResponse :: Aeson.Value
initializeResponse =
  Aeson.object
    [ "protocolVersion" Aeson..= (1 :: Int)
    , "agentCapabilities" Aeson..=
        Aeson.object
          [ "loadSession" Aeson..= False
          , "promptCapabilities" Aeson..=
              Aeson.object
                [ "image" Aeson..= False
                , "audio" Aeson..= False
                , "embeddedContext" Aeson..= False
                ]
          , "sessionCapabilities" Aeson..=
              Aeson.object
                [ "delete" Aeson..= Aeson.object []
                ]
          ]
    , "agentInfo" Aeson..=
        Aeson.object
          [ "name" Aeson..= ("cosmobot" :: Text)
          , "title" Aeson..= ("Cosmobot" :: Text)
          , "version" Aeson..= ("0.1.0.0" :: Text)
          ]
    , "authMethods" Aeson..= ([] :: [Aeson.Value])
    ]

dispatchNewSession
  :: Storage.Storage :> es
  => ACP.AcpRequest
  -> Eff es ACP.AcpResponse
dispatchNewSession request =
  case AesonTypes.parseEither parseNewSessionParams (ACP.requestParams request) of
    Left err ->
      pure (ACP.errorResponse (ACP.requestId request) "invalid_params" (Text.pack err))
    Right label -> do
      session <- State.openSession label
      pure $
        ACP.successResponse (ACP.requestId request) $
          Aeson.object
            [ "sessionId" Aeson..= State.acpSessionIdText session.sessionId
            ]

dispatchDeleteSession
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es)
  => ACP.AcpRequest
  -> Eff es ACP.AcpResponse
dispatchDeleteSession request =
  case AesonTypes.parseEither parseSessionIdParams (ACP.requestParams request) of
    Left err ->
      pure (ACP.errorResponse (ACP.requestId request) "invalid_params" (Text.pack err))
    Right sessionId -> do
      _deleted <- State.deleteSession sessionId
      pure (ACP.successResponse (ACP.requestId request) (Aeson.object []))

dispatchPrompt
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => State.AcpState
  -> State.AcpClientQueue
  -> ACP.AcpRequest
  -> Eff es ACP.AcpResponse
dispatchPrompt acpState queue request =
  case AesonTypes.parseEither parsePromptParams (ACP.requestParams request) of
    Left err ->
      pure (ACP.errorResponse (ACP.requestId request) "invalid_params" (Text.pack err))
    Right prompt ->
      ( State.withPromptWaiter acpState prompt.sessionId (enqueuePrompt prompt) >>= \messageId ->
          pure $
            ACP.successResponse (ACP.requestId request) $
              Aeson.object
                [ "stopReason" Aeson..= ("end_turn" :: Text)
                , "messageId" Aeson..= messageId
                ]
      ) `catchSync` \err ->
          case fromException err of
            Just (AcpPromptInvalid invalidParams) ->
              pure (ACP.errorResponse (ACP.requestId request) "invalid_params" invalidParams)
            Just AcpPromptSessionNotFound ->
              pure (ACP.errorResponse (ACP.requestId request) "not_found" "Session not found")
            Nothing ->
              throwIO err
  where
    enqueuePrompt prompt = do
      userMessage <- State.enqueueUserMessage acpState $
        Session.SessionSend
          { sessionId = prompt.sessionId
          , text = prompt.text
          , imageUrls = []
          , attachments = []
          , replyToMessageId = Nothing
          }
      case userMessage of
        Left err ->
          throwIO (AcpPromptInvalid err)
        Right Nothing ->
          throwIO AcpPromptSessionNotFound
        Right (Just IncomingMessage{messageId}) ->
          for_ messageId \userMessageId ->
            State.writeClient queue $
              Aeson.toJSON $
                sessionUpdateNotification prompt.sessionId $
                  userMessageChunkUpdate userMessageId prompt.text

parseNewSessionParams :: Aeson.Value -> AesonTypes.Parser (Maybe Text)
parseNewSessionParams =
  Aeson.withObject "session/new params" \o ->
    o Aeson..:? "cwd"

parseSessionIdParams :: Aeson.Value -> AesonTypes.Parser State.AcpSessionId
parseSessionIdParams =
  Aeson.withObject "session params" \o ->
    Session.SessionId <$> o Aeson..: "sessionId"

parsePromptParams :: Aeson.Value -> AesonTypes.Parser PromptParams
parsePromptParams =
  Aeson.withObject "session/prompt params" \o -> do
    sessionId <- Session.SessionId <$> o Aeson..: "sessionId"
    prompt <- o Aeson..: "prompt"
    text <- parsePromptText prompt
    pure PromptParams{sessionId, text}

parsePromptText :: Aeson.Value -> AesonTypes.Parser Text
parsePromptText =
  Aeson.withArray "prompt" \blocks ->
    Text.intercalate "\n" <$> traverse parseTextBlock (toList blocks)

parseTextBlock :: Aeson.Value -> AesonTypes.Parser Text
parseTextBlock =
  Aeson.withObject "content block" \o -> do
    contentType <- o Aeson..: "type"
    case contentType of
      "text" ->
        o Aeson..: "text"
      _ ->
        fail [i|unsupported ACP content block type: #{contentType :: Text}|]

sessionUpdateNotification :: State.AcpSessionId -> Aeson.Value -> JSONRPC.JSONRPCMessage
sessionUpdateNotification sessionId update =
  JSONRPC.NotificationMessage $
    JSONRPC.JSONRPCNotification
      JSONRPC.rPC_VERSION
      "session/update"
      ( Aeson.object
          [ "sessionId" Aeson..= State.acpSessionIdText sessionId
          , "update" Aeson..= update
          ]
      )

userMessageChunkUpdate :: MessageId -> Text -> Aeson.Value
userMessageChunkUpdate messageId text =
  Aeson.object
    [ "sessionUpdate" Aeson..= ("user_message_chunk" :: Text)
    , "messageId" Aeson..= messageId
    , "content" Aeson..=
        Aeson.object
          [ "type" Aeson..= ("text" :: Text)
          , "text" Aeson..= text
          ]
    ]

notificationToRequest :: ACP.AcpNotification -> ACP.AcpRequest
notificationToRequest notification_ =
  JSONRPC.JSONRPCRequest JSONRPC.rPC_VERSION (JSONRPC.RequestId Aeson.Null) notification_.method notification_.params

requestIsAuthorized :: Config.Config -> WS.RequestHead -> Bool
requestIsAuthorized cfg request =
  authorizationBearer request == Just expectedToken
  where
    Config.Config{token} = cfg
    expectedToken = TextEncoding.encodeUtf8 token

requestIsAcpPath :: WS.RequestHead -> Bool
requestIsAcpPath request =
  path == "/acp"
  where
    (path, _) = ByteString.break (== questionMark) request.requestPath

httpApp :: Wai.Application
httpApp request respond =
  case Wai.requestMethod request of
    "GET" ->
      respond $
        textResponse Http.status404 "not found"
    "HEAD" ->
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
