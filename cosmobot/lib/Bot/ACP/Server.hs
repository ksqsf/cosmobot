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
import qualified Bot.ACP.Types as ACP
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified JSONRPC
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

runAcpServer :: (IOE :> es, KatipE :> es) => Config.Config -> Eff es ()
runAcpServer cfg@Config.Config{enabled} =
  when enabled do
    let Config.Config{host, port} = cfg
        settings =
          Warp.setHost (fromString host) $
            Warp.setPort port Warp.defaultSettings
    logInfo [i|ACP server listening on #{host}:#{port}; websocket endpoint /acp|]
    withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
      liftIO $
        Warp.runSettings settings (acpServerApplication runInIO cfg)

acpServerApplication
  :: IOE :> es
  => (forall a. Eff es a -> IO a)
  -> Config.Config
  -> Wai.Application
acpServerApplication runInIO cfg =
  WaiWS.websocketsOr WS.defaultConnectionOptions websocketApp httpApp
  where
    websocketApp pending =
      runInIO (acpServerApp cfg pending)

acpServerApp :: IOE :> es => Config.Config -> WS.PendingConnection -> Eff es ()
acpServerApp cfg pending
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
      serveAcceptedClient conn
  | otherwise =
      liftIO $
        WS.rejectRequestWith pending $
          WS.defaultRejectRequest
            { WS.rejectCode = 401
            , WS.rejectMessage = "Unauthorized"
            , WS.rejectBody = "unauthorized"
            }

serveAcceptedClient :: IOE :> es => WS.Connection -> Eff es ()
serveAcceptedClient conn =
  forever do
    bytes <- liftIO (WS.receiveData conn :: IO ByteString)
    response <- case Aeson.eitherDecodeStrict bytes of
      Left err ->
        pure (Just (ACP.parseErrorResponse (Text.pack err)))
      Right value ->
        case Aeson.fromJSON value of
          Aeson.Success (JSONRPC.RequestMessage request) ->
            Just <$> dispatchAcpRequest request
          Aeson.Success (JSONRPC.NotificationMessage notification_) -> do
            _ <- dispatchAcpRequest (notificationToRequest notification_)
            pure Nothing
          Aeson.Error err ->
            pure (Just (ACP.invalidRequestResponse (Text.pack err)))
          Aeson.Success _ ->
            pure (Just (ACP.invalidRequestResponse "Expected request or notification"))
    traverse_ (liftIO . WS.sendTextData conn . Aeson.encode . Aeson.toJSON) response

dispatchAcpRequest :: Applicative f => ACP.AcpRequest -> f ACP.AcpResponse
dispatchAcpRequest request =
  pure $
    case ACP.requestMethod request of
      "initialize" ->
        ACP.successResponse (ACP.requestId request) initializeResponse
      "authenticate" ->
        ACP.successResponse (ACP.requestId request) (Aeson.object [])
      method ->
        ACP.errorResponse (ACP.requestId request) "method_not_found" [i|Unknown ACP method: #{method}|]

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
          ]
    , "agentInfo" Aeson..=
        Aeson.object
          [ "name" Aeson..= ("cosmobot" :: Text)
          , "title" Aeson..= ("Cosmobot" :: Text)
          , "version" Aeson..= ("0.1.0.0" :: Text)
          ]
    , "authMethods" Aeson..= ([] :: [Aeson.Value])
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
