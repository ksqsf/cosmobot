{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.QQ.Protocol
Description : QQ OneBot v11 chat driver
Stability   : experimental
-}

module Bot.Chat.Driver.QQ.Protocol where

import Bot.Chat.Driver.QQ.Types
import qualified Bot.Effect.Concurrency as Concurrency
import Bot.Prelude
import qualified Control.Concurrent.Chan as Chan
import qualified Data.IORef as IORef
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import qualified Network.WebSockets as WS
import qualified Effectful.Concurrent.MVar as MVar
import Effectful.Timeout

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

-- | Connection settings for a OneBot v11 websocket endpoint.
data QQDriver = QQDriver
  { config :: !Config
  , eventChan :: !(Chan.Chan Event)
  , actionChan :: !(Chan.Chan ActionRequest)
  , groupDisplayNames :: !(IORef.IORef (Map Integer Text))
  }

newQQDriver :: IOE :> es => Config -> Eff es QQDriver
newQQDriver config = do
  eventChan <- liftIO Chan.newChan
  actionChan <- liftIO Chan.newChan
  groupDisplayNames <- liftIO (IORef.newIORef Map.empty)
  pure QQDriver{config, eventChan, actionChan, groupDisplayNames}

runQQDriver
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => QQDriver
  -> Eff es a
  -> Eff es a
runQQDriver driver inner = do
  Concurrency.withWorker "qq.connection" (qqConnectionLoop cfg eventChan actionChan) inner
  where
    cfg = driver.config
    eventChan = driver.eventChan
    actionChan = driver.actionChan

receiveEvent :: IOE :> es => QQDriver -> Eff es Event
receiveEvent driver =
  liftIO (Chan.readChan driver.eventChan)

sendAction
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es)
  => QQDriver
  -> Aeson.Value
  -> Eff es ActionResponse
sendAction driver value = do
  responseVar <- liftIO newEmptyMVar
  liftIO $ Chan.writeChan driver.actionChan (ActionRequest value responseVar)
  result <- timeout qqActionTimeoutMicroseconds (takeMVar responseVar)
  case result of
    Just response ->
      pure response
    Nothing -> do
      katipAddContext (qqActionRequestContext value) $
        $(logDebug) "Action timed out"
      pure failedActionResponse

data ActionRequest = ActionRequest !Aeson.Value !(MVar ActionResponse)

qqConnectionLoop
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Timeout :> es, Concurrency.Concurrency :> es)
  => Config
  -> Chan.Chan Event
  -> Chan.Chan ActionRequest
  -> Eff es ()
qqConnectionLoop cfg eventChan actionChan =
  forever do
    result <- runQQConnectionOnce cfg eventChan actionChan
    case result of
      Right () ->
        $(logDebug) "QQ websocket disconnected; reconnecting"
      Left err ->
        $(logDebug) [i|QQ websocket failed; reconnecting: #{err}|]
    threadDelay qqReconnectDelayMicroseconds

runQQConnectionOnce
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => Config
  -> Chan.Chan Event
  -> Chan.Chan ActionRequest
  -> Eff es (Either String ())
runQQConnectionOnce cfg eventChan actionChan =
  (Right <$> do
    withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
      liftIO $ WS.runClient cfg.host cfg.port (websocketPath cfg) \conn ->
        runInIO (runConnection eventChan actionChan conn)
  )
    `catch` \(connectionErr :: WS.ConnectionException) ->
      pure (Left (show connectionErr))
    `catch` \(handshakeErr :: WS.HandshakeException) ->
      pure (Left (show handshakeErr))
    `catch` \(ioErr :: IOException) ->
      pure (Left (show ioErr))
    `catchSync` \err ->
      pure (Left (displayException err))

runConnection
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => Chan.Chan Event
  -> Chan.Chan ActionRequest
  -> WS.Connection
  -> Eff es ()
runConnection eventChan actionChan conn = do
  pendingResponses <- liftIO (newMVar Map.empty)
  actionCounter <- liftIO (newMVar (1 :: Integer))
  done <- liftIO newEmptyMVar
  lastFrameAt <- liftIO (getCurrentTime >>= IORef.newIORef)
  frameReader <- forkConnectionThread "reader" done (readFrames eventChan pendingResponses lastFrameAt conn)
  sender <- forkConnectionThread "sender" done (sendActions actionChan pendingResponses actionCounter conn)
  monitor <- forkConnectionThread "heartbeat-monitor" done (monitorConnectionHeartbeat lastFrameAt)
  reason <- liftIO (takeMVar done)
  $(logDebug) [i|QQ websocket connection ending: #{show reason :: String}|]
  closeWebSocketForReconnect conn
  stopConnectionThread "reader" frameReader
  stopConnectionThread "sender" sender
  stopConnectionThread "heartbeat monitor" monitor
  failPendingResponses pendingResponses
  $(logDebug) "QQ websocket connection ended"

forkConnectionThread
  :: (Concurrency.Concurrency :> es, Concurrent :> es)
  => Text
  -> MVar SomeException
  -> Eff es ()
  -> Eff es Concurrency.Handle
forkConnectionThread label done action = Concurrency.fork [i|qq.websocket.#{label}|] do
  result <- try action
  case result of
    Left err ->
      void (MVar.tryPutMVar done err)
    Right () ->
      void (MVar.tryPutMVar done (toException ThreadKilled))

sendActions
  :: (IOE :> es, KatipE :> es, Concurrent :> es)
  => Chan.Chan ActionRequest
  -> PendingResponses
  -> MVar Integer
  -> WS.Connection
  -> Eff es ()
sendActions actionChan pendingResponses actionCounter conn =
  forever do
    ActionRequest value responseVar <- liftIO (Chan.readChan actionChan)
    echo <- nextActionEcho actionCounter
    let echoedValue = addActionEcho echo value
    katipAddContext (qqActionRequestContext value <> sl "qq_echo" echo) $
      $(logDebug) "Action request sent"
    MVar.modifyMVar_ pendingResponses \pending ->
      pure (Map.insert echo responseVar pending)
    (liftIO (WS.sendTextData conn (Aeson.encode echoedValue)) `catchSync` \err -> do
      MVar.modifyMVar_ pendingResponses \pending ->
        pure (Map.delete echo pending)
      void $ MVar.tryPutMVar responseVar failedActionResponse
      throwIO err)

failPendingResponses :: (Concurrent :> es) => PendingResponses -> Eff es ()
failPendingResponses pendingResponses = do
  pending <- MVar.modifyMVar pendingResponses \pending ->
    pure (Map.empty, pending)
  traverse_ (flip MVar.tryPutMVar failedActionResponse) pending

closeWebSocketForReconnect :: (IOE :> es, KatipE :> es, Timeout :> es) => WS.Connection -> Eff es ()
closeWebSocketForReconnect conn = do
  result <- timeout qqConnectionCloseTimeoutMicroseconds $
    trySync (liftIO $ WS.sendClose conn ("reconnect" :: Text))
  case result of
    Nothing ->
      $(logDebug) "QQ websocket close timed out during reconnect"
    Just (Left err) ->
      $(logDebug) [i|QQ websocket close during reconnect failed: #{show err :: String}|]
    Just (Right ()) ->
      pure ()

stopConnectionThread :: (Timeout :> es, KatipE :> es, Concurrency.Concurrency :> es) => Text -> Concurrency.Handle -> Eff es ()
stopConnectionThread label resourceHandle = do
  result <- timeout qqConnectionThreadStopTimeoutMicroseconds (Concurrency.cancel resourceHandle.handleId)
  when (isNothing result) $
    $(logDebug) [i|QQ websocket #{label} thread did not stop before reconnect; continuing|]

websocketPath :: Config -> String
websocketPath Config{path, token = Nothing} = path
websocketPath Config{path, token = Just t} =
  path <> separator <> "access_token=" <> Text.unpack t
  where
    separator
      | "?" `isInfixOf` path = "&"
      | otherwise            = "?"

-- ---------------------------------------------------------------------------
-- Streaming
-- ---------------------------------------------------------------------------

-- | Stream OneBot message events as platform-independent messages.
readFrames
  :: (IOE :> es, KatipE :> es, Concurrent :> es)
  => Chan.Chan Event
  -> PendingResponses
  -> IORef.IORef UTCTime
  -> WS.Connection
  -> Eff es ()
readFrames eventChan pendingResponses lastFrameAt conn = forever do
  value <- readValue conn
  liftIO (getCurrentTime >>= IORef.writeIORef lastFrameAt)
  case Aeson.fromJSON value of
    Aeson.Success event -> do
      liftIO $ Chan.writeChan eventChan event
    Aeson.Error _ ->
      case Aeson.fromJSON value of
        Aeson.Success response ->
          dispatchActionResponse pendingResponses response
        Aeson.Error err ->
          $(logDebug) [i|Ignoring malformed QQ frame: #{Text.pack err}|]

-- | Read frames until an action response is found.
readActionResponse :: (IOE :> es, KatipE :> es) => WS.Connection -> Eff es ActionResponse
readActionResponse conn = do
  value <- readValue conn
  case Aeson.fromJSON value of
    Aeson.Success response -> pure response
    Aeson.Error _ ->
      case Aeson.fromJSON value of
        Aeson.Success (_event :: Event) ->
          readActionResponse conn
        Aeson.Error err -> do
          $(logDebug) [i|Ignoring malformed QQ action response: #{Text.pack err}|]
          readActionResponse conn

readValue :: (IOE :> es, KatipE :> es) => WS.Connection -> Eff es Aeson.Value
readValue conn = do
  bytes <- liftIO (WS.receiveData conn :: IO ByteString)
  case Aeson.eitherDecodeStrict bytes of
    Right value -> pure value
    Left err -> do
      $(logDebug) [i|Ignoring malformed QQ frame: #{Text.pack err}|]
      readValue conn

monitorConnectionHeartbeat :: (IOE :> es, KatipE :> es, Concurrent :> es) => IORef.IORef UTCTime -> Eff es ()
monitorConnectionHeartbeat lastFrameAt = forever do
  threadDelay qqHeartbeatCheckMicroseconds
  now <- liftIO getCurrentTime
  lastSeen <- liftIO (IORef.readIORef lastFrameAt)
  let silence = diffUTCTime now lastSeen
  when (silence > qqHeartbeatTimeout) do
    $(logDebug) [i|QQ websocket heartbeat timed out after #{show silence :: String}; reconnecting|]
    throwIO (QQHeartbeatTimeout silence)

failedActionResponse :: ActionResponse
failedActionResponse =
  ActionResponse
    { status = Just "failed"
    , retcode = Nothing
    , data_ = Nothing
    , message = Just "action failed"
    , echo = Nothing
    }

qqActionTimeoutMicroseconds :: Int
qqActionTimeoutMicroseconds =
  40 * 1000000

qqReconnectDelayMicroseconds :: Int
qqReconnectDelayMicroseconds =
  5 * 1000000

qqConnectionCloseTimeoutMicroseconds :: Int
qqConnectionCloseTimeoutMicroseconds =
  2 * 1000000

qqConnectionThreadStopTimeoutMicroseconds :: Int
qqConnectionThreadStopTimeoutMicroseconds =
  2 * 1000000

qqHeartbeatCheckMicroseconds :: Int
qqHeartbeatCheckMicroseconds =
  15 * 1000000

qqHeartbeatTimeout :: NominalDiffTime
qqHeartbeatTimeout =
  90

newtype QQHeartbeatTimeout = QQHeartbeatTimeout NominalDiffTime
  deriving (Show)

instance Exception QQHeartbeatTimeout

type PendingResponses = MVar (Map Text (MVar ActionResponse))

nextActionEcho :: Concurrent :> es => MVar Integer -> Eff es Text
nextActionEcho counter =
  MVar.modifyMVar counter \value ->
    pure (value + 1, [i|cosmobot-#{value}|])

addActionEcho :: Text -> Aeson.Value -> Aeson.Value
addActionEcho echo value =
  case value of
    Aeson.Object obj ->
      Aeson.Object (KeyMap.insert "echo" (Aeson.String echo) obj)
    _ ->
      value

dispatchActionResponse
  :: (IOE :> es, KatipE :> es, Concurrent :> es)
  => PendingResponses
  -> ActionResponse
  -> Eff es ()
dispatchActionResponse pendingResponses response =
  case response.echo of
    Nothing ->
      $(logDebug) "Ignoring action response without echo"
    Just echo -> do
      katipAddContext (qqActionResponseContext echo response) $
        $(logDebug) "Action response received"
      waiter <- MVar.withMVar pendingResponses \pending ->
        pure (Map.lookup echo pending)
      case waiter of
        Nothing ->
          katipAddContext (qqActionResponseContext echo response) $
            $(logDebug) "Ignoring action response with unknown echo"
        Just responseVar ->
          void $ MVar.tryPutMVar responseVar response

qqActionRequestContext :: Aeson.Value -> SimpleLogPayload
qqActionRequestContext request =
  foldMap (sl "qq_action") (qqActionName request)
    <> foldMap (sl "qq_forward_nodes") (qqForwardNodeCount request)

qqActionResponseContext :: Text -> ActionResponse -> SimpleLogPayload
qqActionResponseContext echo response =
  sl "qq_echo" echo
    <> foldMap (sl "qq_status") response.status
    <> foldMap (sl "qq_retcode") response.retcode
    <> foldMap (sl "qq_response_message") response.message
    <> foldMap (sl "qq_message_id") (responseMessageId response)

qqActionName :: Aeson.Value -> Maybe Text
qqActionName =
  Aeson.parseMaybe (Aeson.withObject "QQ action request" (Aeson..: "action"))

qqForwardNodeCount :: Aeson.Value -> Maybe Int
qqForwardNodeCount = Aeson.parseMaybe $ Aeson.withObject "QQ action request" \request -> do
  params <- request Aeson..: "params"
  Aeson.withObject "QQ action params" (\object -> do
    messages <- object Aeson..: "messages" :: Aeson.Parser [Aeson.Value]
    pure (length messages)) params
