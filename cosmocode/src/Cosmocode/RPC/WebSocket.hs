{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Cosmocode.RPC.WebSocket
  ( runRpcWebSocket
  , decodeServerEvent
  , chatSendRequest
  ) where

import Control.Monad (forever, void)
import Cosmocode.RPC.Internal (Rpc (..))
import Cosmocode.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import Data.Foldable (for_, traverse_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful
import Effectful.Concurrent (Concurrent)
import qualified Effectful.Concurrent.Async as Async
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.Concurrent.STM as STM
import Effectful.Dispatch.Dynamic
import Effectful.Exception (bracket, catchSync, displayException)
import Effectful.Prim (Prim)
import qualified Effectful.Prim.IORef as IORef
import qualified Network.WebSockets as WS

data RpcConnection = RpcConnection
  { connection :: !WS.Connection
  , nextRequestId :: !(IORef.IORef Int)
  , pending :: !(STM.TVar (Map.Map Int (STM.TMVar (Either Text Aeson.Value))))
  , events :: !(STM.TChan (Either Text (Maybe ServerEvent)))
  , writeLock :: !(MVar.MVar ())
  }

runRpcWebSocket
  :: (Concurrent :> es, Prim :> es, IOE :> es)
  => String
  -> Int
  -> Text
  -> Eff (Rpc : es) a
  -> Eff es a
runRpcWebSocket host port token inner = do
  let headers = [("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> token))]
  withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
    liftIO $
      WS.runClientWith host port "/rpc" WS.defaultConnectionOptions headers \connection ->
        runInIO do
          rpcConnection <- newRpcConnection connection
          Async.withAsync (receiveFrames rpcConnection) \_ ->
            interpret (runRpcOperation rpcConnection) inner

newRpcConnection :: (Concurrent :> es, Prim :> es) => WS.Connection -> Eff es RpcConnection
newRpcConnection connection = do
  nextRequestId <- IORef.newIORef 1
  pending <- STM.atomically (STM.newTVar Map.empty)
  events <- STM.atomically STM.newTChan
  writeLock <- MVar.newMVar ()
  pure RpcConnection{connection, nextRequestId, pending, events, writeLock}

runRpcOperation
  :: (Concurrent :> es, Prim :> es, IOE :> es)
  => RpcConnection
  -> LocalEnv localEs es
  -> Rpc (Eff localEs) a
  -> Eff es a
runRpcOperation rpcConnection _ = \case
  OpenSession -> openSessionRpc rpcConnection
  GetSession sessionId -> getSessionRpc rpcConnection sessionId
  ReceiveServerEvent -> STM.atomically (STM.readTChan rpcConnection.events)
  SendChat sessionId body ->
    rpcCall rpcConnection "chat.send" (chatSendParams sessionId body) (const (pure ()))

openSessionRpc :: (Concurrent :> es, Prim :> es, IOE :> es) => RpcConnection -> Eff es (Either Text Text)
openSessionRpc rpcConnection = do
  opened <- rpcCall rpcConnection "chat.open_session" (Aeson.object []) $
    Aeson.withObject "open session result" (Aeson..: "sessionId")
  case opened of
    Left err -> pure (Left err)
    Right sessionId -> subscribeSession rpcConnection sessionId

getSessionRpc :: (Concurrent :> es, Prim :> es, IOE :> es) => RpcConnection -> Text -> Eff es (Either Text (Maybe [SessionMessage]))
getSessionRpc rpcConnection sessionId = do
  loaded <- rpcCall rpcConnection "chat.get_session" (sessionParams sessionId) parseResult
  case loaded of
    Right history@Just{} ->
      fmap (fmap (const history)) (subscribeSession rpcConnection sessionId)
    _ -> pure loaded
  where
    parseResult = Aeson.withObject "get session result" \o -> do
      session <- o Aeson..:? "session"
      case (session :: Maybe Aeson.Value) of
        Nothing -> pure Nothing
        Just Aeson.Null -> pure Nothing
        Just _ -> Just <$> o Aeson..:? "messages" Aeson..!= []

subscribeSession :: (Concurrent :> es, Prim :> es, IOE :> es) => RpcConnection -> Text -> Eff es (Either Text Text)
subscribeSession rpcConnection sessionId =
  rpcCall rpcConnection "chat.subscribe" (sessionParams sessionId) (const (pure sessionId))

rpcCall
  :: (Concurrent :> es, Prim :> es, IOE :> es)
  => RpcConnection
  -> Text
  -> Aeson.Value
  -> (Aeson.Value -> AesonTypes.Parser a)
  -> Eff es (Either Text a)
rpcCall rpcConnection method params parseResult =
  bracket acquire release \(requestId, waiter) -> do
    MVar.withMVar rpcConnection.writeLock \_ ->
      liftIO $ WS.sendTextData rpcConnection.connection (Aeson.encode (requestValue requestId method params))
    response <- STM.atomically (STM.readTMVar waiter)
    pure (response >>= decodeResponseValue parseResult)
  where
    acquire = do
      requestId <- IORef.atomicModifyIORef' rpcConnection.nextRequestId \current -> (current + 1, current)
      waiter <- STM.atomically do
        waiter <- STM.newEmptyTMVar
        STM.modifyTVar' rpcConnection.pending (Map.insert requestId waiter)
        pure waiter
      pure (requestId, waiter)
    release (requestId, _) =
      STM.atomically (STM.modifyTVar' rpcConnection.pending (Map.delete requestId))

decodeResponseValue :: (Aeson.Value -> AesonTypes.Parser a) -> Aeson.Value -> Either Text a
decodeResponseValue parseResult value =
  either (Left . Text.pack) id (AesonTypes.parseEither parser value)
  where
    parser = Aeson.withObject "JSON-RPC response" \o -> do
      rpcError <- o Aeson..:? "error"
      case rpcError of
        Just errorValue -> Left <$> parseErrorValue errorValue
        Nothing -> Right <$> (o Aeson..: "result" >>= parseResult)

receiveFrames :: (Concurrent :> es, IOE :> es) => RpcConnection -> Eff es ()
receiveFrames rpcConnection =
  forever receiveOne `catchSync` \err ->
    failConnection rpcConnection (Text.pack (displayException err))
  where
    receiveOne = do
      bytes <- liftIO (WS.receiveData rpcConnection.connection :: IO ByteString.ByteString)
      case Aeson.eitherDecodeStrict' bytes of
        Left err -> publishEvent rpcConnection (Left (Text.pack err))
        Right value -> routeFrame rpcConnection value

routeFrame :: Concurrent :> es => RpcConnection -> Aeson.Value -> Eff es ()
routeFrame rpcConnection value =
  case AesonTypes.parseEither responseId value of
    Right (Just requestId) -> resolveResponse rpcConnection requestId value
    _ -> publishEvent rpcConnection (decodeServerEventValue value)
  where
    responseId = Aeson.withObject "JSON-RPC message" (Aeson..:? "id")

resolveResponse :: Concurrent :> es => RpcConnection -> Int -> Aeson.Value -> Eff es ()
resolveResponse rpcConnection requestId value =
  STM.atomically do
    pending <- STM.readTVar rpcConnection.pending
    for_ (Map.lookup requestId pending) \waiter -> do
      STM.modifyTVar' rpcConnection.pending (Map.delete requestId)
      void (STM.tryPutTMVar waiter (Right value))

publishEvent :: Concurrent :> es => RpcConnection -> Either Text (Maybe ServerEvent) -> Eff es ()
publishEvent rpcConnection =
  STM.atomically . STM.writeTChan rpcConnection.events

failConnection :: Concurrent :> es => RpcConnection -> Text -> Eff es ()
failConnection rpcConnection reason =
  STM.atomically do
    pending <- STM.readTVar rpcConnection.pending
    STM.writeTVar rpcConnection.pending Map.empty
    traverse_ (\waiter -> STM.tryPutTMVar waiter (Left reason)) pending
    STM.writeTChan rpcConnection.events (Left reason)

decodeServerEvent :: ByteString.ByteString -> Either Text (Maybe ServerEvent)
decodeServerEvent bytes = do
  value <- either (Left . Text.pack) Right (Aeson.eitherDecodeStrict' bytes)
  decodeServerEventValue value

decodeServerEventValue :: Aeson.Value -> Either Text (Maybe ServerEvent)
decodeServerEventValue value =
  either (Left . Text.pack) Right (AesonTypes.parseEither parseValue value)
  where
    parseValue = Aeson.withObject "JSON-RPC message" \o -> do
      method <- o Aeson..:? "method"
      case (method :: Maybe Text) of
        Just "chat.message" -> Just . MessageReceived <$> (o Aeson..: "params" >>= Aeson.parseJSON)
        Just "chat.message_update" -> do
          params <- o Aeson..: "params"
          Just <$> Aeson.withObject "message update" (\p -> MessageUpdated <$> p Aeson..: "sessionId" <*> p Aeson..: "messageId" <*> p Aeson..: "text") params
        Just "chat.message_done" -> parseDone =<< o Aeson..: "params"
        Just "chat.reasoning_start" -> parseActivity (\p -> ReasoningStarted <$> p Aeson..: "runId" <*> p Aeson..: "turn") =<< o Aeson..: "params"
        Just "chat.reasoning_end" -> parseActivity (\p -> ReasoningFinished <$> p Aeson..: "runId" <*> p Aeson..: "turn" <*> p Aeson..: "answerKind") =<< o Aeson..: "params"
        Just "chat.tool_call_start" -> parseActivity (\p -> ToolCallStarted <$> p Aeson..: "runId" <*> p Aeson..: "turn" <*> p Aeson..: "toolCallId" <*> p Aeson..: "toolName") =<< o Aeson..: "params"
        Just "chat.tool_call_end" -> parseActivity (\p -> ToolCallFinished <$> p Aeson..: "runId" <*> p Aeson..: "turn" <*> p Aeson..: "toolCallId" <*> p Aeson..: "toolName" <*> p Aeson..: "status") =<< o Aeson..: "params"
        Just _ -> pure Nothing
        Nothing -> traverse (fmap RequestFailed . parseErrorValue) =<< o Aeson..:? "error"

    parseDone = Aeson.withObject "message done" \p ->
      Just <$> (MessageDone <$> p Aeson..: "sessionId" <*> p Aeson..: "messageId")

    parseActivity parser = Aeson.withObject "activity" \p -> do
      sessionId <- p Aeson..: "sessionId"
      Just . ActivityChanged sessionId <$> parser p

parseErrorValue :: Aeson.Value -> AesonTypes.Parser Text
parseErrorValue = Aeson.withObject "JSON-RPC error" (Aeson..: "message")

requestValue :: Int -> Text -> Aeson.Value -> Aeson.Value
requestValue requestId method params = Aeson.object
  [ "jsonrpc" Aeson..= ("2.0" :: Text)
  , "id" Aeson..= requestId
  , "method" Aeson..= method
  , "params" Aeson..= params
  ]

chatSendRequest :: Int -> Text -> Text -> Aeson.Value
chatSendRequest requestId sessionId body =
  requestValue requestId "chat.send" (chatSendParams sessionId body)

chatSendParams :: Text -> Text -> Aeson.Value
chatSendParams sessionId body =
  Aeson.object
    [ "sessionId" Aeson..= sessionId
    , "text" Aeson..= body
    , "replyToMessageId" Aeson..= Aeson.Null
    ]

sessionParams :: Text -> Aeson.Value
sessionParams sessionId =
  Aeson.object ["sessionId" Aeson..= sessionId]
