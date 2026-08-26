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

import Cosmocode.RPC.Internal (Rpc (..))
import Cosmocode.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Dynamic
import Effectful.Prim (Prim)
import qualified Effectful.Prim.IORef as IORef
import qualified Network.WebSockets as WS

runRpcWebSocket
  :: (Concurrent :> es, Prim :> es, IOE :> es)
  => String
  -> Int
  -> Text
  -> Eff (Rpc : es) a
  -> Eff es a
runRpcWebSocket host port token inner = do
  let headers = [("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> token))]
  nextRequestId <- IORef.newIORef 2
  withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
    liftIO $
      WS.runClientWith host port "/rpc" WS.defaultConnectionOptions headers \connection ->
        runInIO (interpret (runRpcOperation connection nextRequestId) inner)

runRpcOperation
  :: (Prim :> es, IOE :> es)
  => WS.Connection
  -> IORef.IORef Int
  -> LocalEnv localEs es
  -> Rpc (Eff localEs) a
  -> Eff es a
runRpcOperation connection nextRequestId _ = \case
  OpenSession -> liftIO (openSessionIO connection)
  GetSession sessionId -> liftIO (getSessionIO connection sessionId)
  ReceiveServerEvent -> decodeServerEvent <$> liftIO (WS.receiveData connection)
  SendChat sessionId body -> do
    requestId <- IORef.atomicModifyIORef' nextRequestId \current -> (current + 1, current)
    liftIO $ WS.sendTextData connection (Aeson.encode (chatSendRequest requestId sessionId body))

openSessionIO :: WS.Connection -> IO (Either Text Text)
openSessionIO connection = do
  WS.sendTextData connection (Aeson.encode (requestValue 1 "chat.open_session" (Aeson.object [])))
  bytes <- WS.receiveData connection
  pure (decodeResponse (Aeson.withObject "open session result" (Aeson..: "sessionId")) bytes)

getSessionIO :: WS.Connection -> Text -> IO (Either Text (Maybe [SessionMessage]))
getSessionIO connection sessionId = do
  WS.sendTextData connection (Aeson.encode (requestValue 1 "chat.get_session" (Aeson.object ["sessionId" Aeson..= sessionId])))
  bytes <- WS.receiveData connection
  pure (decodeResponse parseResult bytes)
  where
    parseResult = Aeson.withObject "get session result" \o -> do
      session <- o Aeson..:? "session"
      case (session :: Maybe Aeson.Value) of
        Nothing -> pure Nothing
        Just Aeson.Null -> pure Nothing
        Just _ -> Just <$> o Aeson..:? "messages" Aeson..!= []

decodeResponse :: (Aeson.Value -> AesonTypes.Parser a) -> ByteString.ByteString -> Either Text a
decodeResponse parseResult bytes = do
  value <- either (Left . Text.pack) Right (Aeson.eitherDecodeStrict' bytes)
  either (Left . Text.pack) id (AesonTypes.parseEither parser value)
  where
    parser = Aeson.withObject "JSON-RPC response" \o -> do
      rpcError <- o Aeson..:? "error"
      case rpcError of
        Just errorValue -> Left <$> parseErrorValue errorValue
        Nothing -> Right <$> (o Aeson..: "result" >>= parseResult)

decodeServerEvent :: ByteString.ByteString -> Either Text (Maybe ServerEvent)
decodeServerEvent bytes = do
  value <- either (Left . Text.pack) Right (Aeson.eitherDecodeStrict' bytes)
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
  requestValue requestId "chat.send" $ Aeson.object
    [ "sessionId" Aeson..= sessionId
    , "text" Aeson..= body
    , "replyToMessageId" Aeson..= Aeson.Null
    ]
