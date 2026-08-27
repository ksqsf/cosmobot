{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Cosmocode (optionsInfo)
import Cosmocode.RPC
import Cosmocode.RPC.WebSocket
import Cosmocode.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Effectful
import Effectful.Concurrent (runConcurrent)
import qualified Effectful.Concurrent.Async as Async
import Effectful.Exception (finally)
import Effectful.Prim (runPrim)
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "cosmocode"
  [ testCase "CLI defaults to a new session" testCliNew
  , testCase "CLI parses resume" testCliResume
  , testCase "events update only the active session" testSessionFiltering
  , testCase "decodes chat and agent lifecycle events" testLifecycleEvents
  , testCase "websocket authenticates, opens, sends, and streams" testWebSocketSmoke ]

testCliNew :: IO ()
testCliNew = case execParserPure defaultPrefs optionsInfo ["--token", "secret"] of
  Success options -> do
    options.host @?= "127.0.0.1"
    options.port @?= 38765
    options.token @?= "secret"
    options.command @?= NewSession
  _ -> assertFailure "expected CLI parse success"

testCliResume :: IO ()
testCliResume = case execParserPure defaultPrefs optionsInfo ["--host", "example.test", "--port", "9000", "--token", "secret", "resume", "work-1"] of
  Success options -> do
    options.host @?= "example.test"
    options.port @?= 9000
    options.command @?= ResumeSession "work-1"
  _ -> assertFailure "expected resume CLI parse success"

testSessionFiltering :: IO ()
testSessionFiltering = do
  let first = SessionMessage "work-1" "m1" "user" "hello"
      other = SessionMessage "work-2" "m2" "assistant" "ignored"
      model = initialModel "localhost:38765" "work-1" []
      received = applyServerEvent (MessageReceived first) model
      filtered = applyServerEvent (MessageReceived other) received
      updated = applyServerEvent (MessageUpdated "work-1" "m1" "streamed") filtered
  filtered.messages @?= [first]
  updated.messages @?= [first{text = "streamed"}]

testLifecycleEvents :: IO ()
testLifecycleEvents = do
  decode "chat.message_done" ["sessionId" Aeson..= ("work-1" :: String), "messageId" Aeson..= ("m1" :: String)]
    @?= Right (Just (MessageDone "work-1" "m1"))
  decode "chat.reasoning_start" common
    @?= Right (Just (ActivityChanged "work-1" (ReasoningStarted "run-1" 2)))
  decode "chat.reasoning_end" (common <> ["answerKind" Aeson..= ("tool_request" :: String)])
    @?= Right (Just (ActivityChanged "work-1" (ReasoningFinished "run-1" 2 "tool_request")))
  decode "chat.tool_call_start" (common <> tool)
    @?= Right (Just (ActivityChanged "work-1" (ToolCallStarted "run-1" 2 "call-1" "run_bash")))
  decode "chat.tool_call_end" (common <> tool <> ["status" Aeson..= ("ok" :: String)])
    @?= Right (Just (ActivityChanged "work-1" (ToolCallFinished "run-1" 2 "call-1" "run_bash" "ok")))
  where
    common = ["sessionId" Aeson..= ("work-1" :: String), "runId" Aeson..= ("run-1" :: String), "turn" Aeson..= (2 :: Int)]
    tool = ["toolCallId" Aeson..= ("call-1" :: String), "toolName" Aeson..= ("run_bash" :: String)]
    decode method params = decodeServerEvent (LazyByteString.toStrict (Aeson.encode (notification method (Aeson.object params))))

testWebSocketSmoke :: IO ()
testWebSocketSmoke = do
  listenSocket <- WS.makeListenSocket "127.0.0.1" 0
  port <- fromIntegral <$> Socket.socketPort listenSocket
  let server = liftIO (serveOne listenSocket) `finally` liftIO (Socket.close listenSocket)
      options = Options "127.0.0.1" port "secret" NewSession
      client = runRpcWebSocket options.host options.port options.token do
        startup <- openSession
        sent <- sendChat "session-1" "hello"
        first <- receiveServerEvent
        second <- receiveServerEvent
        pure (startup, sent, first, second)
  (_, (startup, sent, first, second)) <- runEff . runPrim . runConcurrent $ Async.concurrently server client
  startup @?= Right "session-1"
  sent @?= Right ()
  first @?= Right (Just (MessageUpdated "session-1" "message-2" "streamed"))
  second @?= Right (Just (MessageReceived (SessionMessage "session-1" "message-1" "user" "hello")))

serveOne :: Socket.Socket -> IO ()
serveOne listenSocket = do
  (clientSocket, _) <- Socket.accept listenSocket
  pending <- WS.makePendingConnection clientSocket WS.defaultConnectionOptions
  lookup "Authorization" (WS.requestHeaders (WS.pendingRequest pending)) @?= Just "Bearer secret"
  connection <- WS.acceptRequest pending
  openRequest <- WS.receiveData connection :: IO ByteString.ByteString
  requestMethod openRequest @?= Right "chat.open_session"
  requestId openRequest @?= Right 1
  WS.sendTextData connection (Aeson.encode (rpcResult 1 (Aeson.object ["sessionId" Aeson..= ("session-1" :: String)])))
  subscribeRequest <- WS.receiveData connection :: IO ByteString.ByteString
  requestMethod subscribeRequest @?= Right "chat.subscribe"
  requestId subscribeRequest @?= Right 2
  WS.sendTextData connection (Aeson.encode (notification "chat.message_update" (Aeson.object
    [ "sessionId" Aeson..= ("session-1" :: String), "messageId" Aeson..= ("message-2" :: String)
    , "text" Aeson..= ("streamed" :: String) ])))
  WS.sendTextData connection (Aeson.encode (rpcResult 2 (Aeson.object ["subscribed" Aeson..= True])))
  sendRequest <- WS.receiveData connection :: IO ByteString.ByteString
  requestMethod sendRequest @?= Right "chat.send"
  requestId sendRequest @?= Right 3
  replyTarget sendRequest @?= Right Nothing
  WS.sendTextData connection (Aeson.encode (notification "chat.message" (Aeson.object
    [ "sessionId" Aeson..= ("session-1" :: String), "messageId" Aeson..= ("message-1" :: String)
    , "sender" Aeson..= ("user" :: String), "text" Aeson..= ("hello" :: String) ])))
  WS.sendTextData connection (Aeson.encode (rpcResult 3 (Aeson.object [])))

requestMethod :: ByteString.ByteString -> Either String String
requestMethod bytes = Aeson.eitherDecodeStrict' bytes >>= AesonTypes.parseEither
  (Aeson.withObject "request" (Aeson..: "method"))

requestId :: ByteString.ByteString -> Either String Int
requestId bytes = Aeson.eitherDecodeStrict' bytes >>= AesonTypes.parseEither
  (Aeson.withObject "request" (Aeson..: "id"))

replyTarget :: ByteString.ByteString -> Either String (Maybe String)
replyTarget bytes = Aeson.eitherDecodeStrict' bytes >>= AesonTypes.parseEither
  (Aeson.withObject "request" \o -> o Aeson..: "params" >>= Aeson.withObject "params" (Aeson..:? "replyToMessageId"))

rpcResult :: Int -> Aeson.Value -> Aeson.Value
rpcResult responseId result = Aeson.object
  [ "jsonrpc" Aeson..= ("2.0" :: String), "id" Aeson..= responseId, "result" Aeson..= result ]

notification :: String -> Aeson.Value -> Aeson.Value
notification method params = Aeson.object
  [ "jsonrpc" Aeson..= ("2.0" :: String), "method" Aeson..= method, "params" Aeson..= params ]
