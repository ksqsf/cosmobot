module Main (main) where

import Bot.Prelude
import Bot.Core.Message
import qualified Bot.RPC.Config as RPCConfig
import qualified Bot.RPC.Protocol as Protocol
import qualified Bot.RPC.Server as RPCServer
import qualified Bot.RPC.State as RPC
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Effectful.Concurrent.STM as STM
import qualified JSONRPC
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS
import qualified Streaming.Prelude as S
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit
import qualified Toml
import Toml.Schema

main :: IO ()
main =
  defaultMain $
    testGroup "rpc"
      [ testCase "request params default to empty object" testRequestParamsDefaultToEmptyObject
      , testCase "enabled config requires token" testEnabledConfigRequiresToken
      , testCase "chat.open_session returns generated session id" testOpenSessionReturnsGeneratedSessionId
      , testCase "chat.send constructs PlatformRPC incoming message" testChatSendConstructsIncomingMessage
      , testCase "chat.send broadcasts user chat notification" testChatSendBroadcastsNotification
      , testCase "websocket server authenticates and handles JSON-RPC requests" testWebSocketServerAuthenticatesAndHandlesRequests
      ]

testRequestParamsDefaultToEmptyObject :: IO ()
testRequestParamsDefaultToEmptyObject = do
  let encoded = "{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"method\":\"audit.recent\"}"
  request <- either assertFailure pure (Aeson.eitherDecodeStrict' encoded :: Either String Protocol.RpcRequest)
  request.jsonrpc @?= "2.0"
  request.id @?= JSONRPC.RequestId (Aeson.String "1")
  request.method @?= "audit.recent"
  request.params @?= Aeson.Null

testEnabledConfigRequiresToken :: IO ()
testEnabledConfigRequiresToken =
  case Toml.decode "[rpc]\nenabled = true\n" of
    Toml.Failure errors ->
      assertBool
        "expected rpc.token validation failure"
        ("rpc.token must be non-empty" `Text.isInfixOf` Text.unlines (map toText errors))
    Toml.Success _warnings (config_ :: RpcClientConfig) ->
      assertFailure [i|expected parse failure, got #{show config_ :: String}|]

testOpenSessionReturnsGeneratedSessionId :: IO ()
testOpenSessionReturnsGeneratedSessionId = do
  response <- runEff $ runConcurrent do
    rpcState <- RPC.newRpcState
    RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
      rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("browser" :: Text)])
  response @?= responseResult (Aeson.object ["sessionId" Aeson..= ("browser-1" :: Text)])

testChatSendConstructsIncomingMessage :: IO ()
testChatSendConstructsIncomingMessage = do
  (response, incoming) <- runEff $ runConcurrent do
    rpcState <- RPC.newRpcState
    response <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
      rpcRequest "chat.send" $
        Aeson.object
          [ "session_id" Aeson..= ("browser-1" :: Text)
          , "text" Aeson..= ("hello" :: Text)
          , "image_urls" Aeson..= ["https://example.test/image.png" :: Text]
          , "reply_to_message_id" Aeson..= ("rpc-0" :: Text)
          ]
    incoming <- fromMaybe (error "expected one incoming RPC message") <$> S.head_ (RPC.incomingMessages rpcState)
    pure (response, incoming)

  response @?= responseResult (Aeson.object ["sessionId" Aeson..= ("browser-1" :: Text), "messageId" Aeson..= ("rpc-1" :: Text)])
  incoming.platform @?= PlatformRPC
  incoming.kind @?= ChatPrivate
  incoming.chatAliases @?= ["browser-1"]
  incoming.senderId @?= Just "rpc-user"
  incoming.text @?= "hello"
  incoming.imageUrls @?= ["https://example.test/image.png"]
  incoming.replyToMessageId @?= Just "rpc-0"
  incoming.digest.senderIsAllowed @?= True
  incoming.digest.senderIsSuperuser @?= True
  incoming.digest.mentionsBot @?= True

testChatSendBroadcastsNotification :: IO ()
testChatSendBroadcastsNotification = do
  notificationValue <- runEff $ runConcurrent do
    rpcState <- RPC.newRpcState
    (_clientId, queue) <- RPC.registerClient rpcState
    _response <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
      rpcRequest "chat.send" $
        Aeson.object
          [ "sessionId" Aeson..= ("browser-1" :: Text)
          , "text" Aeson..= ("hello" :: Text)
          ]
    STM.atomically (STM.readTChan queue)

  notification <- parseJson notificationValue :: IO Protocol.RpcNotification
  notification.method @?= "chat.message"
  notification.params @?=
    Aeson.object
      [ "sessionId" Aeson..= ("browser-1" :: Text)
      , "messageId" Aeson..= ("rpc-1" :: Text)
      , "sender" Aeson..= ("user" :: Text)
      , "text" Aeson..= ("hello" :: Text)
      , "imageUrls" Aeson..= ([] :: [Text])
      , "replyToMessageId" Aeson..= (Nothing :: Maybe Text)
      ]

testWebSocketServerAuthenticatesAndHandlesRequests :: IO ()
testWebSocketServerAuthenticatesAndHandlesRequests = do
  result <- timeout 2_000_000 $ runEff $ runConcurrent $ runTestLog do
    rpcState <- RPC.newRpcState
    listenSocket <- liftIO (WS.makeListenSocket "127.0.0.1" 0)
    port <- fromIntegral <$> liftIO (Socket.socketPort listenSocket)
    let cfg = RPCConfig.Config
          { enabled = True
          , host = "127.0.0.1"
          , port
          , token = "secret"
          }
        server =
          finally
            (forever do
              (clientSocket, _) <- liftIO (Socket.accept listenSocket)
              pending <- liftIO (WS.makePendingConnection clientSocket WS.defaultConnectionOptions)
              RPCServer.rpcServerApp cfg rpcState RPCServer.noRpcServerCallbacks pending)
            (liftIO (Socket.close listenSocket))
        client = do
          unauthorized <- trySync (liftIO (WS.runClient "127.0.0.1" port "/" \_ -> pure ()))
          response <- liftIO (openSessionClient port "secret")
          pure (unauthorized, response)
    race server client

  case result of
    Nothing ->
      assertFailure "RPC websocket integration test timed out"
    Just (Left ()) ->
      assertFailure "RPC server exited before client completed"
    Just (Right (unauthorized, response)) -> do
      assertUnauthorizedRejected unauthorized
      response @?= responseResult (Aeson.object ["sessionId" Aeson..= ("integration-1" :: Text)])

data RpcClientConfig = RpcClientConfig
  { rpc :: RPCConfig.FileConfig
  }
  deriving (Show)

instance FromValue RpcClientConfig where
  fromValue = parseTableFromValue $
    RpcClientConfig
      <$> fmap (fromMaybe RPCConfig.defaultFileConfig) (optKey "rpc")

rpcRequest :: Text -> Aeson.Value -> Protocol.RpcRequest
rpcRequest method params =
  Protocol.rpcRequest method params "test-1"

responseResult :: Aeson.Value -> Protocol.RpcResponse
responseResult =
  Protocol.successResponse (JSONRPC.RequestId (Aeson.String "test-1"))

parseJson :: Aeson.FromJSON a => Aeson.Value -> IO a
parseJson value =
  case AesonTypes.parseEither Aeson.parseJSON value of
    Left err -> assertFailure err
    Right parsed -> pure parsed

openSessionClient :: Int -> Text -> IO Protocol.RpcResponse
openSessionClient port token =
  WS.runClient "127.0.0.1" port ("/?access_token=" <> Text.unpack token) \conn -> do
    WS.sendTextData conn $
      Aeson.encode $
        Protocol.rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("integration" :: Text)]) "test-1"
    bytes <- WS.receiveData conn :: IO ByteString
    case Aeson.eitherDecodeStrict' bytes of
      Left err -> fail [i|RPC websocket response was not JSON-RPC: #{err}|]
      Right response -> pure response

assertUnauthorizedRejected :: Either SomeException () -> IO ()
assertUnauthorizedRejected = \case
  Left err
    | Just (WS.RequestRejected _ response) <- fromException err ->
        WS.responseCode response @?= 401
    | Just (WS.MalformedResponse response _) <- fromException err ->
        WS.responseCode response @?= 401
    | otherwise ->
        assertFailure [i|expected websocket 401 rejection, got #{show err :: String}|]
  Right () ->
    assertFailure "expected unauthenticated websocket connection to fail"

runTestLog :: IOE :> es => Eff (Log : es) a -> Eff es a
runTestLog action = do
  logger <- liftIO $ mkLogger "rpc-spec" \_ -> pure ()
  runLog "rpc-spec" logger LogTrace action
