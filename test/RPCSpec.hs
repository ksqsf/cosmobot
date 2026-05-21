module Main (main) where

import Bot.Prelude
import Bot.Chat.Driver.Types
import Bot.Core.Message
import qualified Bot.Effect.Storage as Storage
import qualified Bot.RPC.Config as RPCConfig
import qualified Bot.RPC.Protocol as Protocol
import qualified Bot.RPC.Server as RPCServer
import qualified Bot.RPC.State as RPC
import qualified Bot.Storage.SQLite as StorageSQLite
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import Data.Unique (hashUnique, newUnique)
import qualified Effectful.Concurrent.STM as STM
import Effectful.FileSystem (runFileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as Http
import qualified JSONRPC
import qualified Network.Socket as Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import qualified Streaming.Prelude as S
import System.FilePath ((</>))
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
      , testCase "chat.send rejects missing sessions without persisting orphan messages" testChatSendRejectsMissingSession
      , testCase "chat.send broadcasts user chat notification" testChatSendBroadcastsNotification
      , testCase "chat sessions and messages persist across RPC state restart" testChatSessionsPersistAcrossRestart
      , testCase "rpc driver persists assistant replies and edited stream text" testRpcDriverPersistsAssistantRepliesAndEdits
      , testCase "chat.fork stores immutable parent link and inherited history" testChatForkStoresParentLink
      , testCase "chat.rename_session and chat.delete_session update durable storage" testRenameAndDeleteSession
      , testCase "chat.delete_session cascades fork descendants" testDeleteSessionCascadesForkDescendants
      , testCase "websocket server authenticates and handles JSON-RPC requests" testWebSocketServerAuthenticatesAndHandlesRequests
      , testCase "HTTP server serves static fallback and protects attachment route" testHttpServerServesStaticFallbackAndProtectsAttachmentRoute
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
  response <- runRpcStorage ":memory:" do
    rpcState <- RPC.newRpcState
    RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
      rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("browser" :: Text)])
  response @?=
    responseResult
      ( Aeson.object
          [ "sessionId" Aeson..= ("browser-1" :: Text)
          , "session" Aeson..= sessionValue "browser-1" (Just "browser") Nothing Nothing
          ]
      )

testChatSendConstructsIncomingMessage :: IO ()
testChatSendConstructsIncomingMessage = do
  (response, incoming) <- runRpcStorage ":memory:" do
    rpcState <- RPC.newRpcState
    _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
      rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("browser" :: Text)])
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

testChatSendRejectsMissingSession :: IO ()
testChatSendRejectsMissingSession =
  withSQLiteTempPath "rpc-missing-send" \path -> do
    (sendResponse, openResponse, historyResponse) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      sendResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" $
          Aeson.object
            [ "sessionId" Aeson..= ("missing-1" :: Text)
            , "text" Aeson..= ("orphan" :: Text)
            ]
      openResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("missing" :: Text)])
      historyResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.history" (Aeson.object ["sessionId" Aeson..= ("missing-1" :: Text)])
      pure (sendResponse, openResponse, historyResponse)

    sendResponse @?= responseError "not_found" "Session not found"
    openResponse @?=
      responseResult
        ( Aeson.object
            [ "sessionId" Aeson..= ("missing-1" :: Text)
            , "session" Aeson..= sessionValue "missing-1" (Just "missing") Nothing Nothing
            ]
        )
    historyResponse @?=
      responseResult
        ( Aeson.object
            [ "sessionId" Aeson..= ("missing-1" :: Text)
            , "messages" Aeson..= ([] :: [Aeson.Value])
            ]
        )

testChatSendBroadcastsNotification :: IO ()
testChatSendBroadcastsNotification = do
  notificationValue <- runRpcStorage ":memory:" do
    rpcState <- RPC.newRpcState
    (_clientId, queue) <- RPC.registerClient rpcState
    _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
      rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("browser" :: Text)])
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
      , "parentMessageId" Aeson..= (Nothing :: Maybe Text)
      ]

testChatSessionsPersistAcrossRestart :: IO ()
testChatSessionsPersistAcrossRestart =
  withSQLiteTempPath "rpc-persist" \path -> do
    firstResponse <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("browser" :: Text)])
      RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" $
          Aeson.object
            [ "sessionId" Aeson..= ("browser-1" :: Text)
            , "text" Aeson..= ("persisted" :: Text)
            ]

    firstResponse @?= responseResult (Aeson.object ["sessionId" Aeson..= ("browser-1" :: Text), "messageId" Aeson..= Just ("rpc-1" :: Text)])

    (listResponse, historyResponse, sendResponse) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      listResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.list_sessions" Aeson.Null
      historyResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.history" (Aeson.object ["sessionId" Aeson..= ("browser-1" :: Text)])
      sendResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" $
          Aeson.object
            [ "sessionId" Aeson..= ("browser-1" :: Text)
            , "text" Aeson..= ("after restart" :: Text)
            ]
      pure (listResponse, historyResponse, sendResponse)

    listResponse @?=
      responseResult
        (Aeson.object ["sessions" Aeson..= [sessionValue "browser-1" (Just "browser") Nothing Nothing]])
    historyResponse @?=
      responseResult
        ( Aeson.object
            [ "sessionId" Aeson..= ("browser-1" :: Text)
            , "messages" Aeson..= [messageValue "browser-1" "rpc-1" "persisted" Nothing]
            ]
        )
    sendResponse @?= responseResult (Aeson.object ["sessionId" Aeson..= ("browser-1" :: Text), "messageId" Aeson..= Just ("rpc-2" :: Text)])

testRpcDriverPersistsAssistantRepliesAndEdits :: IO ()
testRpcDriverPersistsAssistantRepliesAndEdits =
  withSQLiteTempPath "rpc-assistant" \path -> do
    (replyId, edited) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("browser" :: Text)])
      _sent <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" $
          Aeson.object
            [ "sessionId" Aeson..= ("browser-1" :: Text)
            , "text" Aeson..= ("question" :: Text)
            ]
      incoming <- fromMaybe (error "expected one incoming RPC message") <$> S.head_ (RPC.incomingMessages rpcState)
      let driver = RPC.rpcChatDriver rpcState
      replyId <- fromMaybe (error "expected rpc reply id") <$> driver.replyTo incoming "draft answer"
      edited <- driver.editMessage incoming replyId "final answer"
      pure (replyId, edited)

    replyId @?= "rpc-2"
    edited @?= True

    historyResponse <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.history" (Aeson.object ["sessionId" Aeson..= ("browser-1" :: Text)])

    responseMessageSummaries historyResponse @?=
      [ ("user", "rpc-1", "question")
      , ("assistant", "rpc-2", "final answer")
      ]

testChatForkStoresParentLink :: IO ()
testChatForkStoresParentLink =
  withSQLiteTempPath "rpc-fork" \path -> do
    (forkResponse, forkHistory) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("root" :: Text)])
      _first <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" (Aeson.object ["sessionId" Aeson..= ("root-1" :: Text), "text" Aeson..= ("first" :: Text)])
      _second <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" (Aeson.object ["sessionId" Aeson..= ("root-1" :: Text), "text" Aeson..= ("second" :: Text)])
      forkResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.fork" $
          Aeson.object
            [ "sessionId" Aeson..= ("root-1" :: Text)
            , "messageId" Aeson..= ("rpc-1" :: Text)
            , "label" Aeson..= ("branch" :: Text)
            ]
      _branch <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" (Aeson.object ["sessionId" Aeson..= ("branch-1" :: Text), "text" Aeson..= ("branch only" :: Text)])
      forkHistory <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.history" (Aeson.object ["sessionId" Aeson..= ("branch-1" :: Text)])
      pure (forkResponse, forkHistory)

    forkResponse @?=
      responseResult
        ( Aeson.object
            [ "sessionId" Aeson..= ("branch-1" :: Text)
            , "session" Aeson..= sessionValue "branch-1" (Just "branch") (Just "root-1") (Just "rpc-1")
            ]
        )
    responseMessageTexts forkHistory @?= ["first", "branch only"]

testRenameAndDeleteSession :: IO ()
testRenameAndDeleteSession =
  withSQLiteTempPath "rpc-delete" \path -> do
    (renameResponse, deleteResponse, listResponse) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("old" :: Text)])
      _sent <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" (Aeson.object ["sessionId" Aeson..= ("old-1" :: Text), "text" Aeson..= ("gone" :: Text)])
      renameResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.rename_session" (Aeson.object ["sessionId" Aeson..= ("old-1" :: Text), "label" Aeson..= ("new" :: Text)])
      deleteResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.delete_session" (Aeson.object ["sessionId" Aeson..= ("old-1" :: Text)])
      listResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.list_sessions" Aeson.Null
      pure (renameResponse, deleteResponse, listResponse)

    responseSessionLabel renameResponse @?= Just "new"
    deleteResponse @?= responseResult (Aeson.object ["sessionId" Aeson..= ("old-1" :: Text), "deleted" Aeson..= True])
    listResponse @?= responseResult (Aeson.object ["sessions" Aeson..= ([] :: [Aeson.Value])])

testDeleteSessionCascadesForkDescendants :: IO ()
testDeleteSessionCascadesForkDescendants =
  withSQLiteTempPath "rpc-delete-fork" \path -> do
    (deleteResponse, listResponse, branchSendResponse) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("root" :: Text)])
      _first <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" (Aeson.object ["sessionId" Aeson..= ("root-1" :: Text), "text" Aeson..= ("first" :: Text)])
      _fork <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.fork" $
          Aeson.object
            [ "sessionId" Aeson..= ("root-1" :: Text)
            , "messageId" Aeson..= ("rpc-1" :: Text)
            , "label" Aeson..= ("branch" :: Text)
            ]
      _branch <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" (Aeson.object ["sessionId" Aeson..= ("branch-1" :: Text), "text" Aeson..= ("branch only" :: Text)])
      deleteResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.delete_session" (Aeson.object ["sessionId" Aeson..= ("root-1" :: Text)])
      listResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.list_sessions" Aeson.Null
      branchSendResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" (Aeson.object ["sessionId" Aeson..= ("branch-1" :: Text), "text" Aeson..= ("after delete" :: Text)])
      pure (deleteResponse, listResponse, branchSendResponse)

    deleteResponse @?= responseResult (Aeson.object ["sessionId" Aeson..= ("root-1" :: Text), "deleted" Aeson..= True])
    listResponse @?= responseResult (Aeson.object ["sessions" Aeson..= ([] :: [Aeson.Value])])
    branchSendResponse @?= responseError "not_found" "Session not found"

testWebSocketServerAuthenticatesAndHandlesRequests :: IO ()
testWebSocketServerAuthenticatesAndHandlesRequests = do
  result <- timeout 2_000_000 $ runEff $ runConcurrent $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" do
    rpcState <- RPC.newRpcState
    listenSocket <- liftIO (WS.makeListenSocket "127.0.0.1" 0)
    port <- fromIntegral <$> liftIO (Socket.socketPort listenSocket)
    let cfg = RPCConfig.Config
          { enabled = True
        , host = "127.0.0.1"
        , port
        , token = "secret"
        , staticDir = "web/dist"
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
      response @?=
        responseResult
          ( Aeson.object
              [ "sessionId" Aeson..= ("integration-1" :: Text)
              , "session" Aeson..= sessionValue "integration-1" (Just "integration") Nothing Nothing
              ]
          )

testHttpServerServesStaticFallbackAndProtectsAttachmentRoute :: IO ()
testHttpServerServesStaticFallbackAndProtectsAttachmentRoute = do
  result <- timeout 2_000_000 $ runEff $ runConcurrent $ runFileSystem $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" do
    rpcState <- RPC.newRpcState
    listenSocket <- liftIO (WS.makeListenSocket "127.0.0.1" 0)
    port <- fromIntegral <$> liftIO (Socket.socketPort listenSocket)
    let cfg = RPCConfig.Config
          { enabled = True
          , host = "127.0.0.1"
          , port
          , token = "secret"
          , staticDir = "web/dist"
          }
        server =
          withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
            liftIO $
              Warp.runSettingsSocket Warp.defaultSettings listenSocket $
                RPCServer.rpcServerApplication runInIO cfg rpcState RPCServer.noRpcServerCallbacks
        client = liftIO do
          manager <- HTTP.newManager HTTP.defaultManagerSettings
          root <- httpGet manager [i|http://127.0.0.1:#{port}/|]
          attachmentWithoutToken <- httpGet manager [i|http://127.0.0.1:#{port}/attachments/missing|]
          attachmentWithToken <- httpGet manager [i|http://127.0.0.1:#{port}/attachments/missing?access_token=secret|]
          response <- openSessionClientAtPath port "/rpc?access_token=secret"
          pure (root, attachmentWithoutToken, attachmentWithToken, response)
    race server client

  case result of
    Nothing ->
      assertFailure "RPC HTTP integration test timed out"
    Just (Left ()) ->
      assertFailure "RPC HTTP server exited before client completed"
    Just (Right (root, attachmentWithoutToken, attachmentWithToken, response)) -> do
      HTTP.responseStatus root @?= Http.status200
      assertBool
        "expected fallback web/index.html"
        ("<title>cosmobot</title>" `ByteStringChar8.isInfixOf` LazyByteString.toStrict (HTTP.responseBody root))
      HTTP.responseStatus attachmentWithoutToken @?= Http.status401
      HTTP.responseStatus attachmentWithToken @?= Http.status501
      response @?=
        responseResult
          ( Aeson.object
              [ "sessionId" Aeson..= ("integration-1" :: Text)
              , "session" Aeson..= sessionValue "integration-1" (Just "integration") Nothing Nothing
              ]
          )

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

responseError :: Text -> Text -> Protocol.RpcResponse
responseError code message =
  Protocol.errorResponse (JSONRPC.RequestId (Aeson.String "test-1")) code message

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

openSessionClientAtPath :: Int -> String -> IO Protocol.RpcResponse
openSessionClientAtPath port path =
  WS.runClient "127.0.0.1" port path \conn -> do
    WS.sendTextData conn $
      Aeson.encode $
        Protocol.rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("integration" :: Text)]) "test-1"
    bytes <- WS.receiveData conn :: IO ByteString
    case Aeson.eitherDecodeStrict' bytes of
      Left err -> fail [i|RPC websocket response was not JSON-RPC: #{err}|]
      Right response -> pure response

httpGet :: HTTP.Manager -> String -> IO (HTTP.Response LazyByteString.ByteString)
httpGet manager url = do
  request <- HTTP.parseRequest url
  HTTP.httpLbs
    request
      { HTTP.checkResponse = \_ _ -> pure ()
      }
    manager

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

runRpcStorage :: FilePath -> Eff '[Storage.Storage, Concurrent, IOE] a -> IO a
runRpcStorage path action =
  runEff $ runConcurrent $ StorageSQLite.runStorageSQLitePath path action

withSQLiteTempPath :: String -> (FilePath -> IO a) -> IO a
withSQLiteTempPath label action =
  runEff $ runFileSystem do
    root <- FileSystem.getTemporaryDirectory
    unique <- liftIO (hashUnique <$> newUnique)
    let dir = root </> [i|cosmobot-#{label}-#{unique}|]
        path = dir </> "rpc.sqlite"
    bracket
      (FileSystem.createDirectory dir $> path)
      (\_ -> FileSystem.removeDirectoryRecursive dir)
      (liftIO . action)

sessionValue :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Aeson.Value
sessionValue sessionId label parentSessionId parentMessageId =
  Aeson.object
    [ "sessionId" Aeson..= sessionId
    , "label" Aeson..= label
    , "parentSessionId" Aeson..= parentSessionId
    , "parentMessageId" Aeson..= parentMessageId
    ]

messageValue :: Text -> Text -> Text -> Maybe Text -> Aeson.Value
messageValue sessionId messageId body parentMessageId =
  Aeson.object
    [ "sessionId" Aeson..= sessionId
    , "messageId" Aeson..= messageId
    , "sender" Aeson..= ("user" :: Text)
    , "text" Aeson..= body
    , "imageUrls" Aeson..= ([] :: [Text])
    , "replyToMessageId" Aeson..= parentMessageId
    , "parentMessageId" Aeson..= parentMessageId
    ]

responseMessageTexts :: Protocol.RpcResponse -> [Text]
responseMessageTexts response =
  case response of
    JSONRPC.ResponseMessage result ->
      fromMaybe [] do
        messages <- AesonTypes.parseMaybe (Aeson.withObject "history" (Aeson..: "messages")) result.result
        traverse (AesonTypes.parseMaybe (Aeson.withObject "message" (Aeson..: "text"))) messages
    _ ->
      []

responseMessageSummaries :: Protocol.RpcResponse -> [(Text, Text, Text)]
responseMessageSummaries response =
  case response of
    JSONRPC.ResponseMessage result ->
      fromMaybe [] do
        messages <- AesonTypes.parseMaybe (Aeson.withObject "history" (Aeson..: "messages")) result.result
        traverse messageSummary messages
    _ ->
      []
  where
    messageSummary =
      AesonTypes.parseMaybe $
        Aeson.withObject "message" \o -> do
          sender <- o Aeson..: "sender"
          messageId <- o Aeson..: "messageId"
          body <- o Aeson..: "text"
          pure (sender, messageId, body)

responseSessionLabel :: Protocol.RpcResponse -> Maybe Text
responseSessionLabel response =
  case response of
    JSONRPC.ResponseMessage result -> do
      session <- AesonTypes.parseMaybe (Aeson.withObject "rename" (Aeson..: "session")) result.result
      AesonTypes.parseMaybe (Aeson.withObject "session" (Aeson..: "label")) session
    _ ->
      Nothing
