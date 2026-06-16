module Main (main) where

import qualified Bot.ACP.Config as ACPConfig
import qualified Bot.ACP.Server as ACPServer
import qualified Bot.ACP.State as ACPState
import qualified Bot.ACP.Types as ACP
import qualified Bot.Chat.Driver.ACP as ACPDriver
import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Concurrency.Manager as ConcurrencyManager
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Session as Session
import Bot.Prelude
import qualified Bot.Storage.SQLite as StorageSQLite
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.Async as Async
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.FileSystem as FileSystem
import qualified JSONRPC
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS
import qualified Streaming.Prelude as S
import System.IO.Error (userError)
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit
import qualified Toml
import Toml.Schema

newtype AcpClientConfig = AcpClientConfig
  { acp :: ACPConfig.FileConfig
  }
  deriving (Show)

instance FromValue AcpClientConfig where
  fromValue = parseTableFromValue $
    AcpClientConfig
      <$> fmap (fromMaybe ACPConfig.defaultFileConfig) (optKey "acp")

main :: IO ()
main =
  defaultMain $
    testGroup "acp"
      [ testCase "enabled config requires token" testEnabledConfigRequiresToken
      , testCase "session/new creates session and session/delete deletes it" testSessionNewAndDelete
      , testCase "session/list returns durable sessions with cwd filter" testSessionList
      , testCase "session/load replays durable text history" testSessionLoadReplaysHistory
      , testCase "session/resume close and cancel check existing sessions" testSessionExistingRequests
      , testCase "session/prompt streams text response updates" testSessionPromptStreamsTextResponse
      , testCase "websocket server authenticates and handles initialize" testWebSocketServerAuthenticatesAndHandlesInitialize
      ]

testEnabledConfigRequiresToken :: IO ()
testEnabledConfigRequiresToken =
  case Toml.decode "[acp]\nenabled = true\n" of
    Toml.Failure errors ->
      assertBool
        "expected acp.token validation failure"
        ("acp.token must be non-empty" `Text.isInfixOf` Text.unlines (map toText errors))
    Toml.Success _warnings (config_ :: AcpClientConfig) ->
      assertFailure [i|expected parse failure, got #{show config_ :: String}|]

testSessionNewAndDelete :: IO ()
testSessionNewAndDelete =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    newResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    sessionId <- liftIO $
      case responseField newResponse "sessionId" of
        Nothing ->
          assertFailure "expected sessionId"
        Just (value :: Text) ->
          pure value
    deleteResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/delete" $
        Aeson.object
          [ "sessionId" Aeson..= sessionId
          ]
    liftIO do
      responseHasObjectResult deleteResponse @?= True

testSessionList :: IO ()
testSessionList =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    _local <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    _other <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-other" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    listResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/list" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          ]
    liftIO do
      responseField listResponse "sessions" @?=
        Just
          [ Aeson.object
              [ "sessionId" Aeson..= ("/tmp/cosmobot-acp-spec-1" :: Text)
              , "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
              , "title" Aeson..= Just ("/tmp/cosmobot-acp-spec" :: Text)
              ]
          ]

testSessionLoadReplaysHistory :: IO ()
testSessionLoadReplaysHistory =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    session <- ACPState.openSession (Just "/tmp/cosmobot-acp-spec")
    userMessage <- ACPState.enqueueUserMessage acpState $
      Session.SessionSend
        { sessionId = session.sessionId
        , text = "question"
        , imageUrls = []
        , attachments = []
        , replyToMessageId = Nothing
        }
    incoming <- liftIO $
      case userMessage of
        Left err ->
          assertFailure [i|unexpected ACP user message error: #{err}|]
        Right Nothing ->
          assertFailure "expected stored ACP user message"
        Right (Just incoming) ->
          pure incoming
    let driver = ACPDriver.acpChatDriver acpState
    Driver.sendReplyMessage driver incoming "answer" >>= \case
      Left err ->
        liftIO (assertFailure [i|unexpected ACP reply error: #{err}|])
      Right _ ->
        pure ()
    (_clientId, queue) <- ACPState.registerClient acpState
    loadResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/load" $
        Aeson.object
          [ "sessionId" Aeson..= ACPState.acpSessionIdText session.sessionId
          , "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    events <- replicateM 2 (ACPState.readClient queue)
    liftIO do
      responseResult loadResponse @?= Just Aeson.Null
      acpEventUpdateKinds events @?=
        [ "user_message_chunk"
        , "agent_message_chunk"
        ]

testSessionExistingRequests :: IO ()
testSessionExistingRequests =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    newResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    sessionId <- liftIO $
      case responseField newResponse "sessionId" of
        Nothing ->
          assertFailure "expected sessionId"
        Just (value :: Text) ->
          pure value
    responses <- traverse (dispatchSessionIdRequest acpState queue sessionId)
      [ "session/resume"
      , "session/close"
      , "session/cancel"
      ]
    missingResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/resume" $
        Aeson.object
          [ "sessionId" Aeson..= ("missing" :: Text)
          ]
    liftIO do
      map responseHasObjectResult responses @?= [True, True, True]
      responseErrorCode missingResponse @?= Just "not_found"

testSessionPromptStreamsTextResponse :: IO ()
testSessionPromptStreamsTextResponse =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    newResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    sessionId <- liftIO $
      case responseField newResponse "sessionId" of
        Nothing ->
          assertFailure "expected sessionId"
        Just (value :: Text) ->
          pure value
    responderDone <- MVar.newEmptyMVar
    _responder <- forkIO do
      S.head_ (ACPState.incomingMessages acpState) >>= \case
        Nothing ->
          throwIO (userError "expected ACP incoming message")
        Just incoming -> do
          let driver = ACPDriver.acpChatDriver acpState
          Driver.sendStreamingReplyMessage driver incoming "hel" >>= \case
            Left err ->
              throwIO (userError (Text.unpack err))
            Right messageId -> do
              _ <- Driver.editMessage driver incoming messageId "hello"
              _ <- Driver.completeMessageEdit driver incoming messageId
              MVar.putMVar responderDone ()
    promptResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/prompt" $
        Aeson.object
          [ "sessionId" Aeson..= sessionId
          , "prompt" Aeson..=
              [ Aeson.object
                  [ "type" Aeson..= ("text" :: Text)
                  , "text" Aeson..= ("say hello" :: Text)
                  ]
              ]
          ]
    MVar.takeMVar responderDone
    events <- replicateM 3 (ACPState.readClient queue)
    liftIO do
      responseField promptResponse "stopReason" @?= Just ("end_turn" :: Text)
      acpEventUpdateKinds events @?=
        [ "user_message_chunk"
        , "agent_message_chunk"
        , "agent_message_chunk"
        ]

testWebSocketServerAuthenticatesAndHandlesInitialize :: IO ()
testWebSocketServerAuthenticatesAndHandlesInitialize = do
  result <- timeout 2_000_000 $ runAcpStorage do
    listenSocket <- liftIO (WS.makeListenSocket "127.0.0.1" 0)
    port <- (fromIntegral :: Socket.PortNumber -> Int) <$> liftIO (Socket.socketPort listenSocket)
    acpState <- ACPState.newAcpState
    let cfg = ACPConfig.Config
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
              ACPServer.acpServerApp cfg acpState pending)
            (liftIO (Socket.close listenSocket))
        client = do
          unauthorized <- try @WS.HandshakeException (liftIO (WS.runClient "127.0.0.1" port "/acp" \_ -> pure ()))
          response <- liftIO (initializeClient port "secret")
          pure (unauthorized, response)
    Async.race server client

  case result of
    Nothing ->
      assertFailure "ACP websocket integration test timed out"
    Just (Left ()) ->
      assertFailure "ACP server exited before client completed"
    Just (Right (unauthorized, response)) -> do
      assertBool "expected unauthenticated websocket rejection" (isLeft unauthorized)
      response @?= initializeResponse

initializeClient :: Int -> Text -> IO ACP.AcpResponse
initializeClient port token =
  WS.runClientWith "127.0.0.1" port "/acp" WS.defaultConnectionOptions [("Authorization", "Bearer " <> TextEncoding.encodeUtf8 token)] \conn -> do
    WS.sendTextData conn $
      Aeson.encode $
        JSONRPC.JSONRPCRequest
          JSONRPC.rPC_VERSION
          (JSONRPC.RequestId (Aeson.String "test-1"))
          "initialize"
          ( Aeson.object
              [ "protocolVersion" Aeson..= (1 :: Int)
              , "clientCapabilities" Aeson..= Aeson.object []
              , "clientInfo" Aeson..=
                  Aeson.object
                    [ "name" Aeson..= ("acp-spec" :: Text)
                    , "version" Aeson..= ("0.0.0" :: Text)
                    ]
              ]
          )
    bytes <- WS.receiveData conn :: IO ByteString.ByteString
    case Aeson.eitherDecodeStrict' bytes of
      Left err -> fail [i|ACP websocket response was not JSON-RPC: #{err}|]
      Right response -> pure response

initializeResponse :: ACP.AcpResponse
initializeResponse =
  ACP.successResponse (JSONRPC.RequestId (Aeson.String "test-1")) $
    Aeson.object
      [ "protocolVersion" Aeson..= (1 :: Int)
      , "agentCapabilities" Aeson..=
          Aeson.object
            [ "loadSession" Aeson..= True
            , "promptCapabilities" Aeson..=
                Aeson.object
                  [ "image" Aeson..= False
                  , "audio" Aeson..= False
                  , "embeddedContext" Aeson..= False
                  ]
            , "sessionCapabilities" Aeson..=
                Aeson.object
                  [ "delete" Aeson..= Aeson.object []
                  , "list" Aeson..= Aeson.object []
                  , "resume" Aeson..= Aeson.object []
                  , "close" Aeson..= Aeson.object []
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

acpRequest :: Text -> Aeson.Value -> ACP.AcpRequest
acpRequest method params =
  JSONRPC.JSONRPCRequest JSONRPC.rPC_VERSION (JSONRPC.RequestId (Aeson.String "test-1")) method params

responseField :: Aeson.FromJSON a => ACP.AcpResponse -> Text -> Maybe a
responseField response name =
  case response of
    JSONRPC.ResponseMessage result -> do
      Aeson.Object object <- Just result.result
      AesonTypes.parseMaybe (Aeson..: fromString (Text.unpack name)) object
    JSONRPC.ErrorMessage{} ->
      Nothing
    JSONRPC.NotificationMessage{} ->
      Nothing
    JSONRPC.RequestMessage{} ->
      Nothing

responseHasObjectResult :: ACP.AcpResponse -> Bool
responseHasObjectResult = \case
  JSONRPC.ResponseMessage result ->
    case result.result of
      Aeson.Object{} ->
        True
      _ ->
        False
  JSONRPC.ErrorMessage{} ->
    False
  JSONRPC.NotificationMessage{} ->
    False
  JSONRPC.RequestMessage{} ->
    False

responseResult :: ACP.AcpResponse -> Maybe Aeson.Value
responseResult = \case
  JSONRPC.ResponseMessage result ->
    Just result.result
  JSONRPC.ErrorMessage{} ->
    Nothing
  JSONRPC.NotificationMessage{} ->
    Nothing
  JSONRPC.RequestMessage{} ->
    Nothing

responseErrorCode :: ACP.AcpResponse -> Maybe Text
responseErrorCode = \case
  JSONRPC.ErrorMessage err -> do
    Aeson.Object object <- err.error.errorData
    AesonTypes.parseMaybe (Aeson..: "code") object
  JSONRPC.ResponseMessage{} ->
    Nothing
  JSONRPC.NotificationMessage{} ->
    Nothing
  JSONRPC.RequestMessage{} ->
    Nothing

dispatchSessionIdRequest
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => ACPState.AcpState
  -> ACPState.AcpClientQueue
  -> Text
  -> Text
  -> Eff es ACP.AcpResponse
dispatchSessionIdRequest acpState queue sessionId method =
  ACPServer.dispatchAcpRequest acpState queue $
    acpRequest method $
      Aeson.object
        [ "sessionId" Aeson..= sessionId
        ]

acpEventUpdateKinds :: [ACPState.AcpClientEvent] -> [Text]
acpEventUpdateKinds events =
  mapMaybe acpEventUpdateKind events

acpEventUpdateKind :: ACPState.AcpClientEvent -> Maybe Text
acpEventUpdateKind = \case
  ACPState.AcpClientSend value -> do
    JSONRPC.NotificationMessage notification_ <- Aeson.decode (Aeson.encode value)
    guard (notification_.method == "session/update")
    Aeson.Object params <- Just notification_.params
    Aeson.Object update <- AesonTypes.parseMaybe (Aeson..: "update") params
    AesonTypes.parseMaybe (Aeson..: "sessionUpdate") update
  ACPState.AcpClientDisconnect{} ->
    Nothing

runAcpStorage
  :: Eff '[Media.Media, Storage.Storage, KatipE, FileSystem.FileSystem, Concurrency.Concurrency, Prim, Concurrent, IOE] a
  -> IO a
runAcpStorage action =
  runEff $
    runConcurrent $
      runPrim $
        ConcurrencyManager.runConcurrencyManager $
          FileSystem.runFileSystem $
            runTestLog $
              StorageSQLite.runStorageSQLitePath ":memory:" $
                Media.runMediaPassthrough action

runTestLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runTestLog =
  startKatipE "acp-spec" "test"
