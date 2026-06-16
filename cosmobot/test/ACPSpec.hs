module Main (main) where

import qualified Bot.ACP.Config as ACPConfig
import qualified Bot.ACP.Server as ACPServer
import qualified Bot.ACP.Types as ACP
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.Async as Async
import qualified JSONRPC
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS
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

testWebSocketServerAuthenticatesAndHandlesInitialize :: IO ()
testWebSocketServerAuthenticatesAndHandlesInitialize = do
  result <- timeout 2_000_000 $ runEff $ runConcurrent do
    listenSocket <- liftIO (WS.makeListenSocket "127.0.0.1" 0)
    port <- (fromIntegral :: Socket.PortNumber -> Int) <$> liftIO (Socket.socketPort listenSocket)
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
              ACPServer.acpServerApp cfg pending)
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
