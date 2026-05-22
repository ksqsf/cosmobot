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
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Unique (hashUnique, newUnique)
import Effectful.FileSystem (runFileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as Http
import qualified JSONRPC
import qualified Network.Socket as Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import qualified Streaming.Prelude as S
import System.FilePath ((</>), takeDirectory)
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit
import qualified Toml
import Toml.Schema

newtype TestRpcException = TestRpcException Text
  deriving (Show)

instance Exception TestRpcException

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
      , testCase "client notification queue overflow disconnects slow client" testClientNotificationQueueOverflowDisconnects
      , testCase "sync request exception returns JSON-RPC error" testSyncRequestExceptionReturnsJsonRpcError
      , testCase "attachments upload, send, history, and guarded delete" testAttachmentLifecycle
      , testCase "chat sessions and messages persist across RPC state restart" testChatSessionsPersistAcrossRestart
      , testCase "rpc driver persists assistant replies and edited stream text" testRpcDriverPersistsAssistantRepliesAndEdits
      , testCase "rpc driver stores local image replies as attachments" testRpcDriverStoresLocalImageRepliesAsAttachments
      , testCase "chat.fork stores immutable parent link and inherited history" testChatForkStoresParentLink
      , testCase "chat.rename_session and chat.delete_session update durable storage" testRenameAndDeleteSession
      , testCase "chat.delete_session cascades fork descendants" testDeleteSessionCascadesForkDescendants
      , testCase "websocket server authenticates and handles JSON-RPC requests" testWebSocketServerAuthenticatesAndHandlesRequests
      , testCase "HTTP server rejects static paths and protects attachment route" testHttpServerReturns404ForStaticAndProtectsAttachmentRoute
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
    RPC.readClient queue >>= \case
      RPC.RpcClientSend value ->
        pure value
      RPC.RpcClientDisconnect reason ->
        liftIO (assertFailure [i|unexpected RPC client disconnect: #{reason}|])

  notification <- parseJson notificationValue :: IO Protocol.RpcNotification
  notification.method @?= "chat.message"
  notification.params @?=
    Aeson.object
      [ "sessionId" Aeson..= ("browser-1" :: Text)
      , "messageId" Aeson..= ("rpc-1" :: Text)
      , "sender" Aeson..= ("user" :: Text)
      , "text" Aeson..= ("hello" :: Text)
      , "imageUrls" Aeson..= ([] :: [Text])
      , "attachments" Aeson..= ([] :: [Aeson.Value])
      , "replyToMessageId" Aeson..= (Nothing :: Maybe Text)
      , "parentMessageId" Aeson..= (Nothing :: Maybe Text)
      ]

testClientNotificationQueueOverflowDisconnects :: IO ()
testClientNotificationQueueOverflowDisconnects = do
  event <- runRpcStorage ":memory:" do
    rpcState <- RPC.newRpcState
    (_clientId, queue) <- RPC.registerClient rpcState
    replicateM_ 257 $
      RPC.broadcast rpcState (Aeson.object ["event" Aeson..= ("notification" :: Text)])
    RPC.readClient queue

  case event of
    RPC.RpcClientDisconnect reason ->
      reason @?= "RPC notification queue overflow"
    RPC.RpcClientSend value ->
      assertFailure [i|expected queue overflow disconnect, got #{Aeson.encode value}|]

testSyncRequestExceptionReturnsJsonRpcError :: IO ()
testSyncRequestExceptionReturnsJsonRpcError = do
  response <- runRpcStorage ":memory:" do
    rpcState <- RPC.newRpcState
    let callbacks =
          RPCServer.noRpcServerCallbacks
            { RPCServer.auditMethod = \_ ->
                throwIO (TestRpcException "audit exploded")
            }
    RPCServer.dispatchRpcRequest rpcState callbacks $
      rpcRequest "audit.recent" Aeson.Null

  case response of
    JSONRPC.ErrorMessage err -> do
      err.id @?= JSONRPC.RequestId (Aeson.String "test-1")
      JSONRPC.code err.error @?= JSONRPC.iNTERNAL_ERROR
      JSONRPC.message err.error @?= "RPC request failed: TestRpcException \"audit exploded\""
    _ ->
      assertFailure [i|expected JSON-RPC error response, got #{Aeson.encode response}|]

testAttachmentLifecycle :: IO ()
testAttachmentLifecycle =
  withSQLiteTempPath "rpc-attachments" \path -> do
    let cfg :: RPCConfig.Config
        cfg = RPCConfig.Config
          { enabled = False
          , host = "127.0.0.1"
          , port = 38765
          , token = ""
          , attachmentDir = takeDirectory path </> "attachments"
          , attachmentMaxBytes = 1024
          }
    (uploadResponse, imageUploadResponse, unsafeMediaResponse, oversizedResponse, sendResponse, historyResponse, incoming, deleteReferencedResponse) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      uploadResponse <- RPCServer.dispatchRpcRequestWithConfig rpcState cfg RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.upload_attachment" $
          Aeson.object
            [ "name" Aeson..= ("notes.txt" :: Text)
            , "mediaType" Aeson..= ("text/plain" :: Text)
            , "kind" Aeson..= ("file" :: Text)
            , "size" Aeson..= (5 :: Int)
            , "data" Aeson..= ("aGVsbG8=" :: Text)
            ]
      let attachment = responseAttachmentUnsafe uploadResponse
      imageUploadResponse <- RPCServer.dispatchRpcRequestWithConfig rpcState cfg RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.upload_attachment" $
          Aeson.object
            [ "name" Aeson..= ("pixel.png" :: Text)
            , "mediaType" Aeson..= ("image/png" :: Text)
            , "kind" Aeson..= ("image" :: Text)
            , "size" Aeson..= (1 :: Int)
            , "data" Aeson..= ("AA==" :: Text)
            ]
      let imageAttachment = responseAttachmentUnsafe imageUploadResponse
      unsafeMediaResponse <- RPCServer.dispatchRpcRequestWithConfig rpcState cfg RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.upload_attachment" $
          Aeson.object
            [ "name" Aeson..= ("unsafe.html" :: Text)
            , "mediaType" Aeson..= ("text/html\r\nx" :: Text)
            , "kind" Aeson..= ("file" :: Text)
            , "size" Aeson..= (1 :: Int)
            , "data" Aeson..= ("AA==" :: Text)
            ]
      let smallAttachmentConfig =
            RPCConfig.Config
              { enabled = cfg.enabled
              , host = cfg.host
              , port = cfg.port
              , token = cfg.token
              , attachmentDir = cfg.attachmentDir
              , attachmentMaxBytes = 1
              }
      oversizedResponse <- RPCServer.dispatchRpcRequestWithConfig rpcState smallAttachmentConfig RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.upload_attachment" $
          Aeson.object
            [ "name" Aeson..= ("large.bin" :: Text)
            , "mediaType" Aeson..= ("application/octet-stream" :: Text)
            , "kind" Aeson..= ("file" :: Text)
            , "data" Aeson..= (Text.replicate 8 "A" :: Text)
            ]
      _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("browser" :: Text)])
      sendResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" $
          Aeson.object
            [ "sessionId" Aeson..= ("browser-1" :: Text)
            , "text" Aeson..= ("see attached" :: Text)
            , "imageUrls" Aeson..= [imageAttachment.url, "https://example.test/context.png"]
            , "attachments" Aeson..= [attachment, imageAttachment]
            ]
      incoming <- fromMaybe (error "expected one incoming RPC message") <$> S.head_ (RPC.incomingMessages rpcState)
      historyResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.history" (Aeson.object ["sessionId" Aeson..= ("browser-1" :: Text)])
      deleteReferencedResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.delete_attachment" (Aeson.object ["attachmentId" Aeson..= attachment.attachmentId])
      pure (uploadResponse, imageUploadResponse, unsafeMediaResponse, oversizedResponse, sendResponse, historyResponse, incoming, deleteReferencedResponse)

    attachment <- responseAttachment uploadResponse
    imageAttachment <- responseAttachment imageUploadResponse
    unsafeMediaAttachment <- responseAttachment unsafeMediaResponse
    attachment.name @?= "notes.txt"
    attachment.mediaType @?= "text/plain"
    attachment.kind @?= "file"
    imageAttachment.kind @?= "image"
    unsafeMediaAttachment.mediaType @?= "application/octet-stream"
    oversizedResponse @?= responseError "invalid_params" "Error in $: encoded attachment exceeds configured limit"
    sendResponse @?= responseResult (Aeson.object ["sessionId" Aeson..= ("browser-1" :: Text), "messageId" Aeson..= Just ("rpc-1" :: Text)])
    assertBool "non-image attachment should be visible in incoming context" ("Attachments:" `Text.isInfixOf` incoming.text)
    incoming.imageUrls @?= ["https://example.test/context.png", "data:image/png;base64,AA=="]
    assertEqual [i|history response: #{show historyResponse :: String}|] [[attachment.attachmentId, imageAttachment.attachmentId]] (responseMessageAttachments historyResponse)
    deleteReferencedResponse @?=
      responseResult
        ( Aeson.object
            [ "attachmentId" Aeson..= attachment.attachmentId
            , "id" Aeson..= attachment.attachmentId
            , "deleted" Aeson..= False
            ]
        )

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
      let driver = RPC.rpcChatDriver testRpcConfig rpcState
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

testRpcDriverStoresLocalImageRepliesAsAttachments :: IO ()
testRpcDriverStoresLocalImageRepliesAsAttachments =
  withSQLiteTempPath "rpc-assistant-image" \path -> do
    historyResponse <- runRpcStorage path do
      let dir = takeDirectory path
          imagePath = dir </> "generated.webp"
          cfg :: RPCConfig.Config
          cfg = RPCConfig.Config
            { enabled = True
            , host = "127.0.0.1"
            , port = 38765
            , token = "secret"
            , attachmentDir = dir </> "attachments"
            , attachmentMaxBytes = 1024 * 1024
            }
      FileSystemByteString.writeFile imagePath "fake-webp"
      rpcState <- RPC.newRpcState
      _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("browser" :: Text)])
      _sent <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" $
          Aeson.object
            [ "sessionId" Aeson..= ("browser-1" :: Text)
            , "text" Aeson..= ("make an image" :: Text)
            ]
      incoming <- fromMaybe (error "expected one incoming RPC message") <$> S.head_ (RPC.incomingMessages rpcState)
      let driver = RPC.rpcChatDriver cfg rpcState
      _reply <- driver.replyTo incoming ("done\n[image] file://" <> Text.pack imagePath)
      RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.history" (Aeson.object ["sessionId" Aeson..= ("browser-1" :: Text)])

    responseMessageSummaries historyResponse @?=
      [ ("user", "rpc-1", "make an image")
      , ("assistant", "rpc-2", "done")
      ]
    case responseMessageAttachments historyResponse of
      [[], [attachmentId]] ->
        assertBool "expected stored image attachment id" ("att-" `Text.isPrefixOf` attachmentId)
      other ->
        assertFailure [i|expected one image attachment on assistant reply, got #{show other :: String}|]

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
  result <- timeout 2_000_000 $ runEff $ runConcurrent $ runFileSystem $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" do
    rpcState <- RPC.newRpcState
    listenSocket <- liftIO (WS.makeListenSocket "127.0.0.1" 0)
    port <- fromIntegral <$> liftIO (Socket.socketPort listenSocket)
    let cfg = RPCConfig.Config
          { enabled = True
        , host = "127.0.0.1"
        , port
        , token = "secret"
        , attachmentDir = "attachments"
        , attachmentMaxBytes = 1024 * 1024
        }
        server =
          finally
            (forever do
              (clientSocket, _) <- liftIO (Socket.accept listenSocket)
              pending <- liftIO (WS.makePendingConnection clientSocket WS.defaultConnectionOptions)
              RPCServer.rpcServerApp cfg rpcState RPCServer.noRpcServerCallbacks pending)
            (liftIO (Socket.close listenSocket))
        client = do
          unauthorized <- trySync (liftIO (WS.runClient "127.0.0.1" port "/rpc" \_ -> pure ()))
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

testHttpServerReturns404ForStaticAndProtectsAttachmentRoute :: IO ()
testHttpServerReturns404ForStaticAndProtectsAttachmentRoute = do
  result <- timeout 2_000_000 $ runEff $ runConcurrent $ runFileSystem $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" do
    rpcState <- RPC.newRpcState
    listenSocket <- liftIO (WS.makeListenSocket "127.0.0.1" 0)
    port <- fromIntegral <$> liftIO (Socket.socketPort listenSocket)
    let cfg = RPCConfig.Config
          { enabled = True
          , host = "127.0.0.1"
          , port
          , token = "secret"
          , attachmentDir = "attachments"
          , attachmentMaxBytes = 1024 * 1024
          }
        server =
          withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
            liftIO $
              Warp.runSettingsSocket Warp.defaultSettings listenSocket $
                RPCServer.rpcServerApplication runInIO cfg rpcState RPCServer.noRpcServerCallbacks
        client = liftIO do
          manager <- HTTP.newManager HTTP.defaultManagerSettings
          root <- httpGet manager [i|http://127.0.0.1:#{port}/|]
          attachmentPreflight <- httpOptions manager [i|http://127.0.0.1:#{port}/attachments/missing|]
          attachmentWithoutAuth <- httpGet manager [i|http://127.0.0.1:#{port}/attachments/missing|]
          attachmentWithAuth <- httpGetWithBearer manager "secret" [i|http://127.0.0.1:#{port}/attachments/missing|]
          response <- openSessionClient port "secret"
          pure (root, attachmentPreflight, attachmentWithoutAuth, attachmentWithAuth, response)
    race server client

  case result of
    Nothing ->
      assertFailure "RPC HTTP integration test timed out"
    Just (Left ()) ->
      assertFailure "RPC HTTP server exited before client completed"
    Just (Right (root, attachmentPreflight, attachmentWithoutAuth, attachmentWithAuth, response)) -> do
      HTTP.responseStatus root @?= Http.status404
      HTTP.responseStatus attachmentPreflight @?= Http.status204
      responseHeader "Access-Control-Allow-Origin" attachmentPreflight @?= Just "*"
      responseHeader "Access-Control-Allow-Headers" attachmentPreflight @?= Just "Authorization, Content-Type"
      HTTP.responseStatus attachmentWithoutAuth @?= Http.status401
      responseHeader "Access-Control-Allow-Origin" attachmentWithoutAuth @?= Just "*"
      HTTP.responseStatus attachmentWithAuth @?= Http.status404
      responseHeader "Access-Control-Allow-Origin" attachmentWithAuth @?= Just "*"
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

responseAttachment :: Protocol.RpcResponse -> IO RPC.RpcChatAttachmentRef
responseAttachment = \case
  JSONRPC.ResponseMessage result ->
    parseJson result.result
  other ->
    assertFailure [i|expected attachment response, got #{show other :: String}|]

responseAttachmentUnsafe :: Protocol.RpcResponse -> RPC.RpcChatAttachmentRef
responseAttachmentUnsafe = \case
  JSONRPC.ResponseMessage result ->
    fromMaybe (error "expected attachment response") (AesonTypes.parseMaybe Aeson.parseJSON result.result)
  _ ->
    error "expected attachment response"

openSessionClient :: Int -> Text -> IO Protocol.RpcResponse
openSessionClient port token =
  WS.runClientWith "127.0.0.1" port "/rpc" WS.defaultConnectionOptions [("Authorization", "Bearer " <> TextEncoding.encodeUtf8 token)] \conn -> do
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

httpGetWithBearer :: HTTP.Manager -> ByteString -> String -> IO (HTTP.Response LazyByteString.ByteString)
httpGetWithBearer manager token url = do
  request <- HTTP.parseRequest url
  HTTP.httpLbs
    request
      { HTTP.checkResponse = \_ _ -> pure ()
      , HTTP.requestHeaders = [("Authorization", "Bearer " <> token)]
      }
    manager

httpOptions :: HTTP.Manager -> String -> IO (HTTP.Response LazyByteString.ByteString)
httpOptions manager url = do
  request <- HTTP.parseRequest url
  HTTP.httpLbs
    request
      { HTTP.checkResponse = \_ _ -> pure ()
      , HTTP.method = "OPTIONS"
      , HTTP.requestHeaders =
          [ ("Origin", "http://localhost:5173")
          , ("Access-Control-Request-Method", "GET")
          , ("Access-Control-Request-Headers", "authorization")
          ]
      }
    manager

responseHeader :: Http.HeaderName -> HTTP.Response body -> Maybe ByteString
responseHeader name response =
  snd <$> find ((== name) . fst) (HTTP.responseHeaders response)

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

runRpcStorage :: FilePath -> Eff '[Storage.Storage, FileSystem.FileSystem, Concurrent, IOE] a -> IO a
runRpcStorage path action =
  runEff $ runConcurrent $ runFileSystem $ StorageSQLite.runStorageSQLitePath path action

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
    , "attachments" Aeson..= ([] :: [Aeson.Value])
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

testRpcConfig :: RPCConfig.Config
testRpcConfig = RPCConfig.Config
  { enabled = True
  , host = "127.0.0.1"
  , port = 38765
  , token = "secret"
  , attachmentDir = "attachments"
  , attachmentMaxBytes = 1024 * 1024
  }

responseMessageAttachments :: Protocol.RpcResponse -> [[Text]]
responseMessageAttachments response =
  case response of
    JSONRPC.ResponseMessage result ->
      fromMaybe [] do
        messages <- AesonTypes.parseMaybe (Aeson.withObject "history" (Aeson..: "messages")) result.result
        traverse messageAttachments messages
    _ ->
      []
  where
    messageAttachments =
      AesonTypes.parseMaybe $
        Aeson.withObject "message" \o -> do
          attachments <- o Aeson..: "attachments"
          traverse (Aeson.withObject "attachment" \attachment -> attachment Aeson..: "attachmentId" <|> attachment Aeson..: "id") attachments

responseSessionLabel :: Protocol.RpcResponse -> Maybe Text
responseSessionLabel response =
  case response of
    JSONRPC.ResponseMessage result -> do
      session <- AesonTypes.parseMaybe (Aeson.withObject "rename" (Aeson..: "session")) result.result
      AesonTypes.parseMaybe (Aeson.withObject "session" (Aeson..: "label")) session
    _ ->
      Nothing
