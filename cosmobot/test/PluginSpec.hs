{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Bot.Core.Message
import qualified Bot.Effect.Plugin as Plugin
import qualified Bot.Plugin.Manager as Manager
import qualified Bot.Plugin.Sandbox as Sandbox
import Bot.Plugin.Types
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Effectful.Concurrent.Async as Async
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.Process as Process
import Effectful.Timeout
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit
import Test.Tasty.Runners (NumThreads (..))

main :: IO ()
main = defaultMain . localOption (NumThreads 1) $ testGroup "plugins"
  [ testCase "initializes routes, callbacks, tools, and shutdown" testRoundTrip
  , testCase "reload isolates generations and unload invalidates snapshots" testGenerationIsolation
  , testCase "concurrent lifecycle operations are serialized" testConcurrentLifecycle
  , testCase "tool invocations may overlap" testConcurrentTools
  , testCase "invalid tool arguments are permanent" testInvalidToolArguments
  , testCase "late callbacks lose their invocation context" testInvocationExpiry
  , testCase "route timeout continues normal routing" testRouteTimeout
  , testCase "optional crash is removed and required crash aborts" testFailurePolicy
  , testCase "transient startup retries and crash loop opens" testTransientRetry
  , testCase "successful initialization is finalized when manifest validation fails" testInvalidManifestFinalize
  , testCase "leader exit kills descendants that inherited protocol pipes" testLeaderExitCleanup
  , testCase "explicit transient process exit reloads a new generation" testTransientProcessRestart
  , testCase "bubblewrap arguments and media translation are isolated" testSandboxContract
  , testCase "real SDK echo executable smoke" testRealSdkEcho
  ]

testRoundTrip :: Assertion
testRoundTrip = withFixture "normal" False \root finalizePath _ -> do
  replies <- IORef.newIORef []
  (statuses, disposition, toolResult) <- runManager root (callbacks replies) do
    statuses <- Plugin.statuses
    disposition <- Plugin.dispatchRoute (message "!echo hello")
    tools <- Plugin.toolSnapshot
    toolResult <- invokeOnly tools (Aeson.object ["text" Aeson..= ("tool hello" :: Text)])
    pure (statuses, disposition, toolResult)
  map (.pluginVersion) statuses @?= ["1.2.3"]
  disposition @?= Just StopRouting
  IORef.readIORef replies >>= (@?= ["hello"])
  pluginContent toolResult @?= "tool hello"
  Directory.doesFileExist finalizePath >>= assertBool "finalize marker missing"
  TextIO.readFile finalizePath >>= (@?= "finalize\n")

testGenerationIsolation :: Assertion
testGenerationIsolation = withFixture "normal" False \root _ _ -> do
  (oldGeneration, newGeneration, oldFailure, newContent, unloadedFailure) <- runManager root emptyCallbacks do
    [old] <- Plugin.toolSnapshot
    reloaded <- Plugin.reload fixtureId >>= either error pure
    oldResult <- Plugin.invokeTool old (message "hello") (Aeson.object ["text" Aeson..= ("old" :: Text)])
    [new] <- Plugin.toolSnapshot
    newResult <- Plugin.invokeTool new (message "hello") (Aeson.object ["text" Aeson..= ("new" :: Text)])
    Plugin.unload fixtureId >>= either error pure
    unloaded <- Plugin.invokeTool new (message "hello") (Aeson.object ["text" Aeson..= ("gone" :: Text)])
    pure
      ( old.generation
      , reloaded.generation
      , pluginFailure oldResult
      , pluginContent newResult
      , pluginFailure unloaded
      )
  assertBool "reload reused generation" (newGeneration > oldGeneration)
  assertBool "old generation redirected" (isJust oldFailure)
  newContent @?= "new"
  assertBool "unloaded generation remained callable" (isJust unloadedFailure)

testConcurrentTools :: Assertion
testConcurrentTools = withFixture "normal" False \root _ _ -> do
  contents <- runManager root emptyCallbacks do
    [tool] <- Plugin.toolSnapshot
    let invoke value = Plugin.invokeTool tool (message "hello") (Aeson.object ["text" Aeson..= (value :: Text)])
    (left, right) <- Async.concurrently (invoke "left") (invoke "right")
    pure [pluginContent left, pluginContent right]
  contents @?= ["left", "right"]

testConcurrentLifecycle :: Assertion
testConcurrentLifecycle = withFixture "normal" False \root _ _ -> do
  results <- runManager root emptyCallbacks do
    Plugin.unload fixtureId >>= either error pure
    Async.concurrently (Plugin.load fixtureId) (Plugin.load fixtureId)
  length (filter isRight [fst results, snd results]) @?= 1
  [failure | Left failure <- [fst results, snd results]] @?= ["plugin is already loaded"]

testInvalidToolArguments :: Assertion
testInvalidToolArguments = withFixture "normal" False \root _ _ -> do
  result <- runManager root emptyCallbacks do
    [tool] <- Plugin.toolSnapshot
    Plugin.invokeTool tool (message "hello") Aeson.Null
  pluginFailure result @?= Just PermanentArguments

testInvocationExpiry :: Assertion
testInvocationExpiry = withFixture "late-callback" False \root _ latePath -> do
  replies <- IORef.newIORef []
  runManager root (callbacks replies) do
    void (Plugin.dispatchRoute (message "!echo now"))
    threadDelay 300_000
  payload <- TextIO.readFile latePath
  assertBool "late callback was accepted" ("no longer active" `Text.isInfixOf` payload)
  IORef.readIORef replies >>= (@?= ["now"])

testRouteTimeout :: Assertion
testRouteTimeout = withFixture "route-timeout" False \root _ _ -> do
  replies <- IORef.newIORef []
  disposition <- runManager root (callbacks replies) $
    Plugin.dispatchRoute (message "!echo slow")
  disposition @?= Nothing

testFailurePolicy :: Assertion
testFailurePolicy = do
  withFixture "crash" False \root _ _ ->
    runManager root emptyCallbacks Plugin.statuses >>= (@?= [])
  withFixture "crash" True \root _ _ -> do
    outcome <- runEff $ trySync (liftIO (runManager root emptyCallbacks (pure ())))
    assertBool "required crash did not abort startup" (isLeft outcome)

testTransientRetry :: Assertion
testTransientRetry = do
  withFixture "transient-once" True \root _ _ -> do
    active <- runManager root emptyCallbacks Plugin.statuses
    length active @?= 1
  withFixture "transient-always" False \root _ _ -> do
    failure <- runManager root emptyCallbacks (Plugin.load fixtureId)
    assertBool "crash-loop breaker did not open" $
      either (Text.isInfixOf "crash-loop breaker") (const False) failure

testInvalidManifestFinalize :: Assertion
testInvalidManifestFinalize = withFixture "malformed-manifest" False \root finalizePath _ -> do
  runManager root emptyCallbacks Plugin.statuses >>= (@?= [])
  TextIO.readFile finalizePath >>= (@?= "finalize\n")

testLeaderExitCleanup :: Assertion
testLeaderExitCleanup = withFixture "leader-exit" False \root _ survivorPath -> do
  statuses <- runManager root emptyCallbacks do
    threadDelay 1_300_000
    Plugin.statuses
  statuses @?= []
  Directory.doesFileExist survivorPath >>= assertBool "plugin descendant survived process-group cleanup" . not

testTransientProcessRestart :: Assertion
testTransientProcessRestart = withFixture "transient-process-once" False \root _ _ -> do
  statuses <- runManager root emptyCallbacks do
    threadDelay 1_400_000
    Plugin.statuses
  case statuses of
    [status] -> assertBool "transient restart reused generation" (status.generation > 1)
    _ -> assertFailure ("expected restarted plugin, got " <> show statuses)

testSandboxContract :: Assertion
testSandboxContract = do
  let args = Sandbox.bubblewrapArguments "/bundles/echo" "/cache/media" "echo"
  assertBool "bundle was not writable" (["--bind", "/bundles/echo", "/plugin"] `List.isInfixOf` args)
  assertBool "lifecycle config was writable"
    (["--ro-bind", "/bundles/echo/config.toml", "/plugin/config.toml"] `List.isInfixOf` args)
  assertBool "media was not read-only" (["--ro-bind", "/cache/media", "/media"] `List.isInfixOf` args)
  assertBool "tmpfs missing" (["--tmpfs", "/tmp"] `List.isInfixOf` args)
  assertBool "network was not shared" ("--share-net" `elem` args)
  assertBool "environment was not cleared" ("--clearenv" `elem` args)
  assertBool "sandbox exposed all host executables" (not (["--ro-bind", "/usr", "/usr"] `List.isInfixOf` args))
  Sandbox.translateSandboxMediaPath "/cache/media" "/cache/media/aa/file.png"
    @?= Just "/media/aa/file.png"
  Sandbox.translateSandboxMediaPath "/cache/media" "/etc/passwd" @?= Nothing

testRealSdkEcho :: Assertion
testRealSdkEcho = Environment.lookupEnv "COSMOBOT_E2E_ECHO_EXECUTABLE" >>= traverse_ \source ->
  withExternalEcho source \pluginRoot -> do
    replies <- IORef.newIORef []
    disposition <- runManager pluginRoot (callbacks replies) $
      Plugin.dispatchRoute (message "!echo end-to-end")
    disposition @?= Just StopRouting
    IORef.readIORef replies >>= (@?= ["end-to-end"])

invokeOnly
  :: Plugin.Plugin :> es
  => [Plugin.PluginTool]
  -> Aeson.Value
  -> Eff es ToolInvocationResult
invokeOnly [tool] = Plugin.invokeTool tool (message "hello")
invokeOnly tools = const (error ("expected one tool, got " <> show (length tools)))

pluginContent :: ToolInvocationResult -> Text
pluginContent = \case
  ToolInvocationSuccess content _ -> content
  ToolInvocationFailure _ _ detail -> error detail

pluginFailure :: ToolInvocationResult -> Maybe ToolFailureKind
pluginFailure = \case
  ToolInvocationSuccess _ _ -> Nothing
  ToolInvocationFailure kind _ _ -> Just kind

type PluginTestBase =
  '[ Fail
   , KatipE
   , Process.Process
   , FileSystem.FileSystem
   , Timeout
   , Prim
   , Concurrent
   , IOE
   ]

callbacks :: IORef.IORef [Text] -> Manager.HostCallbacks PluginTestBase
callbacks replies = emptyCallbacks
  { Manager.chatReply = \_ body ->
      liftIO (IORef.atomicModifyIORef' replies (\values -> (values <> [body], ()))) $> Aeson.Bool True
  }

emptyCallbacks :: Manager.HostCallbacks PluginTestBase
emptyCallbacks = Manager.HostCallbacks
  { chatReply = \_ _ -> pure Aeson.Null
  , chatReferenced = const (pure Aeson.Null)
  , llmComplete = pure
  , agentRun = \_ -> pure
  , mediaResolve = \ref -> pure (Aeson.object ["canonicalReference" Aeson..= ref])
  }

runManager
  :: FilePath
  -> Manager.HostCallbacks PluginTestBase
  -> Eff (Plugin.Plugin : PluginTestBase) a
  -> IO a
runManager root hostCallbacks action =
  runEff $ runConcurrent $ runPrim $ runTimeout $ FileSystem.runFileSystem $ Process.runProcess $
    startKatipE "plugin-spec" "test" $ runFailIO $
      Manager.runPluginManager root (takeDirectory root </> "media") hostCallbacks action

withFixture
  :: Text
  -> Bool
  -> (FilePath -> FilePath -> FilePath -> IO a)
  -> IO a
withFixture mode required use = withSystemTempDirectory "cosmobot-plugin-spec" \root -> do
  let pluginRoot = root </> "plugins"
      bundle = pluginRoot </> "fixture"
      executable = bundle </> "fixture"
      config = bundle </> "config.toml"
      finalizePath = bundle </> "finalize.log"
      latePath = bundle </> "late.json"
      transientMarker = bundle </> "transient.marker"
      restartLimit = if mode `elem` ["transient-once", "transient-always", "transient-process-once"] then (3 :: Int) else 0
  Directory.createDirectory pluginRoot
  Directory.createDirectory bundle
  Directory.copyFile "test/plugin_fixture.py" executable
  permissions <- Directory.getPermissions executable
  Directory.setPermissions executable (Directory.setOwnerExecutable True permissions)
  writeFile config . Text.unpack . Text.unlines $
    [ "[plugin]"
    , "required = " <> if required then "true" else "false"
    , "sandboxed = false"
    , "route_timeout_seconds = 1"
    , "tool_timeout_seconds = 2"
    , "restart_limit = " <> show restartLimit
    , ""
    , "[fixture]"
    , "mode = \"" <> mode <> "\""
    , "finalize_path = \"" <> Text.pack finalizePath <> "\""
    , "late_path = \"" <> Text.pack latePath <> "\""
    , "transient_marker = \"" <> Text.pack transientMarker <> "\""
    ]
  Directory.createDirectory (root </> "media")
  use pluginRoot finalizePath latePath

withExternalEcho :: FilePath -> (FilePath -> IO a) -> IO a
withExternalEcho source use = withSystemTempDirectory "cosmobot-plugin-e2e" \root -> do
  let pluginRoot = root </> "plugins"
      bundle = pluginRoot </> "echo"
      executable = bundle </> "echo"
  Directory.createDirectory pluginRoot
  Directory.createDirectory bundle
  Directory.copyFile source executable
  permissions <- Directory.getPermissions executable
  Directory.setPermissions executable (Directory.setOwnerExecutable True permissions)
  writeFile (bundle </> "config.toml") . Text.unpack . Text.unlines $
    [ "[plugin]"
    , "required = true"
    , "sandboxed = true"
    , "route_timeout_seconds = 5"
    , "tool_timeout_seconds = 5"
    , "restart_limit = 0"
    ]
  Directory.createDirectory (root </> "media")
  use pluginRoot

message :: Text -> IncomingMessage
message text = IncomingMessage
  { eventKind = IncomingMessageCreated
  , platform = PlatformTelegram
  , kind = ChatPrivate
  , chatId = Just 100
  , chatAliases = []
  , digest = emptyMessageDigest{senderIsAllowed = True}
  , senderId = Just "200"
  , senderUsername = Just "alice"
  , messageId = Just "300"
  , replyToMessageId = Nothing
  , mentions = []
  , mentionUsernames = []
  , imageUrls = []
  , files = []
  , text
  , raw = Aeson.Null
  }

fixtureId :: PluginId
fixtureId = PluginId "fixture"
