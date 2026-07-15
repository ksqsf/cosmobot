{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import qualified Bot.Concurrency.Manager as ConcurrencyManager
import qualified Bot.Agent as Agent
import qualified Bot.Agent.Tools.Resource as ResourceTool
import qualified Bot.Agent.Types as AgentTypes
import Bot.Core.Message
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Resource as Resource
import qualified Bot.Effect.Storage as Storage
import Bot.Handler.Resource (removeResources, resourceIds)
import Bot.Prelude
import qualified Bot.Resource as ResourceManager
import qualified Bot.Resource.Sandbox as Sandbox
import qualified Bot.Resource.Workspace as Workspace
import qualified Bot.Storage.SQLite as StorageSQLite
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.Prim.IORef as IORef
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.Process.Typed as TypedProcess
import qualified Data.Unique as Unique
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

data TestObject = TestObject
  { label :: !Text
  , failDestroy :: !(IORef.IORef Bool)
  , destroyed :: !(MVar.MVar ())
  }

data OtherObject = OtherObject

data BlockingObject = BlockingObject
  { destroyStarted :: !(MVar.MVar ())
  , finishDestroy :: !(MVar.MVar ())
  }

newtype PersistentObject = PersistentObject Text

data TestFailure = TestFailure
  deriving stock (Show)

instance Exception TestFailure

data TestInit = TestInit
  { label :: !Text
  , failDestroy :: !(IORef.IORef Bool)
  , destroyed :: !(MVar.MVar ())
  }

instance (Prim :> es, Concurrent :> es) => Resource.ResourceObject (Eff es) TestObject where
  type CreationArgs TestObject = TestInit
  resourceTypeName _ = "Test"
  createResourceObject Resource.Init{arguments} = pure (Right TestObject
    { label = arguments.label
    , failDestroy = arguments.failDestroy
    , destroyed = arguments.destroyed
    })
  destroyResourceObject object = do
    failing <- IORef.readIORef object.failDestroy
    if failing
      then IORef.writeIORef object.failDestroy False $> Left "cleanup failed"
      else MVar.tryPutMVar object.destroyed () $> Right ()
  describeResourceObject object _ = pure object.label
  probeResourceObject _ = pure (Right "healthy")

instance Applicative m => Resource.ResourceObject m OtherObject where
  type CreationArgs OtherObject = ()
  resourceTypeName _ = "Other"
  createResourceObject _ = pure (Right OtherObject)
  destroyResourceObject _ = pure (Right ())
  describeResourceObject _ _ = pure "other"
  probeResourceObject _ = pure (Right "healthy")

instance Concurrent :> es => Resource.ResourceObject (Eff es) BlockingObject where
  type CreationArgs BlockingObject = (MVar.MVar (), MVar.MVar ())
  resourceTypeName _ = "Blocking"
  createResourceObject Resource.Init{arguments = (destroyStarted, finishDestroy)} =
    pure (Right BlockingObject{destroyStarted, finishDestroy})
  destroyResourceObject object =
    MVar.putMVar object.destroyStarted () >> MVar.takeMVar object.finishDestroy $> Right ()
  describeResourceObject _ _ = pure "blocking"
  probeResourceObject _ = pure (Right "healthy")

instance Applicative m => Resource.ResourceObject m PersistentObject where
  type CreationArgs PersistentObject = Text
  resourceTypeName _ = "PersistentTest"
  resourcePersistence _ = Resource.PersistentResource
    { encodeResource = \(PersistentObject value) -> value
    , restoreResource = pure . Right . PersistentObject
    }
  createResourceObject Resource.Init{arguments} = pure (Right (PersistentObject arguments))
  destroyResourceObject _ = pure (Right ())
  describeResourceObject (PersistentObject value) _ = pure value
  probeResourceObject _ = pure (Right "healthy")

main :: IO ()
main = defaultMain $ testGroup "resource"
  [ testCase "typed heterogeneous resources and local ids" testTypedResources
  , testCase "owner isolation and superuser removal" testOwnership
  , testCase "scoped use clears after exceptions" testScopedException
  , testCase "removal cancels and awaits active users" testRemovalCancelsUsers
  , testCase "cleanup failure restores resource for retry" testCleanupRetry
  , testCase "acquisition is blocked while destruction runs" testBlockedDuringDestroy
  , testCase "manager exit destroys resources" testShutdown
  , testCase "persistent resources survive manager restart" testPersistentRestart
  , testCase "destroy_resource removes an owned resource" testDestroyResourceTool
  , testCase "Podman sandbox arguments preserve isolation" testPodmanArguments
  , testCase "Podman sandbox command preserves scripts as argv" testPodmanExecArguments
  , testCase "Podman sandbox parses state and truncates retained output" testPodmanOutput
  , testCase "Podman sandbox renders failures and forced cleanup" testPodmanFailures
  , testCase "workspace create, query, update, and destroy" testWorkspaceLifecycle
  , testCase "resource command ids preserve first occurrence" $
      resourceIds "res-2 res-1 res-2 res-3" @?= ["res-2", "res-1", "res-3"]
  , testCase "resource removal reports partial results independently" testPartialRemoval
  ]

testTypedResources :: Assertion
testTypedResources = runManaged do
  (testInit, _) <- newTestInit "alpha" False
  firstResult <- Resource.create @TestObject Resource.Init{message = ownerMessage, arguments = testInit}
  secondResult <- Resource.create @OtherObject Resource.Init{message = ownerMessage, arguments = ()}
  liftIO $ firstResult @?= Right "res-1"
  liftIO $ secondResult @?= Right "res-2"
  access <- expectRight (Resource.accessFromMessage ownerMessage)
  listed <- Resource.list access
  liftIO $ map (\item -> (item.resourceType, item.sessionId, item.description, item.probeResult)) listed
    @?= [("Test", Nothing, "alpha", Right "healthy"), ("Other", Nothing, "other", Right "healthy")]
  mismatch <- Resource.withResource @OtherObject access "res-1" Nothing (const (pure ()))
  liftIO $ mismatch @?= Left Resource.ResourceTypeMismatch

testOwnership :: Assertion
testOwnership = runManaged do
  (testInit, _) <- newTestInit "owned" False
  resourceId <- Resource.create @TestObject Resource.Init{message = ownerMessage, arguments = testInit} >>= expectRight
  otherAccess <- expectRight (Resource.accessFromMessage (ownerMessage{senderId = Just "other"}))
  Resource.list otherAccess >>= liftIO . (@?= [])
  Resource.destroy otherAccess resourceId >>= liftIO . (@?= Left Resource.ResourceNotFoundOrNotOwned)
  let adminMessage = ownerMessage
        { platform = PlatformDiscord
        , chatId = Just 999
        , senderId = Just "admin"
        , digest = emptyMessageDigest{senderIsSuperuser = True}
        }
  adminAccess <- expectRight (Resource.accessFromMessage adminMessage)
  Resource.list adminAccess >>= liftIO . assertBool "superuser lists resources system-wide" . any ((== resourceId) . (.resourceId))
  Resource.destroy adminAccess resourceId >>= liftIO . (@?= Right ())
  let missing = ownerMessage{senderId = Nothing}
  liftIO $ Resource.accessFromMessage missing @?= Left Resource.MissingResourceIdentity

testScopedException :: Assertion
testScopedException = runManaged do
  (testInit, _) <- newTestInit "scope" False
  resourceId <- Resource.create @TestObject Resource.Init{message = ownerMessage, arguments = testInit} >>= expectRight
  access <- expectRight (Resource.accessFromMessage ownerMessage)
  _ <- trySync $ Resource.withResource @TestObject access resourceId Nothing \_ -> throwIO TestFailure
  Resource.withResource @TestObject access resourceId Nothing (pure . (.label)) >>= liftIO . (@?= Right "scope")

testRemovalCancelsUsers :: Assertion
testRemovalCancelsUsers = runManaged do
  (testInit, destroyed) <- newTestInit "active" False
  resourceId <- Resource.create @TestObject Resource.Init{message = ownerMessage, arguments = testInit} >>= expectRight
  access <- expectRight (Resource.accessFromMessage ownerMessage)
  started <- MVar.newEmptyMVar
  stopped <- MVar.newEmptyMVar
  _ <- Concurrency.forkWithHandle "resource-user" \userHandle ->
    void $ Resource.withResource @TestObject access resourceId (Just userHandle) \_ ->
      (MVar.putMVar started () >> never) `finally` MVar.putMVar stopped ()
  MVar.takeMVar started
  Resource.destroy access resourceId >>= liftIO . (@?= Right ())
  MVar.takeMVar stopped
  void (MVar.takeMVar destroyed)

testCleanupRetry :: Assertion
testCleanupRetry = runManaged do
  (testInit, _) <- newTestInit "retry" True
  resourceId <- Resource.create @TestObject Resource.Init{message = ownerMessage, arguments = testInit} >>= expectRight
  access <- expectRight (Resource.accessFromMessage ownerMessage)
  Resource.destroy access resourceId >>= liftIO . (@?= Left (Resource.ResourceCleanupFailed "cleanup failed"))
  Resource.withResource @TestObject access resourceId Nothing (pure . (.label)) >>= liftIO . (@?= Right "retry")
  Resource.destroy access resourceId >>= liftIO . (@?= Right ())

testBlockedDuringDestroy :: Assertion
testBlockedDuringDestroy = runManaged do
  destroyStarted <- MVar.newEmptyMVar
  finishDestroy <- MVar.newEmptyMVar
  destroyed <- MVar.newEmptyMVar
  resourceId <- Resource.create @BlockingObject Resource.Init
    { message = ownerMessage
    , arguments = (destroyStarted, finishDestroy)
    } >>= expectRight
  access <- expectRight (Resource.accessFromMessage ownerMessage)
  _ <- Concurrency.fork "destroy-resource" $
    Resource.destroy access resourceId >>= MVar.putMVar destroyed
  MVar.takeMVar destroyStarted
  Resource.withResource @BlockingObject access resourceId Nothing (const (pure ()))
    >>= liftIO . (@?= Left Resource.ResourceUnavailable)
  MVar.putMVar finishDestroy ()
  MVar.takeMVar destroyed >>= liftIO . (@?= Right ())

testShutdown :: Assertion
testShutdown = do
  destroyed <- runManaged do
    (testInit, destroyed) <- newTestInit "shutdown" False
    void $ Resource.create @TestObject Resource.Init{message = ownerMessage, arguments = testInit}
    pure destroyed
  runEff $ runConcurrent $ void (MVar.takeMVar destroyed)

testPersistentRestart :: Assertion
testPersistentRestart =
  runEff $ runConcurrent $ FileSystem.runFileSystem do
    tmp <- FileSystem.getTemporaryDirectory
    unique <- liftIO Unique.newUnique
    let database = tmp </> ("cosmobot-resource-" <> show (Unique.hashUnique unique) <> ".sqlite")
        cleanup = FileSystem.removeFile database
    (do
      persistentId <- liftIO $ runPersistent database do
        persistentId <- Resource.create @PersistentObject Resource.Init{message = ownerMessage, arguments = "durable"} >>= expectRight
        void $ Resource.create @OtherObject Resource.Init{message = ownerMessage, arguments = ()} >>= expectRight
        pure persistentId
      liftIO $ persistentId @?= "res-1"
      listed <- liftIO $ runPersistent database do
        access <- expectRight (Resource.accessFromMessage ownerMessage)
        Resource.list access
      liftIO $ map (\resource -> (resource.resourceId, resource.resourceType, resource.description)) listed
        @?= [("res-1", "PersistentTest", "durable")]
      liftIO $ runPersistent database do
        access <- expectRight (Resource.accessFromMessage ownerMessage)
        Resource.destroy access persistentId >>= liftIO . (@?= Right ())
      listedAfter <- liftIO $ runPersistent database do
        access <- expectRight (Resource.accessFromMessage ownerMessage)
        Resource.list access
      liftIO $ listedAfter @?= []
      ) `finally` cleanup

testDestroyResourceTool :: Assertion
testDestroyResourceTool = runManaged do
  (testInit, destroyed) <- newTestInit "agent-owned" False
  resourceId <- Resource.create @TestObject Resource.Init{message = ownerMessage, arguments = testInit} >>= expectRight
  let context = Agent.AgentContext
        { message = ownerMessage
        , input = inputWithImages "" []
        , superuser = False
        , systemContext = ""
        , askCommand = "!ask"
        , toolConfig = Agent.defaultToolConfig
        }
      metadata = AgentTypes.ToolCallMetadata{agentRunId = "agent-1", parent = Nothing}
  runner <- (ResourceTool.destroyResourceTool :: Agent.Tool ManagedStack).start context
  result <- runner metadata (Aeson.object ["resource" Aeson..= resourceId])
  liftIO $ AgentTypes.toolResultContent result @?= "Resource destroyed."
  void (MVar.takeMVar destroyed)

testPartialRemoval :: Assertion
testPartialRemoval = runManaged do
  (okInit, _) <- newTestInit "ok" False
  (badInit, _) <- newTestInit "bad" True
  void $ Resource.create @TestObject Resource.Init{message = ownerMessage, arguments = okInit}
  void $ Resource.create @TestObject Resource.Init{message = ownerMessage, arguments = badInit}
  access <- expectRight (Resource.accessFromMessage ownerMessage)
  results <- removeResources access (resourceIds "res-1 missing res-2 res-1")
  liftIO $ results @?=
    [ "- `res-1`: removed"
    , "- `missing`: not found/not owned"
    , "- `res-2`: cleanup failure"
    ]

testPodmanArguments :: Assertion
testPodmanArguments =
  Sandbox.podmanRunArgs "sandbox-name" @?=
    [ "run", "--detach", "--name", "sandbox-name", "--security-opt=no-new-privileges"
    , "--label", "io.cosmobot.resource=sandbox"
    , "--", "docker.io/library/debian:stable-slim", "sleep", "infinity"
    ]

testPodmanExecArguments :: Assertion
testPodmanExecArguments = do
  let script = "printf '%s' '$HOME; $(touch /tmp/nope)'"
      args = Sandbox.podmanExecArgs "container-id" "cmd-10-20" 7 script
  take 5 args @?= ["exec", "--detach", "container-id", "bash", "-c"]
  drop (length args - 6) args @?=
    [ "--"
    , "/tmp/cosmobot-cmd-10-20.out"
    , "/tmp/cosmobot-cmd-10-20.status"
    , "/tmp/cosmobot-cmd-10-20.pid"
    , "7"
    , Text.unpack script
    ]

testPodmanOutput :: Assertion
testPodmanOutput = do
  Sandbox.parseInspectRunning "true\n" @?= Right True
  Sandbox.parseInspectRunning "false\n" @?= Right False
  Sandbox.parseInspectRunning "not-json" @?= Left "Podman inspect returned malformed output."
  Sandbox.retainOutput 4 "abcdef" @?= ("abcd", True)
  Sandbox.retainOutput 6 "abcdef" @?= ("abcdef", False)
  Sandbox.retainOutput 1 "é" @?= ("", True)
  Sandbox.retainOutput 2 "é" @?= ("é", False)

testPodmanFailures :: Assertion
testPodmanFailures = do
  Sandbox.podmanCleanupArgs "container-id" @?= ["rm", "--force", "--time", "0", "--ignore", "container-id"]
  Sandbox.renderPodmanFailure "inspect" 125 "" "missing container\n" @?=
    "Podman inspect failed (exit 125): missing container"
  Sandbox.renderPodmanFailure "wait" 1 "" "" @?= "Podman wait failed (exit 1)."

testWorkspaceLifecycle :: Assertion
testWorkspaceLifecycle =
  runEff $ runConcurrent $ FileSystem.runFileSystem $ TypedProcess.runTypedProcess do
    tmp <- FileSystem.getTemporaryDirectory
    unique <- liftIO Unique.newUnique
    let root = tmp </> ("cosmobot-workspace-" <> show (Unique.hashUnique unique))
        path = root </> "demo-work"
        cleanup = FileSystem.removePathForcibly root
    (do
      Workspace.createWorkspaceAt root Workspace.WorkspaceArgs{workId = "../escape", goal = "nope"} >>= \case
        Left err -> liftIO $ err @?= "id may contain only letters, digits, dot, underscore, and hyphen."
        Right _ -> liftIO $ assertFailure "unsafe workspace id was accepted"
      workspace <- Workspace.createWorkspaceAt root Workspace.WorkspaceArgs
        { workId = "demo-work"
        , goal = "initial goal"
        } >>= expectRight
      FileSystem.createDirectory (path </> "repo")
      report <- Workspace.queryWorkspace workspace >>= expectRight
      liftIO $ assertBool "query includes WORK.md" ("WORK.md:\ninitial goal" `Text.isInfixOf` report)
      liftIO $ assertBool "query includes depth-one tree" ("repo" `Text.isInfixOf` report)
      Workspace.updateWorkspace workspace "updated goal" >>= liftIO . (@?= Right ())
      updated <- Workspace.queryWorkspace workspace >>= expectRight
      liftIO $ assertBool "query includes updated goal" ("WORK.md:\nupdated goal" `Text.isInfixOf` updated)
      Resource.destroyResourceObject workspace >>= liftIO . (@?= Right ())
      FileSystem.doesDirectoryExist path >>= liftIO . (@?= False)
      ) `finally` cleanup

newTestInit :: (Prim :> es, Concurrent :> es) => Text -> Bool -> Eff es (TestInit, MVar.MVar ())
newTestInit label failing = do
  failDestroy <- IORef.newIORef failing
  destroyed <- MVar.newEmptyMVar
  pure (TestInit{label, failDestroy, destroyed}, destroyed)

ownerMessage :: IncomingMessage
ownerMessage = IncomingMessage
  { platform = PlatformTelegram
  , kind = ChatPrivate
  , chatId = Just 100
  , chatAliases = []
  , digest = emptyMessageDigest
  , senderId = Just "owner"
  , senderUsername = Just "owner"
  , messageId = Just "message"
  , replyToMessageId = Nothing
  , mentions = []
  , mentionUsernames = []
  , imageUrls = []
  , files = []
  , text = ""
  , raw = Aeson.Null
  }

type ManagedStack = '[Resource.Resource, Concurrency.Concurrency, Storage.Storage, Prim, Concurrent, IOE]

runManaged :: Eff ManagedStack a -> IO a
runManaged action =
  runEff $ runConcurrent $ runPrim $ StorageSQLite.runStorageSQLitePath ":memory:" $
    ConcurrencyManager.runConcurrencyManager $ ResourceManager.runResourceManager action

runPersistent :: FilePath -> Eff ManagedStack a -> IO a
runPersistent database action =
  runEff $ runConcurrent $ runPrim $ StorageSQLite.runStorageSQLitePath database $
    ConcurrencyManager.runConcurrencyManager $
      ResourceManager.runResourceManagerWith [ResourceManager.resourceLoader @PersistentObject] action

expectRight :: Applicative m => Either e a -> m a
expectRight = either (const (pure (error "expected Right"))) pure

never :: Concurrent :> es => Eff es ()
never = threadDelay maxBound
