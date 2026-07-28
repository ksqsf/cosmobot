{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import qualified Bot.Concurrency.Manager as ConcurrencyManager
import Bot.Core.Message
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Resource as Resource
import qualified Bot.Effect.Storage as Storage
import Bot.Handler.Resource (removeResources, renderResources, resourceIds)
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

data BlockingCreateObject = BlockingCreateObject

newtype PersistentObject = PersistentObject Text

data PersistentInit = PersistentInit
  { label :: !Text
  , persistentTTLSeconds :: !(Maybe Int)
  }

data TestFailure = TestFailure
  deriving stock (Show)

instance Exception TestFailure

data TestInit = TestInit
  { label :: !Text
  , failDestroy :: !(IORef.IORef Bool)
  , destroyed :: !(MVar.MVar ())
  , ttlSeconds :: !(Maybe Int)
  }

instance (Prim :> es, Concurrent :> es) => Resource.ResourceObject (Eff es) TestObject where
  type CreationArgs TestObject = TestInit
  resourceTypeName _ = "Test"
  resourceTTLSeconds = Right . (.ttlSeconds)
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
  resourceScope _ = Resource.ChatResource
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

instance Concurrent :> es => Resource.ResourceObject (Eff es) BlockingCreateObject where
  type CreationArgs BlockingCreateObject = (MVar.MVar (), MVar.MVar ())
  resourceTypeName _ = "BlockingCreate"
  createResourceObject Resource.Init{arguments = (started, finish)} =
    MVar.putMVar started () >> MVar.takeMVar finish $> Right BlockingCreateObject
  destroyResourceObject _ = pure (Right ())
  describeResourceObject _ _ = pure "blocking create"
  probeResourceObject _ = pure (Right "healthy")

instance Applicative m => Resource.ResourceObject m PersistentObject where
  type CreationArgs PersistentObject = PersistentInit
  resourceTypeName _ = "PersistentTest"
  resourceTTLSeconds = Right . (.persistentTTLSeconds)
  resourcePersistence _ = Resource.PersistentResource
    { encodeResource = \(PersistentObject value) -> value
    , restoreResource = pure . Right . PersistentObject
    }
  createResourceObject Resource.Init{arguments} = pure (Right (PersistentObject arguments.label))
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
  , testCase "associated resources are destroyed together" testAssociatedCleanup
  , testCase "persistent resources survive manager restart" testPersistentRestart
  , testCase "finite resource life refreshes and can become permanent" testResourceLifetime
  , testCase "finite resources expire automatically" testResourceExpiry
  , testCase "finite resources do not expire during active use" testResourceActiveExpiry
  , testCase "persistent resource expiry includes downtime" testPersistentExpiry
  , testCase "resource names are globally unique and renameable" testResourceNames
  , testCase "resource names are reserved during concurrent creation" testConcurrentResourceNames
  , testCase "Podman sandbox arguments preserve isolation" testPodmanArguments
  , testCase "Podman sandbox command preserves scripts as argv" testPodmanExecArguments
  , testCase "Podman sandbox parses state and truncates retained output" testPodmanOutput
  , testCase "Podman sandbox renders failures and forced cleanup" testPodmanFailures
  , testCase "workspace create, query, update, and destroy" testWorkspaceLifecycle
  , testCase "resource command ids preserve first occurrence" $
      resourceIds "res-2 res-1 res-2 res-3" @?= ["res-2", "res-1", "res-3"]
  , testCase "resource TTL has a five-minute minimum" $ do
      Resource.ttlFromMinutes 4 @?= Left "TTL must be at least 5 minutes."
      Resource.ttlFromMinutes 5 @?= Right (Just 300)
  , testCase "sandboxes are chat scoped" $
      Resource.resourceScope @(Eff '[Concurrent, TypedProcess.TypedProcess, IOE]) (Proxy @Sandbox.Sandbox) @?= Resource.ChatResource
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
  otherAccess <- expectRight (Resource.accessFromMessage (ownerMessage{senderId = Just "other"}))
  Resource.list otherAccess >>= liftIO . ((@?= ["res-2"]) . map (.resourceId))
  liftIO $ assertBool "resource list reports life" ("life: permanent" `Text.isInfixOf` renderResources listed)
  mismatch <- Resource.withResource @OtherObject access "res-1" Nothing (const (pure ()))
  liftIO $ mismatch @?= Left Resource.ResourceTypeMismatch
  Resource.detail access "res-1" >>= liftIO . (@?= Right "alpha\nlife: permanent")

testOwnership :: Assertion
testOwnership = runManaged do
  (testInit, _) <- newTestInit "owned" False
  resourceId <- Resource.create @TestObject Resource.Init{message = ownerMessage, arguments = testInit} >>= expectRight
  otherAccess <- expectRight (Resource.accessFromMessage (ownerMessage{senderId = Just "other"}))
  Resource.list otherAccess >>= liftIO . (@?= [])
  Resource.detail otherAccess resourceId >>= liftIO . (@?= Left Resource.ResourceNotFoundOrNotOwned)
  Resource.destroy otherAccess resourceId >>= liftIO . (@?= Left Resource.ResourceNotFoundOrNotOwned)
  let adminMessage = ownerMessage
        { platform = PlatformDiscord
        , chatId = Just 999
        , senderId = Just "admin"
        , digest = emptyMessageDigest{senderIsSuperuser = True}
        }
  adminAccess <- expectRight (Resource.accessFromMessage adminMessage)
  Resource.list adminAccess >>= liftIO . assertBool "superuser lists resources system-wide" . any ((== resourceId) . (.resourceId))
  Resource.detail adminAccess resourceId >>= liftIO . (@?= Right "owned\nlife: permanent")
  Resource.destroy adminAccess resourceId >>= liftIO . (@?= Right ())
  let missing = ownerMessage{senderId = Nothing}
  liftIO $ Resource.accessFromMessage missing @?= Left Resource.MissingResourceIdentity

testResourceNames :: Assertion
testResourceNames = runManaged do
  (testInit, _) <- newTestInit "named" False
  resourceId <- Resource.createNamed @TestObject "agent-choice" Resource.Init{message = ownerMessage, arguments = testInit} >>= expectRight
  liftIO $ resourceId @?= "agent-choice"
  duplicate <- Resource.createNamed @TestObject "agent-choice" Resource.Init
    { message = ownerMessage{senderId = Just "other"}
    , arguments = testInit
    }
  liftIO $ duplicate @?= Left Resource.ResourceNameAlreadyExists
  access <- expectRight (Resource.accessFromMessage ownerMessage)
  Resource.rename access resourceId "renamed" >>= liftIO . (@?= Right "renamed")
  Resource.withResource @TestObject access resourceId Nothing (pure . (.label))
    >>= liftIO . (@?= Left Resource.ResourceNotFoundOrNotOwned)
  Resource.withResource @TestObject access "renamed" Nothing (pure . (.label))
    >>= liftIO . (@?= Right "named")
  Resource.rename access "renamed" "bad name" >>= liftIO . (@?= Left Resource.InvalidResourceName)

testConcurrentResourceNames :: Assertion
testConcurrentResourceNames = runManaged do
  started <- MVar.newEmptyMVar
  finish <- MVar.newEmptyMVar
  created <- MVar.newEmptyMVar
  _ <- Concurrency.fork "create-named-resource" $
    Resource.createNamed @BlockingCreateObject "reserved" Resource.Init
      { message = ownerMessage
      , arguments = (started, finish)
      } >>= MVar.putMVar created
  MVar.takeMVar started
  duplicate <- Resource.createNamed @OtherObject "reserved" Resource.Init
    { message = ownerMessage{senderId = Just "other"}
    , arguments = ()
    }
  liftIO $ duplicate @?= Left Resource.ResourceNameAlreadyExists
  MVar.putMVar finish ()
  MVar.takeMVar created >>= liftIO . (@?= Right "reserved")

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

testAssociatedCleanup :: Assertion
testAssociatedCleanup = runManaged do
  parent <- Concurrency.fork "parent" (pure ())
  (testInit, destroyed) <- newTestInit "child" False
  _ <- Resource.createAssociated @TestObject (Just parent) Resource.Init{message = ownerMessage, arguments = testInit} >>= expectRight
  Resource.destroyAssociated parent >>= liftIO . (@?= [Right ()])
  liftIO $ runEff $ runConcurrent $ void (MVar.takeMVar destroyed)

testPersistentRestart :: Assertion
testPersistentRestart =
  runEff $ runConcurrent $ FileSystem.runFileSystem do
    tmp <- FileSystem.getTemporaryDirectory
    unique <- liftIO Unique.newUnique
    let database = tmp </> ("cosmobot-resource-" <> show (Unique.hashUnique unique) <> ".sqlite")
        cleanup = FileSystem.removeFile database
    (do
      persistentId <- liftIO $ runPersistent database do
        persistentId <- Resource.createNamedForRun @PersistentObject "run-1" Nothing "durable-name" Resource.Init
          { message = ownerMessage
          , arguments = PersistentInit{label = "durable", persistentTTLSeconds = Nothing}
          } >>= expectRight
        void $ Resource.create @OtherObject Resource.Init{message = ownerMessage, arguments = ()} >>= expectRight
        access <- expectRight (Resource.accessFromMessage ownerMessage)
        Resource.rename access persistentId "renamed-durable" >>= expectRight
      liftIO $ persistentId @?= "renamed-durable"
      listed <- liftIO $ runPersistent database do
        access <- expectRight (Resource.accessFromMessage ownerMessage)
        Resource.list access
      liftIO $ map (\resource -> (resource.resourceId, resource.resourceType, resource.description)) listed
        @?= [("renamed-durable", "PersistentTest", "durable")]
      createdByRun <- liftIO $ runPersistent database do
        access <- expectRight (Resource.accessFromMessage ownerMessage)
        Resource.listCreatedByRuns access ["run-1"]
      liftIO $ map (.resourceId) createdByRun @?= ["renamed-durable"]
      liftIO $ runPersistent database do
        access <- expectRight (Resource.accessFromMessage ownerMessage)
        Resource.destroy access persistentId >>= liftIO . (@?= Right ())
      listedAfter <- liftIO $ runPersistent database do
        access <- expectRight (Resource.accessFromMessage ownerMessage)
        Resource.list access
      liftIO $ listedAfter @?= []
      ) `finally` cleanup

testResourceLifetime :: Assertion
testResourceLifetime = runManaged do
  (testInit, _) <- newTestInit "ttl" False
  resourceId <- Resource.create @TestObject Resource.Init
    { message = ownerMessage
    , arguments = testInit{ttlSeconds = Just 1}
    } >>= expectRight
  access <- expectRight (Resource.accessFromMessage ownerMessage)
  otherAccess <- expectRight (Resource.accessFromMessage ownerMessage{senderId = Just "other"})
  Resource.keepAlive otherAccess resourceId >>= liftIO . (@?= Left Resource.ResourceNotFoundOrNotOwned)
  threadDelay 600_000
  Resource.withResource @TestObject access resourceId Nothing (const (pure ())) >>= liftIO . (@?= Right ())
  threadDelay 600_000
  Resource.keepAlive access resourceId >>= liftIO . (@?= Right ())
  Resource.detail access resourceId >>= liftIO . \case
    Right detail -> assertBool "detail reports finite life" ("\nlife: 1m" `Text.isSuffixOf` detail)
    Left err -> assertFailure (show err)
  adminAccess <- expectRight (Resource.accessFromMessage ownerMessage
    { senderId = Just "admin"
    , digest = emptyMessageDigest{senderIsSuperuser = True}
    })
  Resource.makePermanent adminAccess resourceId >>= liftIO . (@?= Right ())
  threadDelay 1_200_000
  Resource.list access >>= liftIO . \resources ->
    map (\resource -> (resource.resourceId, resource.remainingLifeMinutes)) resources @?= [(resourceId, Nothing)]

testResourceExpiry :: Assertion
testResourceExpiry = runManaged do
  (testInit, destroyed) <- newTestInit "expiring" False
  resourceId <- Resource.create @TestObject Resource.Init
    { message = ownerMessage
    , arguments = testInit{ttlSeconds = Just 1}
    } >>= expectRight
  access <- expectRight (Resource.accessFromMessage ownerMessage)
  threadDelay 1_200_000
  Resource.list access >>= liftIO . (@?= [])
  MVar.tryTakeMVar destroyed >>= liftIO . (@?= Just ())
  Resource.detail access resourceId >>= liftIO . (@?= Left Resource.ResourceNotFoundOrNotOwned)

testResourceActiveExpiry :: Assertion
testResourceActiveExpiry = runManaged do
  (testInit, destroyed) <- newTestInit "active-ttl" False
  resourceId <- Resource.create @TestObject Resource.Init
    { message = ownerMessage
    , arguments = testInit{ttlSeconds = Just 1}
    } >>= expectRight
  access <- expectRight (Resource.accessFromMessage ownerMessage)
  started <- MVar.newEmptyMVar
  finish <- MVar.newEmptyMVar
  worker <- Concurrency.fork "active-ttl-user" $
    void $ Resource.withResource @TestObject access resourceId Nothing \_ ->
      MVar.putMVar started () >> MVar.takeMVar finish
  MVar.takeMVar started
  threadDelay 1_200_000
  Resource.list access >>= liftIO . assertBool "active expired resource remains registered" . any ((== resourceId) . (.resourceId))
  MVar.putMVar finish ()
  Concurrency.await worker
  threadDelay 200_000
  Resource.list access >>= liftIO . (@?= [])
  MVar.tryTakeMVar destroyed >>= liftIO . (@?= Just ())

testPersistentExpiry :: Assertion
testPersistentExpiry =
  runEff $ runConcurrent $ FileSystem.runFileSystem do
    tmp <- FileSystem.getTemporaryDirectory
    unique <- liftIO Unique.newUnique
    let database = tmp </> ("cosmobot-resource-expiry-" <> show (Unique.hashUnique unique) <> ".sqlite")
        cleanup = FileSystem.removeFile database
    (do
      liftIO $ runPersistent database do
        void $ Resource.createNamed @PersistentObject "expires" Resource.Init
          { message = ownerMessage
          , arguments = PersistentInit{label = "expires", persistentTTLSeconds = Just 1}
          } >>= expectRight
      threadDelay 1_100_000
      listed <- liftIO $ runPersistent database do
        threadDelay 200_000
        access <- expectRight (Resource.accessFromMessage ownerMessage)
        Resource.list access
      liftIO $ listed @?= []
      ) `finally` cleanup

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
  Sandbox.podmanRunArgs "registry.example.test/custom:latest" "sandbox-name" @?=
    [ "run", "--detach", "--name", "sandbox-name", "--security-opt=no-new-privileges"
    , "--memory=1g", "--cpus=1", "--pids-limit=256"
    , "--label", "io.cosmobot.resource=sandbox"
    , "--", "registry.example.test/custom:latest", "sleep", "infinity"
    ]

testPodmanExecArguments :: Assertion
testPodmanExecArguments = do
  let script = "printf '%s' '$HOME; $(touch /tmp/nope)'"
      args = Sandbox.podmanExecArgs "container-id" 30 7 script
  take 4 args @?= ["exec", "container-id", "bash", "-c"]
  drop (length args - 4) args @?= ["--", "30", "7", Text.unpack script]
  assertBool "sandbox command should be synchronous" ("--detach" `notElem` args)
  assertBool "sandbox wrapper should not use temporary files" (maybe False (not . Text.isInfixOf "/tmp" . Text.pack) (args !!? 4))
  Sandbox.podmanCopyFromArgs "container-id" "/work/out.bin" "/tmp/out.bin" @?=
    ["cp", "--", "container-id:/work/out.bin", "/tmp/out.bin"]
  Sandbox.podmanCopyToArgs "container-id" "/tmp/in.bin" "/work/in.bin" @?=
    ["cp", "--", "/tmp/in.bin", "container-id:/work/in.bin"]

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
      Workspace.createWorkspaceAt root Workspace.WorkspaceArgs{workId = "../escape", goal = "nope", ttlMinutes = 10} >>= \case
        Left err -> liftIO $ err @?= "id may contain only letters, digits, dot, underscore, and hyphen."
        Right _ -> liftIO $ assertFailure "unsafe workspace id was accepted"
      workspace <- Workspace.createWorkspaceAt root Workspace.WorkspaceArgs
        { workId = "demo-work"
        , goal = "initial goal"
        , ttlMinutes = 10
        } >>= expectRight
      FileSystem.createDirectory (path </> "repo")
      report <- Workspace.queryWorkspace workspace >>= expectRight
      liftIO $ assertBool "query includes WORK.md" ("WORK.md:\ninitial goal" `Text.isInfixOf` report)
      liftIO $ assertBool "query includes depth-one tree" ("repo" `Text.isInfixOf` report)
      Workspace.updateWorkspace workspace "updated goal" >>= liftIO . (@?= Right ())
      updated <- Workspace.queryWorkspace workspace >>= expectRight
      liftIO $ assertBool "query includes updated goal" ("WORK.md:\nupdated goal" `Text.isInfixOf` updated)
      restored <- Workspace.restoreWorkspaceAt root "demo-work" >>= expectRight
      Workspace.queryWorkspace restored >>= liftIO . \case
        Right restoredReport -> assertBool "restored workspace keeps its files" ("WORK.md:\nupdated goal" `Text.isInfixOf` restoredReport)
        Left err -> assertFailure (Text.unpack err)
      let persistence = Resource.resourcePersistence (Proxy @Workspace.Workspace)
            :: Resource.ResourcePersistence
                (Eff '[FileSystem.FileSystem, Concurrent, TypedProcess.TypedProcess, IOE])
                Workspace.Workspace
      liftIO $ case persistence of
        Resource.EphemeralResource -> assertFailure "workspace must be persistent"
        Resource.PersistentResource{encodeResource} -> encodeResource workspace @?= "demo-work"
      Resource.destroyResourceObject workspace >>= liftIO . (@?= Right ())
      FileSystem.doesDirectoryExist path >>= liftIO . (@?= False)
      ) `finally` cleanup

newTestInit :: (Prim :> es, Concurrent :> es) => Text -> Bool -> Eff es (TestInit, MVar.MVar ())
newTestInit label failing = do
  failDestroy <- IORef.newIORef failing
  destroyed <- MVar.newEmptyMVar
  pure (TestInit{label, failDestroy, destroyed, ttlSeconds = Nothing}, destroyed)

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
