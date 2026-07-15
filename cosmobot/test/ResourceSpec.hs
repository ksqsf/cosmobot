{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import qualified Bot.Concurrency.Manager as ConcurrencyManager
import qualified Bot.Agent as Agent
import qualified Bot.Agent.Types as AgentTypes
import qualified Bot.Agent.Tools.Shell as Shell
import Bot.Core.Message
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.ACP as ACP
import qualified Bot.Effect.Resource as Resource
import Bot.Handler.Resource (removeResources, resourceIds)
import Bot.Prelude
import qualified Bot.Resource as ResourceManager
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.Prim.IORef as IORef
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

main :: IO ()
main = defaultMain $ testGroup "resource"
  [ testCase "typed heterogeneous resources and local ids" testTypedResources
  , testCase "owner isolation and superuser removal" testOwnership
  , testCase "scoped use clears after exceptions" testScopedException
  , testCase "removal cancels and awaits active users" testRemovalCancelsUsers
  , testCase "cleanup failure restores resource for retry" testCleanupRetry
  , testCase "acquisition is blocked while destruction runs" testBlockedDuringDestroy
  , testCase "manager exit destroys resources" testShutdown
  , testCase "ACP terminal uses local resource ids through release" testAcpTerminal
  , testCase "resource command ids preserve first occurrence" $
      resourceIds "res-2 res-1 res-2 res-3" @?= ["res-2", "res-1", "res-3"]
  , testCase "resource removal reports partial results independently" testPartialRemoval
  ]

testTypedResources :: Assertion
testTypedResources = runManaged do
  (testInit, _) <- newTestInit "alpha" False
  firstResult <- Resource.create @TestObject Resource.Init{message = ownerMessage, agentId = "agent-1", arguments = testInit}
  secondResult <- Resource.create @OtherObject Resource.Init{message = ownerMessage, agentId = "agent-2", arguments = ()}
  liftIO $ firstResult @?= Right "res-1"
  liftIO $ secondResult @?= Right "res-2"
  access <- expectRight (Resource.accessFromMessage ownerMessage)
  listed <- Resource.list access
  liftIO $ map (\item -> (item.resourceType, item.agentId, item.description, item.probeResult)) listed
    @?= [("Test", "agent-1", "alpha", Right "healthy"), ("Other", "agent-2", "other", Right "healthy")]
  mismatch <- Resource.withResource @OtherObject access "res-1" Nothing (const (pure ()))
  liftIO $ mismatch @?= Left Resource.ResourceTypeMismatch

testOwnership :: Assertion
testOwnership = runManaged do
  (testInit, _) <- newTestInit "owned" False
  resourceId <- Resource.create @TestObject Resource.Init{message = ownerMessage, agentId = "agent", arguments = testInit} >>= expectRight
  otherAccess <- expectRight (Resource.accessFromMessage (ownerMessage{senderId = Just "other"}))
  Resource.list otherAccess >>= liftIO . (@?= [])
  Resource.destroy otherAccess resourceId >>= liftIO . (@?= Left Resource.ResourceNotFoundOrNotOwned)
  let adminMessage = (ownerMessage{senderId = Just "admin", digest = emptyMessageDigest{senderIsSuperuser = True}})
  adminAccess <- expectRight (Resource.accessFromMessage adminMessage)
  Resource.destroy adminAccess resourceId >>= liftIO . (@?= Right ())
  let missing = ownerMessage{senderId = Nothing}
  liftIO $ Resource.accessFromMessage missing @?= Left Resource.MissingResourceIdentity

testScopedException :: Assertion
testScopedException = runManaged do
  (testInit, _) <- newTestInit "scope" False
  resourceId <- Resource.create @TestObject Resource.Init{message = ownerMessage, agentId = "agent", arguments = testInit} >>= expectRight
  access <- expectRight (Resource.accessFromMessage ownerMessage)
  _ <- trySync $ Resource.withResource @TestObject access resourceId Nothing \_ -> throwIO TestFailure
  Resource.withResource @TestObject access resourceId Nothing (pure . (.label)) >>= liftIO . (@?= Right "scope")

testRemovalCancelsUsers :: Assertion
testRemovalCancelsUsers = runManaged do
  (testInit, destroyed) <- newTestInit "active" False
  resourceId <- Resource.create @TestObject Resource.Init{message = ownerMessage, agentId = "agent", arguments = testInit} >>= expectRight
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
  resourceId <- Resource.create @TestObject Resource.Init{message = ownerMessage, agentId = "agent", arguments = testInit} >>= expectRight
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
    , agentId = "agent"
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
    void $ Resource.create @TestObject Resource.Init{message = ownerMessage, agentId = "agent", arguments = testInit}
    pure destroyed
  runEff $ runConcurrent $ void (MVar.takeMVar destroyed)

testPartialRemoval :: Assertion
testPartialRemoval = runManaged do
  (okInit, _) <- newTestInit "ok" False
  (badInit, _) <- newTestInit "bad" True
  void $ Resource.create @TestObject Resource.Init{message = ownerMessage, agentId = "agent", arguments = okInit}
  void $ Resource.create @TestObject Resource.Init{message = ownerMessage, agentId = "agent", arguments = badInit}
  access <- expectRight (Resource.accessFromMessage ownerMessage)
  results <- removeResources access (resourceIds "res-1 missing res-2 res-1")
  liftIO $ results @?=
    [ "- `res-1`: removed"
    , "- `missing`: not found/not owned"
    , "- `res-2`: cleanup failure"
    ]

testAcpTerminal :: Assertion
testAcpTerminal = do
  calls <- runEff $ runPrim $ IORef.newIORef []
  runTerminal calls do
    let context = Agent.AgentContext
          { message = ownerMessage{platform = PlatformACP}
          , input = inputWithImages "" []
          , superuser = False
          , systemContext = ""
          , askCommand = "!ask"
          , toolConfig = Agent.defaultToolConfig
          }
        metadata = AgentTypes.ToolCallMetadata{agentRunId = "agent-7", parent = Nothing}
    runner <- (Shell.acpTerminalTool :: Agent.Tool TerminalStack).start context
    createResult <- runner metadata $ Aeson.object
      [ "action" Aeson..= ("create" :: Text)
      , "command" Aeson..= ("sleep" :: Text)
      , "args" Aeson..= (["10"] :: [Text])
      ]
    liftIO $ AgentTypes.toolResultContent createResult @?= "{\"terminalId\":\"res-1\"}"
    outputResult <- runner metadata $ Aeson.object ["action" Aeson..= ("output" :: Text), "terminal_id" Aeson..= ("res-1" :: Text)]
    liftIO $ assertBool "terminal output uses the local id" ("still running" `Text.isInfixOf` AgentTypes.toolResultContent outputResult)
    _ <- runner metadata $ Aeson.object ["action" Aeson..= ("kill" :: Text), "terminal_id" Aeson..= ("res-1" :: Text)]
    access <- expectRight (Resource.accessFromMessage context.message)
    listed <- Resource.list access
    liftIO $ map (.resourceId) listed @?= ["res-1"]
    releaseResult <- runner metadata $ Aeson.object ["action" Aeson..= ("release" :: Text), "terminal_id" Aeson..= ("res-1" :: Text)]
    liftIO $ AgentTypes.toolResultContent releaseResult @?= "Terminal released."
    listedAfter <- Resource.list access
    liftIO $ map (.resourceId) listedAfter @?= []
  runEff (runPrim (IORef.readIORef calls)) >>= (@?=
    [ "create"
    , "output:remote-1"
    , "kill:remote-1"
    , "output:remote-1"
    , "kill:remote-1"
    , "release:remote-1"
    ])

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

runManaged :: Eff '[Resource.Resource, Concurrency.Concurrency, Prim, Concurrent, IOE] a -> IO a
runManaged action =
  runEff $ runConcurrent $ runPrim $ ConcurrencyManager.runConcurrencyManager $ ResourceManager.runResourceManager action

type TerminalStack = '[Resource.Resource, ACP.ACP, Concurrency.Concurrency, Prim, Concurrent, IOE]

runTerminal :: IORef.IORef [Text] -> Eff TerminalStack a -> IO a
runTerminal calls action =
  runEff $ runConcurrent $ runPrim $ ConcurrencyManager.runConcurrencyManager $ runFakeACP calls $ ResourceManager.runResourceManager action

runFakeACP :: forall es a. Prim :> es => IORef.IORef [Text] -> Eff (ACP.ACP : es) a -> Eff es a
runFakeACP calls = interpret \_ -> \case
  ACP.ReadClientFile{} -> pure (Left "unsupported")
  ACP.WriteClientFile{} -> pure (Left "unsupported")
  ACP.CreateClientTerminal _ _ -> record "create" $> Right "remote-1"
  ACP.ReadClientTerminalOutput _ terminalId ->
    record ("output:" <> terminalId) $> Right ACP.TerminalOutput{output = "still running", truncated = False, exitStatus = Nothing}
  ACP.WaitForClientTerminalExit{} -> pure (Right ACP.TerminalExitStatus{exitCode = Just 0, signal = Nothing})
  ACP.KillClientTerminal _ terminalId -> record ("kill:" <> terminalId) $> Right ()
  ACP.ReleaseClientTerminal _ terminalId -> record ("release:" <> terminalId) $> Right ()
  where
    record :: Text -> Eff es ()
    record call = IORef.modifyIORef' calls (<> [call])

expectRight :: Applicative m => Either e a -> m a
expectRight = either (const (pure (error "expected Right"))) pure

never :: Concurrent :> es => Eff es ()
never = threadDelay maxBound
