{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Bot.Resource.Python
  ( PythonWorker
  , PythonArgs
  , FakeInput (..)
  , FakeWorkerProbe
  , FakeCleanupSnapshot (..)
  , newFakePythonArgs
  , fakeWrittenFrames
  , waitForFakeWrite
  , sendFakeFrame
  , fakeCleanupSnapshot
  , withAnonymousPython
  , runPythonState
  )
where

import Bot.Agent.Failure
import Bot.Agent.Program.Python
import Bot.Agent.Tools.Python (PythonRequest (..))
import Bot.Agent.Types (ToolResult)
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude hiding (state)
import Bot.Resource.Python.Protocol
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Effectful.Concurrent.MVar as MVar
import Effectful.Timeout (Timeout, timeout)

newtype PythonWorker = PythonWorker
  { state :: MVar.MVar PythonWorkerState
  }

data PythonWorkerState
  = Paused !PausedHandles
  | Running !RunningHandles !ProtocolState
  | Terminal

newtype PausedHandles = PausedHandles FakeTransport
newtype RunningHandles = RunningHandles FakeTransport

data PythonArgs = PythonArgs
  { transport :: !FakeTransport
  , executionTimeoutMicroseconds :: !Int
  }

data FakeInput
  = FakeFrames ![ByteString]
  | FakeBlocked

data FakeTransport = FakeTransport
  { input :: !(Either (MVar.MVar ByteString) (MVar.MVar [ByteString]))
  , writes :: !(MVar.MVar [ByteString])
  , wroteFrame :: !(MVar.MVar ())
  , launchClaimed :: !(MVar.MVar Bool)
  , cleanup :: !(MVar.MVar FakeCleanupState)
  , timeoutMicroseconds :: !Int
  }

newtype FakeWorkerProbe = FakeWorkerProbe FakeTransport

data FakeCleanupState = FakeCleanupState
  { failCleanup :: !Bool
  , cleanupAttempts :: !Int
  , cleanupKills :: !Int
  , cleanupAwaits :: !Int
  , cleanupLoggedFailures :: !Int
  }

data FakeCleanupSnapshot = FakeCleanupSnapshot
  { attempts :: !Int
  , kills :: !Int
  , awaits :: !Int
  , loggedFailures :: !Int
  }
  deriving stock (Eq, Show)

newFakePythonArgs
  :: Concurrent :> es
  => FakeInput
  -> Int
  -> Bool
  -> Eff es (PythonArgs, FakeWorkerProbe)
newFakePythonArgs fakeInput executionTimeoutMicroseconds failCleanup = do
  input <- case fakeInput of
    FakeFrames frames -> Right <$> MVar.newMVar frames
    FakeBlocked -> Left <$> MVar.newEmptyMVar
  writes <- MVar.newMVar []
  wroteFrame <- MVar.newEmptyMVar
  launchClaimed <- MVar.newMVar False
  cleanup <- MVar.newMVar FakeCleanupState
    { failCleanup
    , cleanupAttempts = 0
    , cleanupKills = 0
    , cleanupAwaits = 0
    , cleanupLoggedFailures = 0
    }
  let timeoutMicroseconds = executionTimeoutMicroseconds
      transport = FakeTransport{input, writes, wroteFrame, launchClaimed, cleanup, timeoutMicroseconds}
  pure (PythonArgs{transport, executionTimeoutMicroseconds}, FakeWorkerProbe transport)

fakeWrittenFrames :: Concurrent :> es => FakeWorkerProbe -> Eff es [ByteString]
fakeWrittenFrames (FakeWorkerProbe transport) =
  MVar.readMVar transport.writes

waitForFakeWrite :: Concurrent :> es => FakeWorkerProbe -> Eff es ()
waitForFakeWrite (FakeWorkerProbe transport) =
  MVar.readMVar transport.wroteFrame

sendFakeFrame :: Concurrent :> es => FakeWorkerProbe -> ByteString -> Eff es Bool
sendFakeFrame (FakeWorkerProbe transport) frame =
  case transport.input of
    Left notifier -> MVar.tryPutMVar notifier frame
    Right _ -> pure False

fakeCleanupSnapshot :: Concurrent :> es => FakeWorkerProbe -> Eff es FakeCleanupSnapshot
fakeCleanupSnapshot (FakeWorkerProbe transport) = do
  cleanup <- MVar.readMVar transport.cleanup
  pure FakeCleanupSnapshot
    { attempts = cleanup.cleanupAttempts
    , kills = cleanup.cleanupKills
    , awaits = cleanup.cleanupAwaits
    , loggedFailures = cleanup.cleanupLoggedFailures
    }

instance (Concurrent :> es, KatipE :> es) => Resource.ResourceObject (Eff es) PythonWorker where
  type CreationArgs PythonWorker = PythonArgs
  resourceTypeName _ = "PythonWorker"
  resourceScope _ = Resource.PersonResource
  resourceIdPrefix _ = "python"
  resourcePersistence _ = Resource.EphemeralResource
  resourceListed _ = False
  resourceTTLSeconds arguments
    | arguments.executionTimeoutMicroseconds <= 0 = Left "Python execution timeout must be positive."
    | otherwise =
        Right (Just (ceilingSeconds arguments.executionTimeoutMicroseconds + cleanupGraceSeconds + orphanMarginSeconds))
  createResourceObject Resource.Init{arguments} = do
    claimed <- MVar.modifyMVarMasked arguments.transport.launchClaimed \case
      False -> pure (True, True)
      True -> pure (True, False)
    if claimed
      then Right . PythonWorker <$> MVar.newMVar (Paused (PausedHandles arguments.transport))
      else pure (Left "Python worker launch may only be used once.")
  destroyResourceObject worker = do
    owned <- MVar.modifyMVarMasked worker.state \case
      Terminal -> pure (Terminal, Nothing)
      Paused (PausedHandles handles) -> pure (Terminal, Just handles)
      Running (RunningHandles handles) _ -> pure (Terminal, Just handles)
    traverse_ cleanupTransport owned
    pure (Right ())
  describeResourceObject _ result = pure (either (const "unavailable") id result)
  probeResourceObject worker = MVar.readMVar worker.state <&> Right . \case
    Paused{} -> "paused"
    Running _ protocol -> Text.toLower (Text.pack (show protocol))
    Terminal -> "terminal"

cleanupGraceSeconds :: Int
cleanupGraceSeconds = 5

orphanMarginSeconds :: Int
orphanMarginSeconds = 10

ceilingSeconds :: Int -> Int
ceilingSeconds microseconds =
  let (seconds, remainder) = max 1 microseconds `quotRem` 1_000_000
  in max 1 (seconds + fromEnum (remainder /= 0))

cleanupTransport :: (Concurrent :> es, KatipE :> es) => FakeTransport -> Eff es ()
cleanupTransport transport = do
  failure <- MVar.modifyMVarMasked transport.cleanup \current ->
    let failed = current.failCleanup
        next :: FakeCleanupState
        next = current
          { cleanupAttempts = current.cleanupAttempts + 1
          , cleanupKills = current.cleanupKills + 1
          , cleanupAwaits = current.cleanupAwaits + 1
          , cleanupLoggedFailures = current.cleanupLoggedFailures + fromEnum failed
          }
    in pure (next, failed)
  when failure $
    logWarning "Python worker cleanup failed after SIGKILL; forgetting the isolated worker as an accepted leak."

withAnonymousPython
  :: ( Resource.Resource :> es
     , Resource.ResourceObject (Eff es) PythonWorker
     , Concurrent :> es
     )
  => Resource.ResourceAccess
  -> Maybe Concurrency.Handle
  -> Resource.Init PythonArgs
  -> (PythonWorker -> Eff es a)
  -> Eff es (Either Resource.ResourceError a)
withAnonymousPython access resourceOwner initValue use =
  case Resource.accessFromMessage initValue.message of
    Left err -> pure (Left err)
    Right messageAccess
      | messageAccess.owner /= access.owner -> pure (Left Resource.ResourceNotFoundOrNotOwned)
      | otherwise -> mask \restore -> do
          created <- Resource.createAssociated @PythonWorker resourceOwner initValue
          case created of
            Left err -> pure (Left err)
            Right resourceId ->
              restore
                (Resource.withResource @PythonWorker access resourceId resourceOwner
                  (\worker -> activateWorker worker >> use worker))
                `finally` void (Resource.destroy access resourceId)

activateWorker :: Concurrent :> es => PythonWorker -> Eff es ()
activateWorker worker =
  MVar.modifyMVarMasked_ worker.state \case
    Paused (PausedHandles handles) -> pure (Running (RunningHandles handles) Created)
    state -> pure state

runPythonState
  :: (Concurrent :> es, Timeout :> es, IOE :> es)
  => (Int -> NonEmpty PythonToolCall -> Eff es (NonEmpty ToolResult))
  -> PythonWorker
  -> PythonRequest
  -> Eff es PythonExit
runPythonState runTools worker request = do
  timeoutMicros <- workerTimeout worker
  outcome <- trySync $ timeout timeoutMicros (runProtocol runTools worker request)
  case outcome of
    Left err -> failWorker worker $ externalServiceFailure
      "Python worker failed."
      (Text.take 500 (Text.pack (show err)))
    Right Nothing -> failWorker worker $ budgetExhaustedFailure
      "Python execution timed out."
      "The Python worker exceeded its wall-time budget."
    Right (Just result) -> pure result

workerTimeout :: Concurrent :> es => PythonWorker -> Eff es Int
workerTimeout worker = MVar.readMVar worker.state <&> \case
  Paused (PausedHandles handles) -> handles.timeoutMicroseconds
  Running (RunningHandles handles) _ -> handles.timeoutMicroseconds
  Terminal -> 1

runProtocol
  :: (Concurrent :> es, IOE :> es)
  => (Int -> NonEmpty PythonToolCall -> Eff es (NonEmpty ToolResult))
  -> PythonWorker
  -> PythonRequest
  -> Eff es PythonExit
runProtocol runTools worker request = do
  beginRun worker >>= \case
    Left err -> pure (PythonFailed (protocolFailureValue err))
    Right handles -> do
      startProtocol worker handles request.code >>= \case
        Left err -> protocolFailure err
        Right () -> loop handles
  where
    loop handles = do
      readFrameValue handles >>= \case
        Left "EOF" -> failWorker worker $ permanentArgumentFailure
          "Python exited before completing."
          "The worker closed its protocol stream before replying to python.run."
        Left err -> protocolFailure err
        Right value -> case parseWorkerMessage value of
          Left err -> protocolFailure err
          Right (RunCompleted content) ->
            completeWorker worker content
          Right (RunFailed message) ->
            failWorker worker (permanentArgumentFailure message message)
          Right (RunTools rpcId calls) -> do
            claimRequest worker rpcId >>= \case
              Left err -> protocolFailure err
              Right () -> do
                results <- runTools rpcId calls
                writeValue handles (toolsRunResponse rpcId results) >>= \case
                  Left err -> protocolFailure err
                  Right () -> loop handles

    protocolFailure detail =
      failWorker worker (protocolFailureValue detail)

    protocolFailureValue detail =
      externalServiceFailure
        "Python worker protocol failed."
        (Text.take 500 detail)

startProtocol
  :: Concurrent :> es
  => PythonWorker
  -> RunningHandles
  -> Text
  -> Eff es (Either Text ())
startProtocol worker handles code = do
  writeValue handles (pythonRunRequest code) >>= \case
    Left err -> pure (Left err)
    Right () -> transitionToWaiting worker

beginRun :: Concurrent :> es => PythonWorker -> Eff es (Either Text RunningHandles)
beginRun worker =
  MVar.modifyMVarMasked worker.state \case
    Running handles Created -> pure (Running handles RunSent, Right handles)
    state -> pure (state, Left [i|Python worker is not ready: #{showState state}|])
  where
    showState = \case
      Paused{} -> "paused" :: Text
      Running _ protocol -> Text.pack (show protocol)
      Terminal -> "terminal"

claimRequest :: Concurrent :> es => PythonWorker -> Int -> Eff es (Either Text ())
claimRequest worker rpcId =
  MVar.modifyMVarMasked worker.state \case
    Running handles (Waiting nextRpcId)
      | rpcId == nextRpcId ->
          pure (Running handles (Waiting (nextRpcId + 1)), Right ())
      | otherwise ->
          pure (Running handles (Waiting nextRpcId), Left [i|expected tools.run id #{nextRpcId}, got #{rpcId}|])
    state -> pure (state, Left "tools.run request arrived outside Waiting state")

completeWorker :: Concurrent :> es => PythonWorker -> Text -> Eff es PythonExit
completeWorker worker content =
  transitionFromWaiting worker (Completed content) >>= \case
    Left err -> failWorker worker (externalServiceFailure "Python worker protocol failed." err)
    Right () -> pure (PythonCompleted content)

failWorker :: Concurrent :> es => PythonWorker -> Failure -> Eff es PythonExit
failWorker worker failure = do
  MVar.modifyMVarMasked_ worker.state \case
    Running handles _ -> pure (Running handles (Failed failure.detail))
    state -> pure state
  pure (PythonFailed failure)

transitionToWaiting :: Concurrent :> es => PythonWorker -> Eff es (Either Text ())
transitionToWaiting worker =
  transitionProtocol worker \case
    RunSent -> Right (Waiting 1)
    state -> Left [i|Python worker cannot wait from #{show state :: String}|]

transitionFromWaiting :: Concurrent :> es => PythonWorker -> ProtocolState -> Eff es (Either Text ())
transitionFromWaiting worker terminal =
  transitionProtocol worker \case
    Waiting _ -> Right terminal
    state -> Left [i|Python worker terminal response arrived in #{show state :: String}|]

transitionProtocol
  :: Concurrent :> es
  => PythonWorker
  -> (ProtocolState -> Either Text ProtocolState)
  -> Eff es (Either Text ())
transitionProtocol worker transition =
  MVar.modifyMVarMasked worker.state \case
    Running handles current -> case transition current of
      Left err -> pure (Running handles current, Left err)
      Right next -> pure (Running handles next, Right ())
    state -> pure (state, Left [i|Python worker is unavailable: #{showWorkerState state}|])
  where
    showWorkerState = \case
      Paused{} -> "paused" :: Text
      Running _ protocol -> Text.pack (show protocol)
      Terminal -> "terminal"

writeValue :: Concurrent :> es => RunningHandles -> Aeson.Value -> Eff es (Either Text ())
writeValue (RunningHandles transport) value =
  case encodeFrame value of
    Left err -> pure (Left (Text.pack (show err)))
    Right frame -> do
      MVar.modifyMVarMasked_ transport.writes (pure . (<> [frame]))
      void (MVar.tryPutMVar transport.wroteFrame ())
      pure (Right ())

readFrameValue :: Concurrent :> es => RunningHandles -> Eff es (Either Text Aeson.Value)
readFrameValue (RunningHandles transport) = do
  next <- case transport.input of
    Left notifier -> MVar.takeMVar notifier <&> Right
    Right frames -> MVar.modifyMVarMasked frames \case
      [] -> pure ([], Left "EOF")
      frame : rest -> pure (rest, Right frame)
  pure $ next >>= first (Text.pack . show) . decodeFrame
