{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Bot.Resource.Python
  ( PythonWorker
  , PythonArgs
  , preparePythonArgs
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
import qualified Bot.Resource.Python.Sandbox as Sandbox
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Effectful.Concurrent.MVar as MVar
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem.IO as FileSystemIO
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Effectful.Process.Typed as TypedProcess
import Effectful.Timeout (Timeout, timeout)
import GHC.Clock (getMonotonicTimeNSec)

newtype PythonWorker = PythonWorker
  { state :: MVar.MVar PythonWorkerState
  }

data PythonWorkerState
  = Gated !GatedHandles
  | Running !RunningHandles !ProtocolState
  | Terminal

data GatedHandles = GatedHandles
  { sandbox :: !Sandbox.GatedSandbox
  , deadlineNanoseconds :: !Word64
  }

data RunningHandles = RunningHandles
  { sandbox :: !Sandbox.RunningSandbox
  , deadlineNanoseconds :: !Word64
  }

data PythonArgs = PythonArgs
  { sandboxConfig :: !Sandbox.Config
  , executionTimeoutMicroseconds :: !Int
  }

preparePythonArgs
  :: ( Concurrent :> es
     , FileSystem :> es
     , TypedProcess.TypedProcess :> es
     , Timeout :> es
     , IOE :> es
     )
  => FilePath
  -> Int
  -> Eff es (Either Text PythonArgs)
preparePythonArgs workerPath executionTimeoutMicroseconds
  | executionTimeoutMicroseconds <= 0 = pure (Left "Python execution timeout must be positive.")
  | executionTimeoutMicroseconds > maxExecutionTimeoutMicroseconds =
      pure (Left "Python execution timeout must not exceed one hour.")
  | otherwise =
      Sandbox.prepare workerPath <&> fmap \sandboxConfig ->
        PythonArgs{sandboxConfig, executionTimeoutMicroseconds}

instance
  ( Concurrent :> es
  , KatipE :> es
  , FileSystem :> es
  , TypedProcess.TypedProcess :> es
  , Timeout :> es
  , IOE :> es
  ) => Resource.ResourceObject (Eff es) PythonWorker where
  type CreationArgs PythonWorker = PythonArgs
  resourceTypeName _ = "PythonWorker"
  resourceScope _ = Resource.PersonResource
  resourceIdPrefix _ = "python"
  resourcePersistence _ = Resource.EphemeralResource
  resourceListed _ = False
  resourceTTLSeconds arguments
    | arguments.executionTimeoutMicroseconds <= 0 = Left "Python execution timeout must be positive."
    | arguments.executionTimeoutMicroseconds > maxExecutionTimeoutMicroseconds =
        Left "Python execution timeout must not exceed one hour."
    | otherwise =
        Right (Just (ceilingSeconds arguments.executionTimeoutMicroseconds + cleanupGraceSeconds + orphanMarginSeconds))
  createResourceObject Resource.Init{arguments} = mask \restore -> do
    deadlineNanoseconds <- deadlineAfter arguments.executionTimeoutMicroseconds
    trySync (restore (timeout arguments.executionTimeoutMicroseconds (Sandbox.launchGated arguments.sandboxConfig))) >>= \case
      Left err -> pure (Left (Text.take 500 (Text.pack (displayException err))))
      Right Nothing -> pure (Left "Python sandbox startup timed out.")
      Right (Just sandbox) ->
        (Right . PythonWorker <$> MVar.newMVar
          (Gated GatedHandles{sandbox, deadlineNanoseconds}))
          `onException` Sandbox.stopGated sandbox
  destroyResourceObject worker = do
    owned <- MVar.modifyMVarMasked worker.state \case
      Terminal -> pure (Terminal, Nothing)
      Gated handles -> pure (Terminal, Just (Left handles))
      Running handles _ -> pure (Terminal, Just (Right handles))
    traverse_ cleanupSandbox owned
    pure (Right ())
  describeResourceObject _ result = pure (either (const "unavailable") id result)
  probeResourceObject worker = MVar.readMVar worker.state <&> Right . \case
    Gated{} -> "gated"
    Running _ protocol -> Text.toLower (Text.pack (show protocol))
    Terminal -> "terminal"

cleanupGraceSeconds :: Int
cleanupGraceSeconds = 5

orphanMarginSeconds :: Int
orphanMarginSeconds = 10

maxExecutionTimeoutMicroseconds :: Int
maxExecutionTimeoutMicroseconds = 60 * 60 * 1_000_000

ceilingSeconds :: Int -> Int
ceilingSeconds microseconds =
  let (seconds, remainder) = max 1 microseconds `quotRem` 1_000_000
  in max 1 (seconds + fromEnum (remainder /= 0))

cleanupSandbox
  :: ( TypedProcess.TypedProcess :> es
     , FileSystem :> es
     , Timeout :> es
     , KatipE :> es
     , IOE :> es
     )
  => Either GatedHandles RunningHandles
  -> Eff es ()
cleanupSandbox owned =
  (case owned of
    Left handles -> Sandbox.stopGated handles.sandbox
    Right handles -> Sandbox.stopRunning handles.sandbox)
  `catchSync` \err ->
    logWarning [i|Python worker cleanup failed after SIGKILL; forgetting the isolated worker: #{Text.take 500 (Text.pack (displayException err))}|]

withAnonymousPython
  :: ( Resource.Resource :> es
     , Resource.ResourceObject (Eff es) PythonWorker
     , Concurrent :> es
     , FileSystem :> es
     , TypedProcess.TypedProcess :> es
     , Timeout :> es
     , IOE :> es
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

activateWorker
  :: ( Concurrent :> es
     , TypedProcess.TypedProcess :> es
     , FileSystem :> es
     , Timeout :> es
     , IOE :> es
     )
  => PythonWorker
  -> Eff es ()
activateWorker worker =
  MVar.modifyMVarMasked_ worker.state \case
    Gated handles -> do
      remaining <- remainingMicroseconds handles.deadlineNanoseconds
      timeout remaining (Sandbox.start handles.sandbox) >>= \case
        Nothing -> throwIO PythonStartupTimeout
        Just sandbox ->
          pure (Running RunningHandles
            { sandbox
            , deadlineNanoseconds = handles.deadlineNanoseconds
            } Created)
    state -> pure state

runPythonState
  :: ( Concurrent :> es
     , Timeout :> es
     , FileSystem :> es
     , TypedProcess.TypedProcess :> es
     , IOE :> es
     )
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

workerTimeout :: (Concurrent :> es, IOE :> es) => PythonWorker -> Eff es Int
workerTimeout worker = MVar.readMVar worker.state >>= \case
  Gated{} -> pure 1
  Running handles _ -> remainingMicroseconds handles.deadlineNanoseconds
  Terminal -> pure 1

data PythonStartupTimeout = PythonStartupTimeout
  deriving stock (Show)
  deriving anyclass (Exception)

deadlineAfter :: IOE :> es => Int -> Eff es Word64
deadlineAfter microseconds = do
  now <- liftIO getMonotonicTimeNSec
  pure (now + fromIntegral microseconds * 1_000)

remainingMicroseconds :: IOE :> es => Word64 -> Eff es Int
remainingMicroseconds deadline = do
  now <- liftIO getMonotonicTimeNSec
  pure (max 1 (fromIntegral ((deadline - min deadline now) `quot` 1_000)))

runProtocol
  :: ( Concurrent :> es
     , FileSystem :> es
     , TypedProcess.TypedProcess :> es
     , IOE :> es
     )
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
        Left "EOF" -> do
          (exitCode, stderrText) <- Sandbox.terminalOutcome handles.sandbox
          failWorker worker (terminalFailure exitCode stderrText)
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
                  Left err -> failWorker worker $ uncertainSideEffectFailure
                    "Python lost contact after nested tools ran; their side effects may have happened."
                    (Text.take 500 err)
                  Right () -> loop handles

    protocolFailure detail =
      failWorker worker (protocolFailureValue detail)

    protocolFailureValue detail =
      externalServiceFailure
        "Python worker protocol failed."
        (Text.take 500 detail)

startProtocol
  :: (Concurrent :> es, FileSystem :> es, IOE :> es)
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
      Gated{} -> "gated" :: Text
      Running _ protocol -> Text.pack (show protocol)
      Terminal -> "terminal"

claimRequest :: Concurrent :> es => PythonWorker -> Int -> Eff es (Either Text ())
claimRequest worker rpcId =
  MVar.modifyMVarMasked worker.state \case
    Running handles protocol -> case claimToolsRequest protocol rpcId of
      Left err -> pure (Running handles protocol, Left err)
      Right next -> pure (Running handles next, Right ())
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
      Gated{} -> "gated" :: Text
      Running _ protocol -> Text.pack (show protocol)
      Terminal -> "terminal"

writeValue
  :: (FileSystem :> es, IOE :> es)
  => RunningHandles
  -> Aeson.Value
  -> Eff es (Either Text ())
writeValue handles value =
  case encodeFrame value of
    Left err -> pure (Left (Text.pack (show err)))
    Right frame ->
      trySync
        (FileSystemByteString.hPut (Sandbox.stdinHandle handles.sandbox) frame
          >> FileSystemIO.hFlush (Sandbox.stdinHandle handles.sandbox))
        <&> first (Text.pack . displayException)

readFrameValue
  :: (FileSystem :> es, IOE :> es)
  => RunningHandles
  -> Eff es (Either Text Aeson.Value)
readFrameValue handles = do
  readBoundedFrame (Sandbox.stdoutHandle handles.sandbox) <&>
    (>>= first (Text.pack . show) . decodeFrame)

readBoundedFrame :: FileSystem :> es => Handle -> Eff es (Either Text ByteString)
readBoundedFrame output = go 0 []
  where
    go size chunks = do
      chunk <- FileSystemByteString.hGetSome output 4096
      if ByteString.null chunk
        then pure (Left "EOF")
        else case ByteString.elemIndex 10 chunk of
          Nothing
            | size + ByteString.length chunk > maxRpcBytes ->
                pure (Left "Python worker frame exceeds the 4 MiB limit.")
            | otherwise -> go (size + ByteString.length chunk) (chunk : chunks)
          Just newlineAt
            | size + newlineAt > maxRpcBytes ->
                pure (Left "Python worker frame exceeds the 4 MiB limit.")
            | not (ByteString.null (ByteString.drop (newlineAt + 1) chunk)) ->
                pure (Left "Python worker sent bytes after its frame.")
            | otherwise ->
                pure (Right (ByteString.concat (reverse (ByteString.take (newlineAt + 1) chunk : chunks))))
