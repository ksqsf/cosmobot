{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Plugin.Manager
Description : External plugin process ownership and JSON-RPC dispatch
Stability   : experimental
-}

module Bot.Plugin.Manager
  ( HostCallbacks (..)
  , runPluginManager
  )
where

import Bot.Core.Message
import Bot.Core.Route (RouteHelp (..))
import qualified Bot.Effect.Plugin as Plugin
import qualified Bot.Plugin.Config as Config
import qualified Bot.Plugin.Protocol as Protocol
import qualified Bot.Plugin.Sandbox as Sandbox
import Bot.Plugin.Types
import Bot.Prelude
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import qualified Effectful.Concurrent.Async as Async
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.FileSystem as FileSystem
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem.IO as FileSystemIO
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Effectful.Process as Process
import qualified Effectful.Process.Typed as TypedProcess
import Effectful.Timeout (Timeout, timeout)
import qualified System.Environment as Environment
import System.FilePath ((</>))
import System.Process (Pid)
import System.Exit (ExitCode (..))

data HostCallbacks es = HostCallbacks
  { chatReply :: IncomingMessage -> Text -> Eff es Aeson.Value
  , chatReferenced :: IncomingMessage -> Eff es Aeson.Value
  , llmComplete :: Text -> Eff es Text
  , agentRun :: IncomingMessage -> Text -> Eff es Text
  , mediaResolve :: Text -> Eff es Aeson.Value
  }

type PluginProcess = TypedProcess.Process Handle Handle ()

data Manager es = Manager
  { pluginDirectory :: !FilePath
  , mediaDirectory :: !FilePath
  , callbacks :: !(HostCallbacks es)
  , active :: !(IORef (Map.Map PluginId (RunningPlugin es)))
  , launched :: !(IORef [RunningPlugin es])
  , nextGeneration :: !(IORef Int)
  , requiredFailure :: !(MVar.MVar Text)
  , recentExits :: !(IORef (Map.Map PluginId [UTCTime]))
  , lifecycleLock :: !(MVar.MVar ())
  , operationLock :: !(MVar.MVar ())
  , shuttingDown :: !(IORef Bool)
  }

data RunningPlugin es = RunningPlugin
  { bundle :: !PluginBundle
  , generation :: !Int
  , manifest :: !PluginManifest
  , transport :: !(Transport es)
  , readerThread :: !(Async.Async ())
  }

data Transport es = Transport
  { bundle :: !PluginBundle
  , generation :: !Int
  , process :: !PluginProcess
  , processGroupId :: !(Maybe Pid)
  , input :: !Handle
  , output :: !Handle
  , writeLock :: !(MVar.MVar ())
  , pending :: !(MVar.MVar (Map.Map Text (MVar.MVar (Either Protocol.RpcError Aeson.Value))))
  , invocationContexts :: !(IORef (Map.Map Text IncomingMessage))
  , callbackThreads :: !(MVar.MVar (Maybe (Map.Map Text (Text, Integer, Async.Async ()))))
  , capabilities :: !(IORef (Set.Set Capability))
  , nextRequestId :: !(IORef Integer)
  , nextInvocationId :: !(IORef Integer)
  , nextCallbackToken :: !(IORef Integer)
  , readBuffer :: !(IORef ByteString)
  , stopping :: !(IORef Bool)
  }

data StartFailure = StartFailure
  { message :: !Text
  , transient :: !Bool
  }
  deriving stock (Show)

newtype PluginStartException = PluginStartException StartFailure
  deriving stock (Show)
  deriving anyclass (Exception)

newtype PluginProcessExit = PluginProcessExit ExitCode
  deriving stock (Show)
  deriving anyclass (Exception)

data PluginManagerException
  = RequiredPluginFailed !Text
  | InvalidPluginInitialization !Text
  | InvalidPluginToolNamespace !Text
  | PluginFrameDecodeFailed !Text
  | InvalidPluginMessage !Text
  | PluginProtocolLineTooLong
  | PluginProtocolStreamClosed
  | PluginCallbackMethodNotFound !Text
  | InvalidPluginCallbackParameter !Text !Text
  | PluginFrameEncodeFailed !Text
  | PluginTransportWriteTimedOut
  deriving stock (Eq, Show)

instance Exception PluginManagerException where
  displayException = Text.unpack . \case
    RequiredPluginFailed failure -> failure
    InvalidPluginInitialization err -> err
    InvalidPluginToolNamespace err -> err
    PluginFrameDecodeFailed err -> err
    InvalidPluginMessage err -> "invalid plugin JSON-RPC message: " <> err
    PluginProtocolLineTooLong -> "plugin protocol line exceeds 1 MiB"
    PluginProtocolStreamClosed -> "plugin protocol stream closed"
    PluginCallbackMethodNotFound method -> "plugin callback method not found: " <> method
    InvalidPluginCallbackParameter name err -> [i|invalid plugin callback parameter #{name}: #{err}|]
    PluginFrameEncodeFailed err -> err
    PluginTransportWriteTimedOut -> "plugin transport write timed out"

data ExitWatcherState = WatcherPending | WatcherOwnsFailure | WatcherCancelled
  deriving stock (Eq)

runPluginManager
  :: ( Concurrent :> es
     , FileSystem :> es
     , TypedProcess.TypedProcess :> es
     , Process.Process :> es
     , Timeout :> es
     , Prim :> es
     , KatipE :> es
     , Fail :> es
     , IOE :> es
     )
  => FilePath
  -> FilePath
  -> HostCallbacks es
  -> Eff (Plugin.Plugin : es) a
  -> Eff es a
runPluginManager pluginDirectory mediaDirectory callbacks action = do
  absolutePluginDirectory <- FileSystem.makeAbsolute pluginDirectory
  absoluteMediaDirectory <- FileSystem.makeAbsolute mediaDirectory
  active <- newIORef Map.empty
  launched <- newIORef []
  nextGeneration <- newIORef 1
  requiredFailure <- MVar.newEmptyMVar
  recentExits <- newIORef Map.empty
  lifecycleLock <- MVar.newMVar ()
  operationLock <- MVar.newMVar ()
  shuttingDown <- newIORef False
  let manager = Manager
        { pluginDirectory = absolutePluginDirectory
        , mediaDirectory = absoluteMediaDirectory
        , callbacks
        , active
        , launched
        , nextGeneration
        , requiredFailure
        , recentExits
        , lifecycleLock
        , operationLock
        , shuttingDown
        }
  initializeInstalled manager
  let runAction = interpret (runOperation manager) action
      waitForRequiredFailure = MVar.takeMVar requiredFailure
  outcome <- Async.race runAction waitForRequiredFailure `finally` shutdownAll manager
  case outcome of
    Left result -> pure result
    Right failure -> throwIO (RequiredPluginFailed failure)

runOperation
  :: ( Concurrent :> es
     , FileSystem :> es
     , TypedProcess.TypedProcess :> es
     , Process.Process :> es
     , Timeout :> es
     , Prim :> es
     , KatipE :> es
     , IOE :> es
     )
  => Manager es
  -> EffectHandler Plugin.Plugin es
runOperation manager _ = \case
  Plugin.Statuses -> map statusOf . Map.elems <$> readIORef manager.active
  Plugin.Load pluginId -> withLifecycleOperation manager (loadById manager pluginId)
  Plugin.Unload pluginId -> withLifecycleOperation manager (unloadById manager pluginId)
  Plugin.Reload pluginId -> withLifecycleOperation manager (reloadById manager pluginId)
  Plugin.DispatchRoute message -> dispatchRoutes manager message
  Plugin.HelpEntries message -> dynamicHelp manager message
  Plugin.ToolSnapshot -> snapshotTools manager
  Plugin.InvokeTool tool message arguments -> invokeSnapshottedTool manager tool message arguments

initializeInstalled
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, KatipE :> es, Fail :> es, IOE :> es)
  => Manager es
  -> Eff es ()
initializeInstalled manager =
  Config.discoverPluginBundles manager.pluginDirectory >>= \case
    Left err -> fail ("Failed to discover plugins: " <> toString (configErrorText err))
    Right bundles -> traverse_ loadBundleAtStartup bundles
  where
    loadBundleAtStartup bundle =
      startAndPublish manager bundle >>= \case
        Right _ -> pure ()
        Left failure
          | bundle.lifecycle.required -> do
              let pluginId = pluginIdText bundle
              fail [i|Required plugin #{pluginId} failed to start: #{failure}|]
          | otherwise -> do
              let pluginId = pluginIdText bundle
              $(logError) [i|Optional plugin #{pluginId} failed to start: #{failure}|]

loadById
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, KatipE :> es, IOE :> es)
  => Manager es
  -> PluginId
  -> Eff es (Either Text PluginStatus)
loadById manager pluginId = do
  alreadyLoaded <- Map.member pluginId <$> readIORef manager.active
  if alreadyLoaded
    then pure (Left "plugin is already loaded")
    else Config.loadPluginBundle (manager.pluginDirectory </> toString pluginId.unPluginId) pluginId >>= \case
      Left err -> pure (Left (configErrorText err))
      Right bundle -> startAndPublish manager bundle >>= \case
        Right running -> pure (Right (statusOf running))
        Left failure -> do
          when bundle.lifecycle.required $
            void (MVar.tryPutMVar manager.requiredFailure [i|Required plugin load failed: #{failure}|])
          pure (Left failure)

unloadById
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, KatipE :> es, IOE :> es)
  => Manager es
  -> PluginId
  -> Eff es (Either Text ())
unloadById manager pluginId = do
  Map.lookup pluginId <$> readIORef manager.active >>= \case
    Nothing -> pure (Left "plugin is not loaded")
    Just running
      | running.bundle.lifecycle.required -> pure (Left "required plugins cannot be unloaded")
      | otherwise -> do
          unpublish manager running
          stopRunning running
          forgetLaunched manager running.transport
          pure (Right ())

reloadById
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, KatipE :> es, IOE :> es)
  => Manager es
  -> PluginId
  -> Eff es (Either Text PluginStatus)
reloadById manager pluginId = do
  Map.lookup pluginId <$> readIORef manager.active >>= \case
    Nothing -> pure (Left "plugin is not loaded")
    Just previous -> do
      unpublish manager previous
      stopRunning previous
      forgetLaunched manager previous.transport
      Config.loadPluginBundle (manager.pluginDirectory </> toString pluginId.unPluginId) pluginId >>= \case
        Left err -> reloadFailed previous.bundle.lifecycle.required (configErrorText err)
        Right bundle -> startAndPublish manager bundle >>= \case
          Right running -> pure (Right (statusOf running))
          Left failure -> reloadFailed
            (previous.bundle.lifecycle.required || bundle.lifecycle.required)
            failure
  where
    reloadFailed required failure = do
      when required $
        void (MVar.tryPutMVar manager.requiredFailure [i|Required plugin reload failed: #{failure}|])
      pure (Left failure)

startAndPublish
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, KatipE :> es, IOE :> es)
  => Manager es
  -> PluginBundle
  -> Eff es (Either Text (RunningPlugin es))
startAndPublish manager bundle = do
  attempt 0
  where
    attempt retryNumber = do
      shuttingDown <- readIORef manager.shuttingDown
      if shuttingDown
        then pure (Left "plugin manager is shutting down")
        else do
          generation <- atomicModifyIORef' manager.nextGeneration (\value -> (value + 1, value))
          startBundle manager bundle generation >>= \case
            Right running -> do
              published <- MVar.withMVar manager.lifecycleLock \_ -> do
                stopped <- readIORef running.transport.stopping
                shuttingDownNow <- readIORef manager.shuttingDown
                alreadyActive <- Map.member bundle.pluginId <$> readIORef manager.active
                unless (stopped || shuttingDownNow || alreadyActive) do
                  atomicModifyIORef' manager.active (\plugins -> (Map.insert bundle.pluginId running plugins, ()))
                  atomicModifyIORef' manager.launched (\plugins -> (running : plugins, ()))
                pure (not stopped && not shuttingDownNow && not alreadyActive)
              if published
                then do
                  let pluginId = pluginIdText bundle
                      pluginVersion = running.manifest.pluginVersion
                  $(logInfo) [i|Loaded plugin #{pluginId} generation=#{generation} version=#{pluginVersion}|]
                  pure (Right running)
                else do
                  stopRunning running
                  pure (Left "plugin could not publish because it exited, shutdown began, or the id became active")
            Left failure -> do
              exitCount <- recordRecentExit manager bundle.pluginId
              let crashLoop = exitCount >= 3
              if failure.transient && retryNumber < bundle.lifecycle.restartLimit && not crashLoop
                then do
                  let delaySeconds = min 4 (2 ^ retryNumber)
                      pluginId = pluginIdText bundle
                      failureMessage = failure.message
                  $(logWarning) [i|Transient plugin startup failure: plugin=#{pluginId} retry_in=#{delaySeconds}s failure=#{failureMessage}|]
                  threadDelay (seconds delaySeconds)
                  attempt (retryNumber + 1)
                else pure . Left $
                  failure.message <> if crashLoop then " (crash-loop breaker opened)" else ""

startBundle
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, KatipE :> es, IOE :> es)
  => Manager es
  -> PluginBundle
  -> Int
  -> Eff es (Either StartFailure (RunningPlugin es))
startBundle manager bundle generation = do
  result <- trySync do
    transport <- launchTransport manager bundle generation
    readerThread <- Async.async $ mask \restore -> do
      watcherState <- MVar.newMVar WatcherPending
      exitWatcher <- Async.async do
        exitCode <- TypedProcess.waitExitCode transport.process
        waiters <- Map.elems <$> MVar.readMVar transport.pending
        void (timeout processExitDrainMicroseconds (traverse_ MVar.readMVar waiters))
        ownsFailure <- MVar.modifyMVar watcherState \case
          WatcherPending -> pure (WatcherOwnsFailure, True)
          current -> pure (current, False)
        when ownsFailure $
          transportFailed manager transport (toException (PluginProcessExit exitCode))
      restore (readerLoop manager transport `catchSync` classifyReaderFailure manager transport)
        `finally` finishExitWatcher watcherState exitWatcher
    -- Cleanup must not replace the startup error: its transient bit controls retries.
    let stop = void (trySync (stopLaunched transport readerThread))
        finalizeThenStop =
          void (trySync (timeout (seconds 5) (sendRequest transport 5 Protocol.PluginShutdown Aeson.Null)))
            `finally` stop
        initializeParams = Aeson.object
          [ "protocolVersion" Aeson..= Protocol.protocolVersion
          , "pluginId" Aeson..= pluginIdText bundle
          ]
    initializeResult <- sendRequest transport 10 Protocol.PluginInitialize initializeParams
      `onException` stop
    manifestValue <- case initializeResult of
      Left failure -> stop >> throwIO (PluginStartException (startFailureFromRpc failure))
      Right value -> pure value
    (do
      manifest <- either (throwIO . InvalidPluginInitialization . show) pure (Protocol.parseInitializationResult manifestValue)
      either (throwIO . InvalidPluginToolNamespace) pure (validateToolNamespace bundle.pluginId manifest)
      writeIORef transport.capabilities manifest.requestedCapabilities
      pure RunningPlugin{bundle, generation, manifest, transport, readerThread}
      ) `onException` finalizeThenStop
  pure case result of
    Right running -> Right running
    Left err -> case fromException err of
      Just (PluginStartException failure) -> Left failure
      Nothing -> Left StartFailure
        { message = Text.pack (displayException err)
        , transient = False
        }

launchTransport
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Prim :> es, IOE :> es)
  => Manager es
  -> PluginBundle
  -> Int
  -> Eff es (Transport es)
launchTransport manager bundle generation = do
  environment <- liftIO Environment.getEnvironment
  let (executable, arguments, workingDirectory, childEnvironment) =
        if bundle.lifecycle.sandboxed
          then
            ( Sandbox.bubblewrapExecutable
            , Sandbox.bubblewrapArguments bundle.bundleDir manager.mediaDirectory bundle.pluginId.unPluginId
            , bundle.bundleDir
            , []
            )
          else
            ( bundle.executablePath
            , []
            , bundle.bundleDir
            , setEnvironmentVariable "COSMOBOT_PLUGIN_CONFIG" bundle.configPath environment
            )
      processConfig =
          TypedProcess.setCreateGroup True
        . TypedProcess.setWorkingDir workingDirectory
        . TypedProcess.setEnv childEnvironment
        . TypedProcess.setStdin TypedProcess.createPipe
        . TypedProcess.setStdout TypedProcess.createPipe
        . TypedProcess.setStderr TypedProcess.inherit
        $ TypedProcess.proc executable arguments
  process <- TypedProcess.startProcess processConfig
  processGroupId <- Process.getPid (TypedProcess.unsafeProcessHandle process)
  writeLock <- MVar.newMVar ()
  pending <- MVar.newMVar Map.empty
  invocationContexts <- newIORef Map.empty
  callbackThreads <- MVar.newMVar (Just Map.empty)
  capabilities <- newIORef Set.empty
  nextRequestId <- newIORef 1
  nextInvocationId <- newIORef 1
  nextCallbackToken <- newIORef 1
  readBuffer <- newIORef ByteString.empty
  stopping <- newIORef False
  pure Transport
    { bundle
    , generation
    , process
    , processGroupId
    , input = TypedProcess.getStdin process
    , output = TypedProcess.getStdout process
    , writeLock
    , pending
    , invocationContexts
    , callbackThreads
    , capabilities
    , nextRequestId
    , nextInvocationId
    , nextCallbackToken
    , readBuffer
    , stopping
    }

setEnvironmentVariable :: String -> String -> [(String, String)] -> [(String, String)]
setEnvironmentVariable name value environment =
  (name, value) : filter ((/= name) . fst) environment

readerLoop
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, KatipE :> es, IOE :> es)
  => Manager es
  -> Transport es
  -> Eff es ()
readerLoop manager transport = forever do
  frame <- readFrame transport
  value <- either (throwIO . PluginFrameDecodeFailed . show) pure (Protocol.decodeFrame @Aeson.Value frame)
  case AesonTypes.parseEither parseIncoming value of
    Left err -> throwIO (InvalidPluginMessage (Text.pack err))
    Right (IncomingResponse response) -> deliverResponse transport response
    Right (IncomingRequest request) -> dispatchCallback manager transport request

classifyReaderFailure
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, KatipE :> es, IOE :> es)
  => Manager es
  -> Transport es
  -> SomeException
  -> Eff es ()
classifyReaderFailure manager transport readerError = do
  processExit <- timeout processExitDrainMicroseconds (TypedProcess.waitExitCode transport.process)
  transportFailed manager transport $
    maybe readerError (toException . PluginProcessExit) processExit

finishExitWatcher :: Concurrent :> es => MVar.MVar ExitWatcherState -> Async.Async () -> Eff es ()
finishExitWatcher watcherState watcher = do
  ownsFailure <- MVar.modifyMVar watcherState \case
    WatcherPending -> pure (WatcherCancelled, False)
    current -> pure (current, current == WatcherOwnsFailure)
  unless ownsFailure (Async.cancel watcher)
  void (Async.waitCatch watcher)

data Incoming = IncomingResponse !Protocol.RpcResponse | IncomingRequest !Protocol.RpcRequest

parseIncoming :: Aeson.Value -> AesonTypes.Parser Incoming
parseIncoming value@(Aeson.Object object)
  | KeyMap.member "method" object = IncomingRequest <$> Aeson.parseJSON value
  | otherwise = IncomingResponse <$> Aeson.parseJSON value
parseIncoming _ = fail "JSON-RPC message must be an object"

readFrame :: (FileSystem :> es, Prim :> es, IOE :> es) => Transport es -> Eff es ByteString
readFrame transport = do
  buffered <- readIORef transport.readBuffer
  case ByteString.elemIndex 10 buffered of
    Just index
      | index + 1 > Protocol.maxFrameBytes ->
          throwIO PluginProtocolLineTooLong
      | otherwise -> do
          let lineLength = index + 1
          writeIORef transport.readBuffer (ByteString.drop lineLength buffered)
          pure (ByteString.take lineLength buffered)
    Nothing
      | ByteString.length buffered > Protocol.maxFrameBytes ->
          throwIO PluginProtocolLineTooLong
      | otherwise -> do
          chunk <- FileSystemByteString.hGetSome transport.output 4096
          if ByteString.null chunk
            then throwIO PluginProtocolStreamClosed
            else writeIORef transport.readBuffer (buffered <> chunk) >> readFrame transport

deliverResponse
  :: (Concurrent :> es)
  => Transport es
  -> Protocol.RpcResponse
  -> Eff es ()
deliverResponse transport response = do
  let (responseId, result) = case response of
        Protocol.RpcSuccess rpcId value -> (rpcId, Right value)
        Protocol.RpcFailure (Just rpcId) err -> (rpcId, Left err)
        Protocol.RpcFailure Nothing _ -> (Protocol.RpcId Aeson.Null, Left (rpcError (-32600) "response omitted id"))
      key = rpcIdKey responseId
  waiters <- MVar.readMVar transport.pending
  traverse_ (\waiter -> void (MVar.tryPutMVar waiter result)) (Map.lookup key waiters)

dispatchCallback
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, KatipE :> es, IOE :> es)
  => Manager es
  -> Transport es
  -> Protocol.RpcRequest
  -> Eff es ()
dispatchCallback manager transport request =
  for_ request.requestId \requestId -> mask \restore -> do
    let key = rpcIdKey requestId
        invocationId = either (const "") id (parseInvocationId request.params)
    token <- atomicModifyIORef' transport.nextCallbackToken (\value -> (value + 1, value))
    start <- MVar.newEmptyMVar
    thread <- Async.async $
      (MVar.takeMVar start >> restore (handleCallback manager transport request >>= writeResponse transport requestId))
        `catchSync` transportFailed manager transport
        `finally` MVar.modifyMVar_ transport.callbackThreads (pure . fmap (Map.update (deleteOwn token) key))
    (do
        accepted <- MVar.modifyMVar transport.callbackThreads \case
          Nothing -> pure (Nothing, False)
          Just threads
            | Map.member key threads -> pure (Just threads, False)
            | otherwise -> pure (Just (Map.insert key (invocationId, token, thread) threads), True)
        if accepted
          then MVar.putMVar start ()
          else Async.cancel thread >> void (Async.waitCatch thread))
      `onException` (Async.cancel thread >> void (Async.waitCatch thread))
  where
    deleteOwn token entry@(_, registeredToken, _)
      | token == registeredToken = Nothing
      | otherwise = Just entry

handleCallback
  :: (FileSystem :> es, Prim :> es, KatipE :> es, IOE :> es)
  => Manager es
  -> Transport es
  -> Protocol.RpcRequest
  -> Eff es (Either Protocol.RpcError Aeson.Value)
handleCallback manager transport request =
  case callbackCapability request.method of
    Nothing -> pure (Left (rpcError (-32601) "method not found"))
    Just capability -> do
      declared <- Set.member capability <$> readIORef transport.capabilities
      if not declared
        then pure (Left (rpcError (-32003) "plugin did not declare this capability"))
        else case parseInvocationId request.params of
          Left err -> pure (Left (rpcError (-32602) err))
          Right invocationId -> do
            context <- Map.lookup invocationId <$> readIORef transport.invocationContexts
            case context of
              Nothing -> pure (Left (rpcError (-32004) "invocation context is no longer active"))
              Just message ->
                first (rpcError (-32000) . Text.pack . displayException) <$> trySync
                  (runHostCallback manager transport request.method message request.params)

runHostCallback
  :: (FileSystem :> es, IOE :> es)
  => Manager es
  -> Transport es
  -> Text
  -> IncomingMessage
  -> Aeson.Value
  -> Eff es Aeson.Value
runHostCallback manager transport method message params = case method of
  "chat.reply" -> manager.callbacks.chatReply message =<< parseTextParam "text" params
  "chat.referenced" -> manager.callbacks.chatReferenced message
  "llm.complete" -> Aeson.String <$> (manager.callbacks.llmComplete =<< parseTextParam "prompt" params)
  "agent.run" -> Aeson.String <$> (manager.callbacks.agentRun message =<< parseTextParam "prompt" params)
  "media.resolve" -> do
    result <- manager.callbacks.mediaResolve =<< parseTextParam "ref" params
    pure (translateMediaResult manager transport result)
  _ -> throwIO (PluginCallbackMethodNotFound method)

callbackCapability :: Text -> Maybe Capability
callbackCapability = \case
  "chat.reply" -> Just Chat
  "chat.referenced" -> Just Chat
  "llm.complete" -> Just LLM
  "agent.run" -> Just Agent
  "media.resolve" -> Just Media
  _ -> Nothing

parseInvocationId :: Aeson.Value -> Either Text Text
parseInvocationId = first toText . AesonTypes.parseEither
  (Aeson.withObject "callback parameters" (Aeson..: Key.fromText "invocationId"))

parseTextParam :: IOE :> es => Text -> Aeson.Value -> Eff es Text
parseTextParam name value =
  either (throwIO . InvalidPluginCallbackParameter name . Text.pack) pure $
    AesonTypes.parseEither (Aeson.withObject "callback parameters" (Aeson..: Key.fromText name)) value

translateMediaResult :: Manager es -> Transport es -> Aeson.Value -> Aeson.Value
translateMediaResult manager transport value@(Aeson.Object object)
  | not transport.bundle.lifecycle.sandboxed = value
  | Just (Aeson.String hostPath) <- KeyMap.lookup "localPath" object
  , Just sandboxPath <- Sandbox.translateSandboxMediaPath manager.mediaDirectory (toString hostPath) =
      Aeson.Object (KeyMap.insert "localPath" (Aeson.String (Text.pack sandboxPath)) object)
  | otherwise = value
translateMediaResult _ _ value = value

writeResponse
  :: (Concurrent :> es, FileSystem :> es, IOE :> es)
  => Transport es
  -> Protocol.RpcId
  -> Either Protocol.RpcError Aeson.Value
  -> Eff es ()
writeResponse transport requestId result = do
  let response = case result of
        Right value -> Protocol.RpcSuccess requestId value
        Left err -> Protocol.RpcFailure (Just requestId) err
  case Protocol.encodeFrame response of
    Right frame -> writeEncodedFrame transport frame
    Left _ -> writeFrame transport $
      Protocol.RpcFailure (Just requestId) (rpcError (-32603) "host callback result exceeds 1 MiB")

sendRequest
  :: (Concurrent :> es, FileSystem :> es, Timeout :> es, Prim :> es, IOE :> es)
  => Transport es
  -> Int
  -> Protocol.PluginMethod
  -> Aeson.Value
  -> Eff es (Either Protocol.RpcError Aeson.Value)
sendRequest transport timeoutSeconds method params = mask \restore -> do
  requestNumber <- atomicModifyIORef' transport.nextRequestId (\value -> (value + 1, value))
  let generation = transport.generation
      requestId = Protocol.RpcId (Aeson.String [i|host:#{generation}:#{requestNumber}|])
      key = rpcIdKey requestId
      request = Protocol.RpcRequest (Just requestId) (Protocol.pluginMethodName method) params
  waiter <- MVar.newEmptyMVar
  MVar.modifyMVar_ transport.pending (pure . Map.insert key waiter)
  let cleanup = MVar.modifyMVar_ transport.pending (pure . Map.delete key)
  restore (case Protocol.encodeFrame request of
      Left _ -> pure (Just (Left (rpcError (-32005) "plugin request exceeds 1 MiB")))
      Right frame -> do
        writeResult <- trySync (timeout (seconds transportWriteTimeoutSeconds) (writeEncodedFrame transport frame))
        case writeResult of
          Left err -> pure (Just (Left (rpcError (-32002) ("plugin transport write failed: " <> show err))))
          Right Nothing -> pure (Just (Left (rpcError (-32002) "plugin transport write timed out")))
          Right (Just ()) -> timeout (seconds timeoutSeconds) (MVar.takeMVar waiter))
    `finally` cleanup
    <&> fromMaybe (Left (rpcError (-32001) "plugin invocation timed out"))

writeFrame
  :: (Concurrent :> es, FileSystem :> es, IOE :> es, Aeson.ToJSON value)
  => Transport es
  -> value
  -> Eff es ()
writeFrame transport value = do
  frame <- either (throwIO . PluginFrameEncodeFailed . show) pure (Protocol.encodeFrame value)
  writeEncodedFrame transport frame

writeEncodedFrame
  :: (Concurrent :> es, FileSystem :> es, IOE :> es)
  => Transport es
  -> ByteString
  -> Eff es ()
writeEncodedFrame transport frame =
  MVar.withMVar transport.writeLock \_ -> do
    FileSystemByteString.hPut transport.input frame
    FileSystemIO.hFlush transport.input

dispatchRoutes
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, KatipE :> es, IOE :> es)
  => Manager es
  -> IncomingMessage
  -> Eff es (Maybe RouteDisposition)
dispatchRoutes manager message = do
  plugins <- Map.elems <$> readIORef manager.active
  go
    [ (running, route)
    | running <- plugins
    , route <- running.manifest.routes
    , matchesRouteDeclaration running.manifest.filters route message
    ]
  where
    go [] = pure Nothing
    go ((running, route) : rest) = do
      result <- invokeRoute manager running route message
      case result of
        Right _ -> pure (Just route.disposition)
        Left failure -> do
          let pluginId = pluginIdText running.bundle
              routeId = route.routeId
              renderedFailure = renderRpcError failure
          $(logWarning) [i|Plugin route failed: plugin=#{pluginId} route=#{routeId} failure=#{renderedFailure}|]
          go rest

invokeRoute
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, KatipE :> es, IOE :> es)
  => Manager es
  -> RunningPlugin es
  -> RouteDeclaration
  -> IncomingMessage
  -> Eff es (Either Protocol.RpcError Aeson.Value)
invokeRoute manager running route message = withInvocation running message \invocationId -> do
  result <- sendRequest running.transport (running.bundle.lifecycle.routeTimeoutSeconds + 1) Protocol.PluginRouteInvoke $
    Aeson.toJSON Protocol.RouteInvokeParams
      { invocationId
      , routeId = route.routeId
      , message
      , arguments = routeArguments running.manifest route message
      , timeoutSeconds = running.bundle.lifecycle.routeTimeoutSeconds
      }
  when (isTransportFailure result) $
    transportFailed manager running.transport (toException PluginTransportWriteTimedOut)
  pure result

routeArguments :: PluginManifest -> RouteDeclaration -> IncomingMessage -> Text
routeArguments manifest route message =
  fromMaybe (Text.strip message.text) do
    routeFilter <- Map.lookup route.filter manifest.filters
    commandName <- commandInFilter routeFilter
    Text.strip <$> Text.stripPrefix commandName message.text

commandInFilter :: RouteFilter -> Maybe Text
commandInFilter = \case
  FilterAll filters -> asum (map commandInFilter filters)
  FilterAny filters -> asum (map commandInFilter filters)
  FilterNot _ -> Nothing
  FilterPredicate (CommandIs commandName) -> Just commandName
  FilterPredicate _ -> Nothing

dynamicHelp :: Prim :> es => Manager es -> IncomingMessage -> Eff es [RouteHelp]
dynamicHelp manager message = do
  plugins <- Map.elems <$> readIORef manager.active
  pure
    [ RouteHelp route.helpLabel route.helpDescription
    | running <- plugins
    , route <- running.manifest.routes
    , matchesRouteAccess route.access message
    ]

snapshotTools :: Prim :> es => Manager es -> Eff es [Plugin.PluginTool]
snapshotTools manager = do
  plugins <- Map.elems <$> readIORef manager.active
  pure
    [ Plugin.PluginTool
        { pluginId = running.bundle.pluginId.unPluginId
        , generation = running.generation
        , name = tool.name
        , modelName = running.bundle.pluginId.unPluginId <> "__" <> tool.name
        , description = tool.description
        , schema = tool.schema
        }
    | running <- plugins
    , tool <- running.manifest.tools
    ]

invokeSnapshottedTool
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, KatipE :> es, IOE :> es)
  => Manager es
  -> Plugin.PluginTool
  -> IncomingMessage
  -> Aeson.Value
  -> Eff es ToolInvocationResult
invokeSnapshottedTool manager snapshot message arguments = do
  let pluginId = PluginId snapshot.pluginId
  Map.lookup pluginId <$> readIORef manager.active >>= \case
    Just running | running.generation == snapshot.generation ->
      withInvocation running message (\invocationId -> do
        result <- sendRequest running.transport (running.bundle.lifecycle.toolTimeoutSeconds + 1) Protocol.PluginToolInvoke $
          Aeson.toJSON Protocol.ToolInvokeParams
            { invocationId
            , tool = snapshot.name
            , message
            , arguments
            , timeoutSeconds = running.bundle.lifecycle.toolTimeoutSeconds
            }
        when (isTransportFailure result) $
          transportFailed manager running.transport (toException PluginTransportWriteTimedOut)
        pure result)
        <&> either rpcToolFailure parseToolResult
    _ -> pure $ ToolInvocationFailure TransientInvocation
      "Plugin generation is no longer loaded."
      ("Plugin " <> snapshot.pluginId <> " generation " <> show snapshot.generation <> " was unloaded or replaced.")

parseToolResult :: Aeson.Value -> ToolInvocationResult
parseToolResult value =
  case AesonTypes.parseEither parser value of
    Left err -> ToolInvocationFailure TransientInvocation
      "Plugin returned an invalid tool result."
      (toText err)
    Right (content, imageUrls) -> ToolInvocationSuccess content imageUrls
  where
    parser = Aeson.withObject "plugin tool result" \object -> do
      status <- object Aeson..:? "status" Aeson..!= ("success" :: Text)
      unless (status == "success") (fail "status must be success")
      (,) <$> object Aeson..:? "content" Aeson..!= ""
          <*> object Aeson..:? "imageUrls" Aeson..!= []

rpcToolFailure :: Protocol.RpcError -> ToolInvocationResult
rpcToolFailure failure =
  if failure.code == -32602
    then ToolInvocationFailure PermanentArguments failure.message (renderRpcError failure)
    else ToolInvocationFailure TransientInvocation failure.message (renderRpcError failure)

withInvocation
  :: (Concurrent :> es, Prim :> es)
  => RunningPlugin es
  -> IncomingMessage
  -> (Text -> Eff es a)
  -> Eff es a
withInvocation running message action = mask \restore -> do
  invocationNumber <- atomicModifyIORef' running.transport.nextInvocationId (\value -> (value + 1, value))
  let pluginId = pluginIdText running.bundle
      generation = running.generation
      invocationId = [i|#{pluginId}:#{generation}:#{invocationNumber}|]
  atomicModifyIORef' running.transport.invocationContexts (\contexts -> (Map.insert invocationId message contexts, ()))
  restore (action invocationId) `finally` closeInvocation running.transport invocationId

closeInvocation :: (Concurrent :> es, Prim :> es) => Transport es -> Text -> Eff es ()
closeInvocation transport invocationId = mask_ do
  atomicModifyIORef' transport.invocationContexts (\contexts -> (Map.delete invocationId contexts, ()))
  callbacks <- MVar.modifyMVar transport.callbackThreads \case
    Nothing -> pure (Nothing, [])
    Just threads ->
      let (owned, remaining) = Map.partition (\(owner, _, _) -> owner == invocationId) threads
      in pure (Just remaining, map (\(_, _, thread) -> thread) (Map.elems owned))
  traverse_ Async.cancel callbacks
  traverse_ (void . Async.waitCatch) callbacks

unpublish :: Prim :> es => Manager es -> RunningPlugin es -> Eff es ()
unpublish manager running =
  atomicModifyIORef' manager.active \plugins ->
    let current = Map.lookup running.bundle.pluginId plugins
        updated = case current of
          Just active | active.generation == running.generation -> Map.delete running.bundle.pluginId plugins
          _ -> plugins
    in (updated, ())

transportFailed
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, KatipE :> es, IOE :> es)
  => Manager es
  -> Transport es
  -> SomeException
  -> Eff es ()
transportFailed manager transport err = do
  firstFailure <- atomicModifyIORef' transport.stopping (\stopping -> (True, not stopping))
  when firstFailure do
    let pluginId = pluginIdText transport.bundle
        generation = transport.generation
        exceptionText = Text.pack (displayException err)
        failure = [i|Plugin #{pluginId} generation #{generation} failed permanently: #{exceptionText}|]
    $(logError) failure
    failPending transport (rpcError (-32002) failure)
    wasActive <- MVar.withMVar manager.lifecycleLock \_ ->
      Map.lookup transport.bundle.pluginId <$> readIORef manager.active >>= \case
        Just running | running.generation == transport.generation -> do
          unpublish manager running
          pure True
        _ -> pure False
    cleanupTransport transport
    forgetLaunched manager transport
    when wasActive $
      case fromException err of
        Just (PluginProcessExit (ExitFailure 75)) -> restartTransientProcess manager transport.bundle failure
        _ -> requiredRuntimeFailure manager transport.bundle failure

isTransportFailure :: Either Protocol.RpcError value -> Bool
isTransportFailure = either ((== -32002) . (.code)) (const False)

restartTransientProcess
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, KatipE :> es, IOE :> es)
  => Manager es
  -> PluginBundle
  -> Text
  -> Eff es ()
restartTransientProcess manager bundle failure = do
  exitCount <- recordRecentExit manager bundle.pluginId
  if exitCount >= 3 || exitCount > bundle.lifecycle.restartLimit
    then requiredRuntimeFailure manager bundle (failure <> " (transient retries exhausted or crash-loop breaker opened)")
    else withLifecycleOperation manager do
      threadDelay (seconds (min 4 (2 ^ (exitCount - 1))))
      startAndPublish manager bundle >>= \case
        Right _ -> pure ()
        Left restartFailure -> do
          recoveredElsewhere <- Map.member bundle.pluginId <$> readIORef manager.active
          unless recoveredElsewhere (requiredRuntimeFailure manager bundle restartFailure)

withLifecycleOperation :: Concurrent :> es => Manager es -> Eff es a -> Eff es a
withLifecycleOperation manager action =
  MVar.withMVar manager.operationLock \_ -> action

requiredRuntimeFailure :: Concurrent :> es => Manager es -> PluginBundle -> Text -> Eff es ()
requiredRuntimeFailure manager bundle failure =
  when bundle.lifecycle.required $
    void (MVar.tryPutMVar manager.requiredFailure [i|Required plugin failed: #{failure}|])

failPending :: Concurrent :> es => Transport es -> Protocol.RpcError -> Eff es ()
failPending transport failure = do
  waiters <- Map.elems <$> MVar.readMVar transport.pending
  traverse_ (\waiter -> void (MVar.tryPutMVar waiter (Left failure))) waiters

stopRunning
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, IOE :> es)
  => RunningPlugin es
  -> Eff es ()
stopRunning running = do
  firstStop <- atomicModifyIORef' running.transport.stopping (\stopping -> (True, not stopping))
  when firstStop do
    void (trySync (timeout (seconds 5) (sendRequest running.transport 5 Protocol.PluginShutdown Aeson.Null)))
      `finally` stopLaunched running.transport running.readerThread

stopLaunched
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, IOE :> es)
  => Transport es
  -> Async.Async ()
  -> Eff es ()
stopLaunched transport readerThread = do
  writeIORef transport.stopping True
  Async.cancel readerThread
  void (Async.waitCatch readerThread)
  cleanupTransport transport

cleanupTransport
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, IOE :> es)
  => Transport es
  -> Eff es ()
cleanupTransport transport = do
  failPending transport (rpcError (-32002) "plugin stopped")
  callbacks <- MVar.modifyMVar transport.callbackThreads \case
    Nothing -> pure (Nothing, [])
    Just threads -> pure (Nothing, map (\(_, _, thread) -> thread) (Map.elems threads))
  traverse_ Async.cancel callbacks
  traverse_ (void . Async.waitCatch) callbacks
  traverse_ ProcessUtil.killProcessGroupByPid transport.processGroupId
  void (timeout (seconds 5) (TypedProcess.waitExitCode transport.process))
  traverse_ closeQuietly [transport.input, transport.output]

closeQuietly :: (FileSystem :> es, IOE :> es) => Handle -> Eff es ()
closeQuietly fileHandle = FileSystemIO.hClose fileHandle `catchSync` \_ -> pure ()

shutdownAll
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Process.Process :> es, Timeout :> es, Prim :> es, IOE :> es)
  => Manager es
  -> Eff es ()
shutdownAll manager = do
  running <- MVar.withMVar manager.lifecycleLock \_ -> do
    writeIORef manager.shuttingDown True
    writeIORef manager.active Map.empty
    atomicModifyIORef' manager.launched (\plugins -> ([], plugins))
  traverse_ stopRunning running

forgetLaunched :: Prim :> es => Manager es -> Transport es -> Eff es ()
forgetLaunched manager transport =
  atomicModifyIORef' manager.launched \plugins ->
    ( filter (\running -> running.generation /= transport.generation || running.bundle.pluginId /= transport.bundle.pluginId) plugins
    , ()
    )

statusOf :: RunningPlugin es -> PluginStatus
statusOf running = PluginStatus
  { pluginId = running.bundle.pluginId
  , generation = running.generation
  , pluginVersion = running.manifest.pluginVersion
  , required = running.bundle.lifecycle.required
  , sandboxed = running.bundle.lifecycle.sandboxed
  , routeCount = length running.manifest.routes
  , toolCount = length running.manifest.tools
  }

rpcIdKey :: Protocol.RpcId -> Text
rpcIdKey (Protocol.RpcId value) =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict $ Aeson.encode value

rpcError :: Int -> Text -> Protocol.RpcError
rpcError code message = Protocol.RpcError{code, message, details = Nothing}

renderRpcError :: Protocol.RpcError -> Text
renderRpcError Protocol.RpcError{code, message, details} =
  "JSON-RPC " <> show code <> ": " <> message <> maybe "" (\value -> " " <> show value) details

startFailureFromRpc :: Protocol.RpcError -> StartFailure
startFailureFromRpc failure = StartFailure
  { message = renderRpcError failure
  , transient = case failure.details of
      Just (Aeson.Object details) -> KeyMap.lookup "transient" details == Just (Aeson.Bool True)
      _ -> False
  }

recordRecentExit :: (Prim :> es, IOE :> es) => Manager es -> PluginId -> Eff es Int
recordRecentExit manager pluginId = do
  now <- liftIO getCurrentTime
  atomicModifyIORef' manager.recentExits \history ->
    let recent = now : filter (\previous -> diffUTCTime now previous <= 60) (Map.findWithDefault [] pluginId history)
    in (Map.insert pluginId recent history, length recent)

pluginIdText :: PluginBundle -> Text
pluginIdText bundle = case bundle.pluginId of
  PluginId value -> value

configErrorText :: Config.PluginConfigError -> Text
configErrorText Config.PluginConfigError{path, message} =
  Text.pack path <> ": " <> message

seconds :: Int -> Int
seconds value = value * 1_000_000

transportWriteTimeoutSeconds :: Int
transportWriteTimeoutSeconds = 5

processExitDrainMicroseconds :: Int
processExitDrainMicroseconds = 100_000
