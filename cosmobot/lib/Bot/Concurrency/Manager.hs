{-|
Module      : Bot.Concurrency.Manager
Description : Queryable ownership model for concurrent work and resources
Stability   : experimental
-}

module Bot.Concurrency.Manager
  ( runConcurrencyManager
  )
where

import qualified Bot.Effect.Concurrency as Concurrency
import Bot.Effect.Concurrency
import Bot.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.Ki as Ki

data ManagerState es = ManagerState
  { nextResourceId :: !(IORef ResourceId)
  , runtimes :: !(IORef (Map ResourceId (ResourceRuntime es)))
  , rootScope :: !Ki.Scope
  }

data ResourceRuntime es = ResourceRuntime
  { info :: !ResourceInfo
  , thread :: !(Maybe (Ki.Thread ()))
  , threadId :: !(Maybe ThreadId)
  , cleanup :: !(Maybe (Eff es ()))
  }

runConcurrencyManager
  :: (IOE :> es, Prim :> es, Concurrent :> es, Ki.StructuredConcurrency :> es)
  => Eff (Concurrency : es) a
  -> Eff es a
runConcurrencyManager inner =
  Ki.scoped \rootScope -> do
    nextResourceId <- newIORef (ResourceId 1)
    runtimes <- newIORef Map.empty
    let managerState = ManagerState{nextResourceId, runtimes, rootScope}
    interpret (runConcurrencyOperation managerState) inner

runConcurrencyOperation
  :: (IOE :> es, Prim :> es, Concurrent :> es, Ki.StructuredConcurrency :> es)
  => ManagerState es
  -> EffectHandler Concurrency es
runConcurrencyOperation managerState localEnv operation =
  case operation of
    Concurrency.SpawnResource kind label action ->
      localLift localEnv (ConcUnlift Persistent Unlimited) \runLocal ->
        spawnResourceIn managerState kind label (runLocal action)
    Concurrency.SpawnResourceWithHandle kind label action ->
      localLift localEnv (ConcUnlift Persistent Unlimited) \runLocal ->
        spawnResourceWithHandleIn managerState kind label (runLocal . action)
    Concurrency.RegisterResource kind label cleanup ->
      localLift localEnv (ConcUnlift Persistent Unlimited) \runLocal ->
        registerResourceIn managerState kind label (runLocal cleanup)
    Concurrency.ReleaseResource resourceId ->
      releaseResourceIn managerState resourceId
    Concurrency.CancelResource resourceId ->
      cancelResourceIn managerState resourceId
    Concurrency.AwaitResource handle ->
      awaitResourceIn managerState handle
    Concurrency.ListResources ->
      listResourcesIn managerState
    Concurrency.LookupResource resourceId ->
      lookupResourceIn managerState resourceId

spawnResourceIn
  :: (IOE :> es, Prim :> es, Concurrent :> es, Ki.StructuredConcurrency :> es)
  => ManagerState es
  -> ResourceKind
  -> Text
  -> Eff es ()
  -> Eff es ResourceHandle
spawnResourceIn managerState kind label action =
  spawnResourceWithHandleIn managerState kind label (const action)

spawnResourceWithHandleIn
  :: (IOE :> es, Prim :> es, Concurrent :> es, Ki.StructuredConcurrency :> es)
  => ManagerState es
  -> ResourceKind
  -> Text
  -> (ResourceHandle -> Eff es ())
  -> Eff es ResourceHandle
spawnResourceWithHandleIn managerState kind label action = do
  resourceInfo <- newResourceInfo managerState kind label
  let handle = ResourceHandle{resourceId = resourceInfo.id}
  started <- MVar.newEmptyMVar
  registered <- MVar.newEmptyMVar
  thread <- Ki.fork managerState.rootScope do
    MVar.putMVar started =<< myThreadId
    MVar.takeMVar registered
    runResourceAction managerState resourceInfo.id (action handle)
  threadId <- MVar.takeMVar started
  let runtime = ResourceRuntime
        { info = resourceInfo
        , thread = Just thread
        , threadId = Just threadId
        , cleanup = Nothing
        }
  insertRuntime managerState runtime
  MVar.putMVar registered ()
  pure handle

registerResourceIn
  :: (IOE :> es, Prim :> es)
  => ManagerState es
  -> ResourceKind
  -> Text
  -> Eff es ()
  -> Eff es ResourceHandle
registerResourceIn managerState kind label cleanup = do
  resourceInfo <- newResourceInfo managerState kind label
  insertRuntime managerState ResourceRuntime
    { info = resourceInfo
    , thread = Nothing
    , threadId = Nothing
    , cleanup = Just cleanup
    }
  pure ResourceHandle{resourceId = resourceInfo.id}

runResourceAction
  :: (IOE :> es, Prim :> es)
  => ManagerState es
  -> ResourceId
  -> Eff es ()
  -> Eff es ()
runResourceAction managerState resourceId action =
  trySync action >>= \case
    Right () ->
      finishResource managerState resourceId Completed
    Left err ->
      finishResource managerState resourceId (Failed (Text.pack (show err)))

releaseResourceIn
  :: (IOE :> es, Prim :> es)
  => ManagerState es
  -> ResourceId
  -> Eff es Bool
releaseResourceIn managerState resourceId =
  lookupRuntime managerState resourceId >>= \case
    Nothing ->
      pure False
    Just runtime
      | resourceFinished runtime.info || isJust runtime.thread ->
          pure False
      | otherwise -> do
          result <- traverse trySync runtime.cleanup
          case result of
            Just (Left err) ->
              finishResource managerState resourceId (Failed (Text.pack (show err)))
            _ ->
              finishResource managerState resourceId Released
          pure True

cancelResourceIn
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => ManagerState es
  -> ResourceId
  -> Eff es Bool
cancelResourceIn managerState resourceId = do
  runtime <- lookupRuntime managerState resourceId
  case runtime of
    Nothing ->
      pure False
    Just resource
      | resourceFinished resource.info ->
          pure False
      | Just threadId <- resource.threadId -> do
          killThread threadId
          finishResource managerState resourceId Cancelled
          pure True
      | otherwise ->
          releaseResourceIn managerState resourceId

awaitResourceIn
  :: (Prim :> es, Ki.StructuredConcurrency :> es)
  => ManagerState es
  -> ResourceHandle
  -> Eff es ()
awaitResourceIn managerState handle =
  liftMaybeThread managerState handle.resourceId >>= \case
    Nothing ->
      pure ()
    Just thread ->
      Ki.atomically (Ki.await thread)

listResourcesIn :: Prim :> es => ManagerState es -> Eff es ResourceSnapshot
listResourcesIn managerState =
  ResourceSnapshot . map (.info) . Map.elems <$> readIORef managerState.runtimes

lookupResourceIn :: Prim :> es => ManagerState es -> ResourceId -> Eff es (Maybe ResourceInfo)
lookupResourceIn managerState resourceId =
  fmap (.info) . Map.lookup resourceId <$> readIORef managerState.runtimes

newResourceInfo
  :: (IOE :> es, Prim :> es)
  => ManagerState es
  -> ResourceKind
  -> Text
  -> Eff es ResourceInfo
newResourceInfo managerState kind label = do
  resourceId <- nextId managerState
  startedAt <- liftIO getCurrentTime
  pure ResourceInfo
    { id = resourceId
    , kind
    , label
    , parent = Root
    , status = Running
    , startedAt
    , finishedAt = Nothing
    }

nextId :: Prim :> es => ManagerState es -> Eff es ResourceId
nextId managerState =
  atomicModifyIORef' managerState.nextResourceId \(ResourceId current) ->
    (ResourceId (current + 1), ResourceId current)

insertRuntime :: Prim :> es => ManagerState es -> ResourceRuntime es -> Eff es ()
insertRuntime managerState runtime =
  atomicModifyIORef' managerState.runtimes \runtimes ->
    (Map.insert runtime.info.id runtime runtimes, ())

lookupRuntime :: Prim :> es => ManagerState es -> ResourceId -> Eff es (Maybe (ResourceRuntime es))
lookupRuntime managerState resourceId =
  Map.lookup resourceId <$> readIORef managerState.runtimes

liftMaybeThread :: Prim :> es => ManagerState es -> ResourceId -> Eff es (Maybe (Ki.Thread ()))
liftMaybeThread managerState resourceId = do
  runtime <- lookupRuntime managerState resourceId
  pure (runtime >>= (.thread))

finishResource
  :: (IOE :> es, Prim :> es)
  => ManagerState es
  -> ResourceId
  -> ResourceStatus
  -> Eff es ()
finishResource managerState resourceId status = do
  finishedAt <- liftIO getCurrentTime
  atomicModifyIORef' managerState.runtimes \runtimes ->
    let update runtime =
          runtime
            { info = runtime.info
                { status
                , finishedAt = Just finishedAt
                }
            }
    in (Map.adjust update resourceId runtimes, ())
