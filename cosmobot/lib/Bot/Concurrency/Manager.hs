{-|
Module      : Bot.Concurrency.Manager
Description : Queryable ownership model for concurrent work
Stability   : experimental
-}

module Bot.Concurrency.Manager
  ( runConcurrencyManager
  )
where

import qualified Bot.Effect.Concurrency as Concurrency
import Bot.Effect.Concurrency
import Bot.Prelude hiding (Handle)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Effectful.Concurrent.Async as Async
import Data.Time (getCurrentTime)
import qualified Effectful.Concurrent.MVar as MVar

data ManagerState es = ManagerState
  { nextIdRef :: !(IORef Id)
  , runtimes :: !(IORef (Map Id (EntryRuntime es)))
  }

data EntryRuntime es = EntryRuntime
  { info :: !Info
  , thread :: !(Maybe (Async.Async ()))
  , cleanup :: !(Maybe (Eff es ()))
  }

runConcurrencyManager
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => Eff (Concurrency : es) a
  -> Eff es a
runConcurrencyManager inner = do
  nextIdRef <- newIORef (Id 1)
  runtimes <- newIORef Map.empty
  let managerState = ManagerState{nextIdRef, runtimes}
      runInner = interpret (runConcurrencyOperation managerState) inner
  try runInner >>= \case
    Right result -> do
      cancelAndAwaitAll managerState
      releaseRegistrations managerState
      pure result
    Left err -> do
      cancelAndAwaitAllWith managerState err
      releaseRegistrations managerState
      throwIO (err :: SomeException)

runConcurrencyOperation
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => ManagerState es
  -> EffectHandler Concurrency es
runConcurrencyOperation managerState localEnv operation =
  case operation of
    Concurrency.Fork kind label action ->
      localUnlift localEnv managedActionUnlift \unlift ->
        forkAsIn managerState kind label (unlift action)
    Concurrency.ForkWithHandle kind label action ->
      localUnlift localEnv managedActionUnlift \unlift ->
        forkWithHandleAsIn managerState kind label (unlift . action)
    Concurrency.Register kind label cleanup ->
      localUnlift localEnv managedActionUnlift \unlift ->
        registerIn managerState kind label (unlift cleanup)
    Concurrency.Release handleId ->
      releaseIn managerState handleId
    Concurrency.Cancel handleId ->
      cancelIn managerState handleId
    Concurrency.Await workerHandle ->
      awaitIn managerState workerHandle
    Concurrency.SleepMicroseconds microseconds ->
      threadDelay microseconds
    Concurrency.List ->
      listIn managerState
    Concurrency.Lookup handleId ->
      lookupIn managerState handleId

managedActionUnlift :: UnliftStrategy
managedActionUnlift =
  ConcUnlift Persistent (Limited 1)

forkAsIn
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => ManagerState es
  -> Kind
  -> Text
  -> Eff es ()
  -> Eff es Handle
forkAsIn managerState kind label action =
  forkWithHandleAsIn managerState kind label (const action)

forkWithHandleAsIn
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => ManagerState es
  -> Kind
  -> Text
  -> (Handle -> Eff es ())
  -> Eff es Handle
forkWithHandleAsIn managerState kind label action = mask \restore -> do
  entryInfo <- newInfo managerState kind label
  let workerHandle = Handle{handleId = entryInfo.id}
  startGate <- MVar.newEmptyMVar
  thread <- Async.async $
    restore $
      MVar.takeMVar startGate
        *> runAction managerState entryInfo.id (action workerHandle)
        `finally` clearRuntimeHandles managerState entryInfo.id
  let runtime = EntryRuntime
        { info = entryInfo
        , thread = Just thread
        , cleanup = Nothing
        }
  (insertRuntime managerState runtime >> MVar.putMVar startGate ())
    `onException` Async.cancel thread
  pure workerHandle

registerIn
  :: (IOE :> es, Prim :> es)
  => ManagerState es
  -> Kind
  -> Text
  -> Eff es ()
  -> Eff es Handle
registerIn managerState kind label cleanup = do
  entryInfo <- newInfo managerState kind label
  insertRuntime managerState EntryRuntime
    { info = entryInfo
    , thread = Nothing
    , cleanup = Just cleanup
    }
  pure Handle{handleId = entryInfo.id}

runAction
  :: (IOE :> es, Prim :> es)
  => ManagerState es
  -> Id
  -> Eff es ()
  -> Eff es ()
runAction managerState handleId action =
  trySync action >>= \case
    Right () ->
      finishEntry managerState handleId Completed
    Left err ->
      finishEntry managerState handleId (Failed (Text.pack (show err)))

releaseIn
  :: (IOE :> es, Prim :> es)
  => ManagerState es
  -> Id
  -> Eff es Bool
releaseIn managerState handleId =
  lookupRuntime managerState handleId >>= \case
    Nothing ->
      pure False
    Just runtime
      | finished runtime.info || isJust runtime.thread ->
          pure False
      | otherwise -> do
          result <- traverse trySync runtime.cleanup
          case result of
            Just (Left err) ->
              finishEntry managerState handleId (Failed (Text.pack (show err)))
            _ ->
              finishEntry managerState handleId Released
          pure True

cancelIn
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => ManagerState es
  -> Id
  -> Eff es Bool
cancelIn managerState handleId = do
  runtime <- lookupRuntime managerState handleId
  case runtime of
    Nothing ->
      pure False
    Just entry
      | finished entry.info ->
          pure False
      | Just thread <- entry.thread -> do
          finishEntry managerState handleId Cancelled
          Async.cancel thread
          pure True
      | otherwise ->
          releaseIn managerState handleId

awaitIn
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => ManagerState es
  -> Handle
  -> Eff es ()
awaitIn managerState workerHandle =
  liftMaybeThread managerState workerHandle.handleId >>= \case
    Nothing ->
      pure ()
    Just thread ->
      void (Async.waitCatch thread)

listIn :: Prim :> es => ManagerState es -> Eff es Snapshot
listIn managerState =
  Snapshot . map (.info) . Map.elems <$> readIORef managerState.runtimes

lookupIn :: Prim :> es => ManagerState es -> Id -> Eff es (Maybe Info)
lookupIn managerState handleId =
  fmap (.info) . Map.lookup handleId <$> readIORef managerState.runtimes

newInfo
  :: (IOE :> es, Prim :> es)
  => ManagerState es
  -> Kind
  -> Text
  -> Eff es Info
newInfo managerState kind label = do
  handleId <- allocateId managerState
  startedAt <- liftIO getCurrentTime
  pure Info
    { id = handleId
    , kind
    , label
    , parent = Root
    , status = Running
    , startedAt
    , finishedAt = Nothing
    }

allocateId :: Prim :> es => ManagerState es -> Eff es Id
allocateId managerState =
  atomicModifyIORef' managerState.nextIdRef \(Id current) ->
    (Id (current + 1), Id current)

insertRuntime :: Prim :> es => ManagerState es -> EntryRuntime es -> Eff es ()
insertRuntime managerState runtime =
  atomicModifyIORef' managerState.runtimes \runtimes ->
    (Map.insert runtime.info.id runtime runtimes, ())

lookupRuntime :: Prim :> es => ManagerState es -> Id -> Eff es (Maybe (EntryRuntime es))
lookupRuntime managerState handleId =
  Map.lookup handleId <$> readIORef managerState.runtimes

liftMaybeThread :: Prim :> es => ManagerState es -> Id -> Eff es (Maybe (Async.Async ()))
liftMaybeThread managerState handleId = do
  runtime <- lookupRuntime managerState handleId
  pure (runtime >>= (.thread))

finishEntry
  :: (IOE :> es, Prim :> es)
  => ManagerState es
  -> Id
  -> Status
  -> Eff es ()
finishEntry managerState handleId status = do
  finishedAt <- liftIO getCurrentTime
  atomicModifyIORef' managerState.runtimes \runtimes ->
    let update runtime =
          if finished runtime.info
            then runtime
            else
              runtime
                { info = runtime.info
                    { status
                    , finishedAt = Just finishedAt
                    }
                , cleanup = Nothing
                }
    in (Map.adjust update handleId runtimes, ())

clearRuntimeHandles
  :: Prim :> es
  => ManagerState es
  -> Id
  -> Eff es ()
clearRuntimeHandles managerState handleId =
  atomicModifyIORef' managerState.runtimes \runtimes ->
    let update runtime =
          runtime
            { thread = Nothing
            , cleanup = Nothing
            }
    in (Map.adjust update handleId runtimes, ())

cancelAndAwaitAll :: (IOE :> es, Prim :> es, Concurrent :> es) => ManagerState es -> Eff es ()
cancelAndAwaitAll managerState = do
  threads <- liveThreads managerState
  traverse_ cancelAndAwait threads
  where
    cancelAndAwait (handleId, thread) = do
      finishEntry managerState handleId Cancelled
      Async.cancel thread
      void (Async.waitCatch thread)

cancelAndAwaitAllWith
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => ManagerState es
  -> SomeException
  -> Eff es ()
cancelAndAwaitAllWith managerState err = do
  threads <- liveThreads managerState
  traverse_ cancelAndAwaitWith threads
  where
    cancelAndAwaitWith (handleId, thread) = do
      finishEntry managerState handleId Cancelled
      Async.cancelWith thread err
      void (Async.waitCatch thread)

releaseRegistrations :: (IOE :> es, Prim :> es) => ManagerState es -> Eff es ()
releaseRegistrations managerState = do
  registrations <- liveRegistrations managerState
  traverse_ releaseRegistered registrations
  where
    releaseRegistered (handleId, cleanup) =
      trySync cleanup >>= \case
        Right () ->
          finishEntry managerState handleId Released
        Left err ->
          finishEntry managerState handleId (Failed (Text.pack (show err)))

liveThreads :: Prim :> es => ManagerState es -> Eff es [(Id, Async.Async ())]
liveThreads managerState =
  selectLiveThreads . Map.elems <$> readIORef managerState.runtimes
  where
    selectLiveThreads runtimes =
      [ (runtime.info.id, thread)
      | runtime <- runtimes
      , not (finished runtime.info)
      , Just thread <- [runtime.thread]
      ]

liveRegistrations :: Prim :> es => ManagerState es -> Eff es [(Id, Eff es ())]
liveRegistrations managerState =
  liveRegistered . Map.elems <$> readIORef managerState.runtimes
  where
    liveRegistered runtimes =
      [ (runtime.info.id, cleanup)
      | runtime <- runtimes
      , not (finished runtime.info)
      , isNothing runtime.thread
      , Just cleanup <- [runtime.cleanup]
      ]
