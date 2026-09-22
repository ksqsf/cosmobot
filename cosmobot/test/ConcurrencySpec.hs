{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Bot.Concurrency.Manager
import qualified Bot.Effect.Concurrency as Concurrency
import Bot.Prelude
import qualified Control.Exception as Exception
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.Concurrent.Async as Async
import qualified GHC.Conc as GHC
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

data ManagerAbort = ManagerAbort
  deriving stock (Show)

instance Exception ManagerAbort

main :: IO ()
main =
  defaultMain $
    testGroup
      "concurrency"
      [ testCase "normal manager exit cancels and awaits running tasks" testNormalExitCancelsAndAwaits
      , testCase "top exception is thrown into running tasks" testTopExceptionPropagates
      , testCase "cancel then await returns after task cleanup" testCancelThenAwait
      , testCase "withWorker cancels and awaits after inner failure" testWithWorkerFailureCleanup
      , testCase "withWorker preserves caller masking in worker and body" testWorkerMasking
      , testCase "awaitAny returns the first completion without cancelling others" testAwaitAny
      , testCase "failed tasks record their error" testFailureStatus
      , testCase "finished tasks leave active management" testFinishedTaskRetires
      , testCase "child is registered before its first instruction" testRegisteredBeforeStart
      , testCase "cancellation status survives exception-catching workers" testCaughtCancellation
      , testCase "awaitAny accepts retired and duplicate handles" testAwaitAnyRetired
      , testCase "awaitAny reports cancellation after waiting on live workers" testAwaitAnyCancelled
      , testCase "shutdown rejects forks from worker cleanup" testShutdownAdmission
      , testCase "race cleans up if the second fork fails" testRaceForkFailure
      , testCase "race awaits the loser and propagates the winner" testRaceCompletion
      , testCase "shutdown cancels siblings before awaiting dependent cleanup" testDependentCleanup
      ]

testDependentCleanup :: Assertion
testDependentCleanup = for_ [False, True] \reverseOrder ->
  for_ [False, True] \fails -> do
    result <- timeout 1_000_000 $ runManaged do
      firstStarted <- MVar.newEmptyMVar
      secondStarted <- MVar.newEmptyMVar
      siblingStopped <- MVar.newEmptyMVar
      dependentStopped <- MVar.newEmptyMVar
      let dependent = (MVar.putMVar firstStarted () >> never) `finally`
            (MVar.takeMVar siblingStopped >> MVar.putMVar dependentStopped ())
          sibling = (MVar.putMVar secondStarted () >> never) `finally`
            MVar.putMVar siblingStopped ()
          workers = if reverseOrder then [sibling, dependent] else [dependent, sibling]
      outcome <- try @ManagerAbort $ runConcurrencyManager do
        traverse_ (Concurrency.fork "dependent cleanup") workers
        MVar.takeMVar firstStarted
        MVar.takeMVar secondStarted
        when fails (throwIO ManagerAbort)
      stopped <- MVar.tryTakeMVar dependentStopped
      pure (isLeft outcome, stopped)
    result @?= Just (fails, Just ())

testWorkerMasking :: Assertion
testWorkerMasking = for_ [False, True] \masked -> do
  result <- timeout 1_000_000 $ runManaged $ runConcurrencyManager do
    observed <- MVar.newEmptyMVar
    let action = Concurrency.withWorker "masking"
          (liftIO Exception.getMaskingState >>= MVar.putMVar observed) do
            workerState <- MVar.takeMVar observed
            bodyState <- liftIO Exception.getMaskingState
            pure (workerState, bodyState)
    if masked then mask_ action else action
  let expected = if masked then Exception.MaskedInterruptible else Exception.Unmasked
  result @?= Just (expected, expected)

testRaceForkFailure :: Assertion
testRaceForkFailure = for_ [toException ManagerAbort, toException ThreadKilled] \failure -> do
  result <- timeout 1_000_000 $ runManaged $ runConcurrencyManager do
    started <- MVar.newEmptyMVar
    cleaned <- MVar.newEmptyMVar
    outcome <- try @SomeException $
      interpose @Concurrency.Concurrency (\env op -> case op of
        Concurrency.Fork "right" _ -> MVar.takeMVar started >> throwIO failure
        _ -> passthrough env op) $
        Concurrency.raceTasks_ "left"
          ((MVar.putMVar started () >> never) `finally` MVar.putMVar cleaned ())
          "right" (pure ())
    cleanup <- MVar.tryTakeMVar cleaned
    pure (either exceptionKind (const (False, Nothing)) outcome, cleanup)
  result @?= Just (exceptionKind failure, Just ())
  where
    exceptionKind err = (isJust (fromException err :: Maybe ManagerAbort), fromException err :: Maybe AsyncException)

testRaceCompletion :: Assertion
testRaceCompletion = for_ [False, True] \winnerOnLeft -> for_ [False, True] \fails -> do
  result <- timeout 1_000_000 $ runManaged $ runConcurrencyManager do
    started <- MVar.newEmptyMVar
    cleaned <- MVar.newEmptyMVar
    let loser = (MVar.putMVar started () >> never) `finally` MVar.putMVar cleaned ()
        winner = MVar.takeMVar started >> when fails (throwIO ManagerAbort)
        (left, right) = if winnerOnLeft then (winner, loser) else (loser, winner)
    outcome <- try @ManagerAbort $ Concurrency.raceTasks_ "left" left "right" right
    cleanup <- MVar.tryTakeMVar cleaned
    pure (isLeft outcome, cleanup)
  result @?= Just (fails, Just ())

testNormalExitCancelsAndAwaits :: Assertion
testNormalExitCancelsAndAwaits = do
  result <- timeout 1_000_000 $ runManaged do
    started <- MVar.newEmptyMVar
    stopped <- MVar.newEmptyMVar
    runConcurrencyManager do
      void $
        Concurrency.fork "worker" $
          (MVar.putMVar started () >> never) `finally` MVar.putMVar stopped ()
      MVar.takeMVar started
    MVar.takeMVar stopped
  result @?= Just ()

testTopExceptionPropagates :: Assertion
testTopExceptionPropagates = do
  result <- timeout 1_000_000 $ runManaged do
    started <- MVar.newEmptyMVar
    observed <- MVar.newEmptyMVar
    outcome <- try $ runConcurrencyManager do
      void $
        Concurrency.fork "worker" do
          MVar.putMVar started ()
          never `catch` \(err :: SomeException) ->
            MVar.putMVar observed (isJust (fromException err :: Maybe ManagerAbort))
      MVar.takeMVar started
      throwIO ManagerAbort
    workerSawAbort <- MVar.takeMVar observed
    pure (isLeft (outcome :: Either SomeException ()) && workerSawAbort)
  result @?= Just True

testCancelThenAwait :: Assertion
testCancelThenAwait = do
  result <- timeout 1_000_000 $ runManaged do
    started <- MVar.newEmptyMVar
    cleaned <- MVar.newEmptyMVar
    runConcurrencyManager do
      worker <- Concurrency.fork "worker" $
        (MVar.putMVar started () >> never) `finally` MVar.putMVar cleaned ()
      MVar.takeMVar started
      cancelled <- Concurrency.cancel worker.handleId
      Concurrency.await worker
      MVar.takeMVar cleaned
      pure cancelled
  result @?= Just True

testWithWorkerFailureCleanup :: Assertion
testWithWorkerFailureCleanup = do
  result <- timeout 1_000_000 $ runManaged do
    started <- MVar.newEmptyMVar
    cleaned <- MVar.newEmptyMVar
    outcome <- try $ runConcurrencyManager $
      Concurrency.withWorker "worker"
        ((MVar.putMVar started () >> never) `finally` MVar.putMVar cleaned ()) do
          MVar.takeMVar started
          throwIO ManagerAbort
    MVar.takeMVar cleaned
    pure (isLeft (outcome :: Either SomeException ()))
  result @?= Just True

testAwaitAny :: Assertion
testAwaitAny = do
  result <- timeout 1_000_000 $ runManaged do
    firstGate <- MVar.newEmptyMVar
    secondGate <- MVar.newEmptyMVar
    runConcurrencyManager do
      firstWorker <- Concurrency.fork "first" (MVar.takeMVar firstGate)
      secondWorker <- Concurrency.fork "second" (MVar.takeMVar secondGate)
      MVar.putMVar secondGate ()
      winner <- Concurrency.awaitAny (firstWorker :| [secondWorker])
      firstStatus <- Concurrency.lookup firstWorker.handleId
      MVar.putMVar firstGate ()
      pure (winner, (.status) <$> firstStatus)
  result @?= Just (Concurrency.Handle (Concurrency.Id 2), Just Concurrency.Running)

testFailureStatus :: Assertion
testFailureStatus = do
  result <- timeout 1_000_000 $ runManaged do
    runConcurrencyManager do
      worker <- Concurrency.fork "broken" (throwIO ManagerAbort)
      Concurrency.await worker
      fmap (.status) <$> Concurrency.lookup worker.handleId
  result @?= Just (Just (Concurrency.Failed "ManagerAbort"))

testFinishedTaskRetires :: Assertion
testFinishedTaskRetires = do
  result <- timeout 1_000_000 $ runManaged do
    runConcurrencyManager do
      running <- Concurrency.fork "running" never
      worker <- Concurrency.fork "finished" (pure ())
      Concurrency.await worker
      snapshot <- Concurrency.list
      let statusOf workerHandle = (.status) <$> find ((== workerHandle.handleId) . (.id)) snapshot.entries
      pure (statusOf running, statusOf worker)
  result @?= Just (Just Concurrency.Running, Just Concurrency.Completed)

testRegisteredBeforeStart :: Assertion
testRegisteredBeforeStart = do
  result <- timeout 1_000_000 $ runManaged $ runConcurrencyManager do
    observed <- MVar.newEmptyMVar
    worker <- Concurrency.forkWithHandle "self-inspection" \self -> do
      status <- fmap (.status) <$> Concurrency.lookup self.handleId
      MVar.putMVar observed (self, status)
    observation <- MVar.takeMVar observed
    Concurrency.await worker
    pure (observation == (worker, Just Concurrency.Running))
  result @?= Just True

testCaughtCancellation :: Assertion
testCaughtCancellation = do
  result <- timeout 1_000_000 $ runManaged $ runConcurrencyManager do
    started <- MVar.newEmptyMVar
    worker <- Concurrency.fork "catches-cancellation" $
      (MVar.putMVar started () >> never) `catch` \(_ :: SomeException) -> pure ()
    MVar.takeMVar started
    firstCancel <- Concurrency.cancel worker.handleId
    Concurrency.await worker
    secondCancel <- Concurrency.cancel worker.handleId
    status <- fmap (.status) <$> Concurrency.lookup worker.handleId
    pure (firstCancel, secondCancel, status)
  result @?= Just (True, False, Just Concurrency.Cancelled)

testAwaitAnyRetired :: Assertion
testAwaitAnyRetired = do
  result <- timeout 1_000_000 $ runManaged $ runConcurrencyManager do
    running <- Concurrency.fork "running" never
    finished <- Concurrency.fork "finished" (pure ())
    Concurrency.await finished
    winner <- Concurrency.awaitAny (running :| [finished, finished])
    onlyDuplicate <- Concurrency.awaitAny (finished :| [finished])
    status <- fmap (.status) <$> Concurrency.lookup running.handleId
    pure (winner == finished && onlyDuplicate == finished, status)
  result @?= Just (True, Just Concurrency.Running)

testAwaitAnyCancelled :: Assertion
testAwaitAnyCancelled = for_ [False, True] \reverseOrder -> do
  result <- timeout 1_000_000 $ runManaged $ runConcurrencyManager do
    cancelled <- Concurrency.fork "cancel-me" never
    sibling <- Concurrency.fork "keep-running" never
    let handles = if reverseOrder then sibling :| [cancelled] else cancelled :| [sibling]
    Async.withAsync (Concurrency.awaitAny handles) \waiter -> do
      -- A start signal alone would allow cancellation before awaitAny takes
      -- its runtime snapshot. Wait until it is actually blocked in STM.
      let waitUntilBlocked = liftIO (GHC.threadStatus (Async.asyncThreadId waiter)) >>= \case
            GHC.ThreadBlocked GHC.BlockedOnSTM -> pure ()
            GHC.ThreadRunning -> threadDelay 1_000 >> waitUntilBlocked
            other -> liftIO $ assertFailure ("unexpected waiter state: " <> show other)
      waitUntilBlocked
      void (Concurrency.cancel cancelled.handleId)
      outcome <- Async.waitCatch waiter
      status <- fmap (.status) <$> Concurrency.lookup sibling.handleId
      pure (either (const False) (== cancelled) outcome, status)
  result @?= Just (True, Just Concurrency.Running)

testShutdownAdmission :: Assertion
testShutdownAdmission =
  for_ [Nothing, Just (toException ManagerAbort), Just (toException ThreadKilled)] \failure -> do
    result <- timeout 1_000_000 $ runManaged do
      started <- MVar.newEmptyMVar
      rejected <- MVar.newEmptyMVar
      ran <- MVar.newEmptyMVar
      outcome <- try @SomeException $ runConcurrencyManager do
        void $ Concurrency.fork "parent" $
          (MVar.putMVar started () >> never) `finally` do
            admission <- try @SomeException $ Concurrency.fork "late-child" (void (MVar.tryPutMVar ran ()))
            MVar.putMVar rejected (first exceptionKind admission)
        MVar.takeMVar started
        traverse_ throwIO failure
      refused <- MVar.takeMVar rejected
      childRan <- MVar.tryTakeMVar ran
      pure (refused, childRan, either (Just . exceptionKind) (const Nothing) outcome == fmap exceptionKind failure)
    result @?= Just (Left (exceptionKind (fromMaybe (toException Async.AsyncCancelled) failure)), Nothing, True)
  where
    exceptionKind err =
      ( isJust (fromException err :: Maybe Async.AsyncCancelled)
      , isJust (fromException err :: Maybe ManagerAbort)
      , fromException err :: Maybe AsyncException
      )

runManaged :: Eff '[KatipE, Prim, Concurrent, IOE] a -> IO a
runManaged =
  runEff . runConcurrent . runPrim . runTestLog

runTestLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runTestLog action = startKatipE "concurrency-spec" "test" action

never :: Concurrent :> es => Eff es ()
never =
  threadDelay maxBound
