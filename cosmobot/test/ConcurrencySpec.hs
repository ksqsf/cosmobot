{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Bot.Concurrency.Manager
import qualified Bot.Effect.Concurrency as Concurrency
import Bot.Prelude
import qualified Effectful.Concurrent.MVar as MVar
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
      , testCase "awaitAny returns the first completion without cancelling others" testAwaitAny
      , testCase "failed tasks record their error" testFailureStatus
      , testCase "finished tasks leave active management" testFinishedTaskRetires
      ]

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

runManaged :: Eff '[KatipE, Prim, Concurrent, IOE] a -> IO a
runManaged =
  runEff . runConcurrent . runPrim . runTestLog

runTestLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runTestLog action = startKatipE "concurrency-spec" "test" action

never :: Concurrent :> es => Eff es ()
never =
  threadDelay maxBound
