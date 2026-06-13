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
      ]

testNormalExitCancelsAndAwaits :: Assertion
testNormalExitCancelsAndAwaits = do
  result <- timeout 1_000_000 $ runManaged do
    stopped <- MVar.newEmptyMVar
    runConcurrencyManager do
      void $
        Concurrency.fork "worker" $
          never `finally` MVar.putMVar stopped ()
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

runManaged :: Eff '[Prim, Concurrent, IOE] a -> IO a
runManaged =
  runEff . runConcurrent . runPrim

never :: Concurrent :> es => Eff es ()
never =
  threadDelay maxBound
