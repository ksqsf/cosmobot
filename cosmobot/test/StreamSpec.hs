module Main (main) where

import Bot.Prelude
import qualified Bot.Concurrency.Manager as ConcurrencyManager
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Util.Stream as StreamUtil
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Effectful.Resource as Resource
import qualified Streaming
import qualified Streaming.Prelude as S
import System.IO.Error (userError)
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "stream"
      [ testCase "failed input does not stop merged stream" testFailedInputDoesNotStopMergedStream
      , testCase "finished inputs do not end merge before slower inputs" testFinishedInputsDoNotEndMergeEarly
      , testCase "resource is released when consumption stops early" testEarlyStopReleasesResource
      , testCase "merged pumps stop when consumption stops early" testEarlyStopCancelsMergedPumps
      ]

testFailedInputDoesNotStopMergedStream :: IO ()
testFailedInputDoesNotStopMergedStream = do
  result <- timeout 1_000_000 $ runEff $ runConcurrent $ runPrim $ runTestLog $ Resource.runResource $ ConcurrencyManager.runConcurrencyManager do
    S.toList_ $
      StreamUtil.mergeStreams
        [ Streaming.lift (throwIO (userError "stopped"))
        , S.each [1 :: Int, 2]
        ]
  result @?= Just [1, 2]

testFinishedInputsDoNotEndMergeEarly :: IO ()
testFinishedInputsDoNotEndMergeEarly = do
  result <- timeout 1_000_000 $ runEff $ runConcurrent $ runPrim $ runTestLog $ Resource.runResource $ ConcurrencyManager.runConcurrencyManager do
    S.toList_ $
      StreamUtil.mergeStreams
        [ pure ()
        , pure ()
        , Streaming.lift (threadDelay 1000) >> S.yield (1 :: Int)
        ]
  result @?= Just [1]

testEarlyStopReleasesResource :: IO ()
testEarlyStopReleasesResource = do
  released <- IORef.newIORef False
  firstChunk <- runEff $ Resource.runResource $
    S.head_ $
      StreamUtil.bracketStream
        (pure ())
        (\() -> liftIO (IORef.writeIORef released True))
        (\() -> S.each [1 :: Int, 2])
  firstChunk @?= Just 1
  IORef.readIORef released >>= (@?= True)

testEarlyStopCancelsMergedPumps :: IO ()
testEarlyStopCancelsMergedPumps = do
  (firstChunk, statuses) <- runEff $ runConcurrent $ runPrim $ runTestLog $ ConcurrencyManager.runConcurrencyManager do
    firstChunk <- Resource.runResource $
      S.head_ $
        StreamUtil.mergeStreams
          [S.yield (1 :: Int) >> Streaming.lift (threadDelay maxBound)]
    snapshot <- Concurrency.list
    pure
      ( firstChunk
      , [entry.status | entry <- snapshot.entries, "stream.merge." `Text.isPrefixOf` entry.label]
      )
  firstChunk @?= Just 1
  statuses @?= [Concurrency.Cancelled]

runTestLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runTestLog action = startKatipE "stream-spec" "test" action
