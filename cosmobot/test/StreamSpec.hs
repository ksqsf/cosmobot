module Main (main) where

import Bot.Prelude
import qualified Bot.Concurrency.Manager as ConcurrencyManager
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Util.Stream as StreamUtil
import qualified Data.List as List
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Effectful.Resource as Resource
import qualified Streaming
import qualified Streaming.Prelude as S
import System.IO.Error (IOError, userError)
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "stream"
      [ testCase "empty merge terminates" testEmptyMerge
      , testCase "all failed inputs propagate failure" testAllInputsFail
      , testCase "merge preserves each producer order beyond queue capacity" testMergeOrdering
      , testCase "bracket releases exactly once on completion and failure" testBracketRelease
      , testCase "failed input does not stop merged stream" testFailedInputDoesNotStopMergedStream
      , testCase "lifting a stream preserves chunks and its return value" testLiftLocalStream
      , testCase "finished inputs do not end merge before slower inputs" testFinishedInputsDoNotEndMergeEarly
      , testCase "resource is released when consumption stops early" testEarlyStopReleasesResource
      , testCase "merged pumps stop when consumption stops early" testEarlyStopCancelsMergedPumps
      ]

testLiftLocalStream :: IO ()
testLiftLocalStream = do
  let source :: Stream (Of Int) (Eff '[IOE]) Text
      source = do
        value <- Streaming.lift (pure 1)
        S.yield value
        S.yield 2
        pure "done"
  result <- S.toList (LLM.liftLocalStream runEff source)
  result @?= ([1, 2] S.:> "done")

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

testEmptyMerge :: IO ()
testEmptyMerge = do
  result <- timeout 1_000_000 $ runEff $ runConcurrent $ runPrim $ runTestLog $
    Resource.runResource $ ConcurrencyManager.runConcurrencyManager $
      S.toList_ (StreamUtil.mergeStreams [])
  result @?= Just ([] :: [Int])

testAllInputsFail :: IO ()
testAllInputsFail = do
  result <- timeout 1_000_000 $ runEff $ runConcurrent $ runPrim $ runTestLog $
    Resource.runResource $ ConcurrencyManager.runConcurrencyManager $ trySync $
      S.toList_ $ StreamUtil.mergeStreams $
        replicate 3 (S.yield (1 :: Int) >> Streaming.lift (throwIO (userError "producer failure")))
  case result of
    Just (Left err) -> (fromException err :: Maybe IOError) @?= Just (userError "producer failure")
    _ -> assertFailure "all-failed merge must propagate the producer exception"

testMergeOrdering :: IO ()
testMergeOrdering = do
  let items = [(producer, index) | producer <- [1..3 :: Int], index <- [1..1500 :: Int]]
  result <- timeout 5_000_000 $ runEff $ runConcurrent $ runPrim $ runTestLog $
    Resource.runResource $ ConcurrencyManager.runConcurrencyManager $
      S.toList_ $ StreamUtil.mergeStreams
        [S.each [(producer, index) | index <- [1..1500]] | producer <- [1..3]]
  case result of
    Nothing -> assertFailure "merge did not drain its bounded queue"
    Just actual -> do
      List.sort actual @?= items
      for_ [1..3] \producer ->
        [index | (owner, index) <- actual, owner == producer] @?= [1..1500]

testBracketRelease :: IO ()
testBracketRelease = for_ [False, True] \fails -> do
  releases <- IORef.newIORef (0 :: Int)
  result <- runEff $ Resource.runResource $ trySync $ S.toList $
    StreamUtil.bracketStream
      (pure ())
      (\() -> liftIO $ IORef.modifyIORef' releases (+ 1))
      (\() -> do
        S.yield (7 :: Int)
        when fails (Streaming.lift $ throwIO (userError "stream failure"))
        pure (23 :: Int))
  case result of
    Left err | fails -> (fromException err :: Maybe IOError) @?= Just (userError "stream failure")
    Right value | not fails -> value @?= ([7] S.:> 23)
    _ -> assertFailure "unexpected stream outcome"
  IORef.readIORef releases >>= (@?= 1)

runTestLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runTestLog action = startKatipE "stream-spec" "test" action
