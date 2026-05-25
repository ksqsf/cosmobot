module Main (main) where

import Bot.Prelude
import qualified Bot.Util.Stream as StreamUtil
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
      ]

testFailedInputDoesNotStopMergedStream :: IO ()
testFailedInputDoesNotStopMergedStream = do
  result <- timeout 1_000_000 $ runEff $ runConcurrent $ runTestLog do
    S.toList_ $
      StreamUtil.mergeStreams
        [ Streaming.lift (throwIO (userError "stopped"))
        , S.each [1 :: Int, 2]
        ]
  result @?= Just [1, 2]

runTestLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runTestLog action = startKatipE "stream-spec" "test" action
