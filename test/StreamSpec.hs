module Main (main) where

import Bot.Prelude
import qualified Bot.Util.Stream as StreamUtil
import qualified Control.Exception as Exception
import qualified Streaming
import qualified Streaming.Prelude as S
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "stream"
      [ testCase "killed input does not stop merged stream" testKilledInputDoesNotStopMergedStream
      ]

testKilledInputDoesNotStopMergedStream :: IO ()
testKilledInputDoesNotStopMergedStream = do
  result <- timeout 1_000_000 $ runEff $ runTestLog do
    S.toList_ $
      StreamUtil.mergeStreams
        [ Streaming.lift (liftIO (Exception.throwIO Exception.ThreadKilled))
        , S.each [1 :: Int, 2]
        ]
  result @?= Just [1, 2]

runTestLog :: IOE :> es => Eff (Log : es) a -> Eff es a
runTestLog action = do
  logger <- liftIO $ mkLogger "stream-spec" \_ -> pure ()
  runLog "stream-spec" logger LogTrace action
