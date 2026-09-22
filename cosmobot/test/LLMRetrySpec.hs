module Main (main) where

import Bot.Prelude
import qualified Control.Exception as Exception
import qualified Bot.LLM.OpenAI.Retry as Retry
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.IORef as IORef
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.Internal as HTTPInternal
import qualified Network.HTTP.Types.Header as HTTPHeader
import qualified Network.HTTP.Types.Status as HTTPStatus
import qualified Network.HTTP.Types.Version as HTTPVersion
import qualified Streaming.Prelude as S
import qualified System.Timeout as Timeout
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "llm retry"
      [ testCase "exhaustion preserves the final failure after four attempts" testRetryExhaustion
      , testCase "invalid and short Retry-After values retain backoff" testRetryAfterBoundaries
      , testCase "asynchronous cancellation escapes without retrying" testCancellation
      , testCase "successful streams preserve chunks and return values" testSuccessfulStream
      , testCase "retries transient failures with exponential backoff" testExponentialBackoff
      , testCase "honors Retry-After when longer than backoff" testRetryAfter
      , testCase "retries configured transient HTTP failures" testRetryableFailures
      , testCase "does not retry permanent HTTP failures" testPermanentFailures
      , testCase "does not retry after yielding output" testNoRetryAfterOutput
      , testCase "Retry-After rejects overflowing and signed decimals" testRetryAfterOverflow
      , testCase "large Retry-After sleep remains blocked and cancellable" testLargeRetrySleep
      ]

testRetryAfterOverflow :: IO ()
testRetryAfterOverflow = do
  let limit = toInteger (maxBound :: Int)
      cases = [(show n, 2) | n <- [limit + 1, 2 * (limit + 1) + 10, 10 ^ (100 :: Int)]]
        <> [("+10", 2), (show limit, maxBound), (show (limit - 1), maxBound - 1)]
  for_ cases \(raw, expected) -> do
    attempts <- IORef.newIORef 0
    delays <- IORef.newIORef []
    result <- runRetry delays (flakyStream attempts 1
      (statusException 429 [("Retry-After", ByteStringChar8.pack raw)]))
    assertRight result
    IORef.readIORef delays >>= (@?= [expected])

testLargeRetrySleep :: IO ()
testLargeRetrySleep = do
  attempts <- IORef.newIORef (0 :: Int)
  let stream = do
        attempt <- liftIO $ IORef.atomicModifyIORef' attempts (\n -> (n + 1, n))
        when (attempt == 0) $ lift $ throwIO $
          statusException 429 [("Retry-After", ByteStringChar8.pack (show (maxBound :: Int)))]
        S.yield ("done" :: Text)
  result <- Timeout.timeout 100000 $ runEff $ runConcurrent $
    startKatipE "llm-retry-spec" "test" $
      S.toList_ (Retry.retryLLMStreamRequest "large delay" stream)
  result @?= Nothing
  IORef.readIORef attempts >>= (@?= 1)

testExponentialBackoff :: IO ()
testExponentialBackoff = do
  attempts <- IORef.newIORef 0
  delays <- IORef.newIORef []
  result <- runRetry delays (flakyStream attempts 3 (httpException HTTP.ResponseTimeout))
  assertRight result
  IORef.readIORef attempts >>= (@?= 4)
  IORef.readIORef delays >>= (@?= [2, 4, 8])

testRetryAfter :: IO ()
testRetryAfter = do
  attempts <- IORef.newIORef 0
  delays <- IORef.newIORef []
  result <- runRetry delays (flakyStream attempts 1 (statusException 429 [("Retry-After", "10")]))
  assertRight result
  IORef.readIORef delays >>= (@?= [10])

testRetryableFailures :: IO ()
testRetryableFailures =
  for_ retryableExceptions \err -> do
    attempts <- IORef.newIORef 0
    delays <- IORef.newIORef []
    result <- runRetry delays (flakyStream attempts 1 err)
    assertRight result
    IORef.readIORef attempts >>= (@?= 2)
  where
    retryableExceptions =
      map (`statusException` []) [408, 425, 429, 500, 502, 503, 504]
        <> map httpException
          [ HTTP.ResponseTimeout
          , HTTP.ConnectionTimeout
          , HTTP.ConnectionFailure (toException (HTTP.InvalidUrlException "" "connection failed"))
          , HTTP.NoResponseDataReceived
          , HTTP.IncompleteHeaders
          , HTTP.ConnectionClosed
          , HTTP.ResponseBodyTooShort 1 0
          ]

testPermanentFailures :: IO ()
testPermanentFailures =
  for_ [statusException 400 [], httpException (HTTP.InvalidStatusLine "bad")] \err -> do
    attempts <- IORef.newIORef 0
    delays <- IORef.newIORef []
    result <- runRetry delays (flakyStream attempts 1 err)
    assertBool "expected retry wrapper to preserve permanent failure" (isLeft result)
    IORef.readIORef attempts >>= (@?= 1)
    IORef.readIORef delays >>= (@?= [])

testNoRetryAfterOutput :: IO ()
testNoRetryAfterOutput = do
  attempts <- IORef.newIORef (0 :: Int)
  delays <- IORef.newIORef []
  let stream = do
        liftIO $ IORef.modifyIORef' attempts (+ 1)
        S.yield ("partial" :: Text)
        lift $ throwIO (httpException HTTP.ConnectionClosed)
  result <- runRetry delays stream
  assertBool "expected failure after partial output" (isLeft result)
  IORef.readIORef attempts >>= (@?= 1)
  IORef.readIORef delays >>= (@?= [])

testRetryExhaustion :: IO ()
testRetryExhaustion = do
  attempts <- IORef.newIORef 0
  delays <- IORef.newIORef []
  let err = httpException HTTP.ConnectionClosed
  result <- runRetry delays (flakyStream attempts 4 err)
  case result of
    Left actual -> case fromException actual of
      Just (HTTP.HttpExceptionRequest _ HTTP.ConnectionClosed) -> pure ()
      _ -> assertFailure ("unexpected failure: " <> displayException actual)
    Right () -> assertFailure "exhausted retries must fail"
  IORef.readIORef attempts >>= (@?= 4)
  IORef.readIORef delays >>= (@?= [2, 4, 8])

testRetryAfterBoundaries :: IO ()
testRetryAfterBoundaries =
  for_ ["", "-1", "0", "1", "2", "3seconds", "Wed, 21 Oct 2015 07:28:00 GMT"] \header -> do
    attempts <- IORef.newIORef 0
    delays <- IORef.newIORef []
    result <- runRetry delays (flakyStream attempts 1 (statusException 429 [("Retry-After", header)]))
    assertRight result
    IORef.readIORef delays >>= (@?= [2])

testCancellation :: IO ()
testCancellation = do
  attempts <- IORef.newIORef 0
  delays <- IORef.newIORef []
  result <- Exception.try @Exception.AsyncException $
    runRetry delays (flakyStream attempts 1 (toException Exception.ThreadKilled))
  case result of
    Left Exception.ThreadKilled -> pure ()
    _ -> assertFailure "cancellation must escape the synchronous failure handler"
  IORef.readIORef attempts >>= (@?= 1)
  IORef.readIORef delays >>= (@?= [])

testSuccessfulStream :: IO ()
testSuccessfulStream = do
  delays <- IORef.newIORef []
  result <- runEff $ startKatipE "llm-retry-spec" "test" $
    S.toList $ Retry.retryLLMStreamRequestWith
      (\seconds -> liftIO $ IORef.modifyIORef' delays (<> [seconds]))
      "successful request"
      (S.each ["", "first", "second" :: Text] >> pure (17 :: Int))
  result @?= (["", "first", "second"] S.:> 17)
  IORef.readIORef delays >>= (@?= [])

runRetry
  :: IORef.IORef [Int]
  -> Stream (Of Text) (Eff '[KatipE, IOE]) ()
  -> IO (Either SomeException ())
runRetry delays stream =
  runEff $
    startKatipE "llm-retry-spec" "test" $
      trySync $
        void $
          S.toList_ $
          Retry.retryLLMStreamRequestWith
            (\seconds -> liftIO $ IORef.modifyIORef' delays (<> [seconds]))
            "test request"
            stream

flakyStream
  :: IORef.IORef Int
  -> Int
  -> SomeException
  -> Stream (Of Text) (Eff '[KatipE, IOE]) ()
flakyStream attempts failures err = do
  attempt <- liftIO $ IORef.atomicModifyIORef' attempts \current ->
    let next = current + 1
    in (next, next)
  when (attempt <= failures) (lift $ throwIO err)

assertRight :: Show err => Either err a -> Assertion
assertRight = \case
  Left err ->
    assertFailure ("expected retry to succeed, got " <> show err)
  Right{} ->
    pure ()

httpException :: HTTP.HttpExceptionContent -> SomeException
httpException =
  toException . HTTP.HttpExceptionRequest HTTP.defaultRequest

statusException :: Int -> HTTPHeader.ResponseHeaders -> SomeException
statusException code headers =
  httpException (HTTP.StatusCodeException response ByteString.empty)
  where
    response = HTTPInternal.Response
      { HTTPInternal.responseStatus = HTTPStatus.mkStatus code ""
      , HTTPInternal.responseVersion = HTTPVersion.http11
      , HTTPInternal.responseHeaders = headers
      , HTTPInternal.responseBody = ()
      , HTTPInternal.responseCookieJar = HTTP.createCookieJar []
      , HTTPInternal.responseClose' = HTTPInternal.ResponseClose (pure ())
      , HTTPInternal.responseOriginalRequest = HTTP.defaultRequest
      , HTTPInternal.responseEarlyHints = []
      }
