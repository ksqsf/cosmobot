{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cosmobot.Plugin
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally)
import Data.Aeson (Result (Error, Success), Value (Null, String), eitherDecodeFileStrict', fromJSON, object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (removeFile)
import System.Environment (getEnv, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (BufferMode (LineBuffering), hClose, hFlush, hGetContents, hGetLine, hPutStrLn, hSetBuffering, openTempFile, stderr)
import System.Process (CreateProcess (env, std_err, std_in, std_out), StdStream (CreatePipe), createProcess, proc, readCreateProcessWithExitCode, waitForProcess)

main :: IO ()
main = lookupEnv "COSMOBOT_SDK_TRANSIENT_TEST" >>= \case
  Just "1" ->
    serveWith "test" [] (transientStartup "retry later" :: IO ()) (const (pure ())) (const (pure ()))
  _ -> lookupEnv "COSMOBOT_SDK_PROCESS_FAILURE_TEST" >>= \case
    Just "1" -> processFailureChild
    _ -> lookupEnv "COSMOBOT_SDK_OVERSIZE_TEST" >>= \case
      Just "1" -> serve "test" [] (pure ())
      _ -> lookupEnv "COSMOBOT_SDK_LIFECYCLE_TEST" >>= \case
        Just "1" -> lifecycleChild
        _ -> tests

tests :: IO ()
tests = do
  let plugin = do
        command "!hello" "Say hello." (const (pure "hello"))
        tool "greet" "Generate a greeting." (text "name") pure
        pure ()
      expectedOrder = ["hello", "greet"]
      actualOrder = map declarationName (declarations plugin)
  assertEqual "declaration order" expectedOrder actualOrder
  assertEqual "valid declarations" [] (validationErrors plugin)

  let invalid = do
        command "" "" (const (pure ""))
        tool "" "" ((,) <$> text "same" <*> text "same") (const (pure ""))
        pure ()
  assertEqual "accumulated validation" 6 (length (validationErrors invalid))
  assertEqual "tool identifier validation" 1 (length (validationErrors (tool "not valid" "description" (pure ()) (const (pure "")))))
  assertEqual "reserved tool separator" 1 (length (validationErrors (tool "not__valid" "description" (pure ()) (const (pure "")))))
  mapM_ checkCommand
    [ ("!echo", True)
    , ("/echo", False)
    , ("!hello world", False)
    , ("!", False)
    ]
  case fromJSON (object ["canonicalReference" .= String "media:1", "mimeType" .= Null, "size" .= Null, "publicUrl" .= Null, "localPath" .= Null]) of
    Success media -> do
      assertEqual "nullable media MIME" Nothing (mimeType (media :: MediaResult))
      assertEqual "nullable media size" Nothing (size media)
    Error problem -> error ("nullable media result failed to decode: " <> problem)

  executable <- getExecutablePath
  testSharedFixture
  let child = (proc executable []) {env = Just [("COSMOBOT_SDK_TRANSIENT_TEST", "1")]}
      initializeRequest = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"plugin.initialize\",\"params\":{}}\n"
  (exitCode, output, _) <- readCreateProcessWithExitCode child initializeRequest
  assertEqual "transient startup exits cleanly" ExitSuccess exitCode
  assertEqual "transient startup marker" True ("\"transient\":true" `Text.isInfixOf` Text.pack output)
  assertEqual "transient startup message" True ("retry later" `Text.isInfixOf` Text.pack output)

  testProcessFailure executable
  testLifecycle executable
  let oversizedChild = (proc executable []) {env = Just [("COSMOBOT_SDK_OVERSIZE_TEST", "1")]}
  (oversizedExit, _, oversizedErrors) <- readCreateProcessWithExitCode oversizedChild (replicate (1024 * 1024 + 1) 'x')
  assertEqual "oversized unterminated frame fails" True (oversizedExit /= ExitSuccess)
  assertEqual "oversized frame diagnostic" True ("JSON-RPC line exceeds 1 MiB" `Text.isInfixOf` Text.pack oversizedErrors)

  case declarations plugin of
    [Route route, Tool definition] -> do
      assertEqual "command id strips bang" "hello" (routeId route)
      assertEqual "command access default" "allowed" (routeAccess route)
      assertEqual "command stop default" "stop" (routeDisposition route)
      assertEqual "typed tool schema" expectedSchema (toolSchema definition)
    other -> error ("unexpected declarations: " <> show other)
  putStrLn "sdk-spec: OK"
  where
    expectedSchema =
      object
        [ "type" .= String "object"
        , "properties" .= object ["name" .= object ["type" .= String "string"]]
        , "required" .= (["name"] :: [Text])
        , "additionalProperties" .= False
        ]
    checkCommand (name, expected) =
      assertEqual ("command syntax " <> show name) expected
        (null (validationErrors (command name "help" (const (pure "")))))

testSharedFixture :: IO ()
testSharedFixture = do
  decoded <- eitherDecodeFileStrict' "../cosmobot/protocol-fixtures/route-invoke.json"
  case decoded >>= parseEither parseRouteFixture of
    Left problem -> error ("shared route fixture failed to decode: " <> problem)
    Right actual ->
      assertEqual "shared route fixture"
        ("plugin.route.invoke", 10, "created", "telegram", "private")
        actual

parseRouteFixture :: Value -> Parser (Text, Int, Text, Text, Text)
parseRouteFixture = withObject "route fixture" $ \root -> do
  method <- root .: "method"
  params <- root .: "params"
  withObject "route params" (\values -> do
    timeoutSeconds <- values .: "timeoutSeconds"
    messageValue <- values .: "message"
    withObject "incoming message" (\message -> do
      eventKind <- message .: "eventKind"
      platform <- message .: "platform"
      chatKind <- message .: "kind"
      pure (method, timeoutSeconds, eventKind, platform, chatKind)) messageValue) params

lifecycleChild :: IO ()
lifecycleChild = do
  marker <- getEnv "COSMOBOT_SDK_FINALIZE_MARKER"
  serveWith "test" [] (pure ()) (const (threadDelay 100000 >> writeFile marker "finalized")) $ \() -> do
    command "!slow" "Wait until cancelled." $ \_ ->
      io ((threadDelay 5000000 >> pure "late") `finally` hPutStrLn stderr "handler-cancelled")
    pure ()

processFailureChild :: IO ()
processFailureChild = do
  marker <- getEnv "COSMOBOT_SDK_PROCESS_FINALIZE_MARKER"
  serveWith "test" [] (pure ()) (const (appendFile marker "finalized\n")) $ \() -> do
    command "!fail" "Trigger a process failure." $ \_ ->
      io (transientProcessFailure "retry process")
    pure ()

testProcessFailure :: FilePath -> IO ()
testProcessFailure executable = bracket (openTempFile "/tmp" "cosmobot-sdk-process-finalize") cleanup $ \(marker, markerHandle) -> do
  hClose markerHandle
  let child =
        (proc executable [])
          { env = Just
              [ ("COSMOBOT_SDK_PROCESS_FAILURE_TEST", "1")
              , ("COSMOBOT_SDK_PROCESS_FINALIZE_MARKER", marker)
              ]
          , std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
  createProcess child >>= \case
    (Just childInput, Just childOutput, Just childErrors, processHandle) -> do
      hSetBuffering childInput LineBuffering
      request childInput "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"plugin.initialize\",\"params\":{}}"
      _ <- hGetLine childOutput
      request childInput "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"plugin.route.invoke\",\"params\":{\"invocationId\":\"fail\",\"routeId\":\"fail\",\"message\":{},\"arguments\":\"\",\"timeoutSeconds\":2}}"
      processExitCode <- waitForProcess processHandle
      processOutput <- hGetContents childOutput
      processErrors <- hGetContents childErrors
      finalized <- lines <$> readFile marker
      assertEqual "transient process exit code" (ExitFailure 75) processExitCode
      assertEqual "transient process has no invocation response" "" processOutput
      assertEqual "transient process stderr" True ("retry process" `Text.isInfixOf` Text.pack processErrors)
      assertEqual "transient process finalizes once" ["finalized"] finalized
      hClose childInput
    _ -> error "process failure child pipes were not created"
  where
    cleanup (marker, _) = removeFile marker
    request handle payload = hPutStrLn handle payload >> hFlush handle

testLifecycle :: FilePath -> IO ()
testLifecycle executable = bracket (openTempFile "/tmp" "cosmobot-sdk-finalize") cleanup $ \(marker, markerHandle) -> do
  hClose markerHandle
  let child =
        (proc executable [])
          { env = Just
              [ ("COSMOBOT_SDK_LIFECYCLE_TEST", "1")
              , ("COSMOBOT_SDK_FINALIZE_MARKER", marker)
              ]
          , std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
  createProcess child >>= \case
    (Just childInput, Just childOutput, Just childErrors, processHandle) -> do
      hSetBuffering childInput LineBuffering
      request childInput "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"plugin.initialize\",\"params\":{}}"
      _ <- hGetLine childOutput
      request childInput "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"plugin.route.invoke\",\"params\":{\"invocationId\":\"slow\",\"routeId\":\"slow\",\"message\":{},\"arguments\":\"\",\"timeoutSeconds\":1}}"
      timeoutResponse <- hGetLine childOutput
      assertEqual "handler timeout response" True ("plugin invocation timed out" `Text.isInfixOf` Text.pack timeoutResponse)
      request childInput "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"plugin.shutdown\",\"params\":{}}"
      _ <- hGetLine childOutput
      finalized <- readFile marker
      assertEqual "finalize precedes shutdown response" "finalized" finalized
      hClose childInput
      exitCode <- waitForProcess processHandle
      cancellation <- hGetContents childErrors
      assertEqual "lifecycle child exits cleanly" ExitSuccess exitCode
      assertEqual "timed handler cleanup ran" True ("handler-cancelled" `Text.isInfixOf` Text.pack cancellation)
    _ -> error "lifecycle child pipes were not created"
  where
    cleanup (marker, _) = removeFile marker
    request handle payload = hPutStrLn handle payload >> hFlush handle

declarationName :: Declaration -> Text
declarationName (Route route) = routeId route
declarationName (Tool definition) = toolName definition

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
  | expected == actual = pure ()
  | otherwise = error (label <> ": expected " <> show expected <> ", got " <> show actual)
