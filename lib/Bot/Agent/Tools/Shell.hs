{-|
Module      : Bot.Agent.Tools.Shell
Description : Agent shell execution tool
Stability   : experimental
-}

module Bot.Agent.Tools.Shell
  ( runBashTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Prelude
import qualified Effectful.Concurrent.MVar as MVar
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Exit (ExitCode)
import System.Posix.Signals (signalProcess, signalProcessGroup, sigKILL)
import Effectful.Process (Process, ProcessHandle, StdStream(..), createProcess, create_group, getPid, shell, std_err, std_out, waitForProcess)
import Effectful.FileSystem.IO (FileSystem, hClose)
import Effectful.Timeout

runBashTool :: (IOE :> es, Fail :> es, Timeout :> es, Concurrent :> es, Process :> es, FileSystem :> es) => Tool es
runBashTool = Tool
  { name = "run_bash"
  , description = "Run a bash script and obtain outputs; do not run malicious code."
  , parameters = objectSchema
      [ fieldText "script" "The bash script to be executed in the cwd"
      , fieldInteger "timeout_seconds" "Maximum seconds to wait before killing the process. Defaults to 30."
      ]
      ["script"]
  , noisy = False
  , allowed = superuserOnly
  , start = \_ -> pure \args ->
      withParsedToolArgs runBashArgs args \(script, timeoutSeconds) -> do
        result <- runBashSafe timeoutSeconds (Text.unpack script)
        pure (toolText result)
  }

runBashSafe :: (IOE :> es, Fail :> es, Timeout :> es, Concurrent :> es, Process :> es, FileSystem :> es) => Int -> String -> Eff es Text
runBashSafe timeoutSeconds script = do
  let effectiveTimeout = max 1 timeoutSeconds
  (_, Just hOut, Just hErr, ph) <- createProcess
    (shell script)
      { std_out = CreatePipe
      , std_err = CreatePipe
      , create_group = True
      }
  stdoutVar <- readHandleAsync hOut
  stderrVar <- readHandleAsync hErr
  exitVar <- waitForProcessAsync ph
  outcome <- timeout (effectiveTimeout * 1_000_000) (MVar.takeMVar exitVar)
  case outcome of
    Nothing -> do
      killProcessTree ph
      _ <- timeout processExitGraceMicroseconds (MVar.takeMVar exitVar)
      stdoutText <- readerText "stdout" stdoutVar
      stderrText <- readerText "stderr" stderrVar
      ignoreIO (hClose hOut)
      ignoreIO (hClose hErr)
      pure $ Text.strip $ Text.unlines $ filter (not . Text.null)
        [ "Script timed out after " <> Text.pack (show effectiveTimeout) <> " seconds and was killed."
        , if Text.null stdoutText then "" else "stdout:\n" <> stdoutText
        , if Text.null stderrText then "" else "stderr:\n" <> stderrText
        ]
    Just (Left err) ->
      throwIO err
    Just (Right exitCode) -> do
      stdoutText <- readerText "stdout" stdoutVar
      stderrText <- readerText "stderr" stderrVar
      ignoreIO (hClose hOut)
      ignoreIO (hClose hErr)
      pure (formatBashResult exitCode stdoutText stderrText)

waitForProcessAsync :: (IOE :> es, Concurrent :> es, Process :> es) => ProcessHandle -> Eff es (MVar.MVar (Either SomeException ExitCode))
waitForProcessAsync ph = do
  result <- MVar.newEmptyMVar
  void $ forkIO do
    output <- try (waitForProcess ph)
    void (MVar.tryPutMVar result output)
  pure result

readHandleAsync :: (IOE :> es, Concurrent :> es) => Handle -> Eff es (MVar.MVar (Either SomeException Text))
readHandleAsync processOutputHandle = do
  result <- MVar.newEmptyMVar
  void $ forkIO do
    output <- try do
      text <- liftIO $ Text.hGetContents processOutputHandle
      evaluate (Text.length text) $> text
    void (MVar.tryPutMVar result output)
  pure result

readerText :: (Timeout :> es, Concurrent :> es) => Text -> MVar.MVar (Either SomeException Text) -> Eff es Text
readerText label result = do
  outcome <- timeout processExitGraceMicroseconds (MVar.takeMVar result)
  pure case outcome of
    Nothing ->
      [i|Could not read #{label}: reader timed out.|]
    Just (Left err) ->
      [i|Could not read #{label}: #{show err :: String}|]
    Just (Right text) ->
      text

killProcessTree :: (IOE :> es, Process :> es) => ProcessHandle -> Eff es ()
killProcessTree ph = do
  mPid <- getPid ph
  traverse_ killPid mPid
  where
    killPid pid =
      ignoreIO $
        (liftIO $ signalProcessGroup sigKILL (fromIntegral pid))
          `catchSync` \_ ->
            (liftIO $ signalProcess sigKILL pid)

ignoreIO :: IOE :> es => Eff es () -> Eff es ()
ignoreIO action =
  action `catchSync` \_ -> pure ()

formatBashResult :: Show exitCode => exitCode -> Text -> Text -> Text
formatBashResult exitCode stdoutText stderrText =
  Text.strip $ Text.unlines $ filter (not . Text.null)
    [ if Text.null stdoutText then "" else "stdout:\n" <> stdoutText
    , if Text.null stderrText then "" else "stderr:\n" <> stderrText
    , "exit code: " <> Text.pack (show exitCode)
    ]

processExitGraceMicroseconds :: Int
processExitGraceMicroseconds =
  5 * 1_000_000

runBashArgs :: Aeson.Value -> AesonTypes.Parser (Text, Int)
runBashArgs =
  Aeson.withObject "run bash arguments" $ \o -> do
    script <- o Aeson..: Key.fromText "script"
    timeoutSeconds <- fromMaybe 30 <$> o Aeson..:? Key.fromText "timeout_seconds"
    when (timeoutSeconds <= 0) do
      fail "timeout_seconds must be positive."
    pure (script, timeoutSeconds)
