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
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import System.Posix.Signals (signalProcess, signalProcessGroup, sigKILL)
import qualified Effectful.Process as Process
import qualified Effectful.Process.Typed as TypedProcess
import Effectful.Timeout

runBashTool :: (IOE :> es, Fail :> es, Timeout :> es, Concurrent :> es, TypedProcess.TypedProcess :> es) => Tool es
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

runBashSafe :: (IOE :> es, Fail :> es, Timeout :> es, Concurrent :> es, TypedProcess.TypedProcess :> es) => Int -> String -> Eff es Text
runBashSafe timeoutSeconds script = do
  let effectiveTimeout = max 1 timeoutSeconds
      processConfig =
        TypedProcess.setCreateGroup True .
        TypedProcess.setStdin TypedProcess.closed .
        TypedProcess.setStdout TypedProcess.byteStringOutput .
        TypedProcess.setStderr TypedProcess.byteStringOutput $
        TypedProcess.shell script
  process <- TypedProcess.startProcess processConfig
  outcome <- timeout (effectiveTimeout * 1_000_000) (TypedProcess.waitExitCode process)
  case outcome of
    Nothing -> do
      killProcessTree (TypedProcess.unsafeProcessHandle process)
      _ <- timeout processExitGraceMicroseconds (TypedProcess.waitExitCode process)
      stdoutText <- ProcessUtil.processOutputText (TypedProcess.getStdout process)
      stderrText <- ProcessUtil.processOutputText (TypedProcess.getStderr process)
      pure $ Text.strip $ Text.unlines $ filter (not . Text.null)
        [ "Script timed out after " <> Text.pack (show effectiveTimeout) <> " seconds and was killed."
        , if Text.null stdoutText then "" else "stdout:\n" <> stdoutText
        , if Text.null stderrText then "" else "stderr:\n" <> stderrText
        ]
    Just exitCode -> do
      stdoutText <- ProcessUtil.processOutputText (TypedProcess.getStdout process)
      stderrText <- ProcessUtil.processOutputText (TypedProcess.getStderr process)
      pure (formatBashResult exitCode stdoutText stderrText)

killProcessTree :: (IOE :> es, Process.Process :> es) => Process.ProcessHandle -> Eff es ()
killProcessTree ph = do
  mPid <- Process.getPid ph
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
