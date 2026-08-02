{-|
Module      : Bot.Agent.Tools.Shell
Description : Agent shell execution tool
Stability   : experimental
-}

module Bot.Agent.Tools.Shell
  ( runBashTool
  , runBashSafe
  , runSandboxBashSafe
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import Bot.Prelude
import qualified Bot.Resource.Sandbox as Sandbox
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.Text as Text
import qualified Effectful.Process.Typed as TypedProcess
import Effectful.Timeout

runBashTool :: (IOE :> es, Fail :> es, Timeout :> es, Concurrent :> es, TypedProcess.TypedProcess :> es) => Tool (Eff es)
runBashTool =
  tagged [workTag]
  . allowWhen superuserOnly
  . withDescription "Run a bash script and obtain outputs; do not run malicious code."
  $ tool "run_bash"
      ( validateArgument validScript
          (requiredText "script" "The bash script to be executed in the cwd")
      , validateArgument validTimeout
          (withDefault 30 (optionalInt "timeout_seconds" "Maximum seconds to wait before killing the process. Defaults to 30."))
      )
      \script timeoutSeconds ->
        toolText <$> runBashSafe timeoutSeconds (Text.unpack script)

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
  let killProcess =
        ProcessUtil.killProcessGroup (TypedProcess.unsafeProcessHandle process)
  outcome <- timeout (effectiveTimeout * 1_000_000) (TypedProcess.waitExitCode process)
    `onException` killProcess
  case outcome of
    Nothing -> do
      killProcess
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

runSandboxBashSafe
  :: (IOE :> es, Timeout :> es, Concurrent :> es, TypedProcess.TypedProcess :> es)
  => Int
  -> Sandbox.Sandbox
  -> Text
  -> Maybe Int
  -> Eff es (Either Text Text)
runSandboxBashSafe timeoutSeconds sandbox script outputByteLimit =
  Sandbox.runCommand timeoutSeconds sandbox script outputByteLimit <&> fmap \output ->
    if output.timedOut
      then formatSandboxTimeout (max 1 timeoutSeconds) output
      else formatSandboxResult (fromMaybe 1 output.exitCode) output

formatSandboxTimeout :: Int -> Sandbox.SandboxOutput -> Text
formatSandboxTimeout timeoutSeconds output =
  Text.strip $ Text.unlines $ filter (not . Text.null)
    [ "Script timed out after " <> show timeoutSeconds <> " seconds and was killed."
    , renderSandboxOutput output
    ]

formatSandboxResult :: Int -> Sandbox.SandboxOutput -> Text
formatSandboxResult exitCode output =
  Text.strip $ Text.unlines $ filter (not . Text.null)
    [ renderSandboxOutput output
    , "exit code: " <> show exitCode
    ]

renderSandboxOutput :: Sandbox.SandboxOutput -> Text
renderSandboxOutput output
  | Text.null output.output = ""
  | output.truncated = "output (truncated):\n" <> output.output
  | otherwise = "output:\n" <> output.output

validScript :: Text -> Either Text Text
validScript script
  | Text.any (== '\NUL') script =
      Left "script must not contain NUL."
  | otherwise =
      Right script

validTimeout :: Int -> Either Text Int
validTimeout timeoutSeconds
  | timeoutSeconds <= 0 =
      Left "timeout_seconds must be positive."
  | otherwise =
      Right timeoutSeconds
