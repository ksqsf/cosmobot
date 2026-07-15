{-|
Module      : Bot.Agent.Tools.Shell
Description : Agent shell execution tool
Stability   : experimental
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Bot.Agent.Tools.Shell
  ( runBashTool
  , runBashSafe
  , runSandboxBashSafe
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Bot.Resource.Sandbox as Sandbox
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Effectful.Process.Typed as TypedProcess
import Effectful.Timeout

runBashTool :: (Resource.Resource :> es, IOE :> es, Fail :> es, Timeout :> es, Concurrent :> es, TypedProcess.TypedProcess :> es) => Tool es
runBashTool = Tool
  { name = "run_bash"
  , description = "Run a bash script and obtain outputs; do not run malicious code."
  , parameters = objectSchema
      [ fieldText "script" "The bash script to be executed in the cwd"
      , fieldInteger "timeout_seconds" "Maximum seconds to wait before killing the process. Defaults to 30."
      , fieldText "sandbox" "Optional sandbox id returned by the sandbox tool's create operation."
      ]
      ["script"]
  , noisy = False
  , allowed = superuserOnly
  , start = \context -> pure \metadata args ->
      withParsedToolArgs runBashArgs args \(script, timeoutSeconds, sandboxId) ->
        case sandboxId of
          Nothing -> toolText <$> runBashSafe timeoutSeconds (Text.unpack script)
          Just resourceId -> runInSandbox context metadata resourceId timeoutSeconds script
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

runInSandbox
  :: forall es. (Resource.Resource :> es, IOE :> es, Timeout :> es, Concurrent :> es, TypedProcess.TypedProcess :> es)
  => AgentContext es
  -> ToolCallMetadata
  -> Text
  -> Int
  -> Text
  -> Eff es ToolResult
runInSandbox context metadata sandboxId timeoutSeconds script =
  case Resource.accessFromMessage context.message of
    Left err -> pure (resourceToolFailure err)
    Right access -> do
      result <- Resource.withResource @Sandbox.Sandbox access sandboxId metadata.parent \sandbox ->
        runSandboxBashSafe timeoutSeconds sandbox script Nothing
      pure $ case join (first renderResourceError result) of
        Left err -> clientFailure err
        Right output -> toolText output

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

clientFailure :: Text -> ToolResult
clientFailure err = toolFailure (permanentArgumentFailure err err).failure

runBashArgs :: Aeson.Value -> AesonTypes.Parser (Text, Int, Maybe Text)
runBashArgs =
  Aeson.withObject "run bash arguments" $ \o -> do
    script <- o Aeson..: Key.fromText "script"
    timeoutSeconds <- fromMaybe 30 <$> o Aeson..:? Key.fromText "timeout_seconds"
    sandbox <- o Aeson..:? Key.fromText "sandbox"
    when (Text.any (== '\NUL') script) do
      fail "script must not contain NUL."
    when (timeoutSeconds <= 0) do
      fail "timeout_seconds must be positive."
    when (maybe False (Text.null . Text.strip) sandbox) do
      fail "sandbox must not be empty."
    pure (script, timeoutSeconds, sandbox)
