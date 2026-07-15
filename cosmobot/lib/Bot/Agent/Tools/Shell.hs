{-|
Module      : Bot.Agent.Tools.Shell
Description : Agent shell execution tool
Stability   : experimental
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Bot.Agent.Tools.Shell
  ( runBashTool
  , acpTerminalTool
  , runBashSafe
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Effect.ACP as ACP
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
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
  , start = \_ -> pure \_ args ->
      withParsedToolArgs runBashArgs args \(script, timeoutSeconds) -> do
        result <- runBashSafe timeoutSeconds (Text.unpack script)
        pure (toolText result)
  }

acpTerminalTool :: (ACP.ACP :> es, Resource.Resource :> es) => Tool es
acpTerminalTool = Tool
  { name = "acp_terminal"
  , description = "Run or manage a terminal command in the connected ACP client workspace. Actions: create, output, wait_for_exit, kill, release."
  , parameters = objectSchema
      [ fieldText "action" "One of: create, output, wait_for_exit, kill, release."
      , fieldText "terminal_id" "Terminal id returned by create."
      , fieldText "command" "Command to execute for create."
      , fieldTextArray "args" "Command arguments for create."
      , ("env", envSchema)
      , fieldText "cwd" "Working directory for create. Defaults to the ACP session cwd."
      , fieldInteger "output_byte_limit" "Maximum retained output bytes for create."
      ]
      ["action"]
  , noisy = False
  , allowed = acpOnly
  , start = \context -> pure \metadata args ->
      withParsedToolArgs parseAcpTerminalArgs args \call ->
        runAcpTerminalCall metadata context.message call >>= \case
          Left err ->
            pure (clientFailure err)
          Right value ->
            pure (toolText value)
  }

data AcpTerminalCall
  = AcpTerminalCreate !ACP.TerminalCreate
  | AcpTerminalOutput !Text
  | AcpTerminalWaitForExit !Text
  | AcpTerminalKill !Text
  | AcpTerminalRelease !Text
  deriving (Eq, Show)

data AcpTerminal = AcpTerminal
  { remoteId :: !Text
  , message :: !IncomingMessage
  , create :: !ACP.TerminalCreate
  }

instance ACP.ACP :> es => Resource.ResourceObject (Eff es) AcpTerminal where
  type CreationArgs AcpTerminal = ACP.TerminalCreate

  resourceTypeName _ = "Terminal"

  createResourceObject Resource.Init{message, arguments} =
    ACP.createClientTerminal message arguments <&> fmap \remoteId -> AcpTerminal{remoteId, message, create = arguments}

  destroyResourceObject terminal = do
    _ <- ACP.killClientTerminal terminal.message terminal.remoteId
    ACP.releaseClientTerminal terminal.message terminal.remoteId

  probeResourceObject terminal =
    ACP.readClientTerminalOutput terminal.message terminal.remoteId <&> \case
      Left err -> Left err
      Right output -> Right $ case output.exitStatus of
        Nothing -> "running"
        Just status -> "exited (" <> renderExitStatus status <> ")"

  describeResourceObject terminal probeResult =
    pure $ Text.unwords $ filter (not . Text.null)
      [ "`" <> terminal.create.command <> "`"
      , Text.unwords (map ("`" <>) (map (<> "`") terminal.create.args))
      , maybe "" ("cwd=`" <>) (fmap (<> "`") terminal.create.cwd)
      , "[" <> either (const "unreachable") id probeResult <> "]"
      ]

runAcpTerminalCall :: (ACP.ACP :> es, Resource.Resource :> es) => ToolCallMetadata -> IncomingMessage -> AcpTerminalCall -> Eff es (Either Text Text)
runAcpTerminalCall metadata message call =
  case Resource.accessFromMessage message of
    Left err -> pure (Left (renderResourceError err))
    Right access -> run access call
  where
    run access = \case
      AcpTerminalCreate create ->
        Resource.create @AcpTerminal Resource.Init{message, agentId = metadata.agentRunId, arguments = create}
          <&> first renderResourceError
          <&> fmap (\terminalId -> jsonText (Aeson.object ["terminalId" Aeson..= terminalId]))
      AcpTerminalOutput terminalId ->
        use access terminalId \terminal -> ACP.readClientTerminalOutput terminal.message terminal.remoteId <&> fmap (\output -> jsonText (Aeson.object
            [ "output" Aeson..= output.output
            , "truncated" Aeson..= output.truncated
            , "exitStatus" Aeson..= fmap terminalExitStatusValue output.exitStatus
            ]))
      AcpTerminalWaitForExit terminalId ->
        use access terminalId \terminal -> fmap (jsonText . terminalExitStatusValue) <$> ACP.waitForClientTerminalExit terminal.message terminal.remoteId
      AcpTerminalKill terminalId ->
        use access terminalId \terminal -> fmap (const "Terminal killed.") <$> ACP.killClientTerminal terminal.message terminal.remoteId
      AcpTerminalRelease terminalId ->
        Resource.destroy access terminalId <&> first renderResourceError <&> fmap (const "Terminal released.")

    use access terminalId action =
      Resource.withResource @AcpTerminal access terminalId metadata.parent action
        <&> first renderResourceError
        <&> join

renderResourceError :: Resource.ResourceError -> Text
renderResourceError = \case
  Resource.MissingResourceIdentity -> "Resource operations require chat and sender identity."
  Resource.ResourceNotFoundOrNotOwned -> "Resource not found or not owned."
  Resource.ResourceTypeMismatch -> "Resource has the wrong type."
  Resource.ResourceUnavailable -> "Resource is being destroyed."
  Resource.ResourceCreationFailed err -> err
  Resource.ResourceCleanupFailed err -> err

renderExitStatus :: ACP.TerminalExitStatus -> Text
renderExitStatus status =
  Text.intercalate ", " $ catMaybes
    [ ("code=" <>) . show <$> status.exitCode
    , ("signal=" <>) <$> status.signal
    ]

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

runBashArgs :: Aeson.Value -> AesonTypes.Parser (Text, Int)
runBashArgs =
  Aeson.withObject "run bash arguments" $ \o -> do
    script <- o Aeson..: Key.fromText "script"
    timeoutSeconds <- fromMaybe 30 <$> o Aeson..:? Key.fromText "timeout_seconds"
    when (timeoutSeconds <= 0) do
      fail "timeout_seconds must be positive."
    pure (script, timeoutSeconds)

parseAcpTerminalArgs :: Aeson.Value -> AesonTypes.Parser AcpTerminalCall
parseAcpTerminalArgs =
  Aeson.withObject "acp_terminal arguments" \o -> do
    action <- o Aeson..: Key.fromText "action"
    case action :: Text of
      "create" -> do
        command <- o Aeson..: Key.fromText "command"
        args <- fromMaybe [] <$> o Aeson..:? Key.fromText "args"
        envValues <- fromMaybe [] <$> o Aeson..:? Key.fromText "env"
        env <- traverse
          ( Aeson.withObject "environment variable" \envObject ->
              (,)
                <$> envObject Aeson..: Key.fromText "name"
                <*> envObject Aeson..: Key.fromText "value"
          )
          envValues
        cwd <- nonEmptyText =<< o Aeson..:? Key.fromText "cwd"
        outputByteLimit <- o Aeson..:? Key.fromText "output_byte_limit"
        pure $
          AcpTerminalCreate ACP.TerminalCreate
            { command
            , args
            , env
            , cwd
            , outputByteLimit
            }
      "output" ->
        AcpTerminalOutput <$> requiredTerminalId o
      "wait_for_exit" ->
        AcpTerminalWaitForExit <$> requiredTerminalId o
      "kill" ->
        AcpTerminalKill <$> requiredTerminalId o
      "release" ->
        AcpTerminalRelease <$> requiredTerminalId o
      _ ->
        fail "action must be one of: create, output, wait_for_exit, kill, release."

requiredTerminalId :: AesonTypes.Object -> AesonTypes.Parser Text
requiredTerminalId o =
  o Aeson..: Key.fromText "terminal_id"

nonEmptyText :: Maybe Text -> AesonTypes.Parser (Maybe Text)
nonEmptyText value =
  pure do
    text <- value
    guard (not (Text.null (Text.strip text)))
    pure text

terminalExitStatusValue :: ACP.TerminalExitStatus -> Aeson.Value
terminalExitStatusValue status =
  Aeson.object
    [ "exitCode" Aeson..= status.exitCode
    , "signal" Aeson..= status.signal
    ]

acpOnly :: AgentContext es -> Bool
acpOnly context =
  context.message.platform == PlatformACP

clientFailure :: Text -> ToolResult
clientFailure err =
  toolFailure (permanentArgumentFailure err err).failure

envSchema :: Aeson.Value
envSchema =
  Aeson.object
    [ "type" Aeson..= ("array" :: Text)
    , "description" Aeson..= ("Environment variables for create." :: Text)
    , "items" Aeson..=
        Aeson.object
          [ "type" Aeson..= ("object" :: Text)
          , "properties" Aeson..=
              Aeson.object
                [ "name" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]
                , "value" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]
                ]
          , "required" Aeson..= ["name" :: Text, "value"]
          , "additionalProperties" Aeson..= False
          ]
    ]
