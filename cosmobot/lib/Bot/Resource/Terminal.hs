{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Bot.Resource.Terminal
Description : Chat-owned ACP and sandboxed Podman terminals
Stability   : experimental
-}
module Bot.Resource.Terminal
  ( Terminal
  , terminalOutput
  , terminalWaitForExit
  , terminalKill
  , podmanRunArgs
  , podmanCleanupArgs
  , parseInspectState
  , parseWaitExitStatus
  , truncateOutput
  , renderPodmanFailure
  )
where

import Bot.Core.Message
import qualified Bot.Effect.ACP as ACP
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Unique as Unique
import qualified Effectful.Process.Typed as TypedProcess
import System.Exit (ExitCode (..))
import qualified System.Posix.Process as Posix

data Terminal
  = AcpTerminal
      { remoteId :: !Text
      , message :: !IncomingMessage
      , create :: !ACP.TerminalCreate
      }
  | PodmanTerminal
      { containerId :: !Text
      , create :: !ACP.TerminalCreate
      , outputByteLimit :: !Int
      }

instance
  (ACP.ACP :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Resource.ResourceObject (Eff es) Terminal where
  type CreationArgs Terminal = ACP.TerminalCreate

  resourceTypeName _ = "Terminal"

  createResourceObject Resource.Init{message, arguments}
    | message.platform == PlatformACP =
        ACP.createClientTerminal message arguments <&> fmap \remoteId -> AcpTerminal{remoteId, message, create = arguments}
    | otherwise =
        createPodmanTerminal arguments

  destroyResourceObject = \case
    AcpTerminal{remoteId, message} -> do
      _ <- ACP.killClientTerminal message remoteId
      ACP.releaseClientTerminal message remoteId
    PodmanTerminal{containerId} ->
      runPodman "cleanup" (podmanCleanupArgs containerId) <&> void

  probeResourceObject terminal =
    terminalOutput terminal <&> fmap \output -> case output.exitStatus of
      Nothing -> "running"
      Just status -> "exited (" <> renderExitStatus status <> ")"

  describeResourceObject terminal probeResult =
    pure $ Text.unwords $ filter (not . Text.null)
      [ "`" <> terminal.create.command <> "`"
      , Text.unwords (map (\arg -> "`" <> arg <> "`") terminal.create.args)
      , maybe "" (\cwd -> "cwd=`" <> cwd <> "`") terminal.create.cwd
      , "[" <> either (const "unreachable") id probeResult <> "]"
      ]

terminalOutput
  :: (ACP.ACP :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Terminal
  -> Eff es (Either Text ACP.TerminalOutput)
terminalOutput = \case
  AcpTerminal{remoteId, message} ->
    ACP.readClientTerminalOutput message remoteId
  PodmanTerminal{containerId, outputByteLimit} -> do
    output <- runPodman "logs" ["logs", Text.unpack containerId]
    status <- inspectPodman containerId
    pure do
      rawOutput <- output
      exitStatus <- status
      let (retained, truncated) = truncateOutput outputByteLimit rawOutput
      pure ACP.TerminalOutput{output = retained, truncated, exitStatus}

terminalWaitForExit
  :: (ACP.ACP :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Terminal
  -> Eff es (Either Text ACP.TerminalExitStatus)
terminalWaitForExit = \case
  AcpTerminal{remoteId, message} ->
    ACP.waitForClientTerminalExit message remoteId
  PodmanTerminal{containerId} ->
    runPodman "wait" ["wait", Text.unpack containerId] <&> (>>= parseWaitExitStatus)

terminalKill
  :: (ACP.ACP :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Terminal
  -> Eff es (Either Text ())
terminalKill = \case
  AcpTerminal{remoteId, message} ->
    ACP.killClientTerminal message remoteId
  PodmanTerminal{containerId} ->
    runPodman "kill" ["kill", Text.unpack containerId] <&> void

createPodmanTerminal
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => ACP.TerminalCreate
  -> Eff es (Either Text Terminal)
createPodmanTerminal create = do
  unique <- liftIO Unique.newUnique
  pid <- liftIO Posix.getProcessID
  let name = "cosmobot-terminal-" <> show pid <> "-" <> show (Unique.hashUnique unique)
      cleanup = void $ runPodman "cleanup" (podmanCleanupArgs name)
      createAndValidate = runPodman "create" (podmanRunArgs name create) >>= \case
        Right rawId
          | validContainerId (Text.strip rawId) ->
              pure $ Right PodmanTerminal
                { containerId = Text.strip rawId
                , create
                , outputByteLimit = fromMaybe defaultOutputByteLimit create.outputByteLimit
                }
        result ->
          cleanup $> Left (either id (const "Podman create returned a malformed container id.") result)
  createAndValidate `onException` cleanup

inspectPodman
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Text
  -> Eff es (Either Text (Maybe ACP.TerminalExitStatus))
inspectPodman containerId =
  runPodman "inspect" ["inspect", "--format", "{{json .State}}", Text.unpack containerId]
    <&> (>>= parseInspectState)

runPodman
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Text
  -> [String]
  -> Eff es (Either Text Text)
runPodman operation args =
  trySync (ProcessUtil.readProcessGroupWithExitCode "podman" args) <&> \case
    Left _ -> Left "Podman is unavailable."
    Right (ExitSuccess, stdoutText, _) -> Right stdoutText
    Right (ExitFailure code, stdoutText, stderrText) ->
      Left (renderPodmanFailure operation code stdoutText stderrText)

podmanRunArgs :: Text -> ACP.TerminalCreate -> [String]
podmanRunArgs name create =
  [ "run", "--detach", "--name", Text.unpack name
  , "--security-opt=no-new-privileges"
  , "--label", "io.cosmobot.resource=terminal"
  , "--log-opt", "max-size=" <> show (fromMaybe defaultOutputByteLimit create.outputByteLimit)
  ]
    <> concatMap (\(key, value) -> ["--env", Text.unpack (key <> "=" <> value)]) create.env
    <> maybe [] (\cwd -> ["--workdir", Text.unpack cwd]) create.cwd
    <> [ "--"
       , "docker.io/library/debian:stable-slim"
       , "bash"
       , "-c"
       , "exec \"$@\" 2>&1"
       , "--"
       , Text.unpack create.command
       ]
    <> map Text.unpack create.args

podmanCleanupArgs :: Text -> [String]
podmanCleanupArgs containerId =
  ["rm", "--force", "--time", "0", "--ignore", Text.unpack containerId]

parseInspectState :: Text -> Either Text (Maybe ACP.TerminalExitStatus)
parseInspectState raw =
  first (const "Podman inspect returned malformed output.") $
    AesonTypes.parseEither
      (Aeson.withObject "Podman state" \stateObject -> do
        running <- stateObject Aeson..: Key.fromText "Running"
        exitCode <- stateObject Aeson..: Key.fromText "ExitCode"
        pure $ if running then Nothing else Just ACP.TerminalExitStatus{exitCode = Just exitCode, signal = Nothing})
      =<< Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 (Text.strip raw))

parseWaitExitStatus :: Text -> Either Text ACP.TerminalExitStatus
parseWaitExitStatus raw =
  maybe (Left "Podman wait returned malformed output.")
    (\exitCode -> Right ACP.TerminalExitStatus{exitCode = Just exitCode, signal = Nothing})
    (readMaybe (Text.unpack (Text.strip raw)))

truncateOutput :: Int -> Text -> (Text, Bool)
truncateOutput limit output =
  let bytes = TextEncoding.encodeUtf8 output
      truncated = ByteString.length bytes > limit
      suffix = ByteString.drop (max 0 (ByteString.length bytes - max 0 limit)) bytes
      retained = ByteString.dropWhile (\byte -> byte >= 0x80 && byte <= 0xbf) suffix
  in (TextEncoding.decodeUtf8 retained, truncated)

renderPodmanFailure :: Text -> Int -> Text -> Text -> Text
renderPodmanFailure operation code stdoutText stderrText =
  let detail = Text.take 500 . Text.strip $ if Text.null (Text.strip stderrText) then stdoutText else stderrText
  in "Podman " <> operation <> " failed (exit " <> show code <> ")"
      <> if Text.null detail then "." else ": " <> detail

renderExitStatus :: ACP.TerminalExitStatus -> Text
renderExitStatus status =
  Text.intercalate ", " $ catMaybes
    [ ("code=" <>) . show <$> status.exitCode
    , ("signal=" <>) <$> status.signal
    ]

validContainerId :: Text -> Bool
validContainerId containerId =
  Text.length containerId >= 12
    && Text.length containerId <= 64
    && Text.all (`elem` ("0123456789abcdef" :: String)) containerId

defaultOutputByteLimit :: Int
defaultOutputByteLimit = 1024 * 1024
