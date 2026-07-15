{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Bot.Resource.Sandbox
Description : Chat-owned Podman sandboxes
Stability   : experimental
-}
module Bot.Resource.Sandbox
  ( Sandbox
  , SandboxOutput (..)
  , createCommand
  , commandOutput
  , waitForCommand
  , killCommand
  , releaseCommand
  , podmanRunArgs
  , podmanExecArgs
  , podmanCleanupArgs
  , parseInspectRunning
  , retainOutput
  , renderPodmanFailure
  )
where

import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Unique as Unique
import qualified Effectful.Process.Typed as TypedProcess
import System.Exit (ExitCode (..))
import qualified System.Posix.Process as Posix

data Sandbox = Sandbox
  { containerId :: !Text
  }

data SandboxOutput = SandboxOutput
  { output :: !Text
  , truncated :: !Bool
  , exitCode :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

instance
  (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Resource.ResourceObject (Eff es) Sandbox where
  type CreationArgs Sandbox = ()

  resourceTypeName _ = "Sandbox"

  resourcePersistence _ = Resource.PersistentResource
    { encodeResource = (.containerId)
    , restoreResource = \payload ->
        let containerId = Text.strip payload
        in pure $ if validPodmanId containerId
          then Right Sandbox{containerId}
          else Left "Stored sandbox container id is malformed."
    }

  createResourceObject _ = createSandbox

  destroyResourceObject sandbox =
    runPodman "cleanup" (podmanCleanupArgs sandbox.containerId) <&> void

  probeResourceObject sandbox =
    runPodman "inspect" ["inspect", "--format", "{{.State.Running}}", Text.unpack sandbox.containerId]
      <&> (>>= parseInspectRunning)
      <&> fmap (\running -> if running then "running" else "stopped")

  describeResourceObject _ probeResult =
    pure $ "debian:stable-slim [" <> either (const "unreachable") id probeResult <> "]"

createCommand
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Sandbox
  -> Text
  -> Maybe Int
  -> Eff es (Either Text Text)
createCommand sandbox script requestedLimit = mask \restore -> do
  commandId <- newCommandId
  let paths = commandPaths commandId
      limit = fromMaybe defaultOutputByteLimit requestedLimit
      cleanup = void (releaseCommand sandbox commandId)
  prepared <- restore $ runPodman "prepare command" (podmanPrepareArgs sandbox.containerId paths limit)
  case prepared of
    Left err -> pure (Left err)
    Right _ -> do
      launched <- restore (runPodman "start command" (podmanExecArgs sandbox.containerId commandId limit script))
        `onException` cleanup
      case launched of
        Left err -> cleanup $> Left err
        Right rawExecId
          | validPodmanId (Text.strip rawExecId) -> pure (Right commandId)
          | otherwise -> cleanup $> Left "Podman exec returned a malformed command id."

commandOutput
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Sandbox
  -> Text
  -> Eff es (Either Text SandboxOutput)
commandOutput sandbox commandId =
  withCommandPaths sandbox commandId \paths -> do
    limitResult <- readCommandFile sandbox "read command limit" paths.limit
    outputResult <- readCommandFile sandbox "read command output" paths.output
    statusResult <- readOptionalCommandFile sandbox paths.status
    pure do
      limit <- limitResult >>= maybe (Left "Sandbox command has a malformed output limit.") Right . readMaybe . Text.unpack . Text.strip
      rawOutput <- outputResult
      status <- statusResult >>= traverse parseExitCode
      let (output, truncated) = retainOutput limit rawOutput
      pure SandboxOutput{output, truncated, exitCode = status}

waitForCommand
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Sandbox
  -> Text
  -> Eff es (Either Text Int)
waitForCommand sandbox commandId =
  withCommandPaths sandbox commandId \paths -> go paths
  where
    go paths = readOptionalCommandFile sandbox paths.status >>= \case
      Left err -> pure (Left err)
      Right Nothing -> threadDelay 20_000 >> go paths
      Right (Just rawStatus) -> pure (parseExitCode rawStatus)

killCommand
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Sandbox
  -> Text
  -> Eff es (Either Text ())
killCommand sandbox commandId =
  withCommandPaths sandbox commandId \paths ->
    runPodman "kill command" (podmanKillArgs sandbox.containerId paths) <&> void

releaseCommand
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Sandbox
  -> Text
  -> Eff es (Either Text ())
releaseCommand sandbox commandId =
  case validCommandId commandId of
    False -> pure (Left "Sandbox command not found.")
    True -> do
      let paths = commandPaths commandId
      _ <- runPodman "kill command" (podmanKillArgs sandbox.containerId paths)
      runPodman "release command" (podmanReleaseArgs sandbox.containerId paths) <&> void

withCommandPaths
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Sandbox
  -> Text
  -> (CommandPaths -> Eff es (Either Text a))
  -> Eff es (Either Text a)
withCommandPaths sandbox commandId action
  | not (validCommandId commandId) = pure (Left "Sandbox command not found.")
  | otherwise = do
      let paths = commandPaths commandId
      exists <- commandExists sandbox paths
      case exists of
        Left err -> pure (Left err)
        Right False -> pure (Left "Sandbox command not found.")
        Right True -> action paths

createSandbox
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Eff es (Either Text Sandbox)
createSandbox = do
  unique <- liftIO Unique.newUnique
  pid <- liftIO Posix.getProcessID
  let name = "cosmobot-sandbox-" <> show pid <> "-" <> show (Unique.hashUnique unique)
      cleanup = void $ runPodman "cleanup" (podmanCleanupArgs name)
      createAndValidate = runPodman "create" (podmanRunArgs name) >>= \case
        Right rawId
          | validPodmanId (Text.strip rawId) -> pure (Right Sandbox{containerId = Text.strip rawId})
        result -> cleanup $> Left (either id (const "Podman create returned a malformed container id.") result)
  createAndValidate `onException` cleanup

readCommandFile
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Sandbox
  -> Text
  -> Text
  -> Eff es (Either Text Text)
readCommandFile sandbox operation path =
  runPodman operation ["exec", Text.unpack sandbox.containerId, "cat", Text.unpack path]

readOptionalCommandFile
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Sandbox
  -> Text
  -> Eff es (Either Text (Maybe Text))
readOptionalCommandFile sandbox path =
  runPodman "read command status"
    [ "exec", Text.unpack sandbox.containerId, "bash", "-c"
    , "if [ -f \"$1\" ]; then cat \"$1\"; fi", "--", Text.unpack path
    ] <&> fmap (\value -> value <$ guard (not (Text.null (Text.strip value))))

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

commandExists
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Sandbox
  -> CommandPaths
  -> Eff es (Either Text Bool)
commandExists sandbox paths =
  trySync (ProcessUtil.readProcessGroupWithExitCode "podman"
    ["exec", Text.unpack sandbox.containerId, "test", "-f", Text.unpack paths.limit]) <&> \case
      Left _ -> Left "Podman is unavailable."
      Right (ExitSuccess, _, _) -> Right True
      Right (ExitFailure 1, "", "") -> Right False
      Right (ExitFailure code, stdoutText, stderrText) ->
        Left (renderPodmanFailure "find command" code stdoutText stderrText)

podmanRunArgs :: Text -> [String]
podmanRunArgs name =
  [ "run", "--detach", "--name", Text.unpack name
  , "--security-opt=no-new-privileges"
  , "--label", "io.cosmobot.resource=sandbox"
  , "--", sandboxImage, "sleep", "infinity"
  ]

podmanExecArgs :: Text -> Text -> Int -> Text -> [String]
podmanExecArgs containerId commandId limit script =
  [ "exec", "--detach", Text.unpack containerId
  , "bash", "-c", commandWrapper, "--"
  , Text.unpack paths.output
  , Text.unpack paths.status
  , Text.unpack paths.pid
  , show limit
  , Text.unpack script
  ]
  where
    paths = commandPaths commandId

podmanPrepareArgs :: Text -> CommandPaths -> Int -> [String]
podmanPrepareArgs containerId paths limit =
  [ "exec", Text.unpack containerId, "bash", "-c"
  , ": > \"$1\"; rm -f \"$2\" \"$3\"; printf '%s' \"$4\" > \"$5\""
  , "--", Text.unpack paths.output, Text.unpack paths.status, Text.unpack paths.pid
  , show limit, Text.unpack paths.limit
  ]

podmanKillArgs :: Text -> CommandPaths -> [String]
podmanKillArgs containerId paths =
  [ "exec", Text.unpack containerId, "bash", "-c", killWrapper, "--"
  , Text.unpack paths.pid, Text.unpack paths.status
  ]

podmanReleaseArgs :: Text -> CommandPaths -> [String]
podmanReleaseArgs containerId paths =
  [ "exec", Text.unpack containerId, "rm", "-f"
  , Text.unpack paths.output, Text.unpack paths.status, Text.unpack paths.pid, Text.unpack paths.limit
  ]

podmanCleanupArgs :: Text -> [String]
podmanCleanupArgs containerId =
  ["rm", "--force", "--time", "0", "--ignore", Text.unpack containerId]

parseInspectRunning :: Text -> Either Text Bool
parseInspectRunning raw = case Text.strip raw of
  "true" -> Right True
  "false" -> Right False
  _ -> Left "Podman inspect returned malformed output."

retainOutput :: Int -> Text -> (Text, Bool)
retainOutput limit output =
  let bytes = TextEncoding.encodeUtf8 output
      retained = ByteString.take (max 0 limit) bytes
  in (decodeUtf8Prefix retained, ByteString.length bytes > limit)

decodeUtf8Prefix :: ByteString -> Text
decodeUtf8Prefix bytes =
  case TextEncoding.decodeUtf8' bytes of
    Right text -> text
    Left _ -> maybe "" (decodeUtf8Prefix . fst) (ByteString.unsnoc bytes)

renderPodmanFailure :: Text -> Int -> Text -> Text -> Text
renderPodmanFailure operation code stdoutText stderrText =
  let detail = Text.take 500 . Text.strip $ if Text.null (Text.strip stderrText) then stdoutText else stderrText
  in "Podman " <> operation <> " failed (exit " <> show code <> ")"
      <> if Text.null detail then "." else ": " <> detail

data CommandPaths = CommandPaths
  { output :: !Text
  , status :: !Text
  , pid :: !Text
  , limit :: !Text
  }

commandPaths :: Text -> CommandPaths
commandPaths commandId = CommandPaths
  { output = base <> ".out"
  , status = base <> ".status"
  , pid = base <> ".pid"
  , limit = base <> ".limit"
  }
  where
    base = "/tmp/cosmobot-" <> commandId

newCommandId :: IOE :> es => Eff es Text
newCommandId = do
  unique <- liftIO Unique.newUnique
  pid <- liftIO Posix.getProcessID
  pure ("cmd-" <> show pid <> "-" <> show (abs (Unique.hashUnique unique)))

validCommandId :: Text -> Bool
validCommandId commandId =
  "cmd-" `Text.isPrefixOf` commandId
    && Text.all (\character -> Char.isDigit character || character == '-') commandId

validPodmanId :: Text -> Bool
validPodmanId value =
  Text.length value >= 12
    && Text.length value <= 64
    && Text.all (`elem` ("0123456789abcdef" :: String)) value

parseExitCode :: Text -> Either Text Int
parseExitCode raw =
  maybe (Left "Sandbox command has a malformed exit status.") Right (readMaybe (Text.unpack (Text.strip raw)))

commandWrapper :: String
commandWrapper =
  "output=$1; status=$2; pidfile=$3; limit=$4; script=$5; "
    <> "setsid bash -c 'printf \"%s\" \"$$\" > \"$1\"; exec bash -c \"$2\"' -- \"$pidfile\" \"$script\" 2>&1 "
    <> "| { head -c $((limit + 1)) > \"$output\"; cat >/dev/null; }; "
    <> "code=${PIPESTATUS[0]}; printf '%s' \"$code\" > \"$status\"; rm -f \"$pidfile\""

killWrapper :: String
killWrapper =
  "for ((i=0; i<500; i++)); do "
    <> "[ -f \"$2\" ] && exit 0; "
    <> "if [ -s \"$1\" ]; then pid=$(cat \"$1\"); kill -KILL -- \"-$pid\" 2>/dev/null || true; exit 0; fi; "
    <> "sleep 0.01; done; exit 1"

sandboxImage :: String
sandboxImage = "docker.io/library/debian:stable-slim"

defaultOutputByteLimit :: Int
defaultOutputByteLimit = 1024 * 1024
