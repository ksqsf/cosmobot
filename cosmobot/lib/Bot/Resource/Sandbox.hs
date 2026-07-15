{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Bot.Resource.Sandbox
Description : Chat-owned Podman sandboxes
Stability   : experimental
-}
module Bot.Resource.Sandbox
  ( Config (..)
  , defaultConfig
  , Sandbox
  , SandboxOutput (..)
  , runCommand
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
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Unique as Unique
import qualified Effectful.Process.Typed as TypedProcess
import Effectful.Timeout (Timeout, timeout)
import System.Exit (ExitCode (..))
import qualified System.Posix.Process as Posix
import Toml.Schema

newtype Config = Config
  { image :: Text
  }
  deriving stock (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { image = "localhost/cosmobox:latest"
  }

instance FromValue Config where
  fromValue = parseTableFromValue do
    image <- fromMaybe defaultConfig.image <$> optKey "image"
    unless (validImage image) $ fail "resource.sandbox.image must be non-empty and must not contain NUL"
    pure Config{image}

data Sandbox = Sandbox
  { containerId :: !Text
  , image :: !Text
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data SandboxOutput = SandboxOutput
  { output :: !Text
  , truncated :: !Bool
  , exitCode :: !(Maybe Int)
  , timedOut :: !Bool
  }
  deriving stock (Eq, Show)

instance
  (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Resource.ResourceObject (Eff es) Sandbox where
  type CreationArgs Sandbox = Text

  resourceTypeName _ = "Sandbox"

  resourcePersistence _ = Resource.PersistentResource
    { encodeResource = TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode
    , restoreResource = \payload ->
        pure $ restoreSandbox payload
    }

  createResourceObject initValue = createSandbox initValue.arguments

  destroyResourceObject sandbox =
    runPodman "cleanup" (podmanCleanupArgs sandbox.containerId) <&> void

  probeResourceObject sandbox =
    runPodman "inspect" ["inspect", "--format", "{{.State.Running}}", Text.unpack sandbox.containerId]
      <&> (>>= parseInspectRunning)
      <&> fmap (\running -> if running then "running" else "stopped")

  describeResourceObject sandbox probeResult =
    pure $ sandbox.image <> " [" <> either (const "unreachable") id probeResult <> "]"

  detailResourceObject sandbox =
    Resource.probeResourceObject sandbox >>= Resource.describeResourceObject sandbox <&> \description ->
      description <> "\ncontainer id: " <> sandbox.containerId

runCommand
  :: (Timeout :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Int
  -> Sandbox
  -> Text
  -> Maybe Int
  -> Eff es (Either Text SandboxOutput)
runCommand timeoutSeconds sandbox script requestedLimit = do
  let effectiveTimeout = max 1 timeoutSeconds
      limit = fromMaybe defaultOutputByteLimit requestedLimit
      args = podmanExecArgs sandbox.containerId effectiveTimeout limit script
  outcome <- timeout ((effectiveTimeout + commandExitGraceSeconds) * 1_000_000) (runPodmanCommand args)
  case outcome of
    Nothing -> pure (Left "Podman command did not exit after its timeout.")
    Just result -> pure do
      (exitCode, rawOutput) <- result
      let (output, truncated) = retainOutput limit rawOutput
      -- ponytail: GNU timeout reserves 124/137; add an out-of-band status channel if scripts must preserve them exactly.
      pure SandboxOutput{output, truncated, exitCode = Just exitCode, timedOut = exitCode `elem` [124, 137]}

createSandbox
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Text
  -> Eff es (Either Text Sandbox)
createSandbox image = do
  unique <- liftIO Unique.newUnique
  pid <- liftIO Posix.getProcessID
  let name = "cosmobot-sandbox-" <> show pid <> "-" <> show (Unique.hashUnique unique)
      cleanup = void $ runPodman "cleanup" (podmanCleanupArgs name)
      createAndValidate = runPodman "create" (podmanRunArgs image name) >>= \case
        Right rawId
          | validPodmanId (Text.strip rawId) -> pure (Right Sandbox{containerId = Text.strip rawId, image})
        result -> cleanup $> Left (either id (const "Podman create returned a malformed container id.") result)
  createAndValidate `onException` cleanup

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

runPodmanCommand
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => [String]
  -> Eff es (Either Text (Int, Text))
runPodmanCommand args =
  trySync (ProcessUtil.readProcessGroupWithExitCode "podman" args) <&> \case
    Left _ -> Left "Podman is unavailable."
    Right (ExitFailure 125, stdoutText, stderrText) ->
      Left (renderPodmanFailure "command" 125 stdoutText stderrText)
    Right (exitCode, stdoutText, stderrText) ->
      Right (exitCodeNumber exitCode, stdoutText <> stderrText)
  where
    exitCodeNumber ExitSuccess = 0
    exitCodeNumber (ExitFailure code) = code

podmanRunArgs :: Text -> Text -> [String]
podmanRunArgs image name =
  [ "run", "--detach", "--name", Text.unpack name
  , "--security-opt=no-new-privileges"
  , "--label", "io.cosmobot.resource=sandbox"
  , "--", Text.unpack image, "sleep", "infinity"
  ]

podmanExecArgs :: Text -> Int -> Int -> Text -> [String]
podmanExecArgs containerId timeoutSeconds limit script =
  [ "exec", Text.unpack containerId
  , "bash", "-c", commandWrapper, "--"
  , show timeoutSeconds
  , show limit
  , Text.unpack script
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

validPodmanId :: Text -> Bool
validPodmanId value =
  Text.length value >= 12
    && Text.length value <= 64
    && Text.all (`elem` ("0123456789abcdef" :: String)) value

validImage :: Text -> Bool
validImage value = not (Text.null (Text.strip value)) && not (Text.any (== '\NUL') value)

restoreSandbox :: Text -> Either Text Sandbox
restoreSandbox payload =
  validate $ case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 payload) of
    Right sandbox -> sandbox
    Left _ -> Sandbox{containerId = Text.strip payload, image = legacyImage}
  where
    validate sandbox
      | validPodmanId sandbox.containerId && validImage sandbox.image = Right sandbox
      | otherwise = Left "Stored sandbox payload is malformed."

legacyImage :: Text
legacyImage = "docker.io/library/debian:stable-slim"

commandWrapper :: String
commandWrapper =
  "timeout --kill-after=5 \"$1\" bash -c \"$3\" 2>&1 "
    <> "| { head -c $(( $2 + 1 )); cat >/dev/null; }; "
    <> "exit ${PIPESTATUS[0]}"

defaultOutputByteLimit :: Int
defaultOutputByteLimit = 1024 * 1024

commandExitGraceSeconds :: Int
commandExitGraceSeconds = 10
