{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Agent.Tools.Sandbox
Description : Agent tools for chat-owned Podman sandboxes
Stability   : experimental
-}
module Bot.Agent.Tools.Sandbox
  ( createSandboxTool
  , sandboxBashTool
  )
where

import Bot.Agent.Tools.Common
import qualified Bot.Agent.Tools.Shell as Shell
import Bot.Agent.Types
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Bot.Resource.Sandbox as Sandbox
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Effectful.Process.Typed as TypedProcess
import Effectful.Timeout (Timeout)

createSandboxTool
  :: (Resource.Resource :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Tool es
createSandboxTool = Tool
  { name = "create_sandbox"
  , description = "Create an isolated Debian sandbox owned by the current chat and sender."
  , parameters = objectSchema [] []
  , noisy = False
  , allowed = hasResourceIdentity
  , start = \context -> pure \metadata args ->
      withParsedToolArgs emptyObject args \() ->
        case Resource.accessFromMessage context.message of
          Left err -> pure (resourceToolFailure err)
          Right _ -> Resource.createAssociated @Sandbox.Sandbox metadata.parent Resource.Init{message = context.message, arguments = ()} >>= \case
            Left err -> pure (resourceToolFailure err)
            Right sandboxId -> pure (toolText (jsonText (Aeson.object ["sandbox" Aeson..= sandboxId])))
  }

sandboxBashTool
  :: (Resource.Resource :> es, Timeout :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Tool es
sandboxBashTool = Tool
  { name = "sandbox_bash"
  , description = "Run a Bash script inside a sandbox and return its output when it exits."
  , parameters = objectSchema
      [ fieldText "sandbox" "Sandbox id returned by create_sandbox."
      , fieldText "script" "Bash script to execute."
      , fieldInteger "timeout_seconds" "Maximum seconds to wait before killing the script. Defaults to 30."
      , fieldInteger "output_byte_limit" "Maximum retained output bytes. Defaults to 1048576."
      ]
      ["sandbox", "script"]
  , noisy = False
  , allowed = hasResourceIdentity
  , start = \context -> pure \metadata args ->
      withParsedToolArgs parseSandboxBashArgs args \(sandboxId, script, timeoutSeconds, outputByteLimit) ->
        case Resource.accessFromMessage context.message of
          Left err -> pure (resourceToolFailure err)
          Right access -> do
            result <- Resource.withResource @Sandbox.Sandbox access sandboxId metadata.parent \sandbox ->
              Shell.runSandboxBashSafe timeoutSeconds sandbox script outputByteLimit
            pure $ either clientFailure toolText (join (first renderResourceError result))
  }

parseSandboxBashArgs :: Aeson.Value -> AesonTypes.Parser (Text, Text, Int, Maybe Int)
parseSandboxBashArgs = Aeson.withObject "sandbox_bash arguments" \o -> do
  sandboxId <- o Aeson..: Key.fromText "sandbox" >>= validText "sandbox"
  script <- o Aeson..: Key.fromText "script" >>= validValue "script"
  timeoutSeconds <- fromMaybe 30 <$> o Aeson..:? Key.fromText "timeout_seconds"
  outputByteLimit <- o Aeson..:? Key.fromText "output_byte_limit"
  when (timeoutSeconds <= 0) $ fail "timeout_seconds must be positive."
  when (maybe False (<= 0) outputByteLimit) $ fail "output_byte_limit must be positive."
  pure (sandboxId, script, timeoutSeconds, outputByteLimit)

emptyObject :: Aeson.Value -> AesonTypes.Parser ()
emptyObject = Aeson.withObject "create_sandbox arguments" (const (pure ()))

hasResourceIdentity :: AgentContext es -> Bool
hasResourceIdentity = isRight . Resource.accessFromMessage . (.message)

clientFailure :: Text -> ToolResult
clientFailure err = toolFailure (permanentArgumentFailure err err).failure

validText :: String -> Text -> AesonTypes.Parser Text
validText label value = do
  when (Text.null (Text.strip value)) $ fail (label <> " must not be empty.")
  validValue label value

validValue :: String -> Text -> AesonTypes.Parser Text
validValue label value = do
  when (Text.any (== '\NUL') value) $ fail (label <> " must not contain NUL.")
  pure value
