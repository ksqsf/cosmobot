{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Agent.Tools.Sandbox
Description : Agent tools for chat-owned Podman sandboxes
Stability   : experimental
-}
module Bot.Agent.Tools.Sandbox
  ( createSandboxTool
  , sandboxBashAsyncTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Bot.Resource.Sandbox as Sandbox
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Effectful.Process.Typed as TypedProcess

createSandboxTool
  :: (Resource.Resource :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Tool es
createSandboxTool = Tool
  { name = "create_sandbox"
  , description = "Create an isolated Debian sandbox owned by the current chat and sender."
  , parameters = objectSchema [] []
  , noisy = False
  , allowed = hasResourceIdentity
  , start = \context -> pure \_ args ->
      withParsedToolArgs emptyObject args \() ->
        case Resource.accessFromMessage context.message of
          Left err -> pure (resourceToolFailure err)
          Right _ -> Resource.create @Sandbox.Sandbox Resource.Init{message = context.message, arguments = ()} >>= \case
            Left err -> pure (resourceToolFailure err)
            Right sandboxId -> pure (toolText (jsonText (Aeson.object ["sandbox" Aeson..= sandboxId])))
  }

sandboxBashAsyncTool
  :: (Resource.Resource :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Tool es
sandboxBashAsyncTool = Tool
  { name = "sandbox_bash_async"
  , description = "Run or manage an asynchronous Bash command inside a sandbox. Actions: create, output, wait_for_exit, kill, release."
  , parameters = objectSchema
      [ fieldText "sandbox" "Sandbox id returned by create_sandbox. Required for every action."
      , fieldText "action" "One of: create, output, wait_for_exit, kill, release."
      , fieldText "command_id" "Command id returned by create."
      , fieldText "script" "Bash script to execute for create."
      , fieldInteger "output_byte_limit" "Maximum retained output bytes for create. Defaults to 1048576."
      ]
      ["sandbox", "action"]
  , noisy = False
  , allowed = hasResourceIdentity
  , start = \context -> pure \metadata args ->
      withParsedToolArgs parseSandboxCall args \call ->
        runSandboxCall metadata context.message call >>= \case
          Left err -> pure (clientFailure err)
          Right value -> pure (toolText value)
  }

data SandboxCall = SandboxCall
  { sandboxId :: !Text
  , action :: !SandboxAction
  }

data SandboxAction
  = CreateCommand !Text !(Maybe Int)
  | CommandOutput !Text
  | WaitForCommand !Text
  | KillCommand !Text
  | ReleaseCommand !Text

runSandboxCall
  :: forall es. (Resource.Resource :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => ToolCallMetadata
  -> IncomingMessage
  -> SandboxCall
  -> Eff es (Either Text Text)
runSandboxCall metadata message call =
  case Resource.accessFromMessage message of
    Left err -> pure (Left (renderResourceError err))
    Right access ->
      Resource.withResource @Sandbox.Sandbox access call.sandboxId metadata.parent run
        <&> first renderResourceError
        <&> join
  where
    run sandbox = case call.action of
      CreateCommand script outputByteLimit ->
        Sandbox.createCommand sandbox script outputByteLimit
          <&> fmap (\commandId -> jsonText (Aeson.object ["commandId" Aeson..= commandId]))
      CommandOutput commandId ->
        Sandbox.commandOutput sandbox commandId <&> fmap (jsonText . outputValue)
      WaitForCommand commandId ->
        Sandbox.waitForCommand sandbox commandId <&> fmap (jsonText . exitStatusValue)
      KillCommand commandId ->
        Sandbox.killCommand sandbox commandId <&> fmap (const "Command killed.")
      ReleaseCommand commandId ->
        Sandbox.releaseCommand sandbox commandId <&> fmap (const "Command released.")

parseSandboxCall :: Aeson.Value -> AesonTypes.Parser SandboxCall
parseSandboxCall = Aeson.withObject "sandbox_bash_async arguments" \o -> do
  sandboxId <- o Aeson..: Key.fromText "sandbox" >>= validText "sandbox"
  actionName <- o Aeson..: Key.fromText "action"
  action <- case actionName :: Text of
    "create" -> do
      script <- o Aeson..: Key.fromText "script" >>= validValue "script"
      outputByteLimit <- o Aeson..:? Key.fromText "output_byte_limit"
      when (maybe False (<= 0) outputByteLimit) $ fail "output_byte_limit must be positive."
      pure (CreateCommand script outputByteLimit)
    "output" -> CommandOutput <$> requiredCommandId o
    "wait_for_exit" -> WaitForCommand <$> requiredCommandId o
    "kill" -> KillCommand <$> requiredCommandId o
    "release" -> ReleaseCommand <$> requiredCommandId o
    _ -> fail "action must be one of: create, output, wait_for_exit, kill, release."
  pure SandboxCall{sandboxId, action}

requiredCommandId :: AesonTypes.Object -> AesonTypes.Parser Text
requiredCommandId o = o Aeson..: Key.fromText "command_id" >>= validText "command_id"

outputValue :: Sandbox.SandboxOutput -> Aeson.Value
outputValue output = Aeson.object
  [ "output" Aeson..= output.output
  , "truncated" Aeson..= output.truncated
  , "exitStatus" Aeson..= fmap exitStatusValue output.exitCode
  ]

exitStatusValue :: Int -> Aeson.Value
exitStatusValue exitCode = Aeson.object
  [ "exitCode" Aeson..= exitCode
  , "signal" Aeson..= Aeson.Null
  ]

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
