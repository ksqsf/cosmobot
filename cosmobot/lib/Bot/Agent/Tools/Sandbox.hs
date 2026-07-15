{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Agent.Tools.Sandbox
Description : Agent tools for chat-owned Podman sandboxes
Stability   : experimental
-}
module Bot.Agent.Tools.Sandbox
  ( sandboxTool
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

sandboxTool
  :: (Resource.Resource :> es, Timeout :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Tool es
sandboxTool = Tool
  { name = "sandbox"
  , description = "Create, rename, or delete an isolated Debian sandbox, or run a Bash script in one."
  , parameters = objectSchema
      [ fieldText "op" "One of: create, run, rename, delete."
      , fieldText "name" "Optional globally unique resource name for create; required as the new name for rename."
      , fieldText "sandbox" "Sandbox name; required for run, rename, and delete."
      , fieldText "script" "Bash script; required for run."
      , fieldInteger "timeout_seconds" "Maximum seconds to wait before killing the script. Defaults to 30."
      , fieldInteger "output_byte_limit" "Maximum retained output bytes. Defaults to 1048576."
      ]
      ["op"]
  , noisy = False
  , allowed = hasResourceIdentity
  , start = \context -> pure \metadata args ->
      withParsedToolArgs sandboxArgs args \call ->
        case Resource.accessFromMessage context.message of
          Left err -> pure (resourceToolFailure err)
          Right access -> case call of
            SandboxCreate requestedName ->
              createSandbox metadata.parent requestedName Resource.Init{message = context.message, arguments = ()} <&> \case
                Left err -> resourceToolFailure err
                Right sandboxId -> toolText (jsonText (Aeson.object ["sandbox" Aeson..= sandboxId]))
            SandboxRun sandboxId script timeoutSeconds outputByteLimit -> do
              result <- Resource.withResource @Sandbox.Sandbox access sandboxId metadata.parent \sandbox ->
                Shell.runSandboxBashSafe timeoutSeconds sandbox script outputByteLimit
              pure $ either clientFailure toolText (join (first renderResourceError result))
            SandboxDelete sandboxId ->
              Resource.destroy access sandboxId <&> either resourceToolFailure (const (toolText "Sandbox deleted."))
            SandboxRename sandboxId newName ->
              Resource.rename access sandboxId newName <&> either resourceToolFailure (toolText . ("Sandbox renamed: " <>))
  }
  where
    createSandbox parent = \case
      Nothing -> Resource.createAssociated @Sandbox.Sandbox parent
      Just name -> Resource.createAssociatedNamed @Sandbox.Sandbox parent name

data SandboxCall
  = SandboxCreate !(Maybe Text)
  | SandboxRun !Text !Text !Int !(Maybe Int)
  | SandboxDelete !Text
  | SandboxRename !Text !Text

sandboxArgs :: Aeson.Value -> AesonTypes.Parser SandboxCall
sandboxArgs = Aeson.withObject "sandbox arguments" \o -> do
  op <- o Aeson..: Key.fromText "op"
  case op :: Text of
    "create" -> SandboxCreate <$> o Aeson..:? Key.fromText "name"
    "run" -> do
      sandboxId <- o Aeson..: Key.fromText "sandbox" >>= validText "sandbox"
      script <- o Aeson..: Key.fromText "script" >>= validValue "script"
      timeoutSeconds <- fromMaybe 30 <$> o Aeson..:? Key.fromText "timeout_seconds"
      outputByteLimit <- o Aeson..:? Key.fromText "output_byte_limit"
      when (timeoutSeconds <= 0) $ fail "timeout_seconds must be positive."
      when (maybe False (<= 0) outputByteLimit) $ fail "output_byte_limit must be positive."
      pure (SandboxRun sandboxId script timeoutSeconds outputByteLimit)
    "delete" -> SandboxDelete <$> (o Aeson..: Key.fromText "sandbox" >>= validText "sandbox")
    "rename" -> SandboxRename
      <$> (o Aeson..: Key.fromText "sandbox" >>= validText "sandbox")
      <*> (o Aeson..: Key.fromText "name" >>= validText "name")
    _ -> fail "op must be one of: create, run, rename, delete."

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
