{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Agent.Tools.Terminal
Description : Agent tool for chat-owned terminals
Stability   : experimental
-}
module Bot.Agent.Tools.Terminal
  ( terminalTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Effect.ACP as ACP
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Bot.Resource.Terminal as Terminal
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Effectful.Process.Typed as TypedProcess

terminalTool
  :: (ACP.ACP :> es, Resource.Resource :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Tool es
terminalTool = Tool
  { name = "terminal"
  , description = "Run or manage a terminal command. ACP uses the connected client; other chats use an isolated Debian container. Actions: create, output, wait_for_exit, kill, release."
  , parameters = objectSchema
      [ fieldText "action" "One of: create, output, wait_for_exit, kill, release."
      , fieldText "terminal_id" "Terminal id returned by create."
      , fieldText "command" "Command to execute for create."
      , fieldTextArray "args" "Command arguments for create."
      , ("env", envSchema)
      , fieldText "cwd" "Working directory for create. Defaults to the ACP session cwd or the container default."
      , fieldInteger "output_byte_limit" "Maximum retained output bytes for create. Defaults to 1048576."
      ]
      ["action"]
  , noisy = False
  , allowed = isRight . Resource.accessFromMessage . (.message)
  , start = \context -> pure \metadata args ->
      withParsedToolArgs parseTerminalArgs args \call ->
        runTerminalCall metadata context.message call <&> either clientFailure toolText
  }

data TerminalCall
  = TerminalCreate !ACP.TerminalCreate
  | TerminalOutput !Text
  | TerminalWaitForExit !Text
  | TerminalKill !Text
  | TerminalRelease !Text
  deriving (Eq, Show)

runTerminalCall
  :: forall es. (ACP.ACP :> es, Resource.Resource :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => ToolCallMetadata
  -> IncomingMessage
  -> TerminalCall
  -> Eff es (Either Text Text)
runTerminalCall metadata message call =
  case Resource.accessFromMessage message of
    Left err -> pure (Left (renderResourceError err))
    Right access -> run access call
  where
    run access = \case
      TerminalCreate create ->
        Resource.create @Terminal.Terminal Resource.Init{message, arguments = create}
          <&> first renderResourceError
          <&> fmap (\terminalId -> jsonText (Aeson.object ["terminalId" Aeson..= terminalId]))
      TerminalOutput terminalId ->
        use access terminalId Terminal.terminalOutput <&> fmap (jsonText . terminalOutputValue)
      TerminalWaitForExit terminalId ->
        use access terminalId Terminal.terminalWaitForExit <&> fmap (jsonText . terminalExitStatusValue)
      TerminalKill terminalId ->
        use access terminalId Terminal.terminalKill <&> fmap (const "Terminal killed.")
      TerminalRelease terminalId ->
        Resource.destroy access terminalId <&> first renderResourceError <&> fmap (const "Terminal released.")

    use :: forall a. Resource.ResourceAccess -> Text -> (Terminal.Terminal -> Eff es (Either Text a)) -> Eff es (Either Text a)
    use access terminalId action =
      Resource.withResource @Terminal.Terminal access terminalId metadata.parent action
        <&> first renderResourceError
        <&> join

parseTerminalArgs :: Aeson.Value -> AesonTypes.Parser TerminalCall
parseTerminalArgs =
  Aeson.withObject "terminal arguments" \o -> do
    action <- o Aeson..: Key.fromText "action"
    case action :: Text of
      "create" -> do
        command <- o Aeson..: Key.fromText "command" >>= validText "command"
        args <- fromMaybe [] <$> o Aeson..:? Key.fromText "args"
        traverse_ (validValue "argument") args
        envValues <- fromMaybe [] <$> o Aeson..:? Key.fromText "env"
        env <- traverse parseEnv envValues
        cwd <- traverse (validText "cwd") =<< nonEmptyText =<< o Aeson..:? Key.fromText "cwd"
        outputByteLimit <- o Aeson..:? Key.fromText "output_byte_limit"
        when (maybe False (<= 0) outputByteLimit) $ fail "output_byte_limit must be positive."
        pure $ TerminalCreate ACP.TerminalCreate{command, args, env, cwd, outputByteLimit}
      "output" -> TerminalOutput <$> requiredTerminalId o
      "wait_for_exit" -> TerminalWaitForExit <$> requiredTerminalId o
      "kill" -> TerminalKill <$> requiredTerminalId o
      "release" -> TerminalRelease <$> requiredTerminalId o
      _ -> fail "action must be one of: create, output, wait_for_exit, kill, release."
  where
    parseEnv = Aeson.withObject "environment variable" \envObject -> do
      name <- envObject Aeson..: Key.fromText "name" >>= validText "environment variable name"
      when (Text.any (== '=') name) $ fail "environment variable name must not contain '='."
      value <- envObject Aeson..: Key.fromText "value" >>= validValue "environment variable value"
      pure (name, value)

validText :: String -> Text -> AesonTypes.Parser Text
validText label value = do
  when (Text.null (Text.strip value)) $ fail (label <> " must not be empty.")
  validValue label value

validValue :: String -> Text -> AesonTypes.Parser Text
validValue label value = do
  when (Text.any (== '\NUL') value) $ fail (label <> " must not contain NUL.")
  pure value

requiredTerminalId :: AesonTypes.Object -> AesonTypes.Parser Text
requiredTerminalId o =
  o Aeson..: Key.fromText "terminal_id" >>= validText "terminal_id"

nonEmptyText :: Maybe Text -> AesonTypes.Parser (Maybe Text)
nonEmptyText = pure . (>>= \text -> text <$ guard (not (Text.null (Text.strip text))))

terminalOutputValue :: ACP.TerminalOutput -> Aeson.Value
terminalOutputValue output = Aeson.object
  [ "output" Aeson..= output.output
  , "truncated" Aeson..= output.truncated
  , "exitStatus" Aeson..= fmap terminalExitStatusValue output.exitStatus
  ]

terminalExitStatusValue :: ACP.TerminalExitStatus -> Aeson.Value
terminalExitStatusValue status = Aeson.object
  [ "exitCode" Aeson..= status.exitCode
  , "signal" Aeson..= status.signal
  ]

renderResourceError :: Resource.ResourceError -> Text
renderResourceError = \case
  Resource.MissingResourceIdentity -> "Resource operations require chat and sender identity."
  Resource.ResourceNotFoundOrNotOwned -> "Resource not found or not owned."
  Resource.ResourceTypeMismatch -> "Resource has the wrong type."
  Resource.ResourceUnavailable -> "Resource is being destroyed."
  Resource.ResourceCreationFailed err -> err
  Resource.ResourceCleanupFailed err -> err

clientFailure :: Text -> ToolResult
clientFailure err =
  toolFailure (permanentArgumentFailure err err).failure

envSchema :: Aeson.Value
envSchema = Aeson.object
  [ "type" Aeson..= ("array" :: Text)
  , "description" Aeson..= ("Environment variables for create." :: Text)
  , "items" Aeson..= Aeson.object
      [ "type" Aeson..= ("object" :: Text)
      , "properties" Aeson..= Aeson.object
          [ "name" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]
          , "value" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]
          ]
      , "required" Aeson..= ["name" :: Text, "value"]
      , "additionalProperties" Aeson..= False
      ]
  ]
