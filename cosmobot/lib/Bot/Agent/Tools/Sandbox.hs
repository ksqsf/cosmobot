{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Agent.Tools.Sandbox
Description : Agent tools for chat-owned Podman sandboxes
Stability   : experimental
-}
module Bot.Agent.Tools.Sandbox
  ( sandboxTools
  )
where

import Bot.Agent.Tools.Common
import qualified Bot.Agent.Tools.Shell as Shell
import Bot.Agent.Tool
import Bot.Agent.Types
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Resource as Resource
import qualified Bot.Media.Object as MediaObject
import Bot.Prelude
import qualified Bot.Resource.Sandbox as Sandbox
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import Effectful.FileSystem (FileSystem)
import qualified Effectful.Process.Typed as TypedProcess
import qualified Effectful.Temporary as Temporary
import Effectful.Timeout (Timeout)
import System.FilePath ((</>))
import qualified System.FilePath.Posix as Posix

sandboxTools
  :: (Media.Media :> es, Resource.Resource :> es, FileSystem :> es, Timeout :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => [Tool es]
sandboxTools =
  [ expose "Create an isolated persistent container sandbox."
      $ tool "sandbox_create"
          ( optionalText "name" "Optional globally unique resource name."
          , ttlMinutesArgument
          )
          \requestedName ttlMinutes -> run (SandboxCreate requestedName ttlMinutes)
  , expose "List accessible sandbox names as a JSON array."
      $ tool "sandbox_list" noArguments (run SandboxList)
  , expose "Run a Bash script in an existing sandbox."
      $ tool "sandbox_run"
          ( sandboxArgument
          , validateArgument validScript (requiredText "script" "Bash script to run.")
          , validateArgument positiveTimeout
              (withDefault 30 (optionalInt "timeout_seconds" "Maximum seconds to wait before killing the script. Defaults to 30."))
          , validateArgument positiveOutputLimit
              (optionalInt "output_byte_limit" "Maximum retained output bytes. Defaults to 1048576.")
          )
          \sandboxId script timeoutSeconds outputByteLimit ->
            run (SandboxRun sandboxId script timeoutSeconds outputByteLimit)
  , expose "Copy a file from a sandbox into the media cache and return its media id."
      $ tool "sandbox_file_to_media"
          (sandboxArgument, pathArgument "Sandbox file path to copy.")
          \sandboxId path -> run (SandboxFileToMedia sandboxId path)
  , expose "Copy a cached media object into a sandbox file."
      $ tool "sandbox_media_to_file"
          ( sandboxArgument
          , validateArgument validMediaId (requiredText "media_id" "Cached media id to copy.")
          , pathArgument "Destination path inside the sandbox."
          )
          \sandboxId mediaId path -> run (SandboxMediaToFile sandboxId mediaId path)
  , expose "Rename an accessible sandbox."
      $ tool "sandbox_rename"
          (sandboxArgument, nonEmptyTextArgument "name" "New globally unique resource name.")
          \sandboxId newName -> run (SandboxRename sandboxId newName)
  , expose "Delete an accessible sandbox. Delete sandboxes promptly when work is done unless the user asks to keep them."
      $ tool "sandbox_delete" sandboxArgument (run . SandboxDelete)
  ]
  where
    expose description =
      tagged [sandboxTag]
      . allowWhen hasResourceIdentity
      . withDescription description

    run call = do
      context <- askToolContext
      metadata <- askToolCallMetadata
      runSandboxCall context metadata call

runSandboxCall
  :: (Media.Media :> es, Resource.Resource :> es, FileSystem :> es, Timeout :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => AgentContext
  -> ToolCallMetadata
  -> SandboxCall
  -> Eff es ToolResult
runSandboxCall context metadata call =
  case Resource.accessFromMessage context.message of
    Left err -> pure (resourceToolFailure err)
    Right access -> case call of
      SandboxCreate requestedName ttlMinutes ->
        createSandbox requestedName Resource.Init
          { message = context.message
          , arguments = Sandbox.SandboxArgs{image = context.toolConfig.sandboxImage, ttlMinutes}
          } <&> \case
          Left err -> resourceToolFailure err
          Right sandboxId -> toolText (jsonText (Aeson.object ["sandbox" Aeson..= sandboxId]))
      SandboxList ->
        listResourceNames (Proxy @Sandbox.Sandbox) access
      SandboxRun sandboxId script timeoutSeconds outputByteLimit -> do
        result <- Resource.withResource @Sandbox.Sandbox access sandboxId metadata.parent \sandbox ->
          Shell.runSandboxBashSafe timeoutSeconds sandbox script outputByteLimit
        pure $ either clientFailure toolText (join (first renderResourceError result))
      SandboxFileToMedia sandboxId path -> do
        result <- Resource.withResource @Sandbox.Sandbox access sandboxId metadata.parent \sandbox ->
          copyFileToMedia sandbox path
        pure $ either resourceToolFailure (either clientFailure toolText) result
      SandboxMediaToFile sandboxId mediaId path -> do
        localPath <- Media.localMediaPath (mediaRef mediaId)
        case localPath of
          Nothing -> pure (clientFailure [i|Media object not found: #{mediaId}|])
          Just source -> do
            result <- Resource.withResource @Sandbox.Sandbox access sandboxId metadata.parent \sandbox ->
              Sandbox.copyFileToSandbox sandbox source path
            pure $ either resourceToolFailure (either clientFailure (const (toolText "Media copied to sandbox."))) result
      SandboxDelete sandboxId ->
        Resource.destroy access sandboxId <&> either resourceToolFailure (const (toolText "Sandbox deleted."))
      SandboxRename sandboxId newName ->
        Resource.rename access sandboxId newName <&> either resourceToolFailure (toolText . ("Sandbox renamed: " <>))
  where
    createSandbox = \case
      Nothing -> Resource.createForRun @Sandbox.Sandbox metadata.originRunId metadata.parent
      Just name -> Resource.createNamedForRun @Sandbox.Sandbox metadata.originRunId metadata.parent name

data SandboxCall
  = SandboxCreate !(Maybe Text) !Int
  | SandboxList
  | SandboxRun !Text !Text !Int !(Maybe Int)
  | SandboxFileToMedia !Text !FilePath
  | SandboxMediaToFile !Text !Text !FilePath
  | SandboxDelete !Text
  | SandboxRename !Text !Text

sandboxArgument :: ToolArgument Text
sandboxArgument =
  nonEmptyTextArgument "sandbox" "Existing sandbox name."

nonEmptyTextArgument :: Text -> Text -> ToolArgument Text
nonEmptyTextArgument name description =
  mapArgument (validText (Text.unpack name)) (requiredText name description)

pathArgument :: Text -> ToolArgument FilePath
pathArgument description =
  mapArgument (fmap Text.unpack . validText "path") (requiredText "path" description)

validScript :: Text -> Either Text Text
validScript =
  first toText . AesonTypes.parseEither (validValue "script")

positiveTimeout :: Int -> Either Text Int
positiveTimeout value
  | value <= 0 = Left "timeout_seconds must be positive."
  | otherwise = Right value

positiveOutputLimit :: Maybe Int -> Either Text (Maybe Int)
positiveOutputLimit value
  | maybe False (<= 0) value = Left "output_byte_limit must be positive."
  | otherwise = Right value

validMediaId :: Text -> Either Text Text
validMediaId =
  first toText . AesonTypes.parseEither (fmap Text.strip . validText "media_id")

copyFileToMedia
  :: (Media.Media :> es, FileSystem :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Sandbox.Sandbox
  -> FilePath
  -> Eff es (Either Text Text)
copyFileToMedia sandbox path =
  Temporary.runTemporary $
    Temporary.withSystemTempDirectory "cosmobot-sandbox-media-" \dir -> raise do
      let temporaryPath = dir </> ("media" <> Posix.takeExtension path)
      Sandbox.copyFileFromSandbox sandbox path temporaryPath >>= \case
        Left err -> pure (Left err)
        Right () -> do
          mediaObject <- MediaObject.fileObject ("file://" <> Text.pack temporaryPath)
          Media.storeMediaObject mediaObject
            <&> maybe (Left "Media cache rejected the sandbox file.") Right

mediaRef :: Text -> Text
mediaRef value
  | "media:" `Text.isPrefixOf` value = value
  | otherwise = "media:" <> value

hasResourceIdentity :: AgentContext -> Bool
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
