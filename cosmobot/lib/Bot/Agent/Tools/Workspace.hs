{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Agent.Tools.Workspace
Description : Agent tool for superuser workspaces
Stability   : experimental
-}
module Bot.Agent.Tools.Workspace
  ( workspaceTools
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Bot.Resource.Workspace as Workspace
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.Process.Typed as TypedProcess

workspaceTools
  :: (Resource.Resource :> es, FileSystem.FileSystem :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => [Tool es]
workspaceTools =
  [ expose "Create a dedicated /work workspace before substantial multi-step work. Read repository instructions before editing; keep WORK.md current with the goal, paths, branches, commits, validation, environment notes, and blockers. Verify authentication and permissions before remote operations; prefer a topic branch and pull request."
      $ tool "workspace_create"
          ( optionalText "name" "Optional globally unique resource name."
          , mapArgument validWorkId
              (requiredText "id" "Short stable descriptive id using letters, digits, dot, underscore, or hyphen.")
          , nonEmptyTextArgument "goal" "Initial work goal."
          , ttlMinutesArgument
          )
          \requestedName workId goal ttlMinutes ->
            run (CreateWorkspace requestedName Workspace.WorkspaceArgs{workId, goal, ttlMinutes})
  , expose "List accessible workspace names as a JSON array."
      $ tool "workspace_list" noArguments (run ListWorkspaces)
  , expose "Read an existing workspace's path and WORK.md state."
      $ tool "workspace_query" resourceArgument (run . QueryWorkspace)
  , expose "Replace an existing workspace's WORK.md contents."
      $ tool "workspace_update"
          (resourceArgument, nonEmptyTextArgument "goal" "Complete replacement WORK.md contents.")
          \resourceId goal -> run (UpdateWorkspace resourceId goal)
  , expose "Rename an accessible workspace."
      $ tool "workspace_rename"
          (resourceArgument, nonEmptyTextArgument "name" "New globally unique resource name.")
          \resourceId newName -> run (RenameWorkspace resourceId newName)
  , expose "Delete an accessible workspace."
      $ tool "workspace_delete" resourceArgument (run . DestroyWorkspace)
  ]
  where
    expose description =
      tagged [workspaceTag]
      . allowWhen (\context -> superuserOnly context && isRight (Resource.accessFromMessage context.message))
      . withDescription description

    run call = do
      context <- askToolContext
      metadata <- askToolCallMetadata
      runWorkspaceCall context metadata call

data WorkspaceCall
  = CreateWorkspace !(Maybe Text) !Workspace.WorkspaceArgs
  | ListWorkspaces
  | QueryWorkspace !Text
  | UpdateWorkspace !Text !Text
  | DestroyWorkspace !Text
  | RenameWorkspace !Text !Text

runWorkspaceCall
  :: forall es. (Resource.Resource :> es, FileSystem.FileSystem :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => AgentContext
  -> ToolCallMetadata
  -> WorkspaceCall
  -> Eff es ToolResult
runWorkspaceCall context metadata call =
  case Resource.accessFromMessage context.message of
    Left err -> pure (resourceToolFailure err)
    Right access -> case call of
      CreateWorkspace requestedName arguments ->
        createWorkspace requestedName Resource.Init{message = context.message, arguments} <&> \case
          Left err -> resourceToolFailure err
          Right resourceId -> toolText (jsonText (Aeson.object
            [ "resource" Aeson..= resourceId
            , "path" Aeson..= ("/work/" <> arguments.workId)
            ]))
      ListWorkspaces ->
        listResourceNames (Proxy @Workspace.Workspace) access
      QueryWorkspace resourceId ->
        use access resourceId Workspace.queryWorkspace <&> result
      UpdateWorkspace resourceId goal ->
        use access resourceId (\workspace -> Workspace.updateWorkspace workspace goal) <&> resultWith "Workspace updated."
      DestroyWorkspace resourceId -> use access resourceId (const (pure (Right ()))) >>= \case
        Left err -> pure (clientFailure err)
        Right () -> Resource.destroy access resourceId <&> first renderResourceError <&> resultWith "Workspace destroyed."
      RenameWorkspace resourceId newName ->
        Resource.rename access resourceId newName <&> first renderResourceError <&> resultWith "Workspace renamed."
  where
    createWorkspace = \case
      Nothing -> Resource.createForRun @Workspace.Workspace metadata.originRunId metadata.parent
      Just name -> Resource.createNamedForRun @Workspace.Workspace metadata.originRunId metadata.parent name

    use
      :: forall a. Resource.ResourceAccess
      -> Text
      -> (Workspace.Workspace -> Eff es (Either Text a))
      -> Eff es (Either Text a)
    use access resourceId action =
      Resource.withResource @Workspace.Workspace access resourceId metadata.parent action
        <&> first renderResourceError
        <&> join

    result = either clientFailure toolText
    resultWith message = either clientFailure (const (toolText message))

resourceArgument :: ToolArgument Text
resourceArgument =
  nonEmptyTextArgument "resource" "Existing workspace resource name."

nonEmptyTextArgument :: Text -> Text -> ToolArgument Text
nonEmptyTextArgument name description =
  mapArgument (validNonEmpty (Text.unpack name)) (requiredText name description)

validWorkId :: Text -> AesonTypes.Parser Text
validWorkId = either (fail . Text.unpack) pure . Workspace.validateWorkId

validNonEmpty :: String -> Text -> AesonTypes.Parser Text
validNonEmpty label value
  | Text.null (Text.strip value) = fail (label <> " must not be empty.")
  | otherwise = pure value

clientFailure :: Text -> ToolResult
clientFailure err = toolFailure (permanentArgumentFailure err err).failure
