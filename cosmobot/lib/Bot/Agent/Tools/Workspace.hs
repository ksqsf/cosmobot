{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Agent.Tools.Workspace
Description : Agent tool for superuser workspaces
Stability   : experimental
-}
module Bot.Agent.Tools.Workspace
  ( workspaceTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Bot.Resource.Workspace as Workspace
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.Process.Typed as TypedProcess

workspaceTool
  :: (Resource.Resource :> es, FileSystem.FileSystem :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Tool es
workspaceTool = Tool
  { name = "workspace"
  , description = "Manage dedicated /work workspaces for superuser-requested multi-step work such as repositories, git/PRs, scripts, CI, research, or ops. Create one before substantial work and keep WORK.md current with scope, path, branches/commits/PRs, validation, environment notes, and blockers. Read a repository's AGENTS.md before editing. Before GitHub push/PR operations, verify the authenticated gh user is ksqsfbot; gh may require HOME=/root, use HTTPS if SSH fails, and workflow pushes require workflow scope. Prefer a topic branch and PR with summary and validation. For scheduled feature requests, only comment with an approach and mention @ksqsf. Actions: create, query, update, delete."
  , parameters = objectSchema
      [ fieldText "action" "One of: create, query, update, delete."
      , fieldText "id" "Short stable descriptive id for create; letters, digits, dot, underscore, and hyphen only."
      , fieldText "goal" "Initial work goal for create, or complete replacement WORK.md contents for update."
      , fieldText "resource" "Resource id returned by create; required for query, update, and delete."
      ]
      ["action"]
  , noisy = False
  , allowed = \context -> superuserOnly context && isRight (Resource.accessFromMessage context.message)
  , start = \context -> pure \metadata args ->
      withParsedToolArgs parseWorkspaceCall args (runWorkspaceCall context metadata)
  }

data WorkspaceCall
  = CreateWorkspace !Workspace.WorkspaceArgs
  | QueryWorkspace !Text
  | UpdateWorkspace !Text !Text
  | DestroyWorkspace !Text

runWorkspaceCall
  :: forall es. (Resource.Resource :> es, FileSystem.FileSystem :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => AgentContext es
  -> ToolCallMetadata
  -> WorkspaceCall
  -> Eff es ToolResult
runWorkspaceCall context metadata call =
  case Resource.accessFromMessage context.message of
    Left err -> pure (resourceToolFailure err)
    Right access -> case call of
      CreateWorkspace arguments ->
        Resource.createAssociated @Workspace.Workspace metadata.parent Resource.Init{message = context.message, arguments} <&> \case
          Left err -> resourceToolFailure err
          Right resourceId -> toolText (jsonText (Aeson.object
            [ "resource" Aeson..= resourceId
            , "path" Aeson..= ("/work/" <> arguments.workId)
            ]))
      QueryWorkspace resourceId ->
        use access resourceId Workspace.queryWorkspace <&> result
      UpdateWorkspace resourceId goal ->
        use access resourceId (\workspace -> Workspace.updateWorkspace workspace goal) <&> resultWith "Workspace updated."
      DestroyWorkspace resourceId -> use access resourceId (const (pure (Right ()))) >>= \case
        Left err -> pure (clientFailure err)
        Right () -> Resource.destroy access resourceId <&> first renderResourceError <&> resultWith "Workspace destroyed."
  where
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

parseWorkspaceCall :: Aeson.Value -> AesonTypes.Parser WorkspaceCall
parseWorkspaceCall = Aeson.withObject "workspace arguments" \o -> do
  action <- o Aeson..: Key.fromText "action"
  case action :: Text of
    "create" -> CreateWorkspace <$> (Workspace.WorkspaceArgs
      <$> (o Aeson..: Key.fromText "id" >>= validWorkId)
      <*> (o Aeson..: Key.fromText "goal" >>= validNonEmpty "goal"))
    "query" -> QueryWorkspace <$> requiredResourceId o
    "update" -> UpdateWorkspace <$> requiredResourceId o <*> (o Aeson..: Key.fromText "goal" >>= validNonEmpty "goal")
    "delete" -> DestroyWorkspace <$> requiredResourceId o
    "destroy" -> DestroyWorkspace <$> requiredResourceId o
    _ -> fail "action must be one of: create, query, update, delete."

requiredResourceId :: AesonTypes.Object -> AesonTypes.Parser Text
requiredResourceId o = o Aeson..: Key.fromText "resource" >>= validNonEmpty "resource"

validWorkId :: Text -> AesonTypes.Parser Text
validWorkId = either (fail . Text.unpack) pure . Workspace.validateWorkId

validNonEmpty :: String -> Text -> AesonTypes.Parser Text
validNonEmpty label value
  | Text.null (Text.strip value) = fail (label <> " must not be empty.")
  | otherwise = pure value

clientFailure :: Text -> ToolResult
clientFailure err = toolFailure (permanentArgumentFailure err err).failure
