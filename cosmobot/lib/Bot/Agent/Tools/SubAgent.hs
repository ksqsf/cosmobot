{-# LANGUAGE TypeApplications #-}
{-|
Module      : Bot.Agent.Tools.SubAgent
Description : Agent tool for chat-scoped background agents
Stability   : experimental
-}
module Bot.Agent.Tools.SubAgent
  ( subagentTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Bot.Resource.SubAgent as SubAgent
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.List as List
import qualified Data.Text as Text

subagentTool
  :: (Resource.Resource :> es, Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es)
  => SubAgent.SubAgentRunner es
  -> [Tool es]
  -> Tool es
subagentTool runner availableTools =
  Tool
    { name = "subagent"
    , description = "Manage a background agent scoped to the current chat. Operations: create, send, query, rename, delete."
    , parameters =
        objectSchema
          [ fieldText "op" "One of: create, send, query, rename, delete."
          , fieldText "name" "Optional globally unique resource name for create; required as the new name for rename."
          , fieldText "system_prompt" "System prompt for create; empty inherits the current system prompt."
          , fieldTextArray "tools" "Tool names exposed to the subagent for create; empty exposes none."
          , fieldText "resource" "Subagent resource name; required for send, query, rename, and delete."
          , fieldText "prompt" "Prompt to send; required for send."
          ]
          ["op"]
    , noisy = False
    , allowed = isRight . Resource.accessFromMessage . (.message)
    , start = \context -> pure \metadata args ->
        withParsedToolArgs parseCall args \call ->
          runCall context metadata call
    }
  where
    runCall context metadata call =
      case Resource.accessFromMessage context.message of
        Left err -> pure (resourceToolFailure err)
        Right access ->
          case call of
            Create requestedName systemPrompt toolNames ->
              createSubAgent context metadata requestedName systemPrompt toolNames
            Send resourceId prompt ->
              use access resourceId \subagent ->
                SubAgent.sendPrompt runner availableTools context subagent prompt
                  <&> fmap (const "Prompt sent.")
            Query resourceId ->
              use access resourceId (fmap Right . SubAgent.queryOutput)
            Destroy resourceId ->
              Resource.destroy access resourceId
                <&> either resourceToolFailure (const (toolText "Subagent destroyed."))
            Rename resourceId newName ->
              Resource.rename access resourceId newName
                <&> either resourceToolFailure (toolText . ("Subagent renamed: " <>))
      where
        use access resourceId action =
          Resource.withResource @SubAgent.SubAgent access resourceId metadata.parent action
            <&> join . first renderResourceError
            <&> either clientFailure toolText

    createSubAgent context metadata requestedName systemPrompt toolNames =
      case validateTools context toolNames of
        Left err ->
          pure (clientFailure err)
        Right names ->
          createResource requestedName
            Resource.Init
              { message = context.message
              , arguments = SubAgent.SubAgentArgs{systemContext, toolNames = names}
              }
            <&> either resourceToolFailure (toolText . ("Subagent created: " <>))
      where
        createResource = \case
          Nothing -> Resource.createAssociated @SubAgent.SubAgent metadata.parent
          Just name -> Resource.createAssociatedNamed @SubAgent.SubAgent metadata.parent name

        systemContext
          | Text.null (Text.strip systemPrompt) = context.systemContext
          | otherwise = systemPrompt

    validateTools context = traverse validate . List.nub
      where
        validate name =
          case find ((== name) . (.name)) availableTools of
            Nothing -> Left ("Unknown tool: " <> name)
            Just tool
              | tool.allowed context -> Right name
              | otherwise -> Left ("Tool is not allowed: " <> name)

data Call
  = Create !(Maybe Text) !Text ![Text]
  | Send !Text !Text
  | Query !Text
  | Destroy !Text
  | Rename !Text !Text

parseCall :: Aeson.Value -> AesonTypes.Parser Call
parseCall = Aeson.withObject "subagent arguments" \o -> do
  op <- o Aeson..: Key.fromText "op"
  case op :: Text of
    "create" ->
      Create
        <$> o Aeson..:? Key.fromText "name"
        <*> (fromMaybe "" <$> o Aeson..:? Key.fromText "system_prompt")
        <*> (fromMaybe [] <$> o Aeson..:? Key.fromText "tools")
    "send" -> Send <$> resource o <*> requiredText o "prompt"
    "query" -> Query <$> resource o
    "delete" -> Destroy <$> resource o
    "destroy" -> Destroy <$> resource o
    "rename" -> Rename <$> resource o <*> requiredText o "name"
    _ -> fail "op must be one of: create, send, query, rename, delete."
  where
    resource o = requiredText o "resource"
    requiredText o key =
      o Aeson..: Key.fromText key >>= \value ->
        value <$ when (Text.null (Text.strip value)) (fail (Text.unpack key <> " must not be empty."))

clientFailure :: Text -> ToolResult
clientFailure err = toolFailure (permanentArgumentFailure err err).failure
