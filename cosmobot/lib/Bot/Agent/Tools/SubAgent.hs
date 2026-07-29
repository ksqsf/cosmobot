{-# LANGUAGE TypeApplications #-}
{-|
Module      : Bot.Agent.Tools.SubAgent
Description : Agent tool for chat-scoped background agents
Stability   : experimental
-}
module Bot.Agent.Tools.SubAgent
  ( subagentTools
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Bot.Resource.SubAgent as SubAgent
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.List as List
import qualified Data.Text as Text

subagentTools
  :: (Resource.Resource :> es, Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es)
  => SubAgent.SubAgentRunner es
  -> [Tool es]
  -> [Tool es]
subagentTools runner availableTools =
  [ expose "Create a background agent scoped to the current chat."
      $ tool "subagent_create"
          ( optionalText "name" "Optional globally unique resource name."
          , withDefault "" (optionalText "system_prompt" "System prompt; empty inherits the current agent's system prompt.")
          , withDefault [] (optionalTextArray "tools" "Exact tool names exposed to the subagent; empty exposes none.")
          , ttlMinutesArgument
          )
          \requestedName systemPrompt toolNames ttlMinutes ->
            run (Create requestedName systemPrompt toolNames ttlMinutes)
  , expose "List accessible subagent resource names as a JSON array."
      $ tool "subagent_list" noArguments (run ListResources)
  , expose "Send a prompt to an idle subagent, starting a background run."
      $ tool "subagent_send"
          (resourceArgument, nonEmptyTextArgument "prompt" "Prompt to send.")
          \resourceId prompt -> run (Send resourceId prompt)
  , expose "Return a subagent's current or final output. Prefer subagent_wait_any or subagent_wait_all instead of polling while it runs."
      $ tool "subagent_query" resourceArgument (run . Query)
  , expose "Wait until any named subagent finishes. Already-ready resources return immediately and unfinished agents are not cancelled."
      $ tool "subagent_wait_any" resourcesArgument (run . WaitAny)
  , expose "Wait until all named subagents finish without cancelling them."
      $ tool "subagent_wait_all" resourcesArgument (run . WaitAll)
  , expose "Rename an accessible subagent."
      $ tool "subagent_rename"
          (resourceArgument, nonEmptyTextArgument "name" "New globally unique resource name.")
          \resourceId newName -> run (Rename resourceId newName)
  , expose "Delete an accessible subagent and stop any active run."
      $ tool "subagent_delete" resourceArgument (run . Destroy)
  ]
  where
    expose description =
      tagged [subagentTag]
      . allowWhen (isRight . Resource.accessFromMessage . (.message))
      . withDescription description

    run call = do
      context <- askToolContext
      metadata <- askToolCallMetadata
      raise (runCall context metadata call)

    runCall context metadata call =
      case Resource.accessFromMessage context.message of
        Left err -> pure (resourceToolFailure err)
        Right access ->
          case call of
            Create requestedName systemPrompt toolNames ttlMinutes ->
              createSubAgent context metadata requestedName systemPrompt toolNames ttlMinutes
            ListResources ->
              listResourceNames (Proxy @SubAgent.SubAgent) access
            Send resourceId prompt ->
              use access resourceId \subagent ->
                SubAgent.sendPrompt runner metadata resourceId availableTools context subagent prompt
                  <&> fmap (const "Prompt sent.")
            Query resourceId ->
              use access resourceId (fmap Right . SubAgent.queryOutput)
            WaitAny resourceIds ->
              useMany access resourceIds (fmap (jsonText . outputValue) . SubAgent.waitAnyOutput)
            WaitAll resourceIds ->
              useMany access resourceIds (fmap (jsonText . map outputValue . toList) . SubAgent.waitAllOutputs)
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

        useMany access resourceIds action =
          withSubagents access resourceIds action
            <&> either resourceToolFailure (toolText)
          where
            withSubagents access' (resourceId :| remaining) action' =
              acquire resourceId \subagent ->
                gather (resourceId, subagent) [] remaining
              where
                gather firstSubagent rest [] =
                  Right <$> action' (firstSubagent :| reverse rest)
                gather firstSubagent rest (nextId : nextIds) =
                  acquire nextId \subagent ->
                    gather firstSubagent ((nextId, subagent) : rest) nextIds

                acquire resourceId' callback =
                  Resource.withResource @SubAgent.SubAgent access' resourceId' metadata.parent callback
                    <&> join

        outputValue (resourceId, output) =
          Aeson.object
            [ "resource" Aeson..= resourceId
            , "output" Aeson..= output
            ]

    createSubAgent context metadata requestedName systemPrompt toolNames ttlMinutes =
      case validateTools context toolNames of
        Left err ->
          pure (clientFailure err)
        Right names ->
          createResource requestedName
            Resource.Init
              { message = context.message
              , arguments = SubAgent.SubAgentArgs{systemContext, toolNames = names, ttlMinutes}
              }
            <&> either resourceToolFailure (toolText . ("Subagent created: " <>))
      where
        createResource = \case
          Nothing -> Resource.createForRun @SubAgent.SubAgent metadata.originRunId metadata.parent
          Just name -> Resource.createNamedForRun @SubAgent.SubAgent metadata.originRunId metadata.parent name

        systemContext
          | Text.null (Text.strip systemPrompt) = context.systemContext
          | otherwise = systemPrompt

    validateTools context = traverse validate . List.nub
      where
        validate name =
          case find ((== name) . toolName) availableTools of
            Nothing -> Left ("Unknown tool: " <> name)
            Just definition
              | toolAllowed definition context -> Right name
              | otherwise -> Left ("Tool is not allowed: " <> name)

data Call
  = Create !(Maybe Text) !Text ![Text] !Int
  | ListResources
  | Send !Text !Text
  | Query !Text
  | WaitAny !(NonEmpty Text)
  | WaitAll !(NonEmpty Text)
  | Destroy !Text
  | Rename !Text !Text

resourceArgument :: ToolArgument Text
resourceArgument =
  nonEmptyTextArgument "resource" "Existing subagent resource name."

resourcesArgument :: ToolArgument (NonEmpty Text)
resourcesArgument =
  mapArgument validResources
    (requiredArgument (fieldTextArray "resources" "Non-empty subagent resource names."))

nonEmptyTextArgument :: Text -> Text -> ToolArgument Text
nonEmptyTextArgument name description =
  mapArgument (validText (Text.unpack name)) (requiredText name description)

validResources :: [Text] -> AesonTypes.Parser (NonEmpty Text)
validResources values = do
  valid <- traverse (validText "resources entries") (List.nub values)
  case valid of
    firstValue : rest -> pure (firstValue :| rest)
    [] -> fail "resources must not be empty."

validText :: String -> Text -> AesonTypes.Parser Text
validText label value =
  value <$ when (Text.null (Text.strip value)) (fail (label <> " must not be empty."))

clientFailure :: Text -> ToolResult
clientFailure err = toolFailure (permanentArgumentFailure err err).failure
