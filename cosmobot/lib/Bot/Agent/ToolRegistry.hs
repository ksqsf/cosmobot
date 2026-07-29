{-|
Module      : Bot.Agent.ToolRegistry
Description : Per-run tool registry and tool-call dispatch
Stability   : experimental
-}

module Bot.Agent.ToolRegistry
  ( RunningTool (..)
  , resolveToolSchemas
  , runToolCall
  , startToolRun
  )
where

import Bot.Agent.Tool
import Bot.Agent.Types
import Bot.Core.Transcript (Transcript)
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.MVar as MVar
import System.IO.Error (userError)

-- | A tool runner bound to one agent run.
data RunningTool es = RunningTool
  { name  :: !Text
  , noisy :: !Bool
  , currentSchema :: !(MVar.MVar (Maybe LLM.FunctionTool))
  , resolveSchema :: Transcript -> Int -> Eff es (Maybe LLM.FunctionTool)
  , run  :: ToolCallMetadata -> Aeson.Value -> Eff es ToolResult
  }

-- | Start a tool for this agent run.
startToolRun :: Concurrent :> es => AgentContext -> Tool es -> Eff es (RunningTool es)
startToolRun context definition = do
  currentSchema <- MVar.newMVar Nothing
  run <- startTool definition context
  let name = toolName definition
      toolNoisy = toolIsNoisy definition
      resolveSchema = resolveToolSchema definition context
  pure RunningTool{name, noisy = toolNoisy, currentSchema, resolveSchema, run}

-- | Resolve and freeze the schemas visible in one model turn.
resolveToolSchemas
  :: Concurrent :> es
  => Transcript
  -> Int
  -> [RunningTool es]
  -> Eff es [LLM.FunctionTool]
resolveToolSchemas transcript turn runningTools = do
  schemas <- catMaybes <$> traverse resolve runningTools
  case duplicateNames schemas of
    [] ->
      pure schemas
    duplicates ->
      throwIO (userError [i|Duplicate resolved tool names: #{Text.intercalate ", " duplicates}|])
  where
    resolve runningTool = do
      schema <- runningTool.resolveSchema transcript turn
      MVar.modifyMVar_ runningTool.currentSchema (const (pure schema))
      pure schema

duplicateNames :: [LLM.FunctionTool] -> [Text]
duplicateNames schemas =
  mapMaybe listToMaybe
    . filter ((> 1) . length)
    . group
    . sort
    $ map (.name) schemas

-- | Resolve a model tool call, decode its JSON arguments, and invoke the
-- per-run runner.
runToolCall
  :: Concurrent :> es
  => AgentContext
  -> ToolCallMetadata
  -> [Tool es]
  -> [RunningTool es]
  -> LLM.ToolCall
  -> Eff es ToolResult
runToolCall context metadata tools runningTools call =
  findRunningTool call.name runningTools >>= \case
    Nothing ->
      case find ((== call.name) . toolName) tools of
        Just definition | not (toolAllowed definition context) ->
          pure (toolFailure (permissionDeniedFailure [i|Permission denied for tool: #{callName}|] [i|Tool #{callName} is not allowed in this agent context.|]).failure)
        _ ->
          pure (toolFailure (permanentArgumentFailure [i|Unknown tool: #{callName}|] [i|The model requested an unknown tool: #{callName}|]).failure)
    Just runningTool ->
      case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 call.arguments) of
        Left err ->
          pure (toolFailure (permanentArgumentFailure [i|Invalid JSON arguments for #{callName}: #{err}|] [i|Invalid JSON arguments for #{callName}: #{err}|]).failure)
        Right args ->
          runningTool.run metadata args
  where
    callName = call.name

findRunningTool :: Concurrent :> es => Text -> [RunningTool es] -> Eff es (Maybe (RunningTool es))
findRunningTool _ [] =
  pure Nothing
findRunningTool name (runningTool : rest) = do
  schema <- MVar.readMVar runningTool.currentSchema
  if runningTool.name == name && isJust schema
    then pure (Just runningTool)
    else findRunningTool name rest
