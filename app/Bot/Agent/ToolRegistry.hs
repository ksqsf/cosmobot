{-|
Module      : Bot.Agent.ToolRegistry
Description : Per-run tool registry and tool-call dispatch
Stability   : experimental
-}

module Bot.Agent.ToolRegistry
  ( RunningTool (..)
  , runToolCall
  , startToolRun
  , toolAllowed
  , toolSchema
  )
where

import Bot.Agent.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as TextEncoding

-- | A tool runner bound to one agent run.
data RunningTool es = RunningTool
  { name :: !Text
  , run  :: Aeson.Value -> Eff es ToolResult
  }

toolSchema :: Tool es -> LLM.FunctionTool
toolSchema Tool{name, description, parameters} =
  LLM.FunctionTool
    { name = name
    , description = description
    , parameters = parameters
    }

-- | Start a tool for this agent run.
startToolRun :: Chat.Chat :> es => AgentContext es -> AgentHooks es -> Tool es -> Eff es (RunningTool es)
startToolRun context hooks Tool{name, start} = do
  run <- Chat.runChatRecordingSelfMessages hooks.recordSelfMessage (start context)
  pure RunningTool{name, run = \args -> Chat.runChatRecordingSelfMessages hooks.recordSelfMessage (run args)}

-- | Resolve a model tool call, decode its JSON arguments, and invoke the
-- per-run runner.
runToolCall
  :: AgentContext es
  -> [Tool es]
  -> [RunningTool es]
  -> LLM.ToolCall
  -> Eff es ToolResult
runToolCall context tools runningTools call =
  case find ((== call.name) . (.name)) runningTools of
    Nothing ->
      case find ((== call.name) . (.name)) tools of
        Just tool | not (toolAllowed tool context) ->
          pure (toolFailure (permissionDeniedFailure [i|Permission denied for tool: #{callName}|] [i|Tool #{callName} is not allowed in this agent context.|]).failure)
        _ ->
          pure (toolFailure (permanentArgumentFailure [i|Unknown tool: #{callName}|] [i|The model requested an unknown tool: #{callName}|]).failure)
    Just tool ->
      case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 call.arguments) of
        Left err ->
          pure (toolFailure (permanentArgumentFailure [i|Invalid JSON arguments for #{callName}: #{err}|] [i|Invalid JSON arguments for #{callName}: #{err}|]).failure)
        Right args ->
          tool.run args
  where
    callName = call.name

toolAllowed :: Tool es -> AgentContext es -> Bool
toolAllowed tool context =
  tool.allowed context
