{-|
Module      : Bot.Agent.Middleware.LogContext
Description : Structured logging context for agent runs and tool calls
Stability   : experimental
-}

module Bot.Agent.Middleware.LogContext
  ( withLogContext
  )
where

import Bot.Agent.Core
import Bot.LLM.Types (ToolCall (..))
import Bot.Prelude
import qualified Streaming as Streaming

withLogContext
  :: KatipE :> es
  => Runtime context (Eff es)
  -> Runtime context (Eff es)
withLogContext runtime =
  runtime
    { aroundAgentRun = \context action ->
        Streaming.hoist (katipAddContext (sl "agent_run_id" runtime.runId))
          (runtime.aroundAgentRun context action)
    , aroundToolCall = \turn call@ToolCall{id = callId, name = toolName} context action ->
        katipAddContext
          ( sl "tool_call_id" callId
              <> sl "tool_name" toolName
              <> sl "agent_turn" turn
          )
          (runtime.aroundToolCall turn call context action)
    }
