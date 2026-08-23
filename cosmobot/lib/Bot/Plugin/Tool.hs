{-|
Module      : Bot.Plugin.Tool
Description : Adapt generation-bound plugin tools to the agent tool registry
Stability   : experimental
-}

module Bot.Plugin.Tool
  ( definitions
  )
where

import qualified Bot.Agent.Tool as AgentTool
import qualified Bot.Agent.Failure as AgentFailure
import qualified Bot.Agent.Types as Agent
import Bot.Agent.Types (Context (..))
import qualified Bot.Effect.Plugin as Plugin
import Bot.Prelude

definitions
  :: Plugin.Plugin :> es
  => [Plugin.PluginTool]
  -> [AgentTool.Tool (Eff es)]
definitions = map definition

definition
  :: Plugin.Plugin :> es
  => Plugin.PluginTool
  -> AgentTool.Tool (Eff es)
definition pluginTool =
  AgentTool.withDescription pluginTool.description $
    AgentTool.tool
      pluginTool.modelName
      (AgentTool.parsedArguments pluginTool.schema pure)
      \arguments -> do
        context <- AgentTool.askToolContext
        toAgentResult <$> Plugin.invokeTool pluginTool context.message arguments

toAgentResult :: Plugin.ToolInvocationResult -> Agent.ToolResult
toAgentResult = \case
  Plugin.ToolInvocationSuccess content imageUrls -> Agent.toolTextWithImages content imageUrls
  Plugin.ToolInvocationFailure kind message detail -> Agent.toolFailure $ case kind of
    Plugin.PermanentArguments -> AgentFailure.permanentArgumentFailure message detail
    Plugin.TransientInvocation -> AgentFailure.transientFailure message detail
