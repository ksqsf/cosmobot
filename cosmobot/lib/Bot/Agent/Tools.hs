{-|
Module      : Bot.Agent.Tools
Description : Built-in agent tools
Stability   : experimental
-}

module Bot.Agent.Tools
  ( defaultTools
  )
where

import qualified Bot.Agent as Agent
import Bot.Agent.Tools.Chat
import Bot.Agent.Tools.Emacs
import Bot.Agent.Tools.Files
import Bot.Agent.Tools.Image
import Bot.Agent.Tools.Media
import Bot.Agent.Tools.Memory
import Bot.Agent.Tools.Schedule
import Bot.Agent.Tools.Sandbox
import Bot.Agent.Tools.Shell
import Bot.Agent.Tools.Skills
import Bot.Agent.Tools.SubAgent
import Bot.Agent.Tools.Continuation
import Bot.Agent.Tools.Meta
import Bot.Agent.Tools.Terminal
import Bot.Agent.Tools.Time
import Bot.Agent.Tools.Typst
import Bot.Agent.Tools.Web
import Bot.Agent.Tools.Workspace
import Bot.Agent.Tool
import qualified Bot.Effect.ACP as ACP
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Resource as Resource
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Skills as Skills
import qualified Bot.Effect.Typst as Typst
import Bot.Prelude
import Effectful.Timeout
import Effectful.Process
import Effectful.FileSystem

-- | Built-in tools exposed to the model after per-message permission checks.
defaultTools
  :: ACP.ACP :> es
  => AgentAudit.AgentAudit :> es
  => Chat.Chat :> es
  => ChatLog.ChatLog :> es
  => HTTP.HTTP :> es
  => LLM.LLM :> es
  => Media.Media :> es
  => Memory.Memory :> es
  => Resource.Resource :> es
  => Scheduler.Scheduler :> es
  => Skills.Skills :> es
  => Typst.Typst :> es
  => Fail :> es
  => Concurrency.Concurrency :> es
  => Prim :> es
  => Concurrent :> es
  => Timeout :> es
  => KatipE :> es
  => Process :> es
  => FileSystem :> es
  => IOE :> es
  => [Tool es]
defaultTools = tools
  where
    tools =
      [ toolEnableTool
      , acpReadClientFileTool
      , acpWriteClientFileTool
      , queryChatLogTool
      , queryCurrentSenderChatLogTool
      , webSearchTool
      , webFetchTool
      , datetimeTool
      , readMediaTextTool
      , mediaToFileTool
      , viewImageTool
      , generateImageTool
      , editImageTool
      , typstRenderTool
      , sendReplyTool
      , sendFileTool
      , sendMediaTool
      , mentionUserTool
      , senderMemberInfoTool
      , memberInfoTool
      , userAvatarTool
      , listGroupMembersTool
      , currentMessageInfoTool
      , scheduleTool
      , senderMemoryTool
      , chatMemoryTool
      , loadSkillTool
      ]
      <> sandboxTools
      <> [ runBashTool
         , terminalTool
         ]
      <> workspaceTools
      <> [ captureContinuationTool
         , resumeContinuationTool
         ]
      <> subagentTools
          (\metadata subagentId parent ->
            Agent.runObservedChildAgent AgentAudit.agentAuditObserver metadata subagentId parent 8)
          tools
      <> [ emacsEvalTool
         ]
