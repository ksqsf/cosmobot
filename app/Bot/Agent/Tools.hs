{-|
Module      : Bot.Agent.Tools
Description : Built-in agent tools
Stability   : experimental
-}

module Bot.Agent.Tools
  ( defaultTools
  )
where

import Bot.Agent.Tools.Chat
import Bot.Agent.Tools.Files
import Bot.Agent.Tools.Image
import Bot.Agent.Tools.Memory
import Bot.Agent.Tools.Schedule
import Bot.Agent.Tools.Shell
import Bot.Agent.Tools.Time
import Bot.Agent.Tools.Web
import Bot.Agent.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Scheduler as Scheduler
import Bot.Prelude

-- | Built-in tools exposed to the model after per-message permission checks.
defaultTools :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, IOE :> es) => [Tool es]
defaultTools =
  [ listDirectoryTool
  , readFileTool
  , queryChatLogTool
  , webSearchTool
  , webFetchTool
  , datetimeTool
  , generateImageTool
  , sendReplyTool
  , mentionUserTool
  , senderMemberInfoTool
  , memberInfoTool
  , listGroupMembersTool
  , currentMentionsTool
  , scheduleAgentActionTool
  , deleteScheduledAgentActionTool
  , listCurrentUserSchedulesTool
  , manageMemoryTool
  , manageChatMemoryTool
  , runBashTool
  ]
