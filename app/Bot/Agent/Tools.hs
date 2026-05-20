{-|
Module      : Bot.Agent.Tools
Description : Built-in agent tools
Stability   : experimental
-}

module Bot.Agent.Tools
  ( defaultTools
  )
where

import Bot.Agent.Tools.Audio
import Bot.Agent.Tools.Chat
import Bot.Agent.Tools.Emacs
import Bot.Agent.Tools.Files
import Bot.Agent.Tools.Image
import Bot.Agent.Tools.Memory
import Bot.Agent.Tools.Schedule
import Bot.Agent.Tools.Shell
import Bot.Agent.Tools.Time
import Bot.Agent.Tools.Typst
import Bot.Agent.Tools.Web
import Bot.Agent.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Typst as Typst
import Bot.Prelude
import Effectful.Timeout
import Effectful.Process
import Effectful.FileSystem

-- | Built-in tools exposed to the model after per-message permission checks.
defaultTools
  :: Chat.Chat :> es
  => ChatLog.ChatLog :> es
  => LLM.LLM :> es
  => Memory.Memory :> es
  => Scheduler.Scheduler :> es
  => Typst.Typst :> es
  => Fail :> es
  => Concurrent :> es
  => Timeout :> es
  => Log :> es
  => Process :> es
  => FileSystem :> es
  => IOE :> es
  => [Tool es]
defaultTools =
  [ listDirectoryTool
  , readFileTool
  , queryChatLogTool
  , queryCurrentSenderChatLogTool
  , webSearchTool
  , webFetchTool
  , datetimeTool
  , generateAudioTool
  , generateImageTool
  , editImageTool
  , typstToImageTool
  , sendReplyTool
  , sendFileTool
  , mentionUserTool
  , senderMemberInfoTool
  , memberInfoTool
  , userAvatarTool
  , listGroupMembersTool
  , currentMessageInfoTool
  , scheduleAgentActionTool
  , deleteScheduledAgentActionTool
  , listCurrentUserSchedulesTool
  , manageSenderMemoryTool
  , manageChatMemoryTool
  , runBashTool
  , emacsEvalTool
  ]
