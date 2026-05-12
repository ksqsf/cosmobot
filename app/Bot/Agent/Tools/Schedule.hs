{-|
Module      : Bot.Agent.Tools.Schedule
Description : Agent scheduler tools
Stability   : experimental
-}

module Bot.Agent.Tools.Schedule
  ( scheduleAgentActionTool
  , deleteScheduledAgentActionTool
  , listCurrentUserSchedulesTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Effect.Scheduler as Scheduler
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes

scheduleAgentActionTool :: Scheduler.Scheduler :> es => Tool es
scheduleAgentActionTool = Tool
  { name = "schedule_agent_action"
  , description = "Schedule a future agent action in the current chat. The future action is processed through the same incoming message pipeline and replies to the current user message."
  , parameters = objectSchema
      [ fieldInteger "delay_seconds" "Delay before running the future agent action, in seconds."
      , fieldText "prompt" "Prompt for the future agent action."
      ]
      ["delay_seconds", "prompt"]
  , allowed = everyone
  , start = \context -> pure \args ->
      withParsedToolArgs scheduledActionArgs args \(delaySeconds, prompt) -> do
        scheduled <- Scheduler.scheduleMessage delaySeconds (scheduledAgentMessage context delaySeconds prompt)
        if scheduled
          then pure (toolText [i|Scheduled agent action in #{delaySeconds} seconds.|])
          else pure (toolText "Could not schedule agent action: scheduler is at capacity.")
  }

deleteScheduledAgentActionTool :: Scheduler.Scheduler :> es => Tool es
deleteScheduledAgentActionTool = Tool
  { name = "delete_scheduled_agent_action"
  , description = "Delete a schedule using schedule ID. Only current user's schedules may be deleted."
  , parameters = objectSchema
    [ fieldInteger "schedule_id" "The schedule ID to be deleted."
    ]
    ["schedule_id"]
  , allowed = everyone
  , start = \context -> pure \args -> withIntegerArg "schedule_id" (\scheduleId -> do
      ok <- Scheduler.deleteScheduledMessage context.message scheduleId
      if ok
        then pure (toolText [i|Schedule #{scheduleId} has been removed.|])
        else pure (toolText [i|Schedule #{scheduleId} is not available to the user.|])
      ) args
  }

listCurrentUserSchedulesTool :: Scheduler.Scheduler :> es => Tool es
listCurrentUserSchedulesTool = Tool
  { name = "list_current_user_schedules"
  , description = "List pending scheduled agent actions created by the current user in the current chat. Returns schedule ids, remaining seconds, and scheduled prompts."
  , parameters = objectSchema [] []
  , allowed = everyone
  , start = \context -> pure \_ -> do
      schedules <- Scheduler.listScheduledMessages context.message
      pure (toolText (jsonText (map scheduleSummary schedules)))
  }

data ScheduleSummary = ScheduleSummary
  { scheduleId :: !Integer
  , remainingSeconds :: !Int
  , prompt :: !Text
  }
  deriving (Show, Generic, Aeson.ToJSON)

scheduleSummary :: Scheduler.ScheduledMessage -> ScheduleSummary
scheduleSummary schedule =
  ScheduleSummary
    { scheduleId = schedule.scheduleId
    , remainingSeconds = schedule.remainingSeconds
    , prompt = scheduledPrompt schedule.message
    }

scheduledPrompt :: IncomingMessage -> Text
scheduledPrompt message =
  fromMaybe message.text (AesonTypes.parseMaybe parsePrompt message.raw)
  where
    parsePrompt =
      Aeson.withObject "scheduled action" (Aeson..: Key.fromText "prompt")

scheduledActionArgs :: Aeson.Value -> AesonTypes.Parser (Int, Text)
scheduledActionArgs =
  Aeson.withObject "scheduled action arguments" $ \o -> do
    delaySeconds <- o Aeson..: Key.fromText "delay_seconds"
    prompt <- o Aeson..: Key.fromText "prompt"
    pure (delaySeconds, prompt)

scheduledAgentMessage :: AgentContext es -> Int -> Text -> IncomingMessage
scheduledAgentMessage context delaySeconds prompt =
  let original = context.message
      commandText = context.askCommand <> " " <> prompt
  in original
      { messageId = original.messageId
      , replyToMessageId = Nothing
      , mentions = original.mentions
      , mentionUsernames = original.mentionUsernames
      , imageUrls = []
      , text = commandText
      , raw = Aeson.object
          [ "type" Aeson..= Aeson.String "scheduled_agent_action"
          , "delay_seconds" Aeson..= delaySeconds
          , "prompt" Aeson..= prompt
          , "original_message" Aeson..= original.raw
          ]
      }
