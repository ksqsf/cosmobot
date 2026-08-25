{-|
Module      : Bot.Scheduler.Types
Description : Scheduler domain values
Stability   : experimental
-}

module Bot.Scheduler.Types
  ( Scheduler (..)
  , ScheduledMessage (..)
  , PendingMessage (..)
  , SchedulerState (..)
  , PendingDue (..)
  , scheduleOneShotMessage
  , scheduleRecurringMessage
  , deleteScheduledMessage
  , listScheduledMessages
  , scheduledMessages
  , receiveScheduledMessage
  )
where

import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Streaming as S
import qualified Streaming.Prelude as S

data ScheduledMessage = ScheduledMessage
  { scheduleId :: !Integer
  , remainingSeconds :: !Int
  , recurring :: !Bool
  , message :: !IncomingMessage
  }
  deriving (Show, Generic, Aeson.ToJSON)

data PendingMessage = PendingMessage
  { scheduleId :: !Integer
  , dueAtUnixSeconds :: !Integer
  , recurringIntervalSeconds :: !(Maybe Int)
  , message :: !IncomingMessage
  }

data SchedulerState = SchedulerState
  { pendingById :: !(Map Integer PendingMessage)
  , pendingByDue :: !(Set PendingDue)
  }
  deriving (Generic)

data PendingDue = PendingDue
  { dueAtUnixSeconds :: !Integer
  , scheduleId :: !Integer
  }
  deriving (Eq, Ord)

-- | In-process delayed message scheduler.
data Scheduler :: Effect where
  ScheduleMessage
    :: Int
    -> Maybe Int
    -> IncomingMessage
    -> Scheduler m Bool
  ListScheduledMessages
    :: IncomingMessage
    -> Scheduler m [ScheduledMessage]
  ReceiveScheduledMessage
    :: Scheduler m IncomingMessage
  DeleteScheduledMessage
    :: IncomingMessage
    -> Integer
    -> Scheduler m Bool

type instance DispatchOf Scheduler = Dynamic

-- | Re-inject a message once after a delay in seconds.
scheduleOneShotMessage :: Scheduler :> es => Int -> IncomingMessage -> Eff es Bool
scheduleOneShotMessage delaySeconds message =
  send (ScheduleMessage delaySeconds Nothing message)

-- | Re-inject a message after every positive delay interval until deleted.
scheduleRecurringMessage :: Scheduler :> es => Int -> IncomingMessage -> Eff es Bool
scheduleRecurringMessage delaySeconds message =
  send (ScheduleMessage delaySeconds (Just (max 1 delaySeconds)) message)

-- | Return pending scheduled messages owned by the same platform chat sender.
listScheduledMessages :: Scheduler :> es => IncomingMessage -> Eff es [ScheduledMessage]
listScheduledMessages =
  send . ListScheduledMessages

-- | Stream of messages whose delay has elapsed.
scheduledMessages :: Scheduler :> es => Stream (Of IncomingMessage) (Eff es) ()
scheduledMessages = do
  message <- S.lift receiveScheduledMessage
  S.yield message
  scheduledMessages

receiveScheduledMessage :: Scheduler :> es => Eff es IncomingMessage
receiveScheduledMessage =
  send ReceiveScheduledMessage

-- | Delete a scheduled message with ID. Returns True if there is such an ID.
deleteScheduledMessage :: Scheduler :> es => IncomingMessage -> Integer -> Eff es Bool
deleteScheduledMessage message schedId = send (DeleteScheduledMessage message schedId)
