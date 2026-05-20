{-|
Module      : Bot.Effect.Scheduler
Description : Delayed bot actions as an incoming message stream
Stability   : experimental
-}
{-# LANGUAGE OverloadedLabels #-}

module Bot.Effect.Scheduler
  ( Scheduler
  , ScheduledMessage (..)
  , scheduleMessage
  , deleteScheduledMessage
  , listScheduledMessages
  , scheduledMessages
  , runScheduler
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.Concurrent.STM as STM
import qualified Bot.Storage.Scheduler as SchedulerStorage
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Effectful.Timeout

data ScheduledMessage = ScheduledMessage
  { scheduleId :: !Integer
  , remainingSeconds :: !Int
  , message :: !IncomingMessage
  }
  deriving (Show, Generic, Aeson.ToJSON)

data PendingMessage = PendingMessage
  { scheduleId :: !Integer
  , dueAtUnixSeconds :: !Integer
  , message :: !IncomingMessage
  }

data SchedulerState = SchedulerState
  { nextScheduleId :: !Integer
  , pendingById :: !(Map Integer PendingMessage)
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

-- | Re-inject a message into the incoming stream after a delay in seconds.
scheduleMessage :: Scheduler :> es => Int -> IncomingMessage -> Eff es Bool
scheduleMessage delaySeconds message =
  send (ScheduleMessage delaySeconds message)

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

-- | Interpret scheduled messages with an in-memory delay queue.
runScheduler
  :: (IOE :> es, Storage.Storage :> es, Concurrent :> es, Timeout :> es)
  => Eff (Scheduler : es) a
  -> Eff es a
runScheduler inner = do
  storedMessages <- SchedulerStorage.loadScheduledMessages
  nextScheduleId <- SchedulerStorage.loadNextScheduleId
  queue <- STM.newTBQueueIO scheduledMessageQueueCapacity
  schedulerStateVar <- MVar.newMVar (schedulerStateFromStoredMessages nextScheduleId storedMessages)
  schedulerWake <- MVar.newEmptyMVar
  worker <- forkIO (schedulerWorker schedulerStateVar schedulerWake queue)
  interpret
    (\_ -> \case
      ScheduleMessage delaySeconds message -> do
        scheduled <- registerPendingMessage schedulerStateVar delaySeconds message
        SchedulerStorage.saveScheduledMessage (storedScheduledMessage scheduled)
        signalSchedulerWake schedulerWake
        pure True
      DeleteScheduledMessage message scheduleId -> do
        deleted <- deletePendingMessage schedulerStateVar message scheduleId
        when deleted do
          SchedulerStorage.deleteScheduledMessage scheduleId
          signalSchedulerWake schedulerWake
        pure deleted
      ListScheduledMessages message -> do
        now <- currentUnixSeconds
        pending <- MVar.withMVar schedulerStateVar (pure . Map.elems . (.pendingById))
        pure
          [ scheduledMessage now pendingMessage
          | pendingMessage <- pending
          , sameMessageOwner message pendingMessage.message
          ]
      ReceiveScheduledMessage -> do
        pending <- STM.atomically (STM.readTBQueue queue)
        SchedulerStorage.deleteScheduledMessage pending.scheduleId
        pure pending.message)
    inner
    `finally` killThread worker

schedulerStateFromStoredMessages :: Integer -> [SchedulerStorage.StoredScheduledMessage] -> SchedulerState
schedulerStateFromStoredMessages nextScheduleId storedMessages =
  SchedulerState
    { nextScheduleId
    , pendingById = Map.fromList [(pending.scheduleId, pending) | pending <- pendingMessages]
    , pendingByDue = Set.fromList [PendingDue{dueAtUnixSeconds = pending.dueAtUnixSeconds, scheduleId = pending.scheduleId} | pending <- pendingMessages]
    }
  where
    pendingMessages =
      [ PendingMessage
          { scheduleId = stored.scheduleId
          , dueAtUnixSeconds = stored.dueAtUnixSeconds
          , message = stored.message
          }
      | stored <- storedMessages
      ]

schedulerWorker
  :: (Concurrent :> es, IOE :> es, Timeout :> es)
  => MVar.MVar SchedulerState
  -> MVar.MVar ()
  -> STM.TBQueue PendingMessage
  -> Eff es ()
schedulerWorker schedulerStateVar schedulerWake queue =
  forever do
    now <- currentUnixSeconds
    dueMessages <- popDueMessages schedulerStateVar now
    traverse_ (STM.atomically . STM.writeTBQueue queue) dueMessages
    nextDue <- nextDueAt schedulerStateVar
    case nextDue of
      Nothing ->
        MVar.takeMVar schedulerWake
      Just dueAt ->
        void $ timeout (waitMicrosecondsUntil now dueAt) (MVar.takeMVar schedulerWake)
    drainSchedulerWake schedulerWake

popDueMessages :: Concurrent :> es => MVar.MVar SchedulerState -> Integer -> Eff es [PendingMessage]
popDueMessages schedulerStateVar now =
  MVar.modifyMVar schedulerStateVar \schedulerState -> do
    let (nextState, dueMessages) = popDueMessagesFromState now schedulerState
    pure (nextState, dueMessages)

popDueMessagesFromState :: Integer -> SchedulerState -> (SchedulerState, [PendingMessage])
popDueMessagesFromState now schedulerState =
  go schedulerState []
  where
    go current acc =
      case Set.minView current.pendingByDue of
        Nothing ->
          (current, reverse acc)
        Just (due, rest)
          | due.dueAtUnixSeconds > now ->
              (current, reverse acc)
          | otherwise ->
              let (pendingMessage, nextById) =
                    case Map.lookup due.scheduleId current.pendingById of
                      Nothing ->
                        (Nothing, current.pendingById)
                      Just pending ->
                        (Just pending, Map.delete due.scheduleId current.pendingById)
                  nextState = current{pendingById = nextById, pendingByDue = rest}
              in go nextState (maybe acc (: acc) pendingMessage)

nextDueAt :: (Concurrent :> es) => MVar.MVar SchedulerState -> Eff es (Maybe Integer)
nextDueAt schedulerStateVar =
  MVar.withMVar schedulerStateVar \schedulerState ->
    pure ((.dueAtUnixSeconds) <$> Set.lookupMin schedulerState.pendingByDue)

waitMicrosecondsUntil :: Integer -> Integer -> Int
waitMicrosecondsUntil now dueAt
  | dueAt <= now = 0
  | otherwise =
      fromIntegral (min maxWaitMicroseconds ((dueAt - now) * 1000000))
  where
    maxWaitMicroseconds = toInteger (maxBound :: Int)

signalSchedulerWake :: Concurrent :> es => MVar.MVar () -> Eff es ()
signalSchedulerWake schedulerWake =
  void (MVar.tryPutMVar schedulerWake ())

drainSchedulerWake :: Concurrent :> es => MVar.MVar () -> Eff es ()
drainSchedulerWake schedulerWake = do
  value <- MVar.tryTakeMVar schedulerWake
  when (isJust value) (drainSchedulerWake schedulerWake)

registerPendingMessage ::
  (Concurrent :> es, IOE :> es)
  => MVar.MVar SchedulerState -> Int -> IncomingMessage -> Eff es PendingMessage
registerPendingMessage schedulerStateVar delaySeconds message = do
  now <- currentUnixSeconds
  MVar.modifyMVar schedulerStateVar \schedulerState -> do
    let scheduleId = schedulerState.nextScheduleId
        dueAt = now + fromIntegral (max 0 delaySeconds)
        pendingMessage = PendingMessage
          { scheduleId = scheduleId
          , dueAtUnixSeconds = dueAt
          , message = message
          }
        due = PendingDue
          { dueAtUnixSeconds = dueAt
          , scheduleId = scheduleId
          }
        nextState = schedulerState
          { nextScheduleId = scheduleId + 1
          , pendingById = Map.insert scheduleId pendingMessage schedulerState.pendingById
          , pendingByDue = Set.insert due schedulerState.pendingByDue
          }
    pure (nextState, pendingMessage)

deletePendingMessage :: Concurrent :> es => MVar.MVar SchedulerState -> IncomingMessage -> Integer -> Eff es Bool
deletePendingMessage schedulerStateVar message scheduleId =
  MVar.modifyMVar schedulerStateVar \schedulerState ->
    case Map.lookup scheduleId schedulerState.pendingById of
      Nothing ->
        pure (schedulerState, False)
      Just schedule
        | sameMessageOwner message schedule.message ->
            let due = PendingDue
                  { dueAtUnixSeconds = schedule.dueAtUnixSeconds
                  , scheduleId = scheduleId
                  }
                nextState = schedulerState
                  { pendingById = Map.delete scheduleId schedulerState.pendingById
                  , pendingByDue = Set.delete due schedulerState.pendingByDue
                  }
            in pure (nextState, True)
        | otherwise ->
            pure (schedulerState, False)

scheduledMessage :: Integer -> PendingMessage -> ScheduledMessage
scheduledMessage now pending =
  ScheduledMessage
    { scheduleId = pending.scheduleId
    , remainingSeconds = remainingSecondsUntil now pending.dueAtUnixSeconds
    , message = pending.message
    }

storedScheduledMessage :: PendingMessage -> SchedulerStorage.StoredScheduledMessage
storedScheduledMessage pending =
  SchedulerStorage.StoredScheduledMessage
    { scheduleId = pending.scheduleId
    , dueAtUnixSeconds = pending.dueAtUnixSeconds
    , message = pending.message
    }

remainingSecondsUntil :: Integer -> Integer -> Int
remainingSecondsUntil now dueAt
  | dueAt <= now = 0
  | otherwise =
      fromIntegral (dueAt - now)

sameMessageOwner :: IncomingMessage -> IncomingMessage -> Bool
sameMessageOwner left right =
  left.platform == right.platform
    && left.kind == right.kind
    && left.chatId == right.chatId
    && sameSender left right

sameSender :: IncomingMessage -> IncomingMessage -> Bool
sameSender left right =
  case (left.senderId, right.senderId) of
    (Just leftId, Just rightId) ->
      leftId == rightId
    (Nothing, Nothing) ->
      left.senderUsername == right.senderUsername
    _ ->
      False

currentUnixSeconds :: IOE :> es => Eff es Integer
currentUnixSeconds = liftIO $
  floor <$> getPOSIXTime

scheduledMessageQueueCapacity :: Natural
scheduledMessageQueueCapacity =
  1024
