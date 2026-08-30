{-|
Module      : Bot.Scheduler.Interpreter
Description : Scheduler effect interpreter
Stability   : experimental
-}

module Bot.Scheduler.Interpreter
  ( runScheduler
  , runSchedulerWith
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Scheduler.State
import Bot.Scheduler.Types
import qualified Bot.Storage.Scheduler as SchedulerStorage
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.Concurrent.STM as STM
import Effectful.Timeout

-- | Interpret scheduled messages with an in-memory delay queue.
runScheduler
  :: (IOE :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, Concurrent :> es, Timeout :> es)
  => Eff (Scheduler : es) a
  -> Eff es a
runScheduler =
  runSchedulerWith pure

runSchedulerWith
  :: (IOE :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, Concurrent :> es, Timeout :> es)
  => (IncomingMessage -> Eff es IncomingMessage)
  -> Eff (Scheduler : es) a
  -> Eff es a
runSchedulerWith prepareMessage inner = do
  storedMessages <- SchedulerStorage.loadScheduledMessages
  queue <- STM.newTBQueueIO scheduledMessageQueueCapacity
  schedulerStateVar <- MVar.newMVar (schedulerStateFromStoredMessages storedMessages)
  schedulerWake <- MVar.newEmptyMVar
  Concurrency.withWorker "scheduler.worker" (schedulerWorker schedulerStateVar schedulerWake queue) $
    interpret
      (\_ -> \case
        ScheduleMessage delaySeconds recurringIntervalSeconds message -> do
          _ <- persistPendingMessage schedulerStateVar delaySeconds recurringIntervalSeconds message
          signalSchedulerWake schedulerWake
          pure True
        DeleteScheduledMessage message scheduleId -> do
          deleted <- deletePersistedPendingMessage schedulerStateVar message scheduleId
          when deleted (signalSchedulerWake schedulerWake)
          pure deleted
        DeleteScheduledMessageById scheduleId -> do
          deleted <- deletePersistedPendingMessageById schedulerStateVar scheduleId
          when deleted (signalSchedulerWake schedulerWake)
          pure deleted
        ListScheduledMessages message -> do
          now <- currentUnixSeconds
          pending <- MVar.withMVar schedulerStateVar (pure . Map.elems . (.pendingById))
          pure
            [ scheduledMessage now pendingMessage
            | pendingMessage <- pending
            , sameMessageOwner message pendingMessage.message
            ]
        ListAllScheduledMessages -> do
          now <- currentUnixSeconds
          map (scheduledMessage now) . Map.elems . (.pendingById) <$> MVar.readMVar schedulerStateVar
        ReceiveScheduledMessage -> do
          pending <- receivePendingMessage schedulerStateVar queue
          when (isNothing pending.recurringIntervalSeconds) $
            SchedulerStorage.deleteScheduledMessage pending.scheduleId
          prepareMessage pending.message)
      inner

receivePendingMessage
  :: Concurrent :> es
  => MVar.MVar SchedulerState
  -> STM.TBQueue PendingMessage
  -> Eff es PendingMessage
receivePendingMessage schedulerStateVar queue = do
  pending <- STM.atomically (STM.readTBQueue queue)
  case pending.recurringIntervalSeconds of
    Nothing -> pure pending
    Just _ -> do
      active <- MVar.withMVar schedulerStateVar (pure . Map.member pending.scheduleId . (.pendingById))
      if active then pure pending else receivePendingMessage schedulerStateVar queue

schedulerWorker
  :: (Concurrent :> es, IOE :> es, Storage.Storage :> es, Timeout :> es)
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

popDueMessages :: (Concurrent :> es, Storage.Storage :> es) => MVar.MVar SchedulerState -> Integer -> Eff es [PendingMessage]
popDueMessages schedulerStateVar now =
  MVar.modifyMVarMasked schedulerStateVar \schedulerState -> do
    let (nextState, dueMessages) = popDueMessagesFromState now schedulerState
    let recurringSchedules =
          [ (pending.scheduleId, next.dueAtUnixSeconds)
          | pending <- dueMessages
          , isJust pending.recurringIntervalSeconds
          , Just next <- [Map.lookup pending.scheduleId nextState.pendingById]
          ]
    unless (null recurringSchedules) $
      SchedulerStorage.rescheduleScheduledMessages recurringSchedules
    pure (nextState, dueMessages)

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

persistPendingMessage ::
  (Concurrent :> es, IOE :> es, Storage.Storage :> es)
  => MVar.MVar SchedulerState -> Int -> Maybe Int -> IncomingMessage -> Eff es PendingMessage
persistPendingMessage schedulerStateVar delaySeconds recurringIntervalSeconds message = do
  now <- currentUnixSeconds
  let dueAt = now + fromIntegral (max 0 delaySeconds)
  MVar.modifyMVarMasked schedulerStateVar \schedulerState -> do
    stored <- SchedulerStorage.createScheduledMessage dueAt recurringIntervalSeconds message
    pure (rememberStoredMessage stored schedulerState)

deletePersistedPendingMessage
  :: (Concurrent :> es, Storage.Storage :> es)
  => MVar.MVar SchedulerState
  -> IncomingMessage
  -> Integer
  -> Eff es Bool
deletePersistedPendingMessage schedulerStateVar message scheduleId =
  MVar.modifyMVarMasked schedulerStateVar \schedulerState -> do
    let (nextState, deleted) = deletePendingMessageFromState message scheduleId schedulerState
    when deleted (SchedulerStorage.deleteScheduledMessage scheduleId)
    pure (nextState, deleted)

deletePersistedPendingMessageById
  :: (Concurrent :> es, Storage.Storage :> es)
  => MVar.MVar SchedulerState
  -> Integer
  -> Eff es Bool
deletePersistedPendingMessageById schedulerStateVar scheduleId =
  MVar.modifyMVarMasked schedulerStateVar \schedulerState -> do
    let (nextState, deleted) = deletePendingMessageByIdFromState scheduleId schedulerState
    when deleted (SchedulerStorage.deleteScheduledMessage scheduleId)
    pure (nextState, deleted)

currentUnixSeconds :: IOE :> es => Eff es Integer
currentUnixSeconds = liftIO $
  floor <$> getPOSIXTime

scheduledMessageQueueCapacity :: Natural
scheduledMessageQueueCapacity =
  1024
