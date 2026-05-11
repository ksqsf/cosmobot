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
import Bot.Prelude
import Control.Concurrent (forkIO, killThread)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import GHC.Clock (getMonotonicTimeNSec)
import Optics ((%~), (.~))
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified System.Timeout as Timeout

data ScheduledMessage = ScheduledMessage
  { scheduleId :: !Integer
  , remainingSeconds :: !Int
  , message :: !IncomingMessage
  }
  deriving (Show, Generic, Aeson.ToJSON)

data PendingMessage = PendingMessage
  { scheduleId :: !Integer
  , dueAtNanoseconds :: !Word64
  , message :: !IncomingMessage
  }

data SchedulerState = SchedulerState
  { nextScheduleId :: !Integer
  , pendingMessages :: !(Map Integer PendingMessage)
  }
  deriving (Generic)

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
  :: IOE :> es
  => Eff (Scheduler : es) a
  -> Eff es a
runScheduler inner = do
  queue <- liftIO (TBQueue.newTBQueueIO scheduledMessageQueueCapacity :: IO (TBQueue.TBQueue IncomingMessage))
  schedulerStateVar <- liftIO (MVar.newMVar (SchedulerState 1 Map.empty))
  schedulerWake <- liftIO MVar.newEmptyMVar
  worker <- liftIO $ forkIO (schedulerWorker schedulerStateVar schedulerWake queue)
  interpret
    (\_ -> \case
      ScheduleMessage delaySeconds message -> do
        registered <- liftIO $ registerPendingMessage schedulerStateVar delaySeconds message
        when (isJust registered) do
          liftIO (signalSchedulerWake schedulerWake)
        pure (isJust registered)
      DeleteScheduledMessage message scheduleId -> do
        deleted <- liftIO $ deletePendingMessage schedulerStateVar message scheduleId
        when deleted do
          liftIO (signalSchedulerWake schedulerWake)
        pure deleted
      ListScheduledMessages message -> do
        now <- liftIO getMonotonicTimeNSec
        pending <- liftIO $ MVar.withMVar schedulerStateVar (pure . Map.elems . (.pendingMessages))
        pure
          [ scheduledMessage now pendingMessage
          | pendingMessage <- pending
          , sameMessageOwner message pendingMessage.message
          ]
      ReceiveScheduledMessage ->
        liftIO (STM.atomically (TBQueue.readTBQueue queue)))
    inner
    `finally` liftIO (killThread worker)

schedulerWorker
  :: MVar.MVar SchedulerState
  -> MVar.MVar ()
  -> TBQueue.TBQueue IncomingMessage
  -> IO ()
schedulerWorker schedulerStateVar schedulerWake queue =
  forever do
    now <- getMonotonicTimeNSec
    dueMessages <- popDueMessages schedulerStateVar now
    traverse_ (STM.atomically . TBQueue.writeTBQueue queue) dueMessages
    nextDue <- nextDueAt schedulerStateVar
    case nextDue of
      Nothing ->
        MVar.takeMVar schedulerWake
      Just dueAt ->
        void $ Timeout.timeout (waitMicrosecondsUntil now dueAt) (MVar.takeMVar schedulerWake)
    drainSchedulerWake schedulerWake

popDueMessages :: MVar.MVar SchedulerState -> Word64 -> IO [IncomingMessage]
popDueMessages schedulerStateVar now =
  MVar.modifyMVar schedulerStateVar \schedulerState -> do
    let (due, pending) = Map.partition ((<= now) . (.dueAtNanoseconds)) schedulerState.pendingMessages
    pure (schedulerState{pendingMessages = pending}, map (.message) (Map.elems due))

nextDueAt :: MVar.MVar SchedulerState -> IO (Maybe Word64)
nextDueAt schedulerStateVar =
  MVar.withMVar schedulerStateVar \schedulerState ->
    pure case map (.dueAtNanoseconds) (Map.elems schedulerState.pendingMessages) of
      [] -> Nothing
      firstDue : rest -> Just (foldl' min firstDue rest)

waitMicrosecondsUntil :: Word64 -> Word64 -> Int
waitMicrosecondsUntil now dueAt
  | dueAt <= now = 0
  | otherwise =
      fromIntegral (min maxWaitMicroseconds ((dueAt - now + 999) `div` 1000))
  where
    maxWaitMicroseconds = fromIntegral (maxBound :: Int)

signalSchedulerWake :: MVar.MVar () -> IO ()
signalSchedulerWake schedulerWake =
  void (MVar.tryPutMVar schedulerWake ())

drainSchedulerWake :: MVar.MVar () -> IO ()
drainSchedulerWake schedulerWake = do
  value <- MVar.tryTakeMVar schedulerWake
  when (isJust value) (drainSchedulerWake schedulerWake)

registerPendingMessage :: MVar.MVar SchedulerState -> Int -> IncomingMessage -> IO (Maybe Integer)
registerPendingMessage schedulerStateVar delaySeconds message = do
  now <- getMonotonicTimeNSec
  MVar.modifyMVar schedulerStateVar \schedulerState -> do
    if Map.size schedulerState.pendingMessages >= maxPendingScheduledMessages
      then pure (schedulerState, Nothing)
      else do
        let scheduleId = schedulerState.nextScheduleId
            delayNanoseconds = fromIntegral (max 0 delaySeconds) * nanosecondsPerSecond
            pendingMessage = PendingMessage
              { scheduleId = scheduleId
              , dueAtNanoseconds = now + delayNanoseconds
              , message = message
              }
            nextState =
              schedulerState
                & #nextScheduleId .~ scheduleId + 1
                & #pendingMessages %~ Map.insert scheduleId pendingMessage
        pure (nextState, Just scheduleId)

deletePendingMessage :: MVar.MVar SchedulerState -> IncomingMessage -> Integer -> IO Bool
deletePendingMessage schedulerStateVar message scheduleId =
  MVar.modifyMVar schedulerStateVar \schedulerState ->
    case Map.lookup scheduleId schedulerState.pendingMessages of
      Nothing ->
        pure (schedulerState, False)
      Just schedule
        | sameMessageOwner message schedule.message ->
            pure (schedulerState & #pendingMessages %~ Map.delete scheduleId, True)
        | otherwise ->
            pure (schedulerState, False)

scheduledMessage :: Word64 -> PendingMessage -> ScheduledMessage
scheduledMessage now pending =
  ScheduledMessage
    { scheduleId = pending.scheduleId
    , remainingSeconds = remainingSecondsUntil now pending.dueAtNanoseconds
    , message = pending.message
    }

remainingSecondsUntil :: Word64 -> Word64 -> Int
remainingSecondsUntil now dueAt
  | dueAt <= now = 0
  | otherwise =
      fromIntegral ((dueAt - now + nanosecondsPerSecond - 1) `div` nanosecondsPerSecond)

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

nanosecondsPerSecond :: Word64
nanosecondsPerSecond =
  1000000000

scheduledMessageQueueCapacity :: Natural
scheduledMessageQueueCapacity =
  1024

maxPendingScheduledMessages :: Int
maxPendingScheduledMessages =
  1024
