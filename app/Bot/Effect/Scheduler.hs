{-|
Module      : Bot.Effect.Scheduler
Description : Delayed bot actions as an incoming message stream
Stability   : experimental
-}

module Bot.Effect.Scheduler
  ( Scheduler
  , ScheduledMessage (..)
  , scheduleMessage
  , listScheduledMessages
  , scheduledMessages
  , runScheduler
  )
where

import Bot.Message
import Bot.Prelude
import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import GHC.Clock (getMonotonicTimeNSec)
import qualified Streaming as S
import qualified Streaming.Prelude as S

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

-- | In-process delayed message scheduler.
data Scheduler :: Effect where
  ScheduleMessage
    :: Int
    -> IncomingMessage
    -> Scheduler m ()
  ListScheduledMessages
    :: IncomingMessage
    -> Scheduler m [ScheduledMessage]
  ReceiveScheduledMessage
    :: Scheduler m IncomingMessage

type instance DispatchOf Scheduler = Dynamic

-- | Re-inject a message into the incoming stream after a delay in seconds.
scheduleMessage :: Scheduler :> es => Int -> IncomingMessage -> Eff es ()
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

-- | Interpret scheduled messages with an in-memory delay queue.
runScheduler
  :: IOE :> es
  => Eff (Scheduler : es) a
  -> Eff es a
runScheduler inner = do
  chan <- liftIO (Chan.newChan :: IO (Chan.Chan IncomingMessage))
  schedulerStateVar <- liftIO (MVar.newMVar (SchedulerState 1 Map.empty))
  interpret
    (\_ -> \case
      ScheduleMessage delaySeconds message -> do
        scheduleId <- liftIO $ registerPendingMessage schedulerStateVar delaySeconds message
        void $ liftIO $ forkIO do
          threadDelay (max 0 delaySeconds * 1000000)
          MVar.modifyMVar_ schedulerStateVar \schedulerState ->
            pure schedulerState{pendingMessages = Map.delete scheduleId schedulerState.pendingMessages}
          Chan.writeChan chan message
      ListScheduledMessages message -> do
        now <- liftIO getMonotonicTimeNSec
        pending <- liftIO $ MVar.withMVar schedulerStateVar (pure . Map.elems . (.pendingMessages))
        pure
          [ scheduledMessage now pendingMessage
          | pendingMessage <- pending
          , sameMessageOwner message pendingMessage.message
          ]
      ReceiveScheduledMessage ->
        liftIO (Chan.readChan chan))
    inner

registerPendingMessage :: MVar.MVar SchedulerState -> Int -> IncomingMessage -> IO Integer
registerPendingMessage schedulerStateVar delaySeconds message = do
  now <- getMonotonicTimeNSec
  MVar.modifyMVar schedulerStateVar \schedulerState -> do
    let scheduleId = schedulerState.nextScheduleId
        delayNanoseconds = fromIntegral (max 0 delaySeconds) * nanosecondsPerSecond
        pendingMessage = PendingMessage
          { scheduleId = scheduleId
          , dueAtNanoseconds = now + delayNanoseconds
          , message = message
          }
        nextState = schedulerState
          { nextScheduleId = scheduleId + 1
          , pendingMessages = Map.insert scheduleId pendingMessage schedulerState.pendingMessages
          }
    pure (nextState, scheduleId)

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
