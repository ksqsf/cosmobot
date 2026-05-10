{-|
Module      : Bot.Effect.Scheduler
Description : Delayed bot actions as an incoming message stream
Stability   : experimental
-}

module Bot.Effect.Scheduler
  ( Scheduler
  , scheduleMessage
  , scheduledMessages
  , runScheduler
  )
where

import Bot.Message
import Bot.Prelude
import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.Chan as Chan
import qualified Streaming as S
import qualified Streaming.Prelude as S

-- | In-process delayed message scheduler.
data Scheduler :: Effect where
  ScheduleMessage
    :: Int
    -> IncomingMessage
    -> Scheduler m ()
  ReceiveScheduledMessage
    :: Scheduler m IncomingMessage

type instance DispatchOf Scheduler = Dynamic

-- | Re-inject a message into the incoming stream after a delay in seconds.
scheduleMessage :: Scheduler :> es => Int -> IncomingMessage -> Eff es ()
scheduleMessage delaySeconds message =
  send (ScheduleMessage delaySeconds message)

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
  interpret
    (\_ -> \case
      ScheduleMessage delaySeconds message ->
        void $ liftIO $ forkIO do
          threadDelay (max 0 delaySeconds * 1000000)
          Chan.writeChan chan message
      ReceiveScheduledMessage ->
        liftIO (Chan.readChan chan))
    inner
