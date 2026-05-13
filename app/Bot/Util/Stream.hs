{-|
Module      : Bot.Util.Stream
Description : Small stream helpers
Stability   : experimental
-}

module Bot.Util.Stream
  ( mergeStreams
  )
where

import Bot.Prelude
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import qualified Streaming as S
import qualified Streaming.Prelude as S

-- | Merge several streams into one stream through a bounded FIFO queue.
--
-- The @streaming-concurrency@ package provides @Streaming.Concurrent@ with a
-- similar @withMergedStreams@ helper, but its current Hackage release depends
-- on older @streaming-with@ / @streaming-bytestring@ bounds that do not solve
-- with this project's GHC 9.6 toolchain. Keep this small local version until
-- that dependency chain is usable here.
mergeStreams
  :: (Log :> es, IOE :> es)
  => [Stream (Of a) (Eff es) ()]
  -> Stream (Of a) (Eff es) ()
mergeStreams streams = do
  queue <- S.lift (liftIO (TBQueue.newTBQueueIO streamQueueCapacity :: IO (TBQueue.TBQueue (MergeEvent a))))
  shuttingDown <- S.lift (liftIO (STM.newTVarIO False))
  pumps <- S.lift $ withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
    traverse (liftIO . Async.async . runInIO . pump shuttingDown queue) streams
  readMerged (length streams) queue `streamFinally` cleanupPumps shuttingDown pumps

streamQueueCapacity :: Natural
streamQueueCapacity =
  1024

data MergeEvent a
  = MergeItem !a
  | MergeDone
  | MergeFailed !SomeException

data MergeState = MergeState
  { remaining :: !Int
  , failures :: !Int
  , lastFailure :: !(Maybe SomeException)
  }

readMerged
  :: (Log :> es, IOE :> es)
  => Int
  -> TBQueue.TBQueue (MergeEvent a)
  -> Stream (Of a) (Eff es) ()
readMerged streamCount queue =
  go MergeState{remaining = streamCount, failures = 0, lastFailure = Nothing}
  where
    go mergeState
      | mergeState.remaining <= 0 =
          finishMerged mergeState
      | otherwise = do
          event <- S.lift (liftIO (STM.atomically (TBQueue.readTBQueue queue)))
          case event of
            MergeItem item -> do
              S.yield item
              go mergeState
            MergeDone ->
              go mergeState{remaining = mergeState.remaining - 1}
            MergeFailed err ->
              go mergeState
                { remaining = mergeState.remaining - 1
                , failures = mergeState.failures + 1
                , lastFailure = Just err
                }

    finishMerged mergeState
      | mergeState.failures == streamCount
      , Just err <- mergeState.lastFailure = do
          S.lift (logAttention_ [i|All merged stream inputs failed: #{show err :: String}|])
          S.lift (liftIO (Exception.throwIO err))
      | otherwise =
          pure ()

streamFinally
  :: Stream (Of a) (Eff es) r
  -> Eff es ()
  -> Stream (Of a) (Eff es) r
streamFinally stream cleanup =
  go stream
  where
    go current = do
      next <- S.lift (S.next current `onException` cleanup)
      case next of
        Left result -> do
          S.lift cleanup
          pure result
        Right (item, rest) -> do
          S.yield item
          go rest

cleanupPumps :: IOE :> es => STM.TVar Bool -> [Async.Async ()] -> Eff es ()
cleanupPumps shuttingDown pumps = do
  liftIO $ STM.atomically (STM.writeTVar shuttingDown True)
  traverse_ (liftIO . Async.cancel) pumps

pump
  :: (Log :> es, IOE :> es)
  => STM.TVar Bool
  -> TBQueue.TBQueue (MergeEvent a)
  -> Stream (Of a) (Eff es) ()
  -> Eff es ()
pump shuttingDown queue stream =
  (S.mapM_ (writeMergeEvent queue . MergeItem) stream *> writeMergeEvent queue MergeDone)
    `catch` \(err :: SomeException) ->
      if isStreamCancelled err
        then do
          isShuttingDown <- liftIO $ STM.readTVarIO shuttingDown
          unless isShuttingDown (writeMergeEvent queue MergeDone)
        else do
          logInfo_ [i|Merged stream input stopped: #{show err :: String}|]
          writeMergeEvent queue (MergeFailed err)

isStreamCancelled :: SomeException -> Bool
isStreamCancelled err =
  Just Exception.ThreadKilled == Exception.fromException err
    || Just Async.AsyncCancelled == Exception.fromException err

writeMergeEvent :: IOE :> es => TBQueue.TBQueue (MergeEvent a) -> MergeEvent a -> Eff es ()
writeMergeEvent queue =
  liftIO . STM.atomically . TBQueue.writeTBQueue queue
