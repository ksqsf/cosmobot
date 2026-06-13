{-|
Module      : Bot.Effect.Concurrency
Description : Queryable concurrency capability facade
Stability   : experimental
-}

module Bot.Effect.Concurrency
  ( Concurrency (..)
  , Id (..)
  , Kind (..)
  , Status (..)
  , Info (..)
  , Handle (..)
  , Snapshot (..)
  , Parent (..)
  , rootParent
  , childParent
  , finished
  , fire
  , fireWithHandle
  , fork
  , withWorker
  , forkStreamPump
  , raceTasks_
  , forkAs
  , forkWithHandleAs
  , register
  , release
  , cancel
  , await
  , sleepMicroseconds
  , list
  , lookup
  )
where

import Bot.Prelude hiding (Handle)
import Data.Time (UTCTime)
import qualified Effectful.Concurrent.MVar as MVar

newtype Id = Id
  { unId :: Integer
  }
  deriving stock (Eq, Ord, Show)

data Kind
  = Task
  | Registration
  | Terminal
  | Subagent
  | DriverSession
  | StreamPump
  deriving stock (Eq, Ord, Show)

data Status
  = Running
  | Completed
  | Failed !Text
  | Cancelled
  | Released
  deriving stock (Eq, Show)

data Parent
  = Root
  | Child !Id
  deriving stock (Eq, Ord, Show)

data Info = Info
  { id :: !Id
  , kind :: !Kind
  , label :: !Text
  , parent :: !Parent
  , status :: !Status
  , startedAt :: !UTCTime
  , finishedAt :: !(Maybe UTCTime)
  }
  deriving stock (Eq, Show)

newtype Handle = Handle
  { handleId :: Id
  }
  deriving stock (Eq, Ord, Show)

data Snapshot = Snapshot
  { entries :: ![Info]
  }
  deriving stock (Eq, Show)

rootParent :: Parent
rootParent =
  Root

childParent :: Id -> Parent
childParent =
  Child

finished :: Info -> Bool
finished info =
  isJust info.finishedAt

fire
  :: Concurrency :> es
  => Text
  -> Eff es ()
  -> Eff es ()
fire label =
  void . fork label

fireWithHandle
  :: Concurrency :> es
  => Text
  -> (Handle -> Eff es ())
  -> Eff es ()
fireWithHandle label =
  void . forkWithHandleAs Task label

fork
  :: Concurrency :> es
  => Text
  -> Eff es ()
  -> Eff es Handle
fork =
  forkAs Task

withWorker
  :: Concurrency :> es
  => Text
  -> Eff es ()
  -> Eff es a
  -> Eff es a
withWorker label worker inner = do
  workerHandle <- forkAs Task label worker
  inner `finally` cancelAndAwait workerHandle

forkStreamPump
  :: Concurrency :> es
  => Text
  -> Eff es ()
  -> Eff es Handle
forkStreamPump =
  forkAs StreamPump

raceTasks_
  :: (Concurrency :> es, Concurrent :> es, IOE :> es)
  => Text
  -> Eff es ()
  -> Text
  -> Eff es ()
  -> Eff es ()
raceTasks_ leftLabel leftAction rightLabel rightAction = do
  done <- MVar.newEmptyMVar
  left <- fork leftLabel (capture done leftAction)
  right <- fork rightLabel (capture done rightAction)
  let cancelBoth = do
        cancelAndAwait left
        cancelAndAwait right
  result <- MVar.takeMVar done `finally` cancelBoth
  either throwIO pure result
  where
    capture
      :: (Concurrent :> es, IOE :> es)
      => MVar.MVar (Either SomeException ())
      -> Eff es ()
      -> Eff es ()
    capture done action =
      try action >>= void . MVar.tryPutMVar done

cancelAndAwait :: Concurrency :> es => Handle -> Eff es ()
cancelAndAwait workerHandle = do
  void (cancel workerHandle.handleId)
  await workerHandle

data Concurrency :: Effect where
  Fork :: Kind -> Text -> m () -> Concurrency m Handle
  ForkWithHandle :: Kind -> Text -> (Handle -> m ()) -> Concurrency m Handle
  Register :: Kind -> Text -> m () -> Concurrency m Handle
  Release :: Id -> Concurrency m Bool
  Cancel :: Id -> Concurrency m Bool
  Await :: Handle -> Concurrency m ()
  SleepMicroseconds :: Int -> Concurrency m ()
  List :: Concurrency m Snapshot
  Lookup :: Id -> Concurrency m (Maybe Info)

type instance DispatchOf Concurrency = Dynamic

forkAs
  :: Concurrency :> es
  => Kind
  -> Text
  -> Eff es ()
  -> Eff es Handle
forkAs kind label action =
  send (Fork kind label action)

forkWithHandleAs
  :: Concurrency :> es
  => Kind
  -> Text
  -> (Handle -> Eff es ())
  -> Eff es Handle
forkWithHandleAs kind label action =
  send (ForkWithHandle kind label action)

register
  :: Concurrency :> es
  => Kind
  -> Text
  -> Eff es ()
  -> Eff es Handle
register kind label cleanup =
  send (Register kind label cleanup)

release :: Concurrency :> es => Id -> Eff es Bool
release =
  send . Release

cancel :: Concurrency :> es => Id -> Eff es Bool
cancel =
  send . Cancel

await :: Concurrency :> es => Handle -> Eff es ()
await =
  send . Await

sleepMicroseconds :: Concurrency :> es => Int -> Eff es ()
sleepMicroseconds =
  send . SleepMicroseconds

list :: Concurrency :> es => Eff es Snapshot
list =
  send List

lookup :: Concurrency :> es => Id -> Eff es (Maybe Info)
lookup =
  send . Lookup
