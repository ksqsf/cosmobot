{-|
Module      : Bot.Effect.Memory
Description : Persistent memory capability facade
Stability   : experimental
-}

module Bot.Effect.Memory
  ( Memory
  , loadMemory
  , listMemories
  , memoryHistory
  , loadMemoryRevision
  , revertMemory
  , replaceMemory
  , clearMemory
  , runMemory
  )
where

import qualified Bot.Memory as MemoryStore
import Bot.Prelude
import qualified Effectful.Concurrent.MVar as MVar
import Effectful.FileSystem (FileSystem)
import Effectful.Process (Process)

data Memory :: Effect where
  LoadMemory :: MemoryStore.MemoryScope -> Memory m (Maybe Text)
  ListMemories :: Memory m [MemoryStore.MemoryEntry]
  MemoryHistory :: MemoryStore.MemoryScope -> Memory m [MemoryStore.MemoryHistoryEntry]
  LoadMemoryRevision :: MemoryStore.MemoryScope -> MemoryStore.MemoryRevision -> Memory m (Maybe Text)
  RevertMemory :: MemoryStore.MemoryScope -> MemoryStore.MemoryRevision -> Memory m ()
  ReplaceMemory :: MemoryStore.MemoryScope -> MemoryStore.MemoryCommitMessage -> Text -> Memory m ()
  ClearMemory :: MemoryStore.MemoryScope -> MemoryStore.MemoryCommitMessage -> Memory m ()

type instance DispatchOf Memory = Dynamic

loadMemory :: Memory :> es => MemoryStore.MemoryScope -> Eff es (Maybe Text)
loadMemory =
  send . LoadMemory

listMemories :: Memory :> es => Eff es [MemoryStore.MemoryEntry]
listMemories =
  send ListMemories

memoryHistory :: Memory :> es => MemoryStore.MemoryScope -> Eff es [MemoryStore.MemoryHistoryEntry]
memoryHistory =
  send . MemoryHistory

loadMemoryRevision :: Memory :> es => MemoryStore.MemoryScope -> MemoryStore.MemoryRevision -> Eff es (Maybe Text)
loadMemoryRevision scope revision =
  send (LoadMemoryRevision scope revision)

revertMemory :: Memory :> es => MemoryStore.MemoryScope -> MemoryStore.MemoryRevision -> Eff es ()
revertMemory scope revision =
  send (RevertMemory scope revision)

replaceMemory :: Memory :> es => MemoryStore.MemoryScope -> MemoryStore.MemoryCommitMessage -> Text -> Eff es ()
replaceMemory scope message memory =
  send (ReplaceMemory scope message memory)

clearMemory :: Memory :> es => MemoryStore.MemoryScope -> MemoryStore.MemoryCommitMessage -> Eff es ()
clearMemory scope message =
  send (ClearMemory scope message)

runMemory
  :: (Concurrent :> es, FileSystem :> es, IOE :> es, Process :> es)
  => MemoryStore.MemoryConfig
  -> Eff (Memory : es) a
  -> Eff es a
runMemory cfg action = do
  MemoryStore.initializeMemoryRepo cfg
  lock <- MVar.newMVar ()
  interpret (\_ -> \case
    LoadMemory scope ->
      withMemoryLock lock (MemoryStore.loadMemory cfg scope)
    ListMemories ->
      withMemoryLock lock (MemoryStore.listMemories cfg)
    MemoryHistory scope ->
      withMemoryLock lock (MemoryStore.memoryHistory cfg scope)
    LoadMemoryRevision scope revision ->
      withMemoryLock lock (MemoryStore.loadMemoryRevision cfg scope revision)
    RevertMemory scope revision ->
      withMemoryLock lock (MemoryStore.revertMemory cfg scope revision)
    ReplaceMemory scope message memory ->
      withMemoryLock lock (MemoryStore.replaceMemory cfg scope message memory)
    ClearMemory scope message ->
      withMemoryLock lock (MemoryStore.clearMemory cfg scope message)
    ) action

withMemoryLock :: Concurrent :> es => MVar.MVar () -> Eff es a -> Eff es a
withMemoryLock lock operation =
  MVar.withMVar lock (const operation)
