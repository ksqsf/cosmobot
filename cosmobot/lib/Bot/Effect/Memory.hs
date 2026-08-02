{-|
Module      : Bot.Effect.Memory
Description : Persistent memory capability facade
Stability   : experimental
-}

module Bot.Effect.Memory
  ( Memory
  , loadMemory
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
  ReplaceMemory :: MemoryStore.MemoryScope -> Text -> Memory m ()
  ClearMemory :: MemoryStore.MemoryScope -> Memory m ()

type instance DispatchOf Memory = Dynamic

loadMemory :: Memory :> es => MemoryStore.MemoryScope -> Eff es (Maybe Text)
loadMemory =
  send . LoadMemory

replaceMemory :: Memory :> es => MemoryStore.MemoryScope -> Text -> Eff es ()
replaceMemory scope memory =
  send (ReplaceMemory scope memory)

clearMemory :: Memory :> es => MemoryStore.MemoryScope -> Eff es ()
clearMemory =
  send . ClearMemory

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
      MemoryStore.loadMemory cfg scope
    ReplaceMemory scope memory ->
      MVar.withMVar lock $ \_ ->
        MemoryStore.replaceMemory cfg scope memory
    ClearMemory scope ->
      MVar.withMVar lock $ \_ ->
        MemoryStore.clearMemory cfg scope
    ) action
