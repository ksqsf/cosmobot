{-|
Module      : Bot.Effect.Memory
Description : Application effect for persistent memory
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
  :: IOE :> es
  => MemoryStore.MemoryConfig
  -> Eff (Memory : es) a
  -> Eff es a
runMemory cfg = interpret $ \_ -> \case
  LoadMemory scope ->
    MemoryStore.loadMemory cfg scope
  ReplaceMemory scope memory ->
    MemoryStore.replaceMemory cfg scope memory
  ClearMemory scope ->
    MemoryStore.clearMemory cfg scope
