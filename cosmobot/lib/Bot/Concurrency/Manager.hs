{-|
Module      : Bot.Concurrency.Manager
Description : Queryable ownership model for concurrent work and resources
Stability   : experimental
-}

module Bot.Concurrency.Manager
  ( ManagedId (..)
  , ManagedKind (..)
  , ManagedStatus (..)
  , ManagedEntry (..)
  , ManagedSnapshot (..)
  , ManagedParent (..)
  , rootManagedParent
  , childManagedParent
  , managedEntryFinished
  )
where

import Bot.Prelude
import Data.Time (UTCTime)

newtype ManagedId = ManagedId
  { unManagedId :: Integer
  }
  deriving stock (Eq, Ord, Show)

data ManagedKind
  = ManagedTask
  | ManagedResource
  | ManagedTerminal
  | ManagedSubagent
  | ManagedDriverSession
  | ManagedStreamPump
  deriving stock (Eq, Ord, Show)

data ManagedStatus
  = ManagedRunning
  | ManagedCompleted
  | ManagedFailed !Text
  | ManagedCancelled
  | ManagedReleased
  deriving stock (Eq, Show)

data ManagedParent
  = RootManaged
  | ChildManaged !ManagedId
  deriving stock (Eq, Ord, Show)

data ManagedEntry = ManagedEntry
  { id :: !ManagedId
  , kind :: !ManagedKind
  , label :: !Text
  , parent :: !ManagedParent
  , status :: !ManagedStatus
  , startedAt :: !UTCTime
  , finishedAt :: !(Maybe UTCTime)
  }
  deriving stock (Eq, Show)

data ManagedSnapshot = ManagedSnapshot
  { entries :: ![ManagedEntry]
  }
  deriving stock (Eq, Show)

rootManagedParent :: ManagedParent
rootManagedParent =
  RootManaged

childManagedParent :: ManagedId -> ManagedParent
childManagedParent =
  ChildManaged

managedEntryFinished :: ManagedEntry -> Bool
managedEntryFinished entry =
  isJust entry.finishedAt
