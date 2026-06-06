{-|
Module      : Bot.Concurrency.Manager
Description : Queryable ownership model for concurrent work and resources
Stability   : experimental
-}

module Bot.Concurrency.Manager
  ( ResourceId (..)
  , ResourceKind (..)
  , ResourceStatus (..)
  , ResourceInfo (..)
  , ResourceSnapshot (..)
  , Parent (..)
  , rootParent
  , childParent
  , resourceFinished
  )
where

import Bot.Prelude
import Data.Time (UTCTime)

newtype ResourceId = ResourceId
  { unResourceId :: Integer
  }
  deriving stock (Eq, Ord, Show)

data ResourceKind
  = Task
  | Resource
  | Terminal
  | Subagent
  | DriverSession
  | StreamPump
  deriving stock (Eq, Ord, Show)

data ResourceStatus
  = Running
  | Completed
  | Failed !Text
  | Cancelled
  | Released
  deriving stock (Eq, Show)

data Parent
  = Root
  | Child !ResourceId
  deriving stock (Eq, Ord, Show)

data ResourceInfo = ResourceInfo
  { id :: !ResourceId
  , kind :: !ResourceKind
  , label :: !Text
  , parent :: !Parent
  , status :: !ResourceStatus
  , startedAt :: !UTCTime
  , finishedAt :: !(Maybe UTCTime)
  }
  deriving stock (Eq, Show)

data ResourceSnapshot = ResourceSnapshot
  { resources :: ![ResourceInfo]
  }
  deriving stock (Eq, Show)

rootParent :: Parent
rootParent =
  Root

childParent :: ResourceId -> Parent
childParent =
  Child

resourceFinished :: ResourceInfo -> Bool
resourceFinished resource =
  isJust resource.finishedAt
