{-|
Module      : Bot.Effect.Concurrency
Description : Queryable concurrency and runtime resource capability facade
Stability   : experimental
-}

module Bot.Effect.Concurrency
  ( Concurrency (..)
  , ResourceId (..)
  , ResourceKind (..)
  , ResourceStatus (..)
  , ResourceInfo (..)
  , ResourceHandle (..)
  , ResourceSnapshot (..)
  , Parent (..)
  , rootParent
  , childParent
  , resourceFinished
  , spawnResource
  , registerResource
  , releaseResource
  , cancelResource
  , awaitResource
  , listResources
  , lookupResource
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

newtype ResourceHandle = ResourceHandle
  { resourceId :: ResourceId
  }
  deriving stock (Eq, Ord, Show)

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

data Concurrency :: Effect where
  SpawnResource :: ResourceKind -> Text -> m () -> Concurrency m ResourceHandle
  RegisterResource :: ResourceKind -> Text -> m () -> Concurrency m ResourceHandle
  ReleaseResource :: ResourceId -> Concurrency m Bool
  CancelResource :: ResourceId -> Concurrency m Bool
  AwaitResource :: ResourceHandle -> Concurrency m ()
  ListResources :: Concurrency m ResourceSnapshot
  LookupResource :: ResourceId -> Concurrency m (Maybe ResourceInfo)

type instance DispatchOf Concurrency = Dynamic

spawnResource
  :: Concurrency :> es
  => ResourceKind
  -> Text
  -> Eff es ()
  -> Eff es ResourceHandle
spawnResource kind label action =
  send (SpawnResource kind label action)

registerResource
  :: Concurrency :> es
  => ResourceKind
  -> Text
  -> Eff es ()
  -> Eff es ResourceHandle
registerResource kind label cleanup =
  send (RegisterResource kind label cleanup)

releaseResource :: Concurrency :> es => ResourceId -> Eff es Bool
releaseResource =
  send . ReleaseResource

cancelResource :: Concurrency :> es => ResourceId -> Eff es Bool
cancelResource =
  send . CancelResource

awaitResource :: Concurrency :> es => ResourceHandle -> Eff es ()
awaitResource =
  send . AwaitResource

listResources :: Concurrency :> es => Eff es ResourceSnapshot
listResources =
  send ListResources

lookupResource :: Concurrency :> es => ResourceId -> Eff es (Maybe ResourceInfo)
lookupResource =
  send . LookupResource
