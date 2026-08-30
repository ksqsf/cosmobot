{-|
Module      : Bot.Effect.Storage
Description : Selda storage capability facade
Stability   : experimental
-}

module Bot.Effect.Storage
  ( Storage(..)
  , runSelda
  , runImmediate
  )
where

import Bot.Prelude
import qualified Database.Selda as Selda
import qualified Database.Selda.SQLite as SeldaSQLite

data Storage :: Effect where
  RunSelda
    :: Selda.SeldaT SeldaSQLite.SQLite IO a
    -> Storage m a
  RunImmediate
    :: Selda.SeldaT SeldaSQLite.SQLite IO a
    -> Storage m a

type instance DispatchOf Storage = Dynamic

runSelda :: Storage :> es => Selda.SeldaT SeldaSQLite.SQLite IO a -> Eff es a
runSelda action =
  send (RunSelda action)

-- | Run a write transaction that acquires SQLite's writer lock before the
-- first statement, avoiding deferred read-to-write lock upgrades.
runImmediate :: Storage :> es => Selda.SeldaT SeldaSQLite.SQLite IO a -> Eff es a
runImmediate action =
  send (RunImmediate action)
