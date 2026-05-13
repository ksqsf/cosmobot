{-|
Module      : Bot.Effect.Storage
Description : Application storage capability
Stability   : experimental
-}

module Bot.Effect.Storage
  ( Storage
  , runStorageSQLite
  , runStorageSQLitePath
  , runSelda
  )
where

import Bot.Prelude
import qualified Database.Selda as Selda
import qualified Database.Selda.Backend as SeldaBackend
import qualified Database.Selda.SQLite as SeldaSQLite

data Storage :: Effect where
  RunSelda
    :: Selda.SeldaT SeldaSQLite.SQLite IO a
    -> Storage m a

type instance DispatchOf Storage = Dynamic

runStorageSQLite
  :: IOE :> es
  => SeldaBackend.SeldaConnection SeldaSQLite.SQLite
  -> Eff (Storage : es) a
  -> Eff es a
runStorageSQLite seldaConnection =
  interpret \_ -> \case
    RunSelda action ->
      liftIO (SeldaBackend.runSeldaT action seldaConnection)

runStorageSQLitePath
  :: IOE :> es
  => FilePath
  -> Eff (Storage : es) a
  -> Eff es a
runStorageSQLitePath path inner = do
  seldaConnection <- liftIO (SeldaSQLite.sqliteOpen path)
  runStorageSQLite seldaConnection inner
    `finally` liftIO (SeldaBackend.seldaClose seldaConnection)

runSelda :: Storage :> es => Selda.SeldaT SeldaSQLite.SQLite IO a -> Eff es a
runSelda action =
  send (RunSelda action)
