{-|
Module      : Bot.Storage.SQLite
Description : SQLite interpreter for application storage
Stability   : experimental
-}

module Bot.Storage.SQLite
  ( runStorageSQLite
  , runStorageSQLitePath
  )
where

import Bot.Prelude
import qualified Bot.Effect.Storage as Storage
import qualified Database.Selda.Backend as SeldaBackend
import qualified Database.Selda.SQLite as SeldaSQLite

runStorageSQLite
  :: IOE :> es
  => SeldaBackend.SeldaConnection SeldaSQLite.SQLite
  -> Eff (Storage.Storage : es) a
  -> Eff es a
runStorageSQLite seldaConnection =
  interpret \_ -> \case
    Storage.RunSelda action ->
      liftIO (SeldaBackend.runSeldaT action seldaConnection)

runStorageSQLitePath
  :: IOE :> es
  => FilePath
  -> Eff (Storage.Storage : es) a
  -> Eff es a
runStorageSQLitePath path inner = do
  seldaConnection <- liftIO (SeldaSQLite.sqliteOpen path)
  runStorageSQLite seldaConnection inner
    `finally` liftIO (SeldaBackend.seldaClose seldaConnection)
