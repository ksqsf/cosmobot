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
import qualified Effectful.Concurrent.MVar as MVar

runStorageSQLite
  :: (IOE :> es, Concurrent :> es)
  => SeldaBackend.SeldaConnection SeldaSQLite.SQLite
  -> Eff (Storage.Storage : es) a
  -> Eff es a
runStorageSQLite seldaConnection inner = do
  seldaLock <- MVar.newMVar ()
  interpret
    ( \_ -> \case
        Storage.RunSelda action ->
          MVar.withMVar seldaLock \_ ->
            liftIO (SeldaBackend.runSeldaT action seldaConnection)
    )
    inner

runStorageSQLitePath
  :: (IOE :> es, Concurrent :> es)
  => FilePath
  -> Eff (Storage.Storage : es) a
  -> Eff es a
runStorageSQLitePath path inner = do
  seldaConnection <- liftIO (SeldaSQLite.sqliteOpen path)
  runStorageSQLite seldaConnection inner
    `finally` liftIO (SeldaBackend.seldaClose seldaConnection)
