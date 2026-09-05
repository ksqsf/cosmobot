{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Util.Image
Description : Shared image helpers
Stability   : experimental
-}

module Bot.Util.Image
  ( removeFilesIfExists
  )
where

import Bot.Prelude
import Effectful.FileSystem (FileSystem, removeFile)

removeFilesIfExists :: (IOE :> es, Fail :> es, FileSystem :> es) => [FilePath] -> Eff es ()
removeFilesIfExists =
  traverse_ removeIfExists

removeIfExists :: (Fail :> es, FileSystem :> es) => FilePath -> Eff es ()
removeIfExists path =
  removeFile path `catchSync` \_ -> pure ()
