{-|
Module      : Bot.Storage.Prelude
Description : Local prelude for component-owned Selda storage
Stability   : experimental
-}

module Bot.Storage.Prelude
  ( runSelda
  , runImmediate
  , queryLimit
  , module Database.Selda
  )
where

import Bot.Effect.Storage (runImmediate, runSelda)
import Bot.Prelude (Int)
import Database.Selda hiding (inner, limit, row, text, toString)
import qualified Database.Selda as Selda

queryLimit :: Selda.Same s t => Int -> Int -> Selda.Query (Selda.Inner s) a -> Selda.Query t (Selda.OuterCols a)
queryLimit =
  Selda.limit
