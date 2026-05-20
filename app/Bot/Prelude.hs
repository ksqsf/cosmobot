{-|
Module      : Bot.Prelude
Description : Custom prelude
Stability   : experimental
-}

module Bot.Prelude
  ( module Relude
  , module Data.String.Interpolate
  , module Effectful
  , module Effectful.Concurrent
  , module Effectful.Concurrent.Async
  , module Effectful.Dispatch.Dynamic
  , module Effectful.Log
  , module Effectful.Exception
  , module Effectful.Fail
  , module Effectful.Prim.IORef
  , Stream
  , Of
  , spawnTask
  )
where

-- ---------------------------------------------------------------------------
-- Prelude
-- ---------------------------------------------------------------------------

import Relude hiding
  ( newIORef
  , readIORef
  , writeIORef
  , modifyIORef
  , modifyIORef'
  , atomicWriteIORef
  , atomicModifyIORef
  , atomicModifyIORef'
  , IORef
  )

-- ---------------------------------------------------------------------------
-- String
-- ---------------------------------------------------------------------------

import Data.String.Interpolate

-- ---------------------------------------------------------------------------
-- Effects
-- ---------------------------------------------------------------------------

import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Dispatch.Dynamic
import Effectful.Log
import Effectful.Exception
import Effectful.Fail
import Effectful.Prim.IORef

-- ---------------------------------------------------------------------------
-- Streams
-- ---------------------------------------------------------------------------

import Streaming (Stream, Of)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

spawnTask :: (Log :> es, Concurrent :> es) => Eff es () -> Eff es ()
spawnTask action = void $ forkIO do
  try action >>= \case
    Right () -> pure ()
    Left err
      | Just ThreadKilled <- fromException err ->
        pure ()
      | otherwise -> logAttention_ [i|Forked action failed: #{show err :: String}|]
