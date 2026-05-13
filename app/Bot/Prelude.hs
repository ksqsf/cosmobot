{-|
Module      : Bot.Prelude
Description : Custom prelude
Stability   : experimental
-}

module Bot.Prelude
  ( module Relude
  , module Data.String.Interpolate
  , module Effectful
  , module Effectful.Dispatch.Dynamic
  , module Effectful.Log
  , module Effectful.Exception
  , module Effectful.Fail
  , Stream
  , Of
  , forkEff
  )
where

-- ---------------------------------------------------------------------------
-- Prelude
-- ---------------------------------------------------------------------------

import Relude
import Control.Concurrent (forkIO)
import qualified Control.Exception as Exception

-- ---------------------------------------------------------------------------
-- String
-- ---------------------------------------------------------------------------

import Data.String.Interpolate

-- ---------------------------------------------------------------------------
-- Effects
-- ---------------------------------------------------------------------------

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log
import Effectful.Exception
import Effectful.Fail

-- ---------------------------------------------------------------------------
-- Streams
-- ---------------------------------------------------------------------------

import Streaming (Stream, Of)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

forkEff :: (IOE :> es, Log :> es) => Eff es () -> Eff es ()
forkEff action =
  withEffToIO (ConcUnlift Persistent Unlimited) $ \runInIO ->
    void $ liftIO $ forkIO do
      result <- Exception.try (runInIO action)
      case result of
        Right () ->
          pure ()
        Left err
          | Just Exception.ThreadKilled <- Exception.fromException err ->
              pure ()
          | otherwise ->
              runInIO (logAttention_ [i|Forked action failed: #{show err :: String}|])
