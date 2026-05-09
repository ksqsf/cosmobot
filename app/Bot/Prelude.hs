{-
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
  )
where

-- ---------------------------------------------------------------------------
-- Prelude
-- ---------------------------------------------------------------------------

import Relude

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
