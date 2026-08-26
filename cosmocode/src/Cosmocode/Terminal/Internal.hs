{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Cosmocode.Terminal.Internal
  ( Terminal (..)
  ) where

import Cosmocode.Types
import Effectful

data Terminal :: Effect where
  RunSessionUi :: Model -> Terminal m ()
  PublishServerEvent :: ServerEvent -> Terminal m ()

type instance DispatchOf Terminal = Dynamic
