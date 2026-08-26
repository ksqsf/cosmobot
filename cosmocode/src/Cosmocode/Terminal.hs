{-# LANGUAGE DataKinds #-}
module Cosmocode.Terminal
  ( Terminal
  , runSessionUi
  , publishServerEvent
  ) where

import Cosmocode.Types
import Cosmocode.Terminal.Internal
import Effectful
import Effectful.Dispatch.Dynamic

runSessionUi :: Terminal :> es => Model -> Eff es ()
runSessionUi = send . RunSessionUi

publishServerEvent :: Terminal :> es => ServerEvent -> Eff es ()
publishServerEvent = send . PublishServerEvent
