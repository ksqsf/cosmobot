{-|
Module      : Bot.Effect.Lifecycle
Description : Process lifecycle request capability
Stability   : experimental
-}

module Bot.Effect.Lifecycle
  ( Lifecycle (..)
  , requestRestart
  )
where

import Bot.Prelude

data Lifecycle :: Effect where
  RequestRestart :: Lifecycle m ()

type instance DispatchOf Lifecycle = Dynamic

requestRestart :: Lifecycle :> es => Eff es ()
requestRestart =
  send RequestRestart
