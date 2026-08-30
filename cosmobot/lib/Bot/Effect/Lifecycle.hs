{-|
Module      : Bot.Effect.Lifecycle
Description : Process lifecycle request capability
Stability   : experimental
-}

module Bot.Effect.Lifecycle
  ( Lifecycle (..)
  , requestRestart
  , requestRestartSilently
  )
where

import Bot.Core.Message
import Bot.Prelude

data Lifecycle :: Effect where
  RequestRestart :: IncomingMessage -> Text -> Lifecycle m ()
  RequestRestartSilently :: Lifecycle m ()

type instance DispatchOf Lifecycle = Dynamic

requestRestart :: Lifecycle :> es => IncomingMessage -> Text -> Eff es ()
requestRestart message body =
  send (RequestRestart message body)

requestRestartSilently :: Lifecycle :> es => Eff es ()
requestRestartSilently = send RequestRestartSilently
