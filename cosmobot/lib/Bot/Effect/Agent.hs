{-# LANGUAGE DataKinds #-}

{-|
Module      : Bot.Effect.Agent
Description : Re-entrant agent execution capability
Stability   : experimental
-}
module Bot.Effect.Agent
  ( Agent (..)
  , ChildRun (..)
  , registerChild
  , consumeChild
  , wait
  , hasPendingChildren
  , withRun
  )
where

import Bot.Agent.Types (BotRuntime)
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Effectful.Concurrent.MVar as MVar
import Bot.Prelude

data ChildRun = ChildRun
  { name :: !Text
  , worker :: !Concurrency.Handle
  , output :: !(MVar.MVar Text)
  }

data Agent :: Effect where
  Wait :: m a -> Agent m (Maybe a)
  HasPendingChildren :: Text -> Agent m Bool
  RegisterChild :: Text -> ChildRun -> Agent m ()
  ConsumeChild :: Text -> Concurrency.Handle -> Agent m ()
  RunAgent
    :: !(BotRuntime '[] m)
    -> (BotRuntime '[] m -> m a)
    -> Agent m a

type instance DispatchOf Agent = Dynamic

withRun
  :: Agent :> es
  => BotRuntime '[] (Eff es)
  -> (BotRuntime '[] (Eff es) -> Eff es a)
  -> Eff es a
withRun runtime =
  send . RunAgent runtime

registerChild :: Agent :> es => Text -> ChildRun -> Eff es ()
registerChild parent = send . RegisterChild parent

consumeChild :: Agent :> es => Text -> Concurrency.Handle -> Eff es ()
consumeChild parent = send . ConsumeChild parent

-- Only suspend observation; callers retain ownership of the underlying work.
wait :: Agent :> es => Eff es a -> Eff es (Maybe a)
wait = send . Wait

hasPendingChildren :: Agent :> es => Text -> Eff es Bool
hasPendingChildren = send . HasPendingChildren
