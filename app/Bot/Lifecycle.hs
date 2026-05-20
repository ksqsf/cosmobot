{-|
Module      : Bot.Lifecycle
Description : Process lifecycle hooks
Stability   : experimental
-}
{-# LANGUAGE TypeApplications #-}

module Bot.Lifecycle
  ( runLifecycle
  )
where

import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import qualified Bot.Storage.Lifecycle as LifecycleStorage

runLifecycle
  :: (Chat.Chat :> es, Storage.Storage :> es, Log :> es)
  => Eff es a
  -> Eff es a
runLifecycle inner =
  bracket_ runStartupActions runShutdownActions inner

runStartupActions
  :: (Chat.Chat :> es, Storage.Storage :> es, Log :> es)
  => Eff es ()
runStartupActions = do
  actions <- LifecycleStorage.loadStartupActions
  for_ actions \action@LifecycleStorage.StartupReply{actionId, message, body} -> do
    result <- trySync $ Chat.replyTo message body `finally` LifecycleStorage.deleteStartupAction action
    case result of
      Right response -> do
        logInfo_ [i|Ran startup reply lifecycle action #{actionId}; response=#{show response :: Text}|]
      Left err -> do
        logAttention_ [i|Startup reply lifecycle action #{actionId} failed and was deleted: #{show err :: String}|]

runShutdownActions :: Log :> es => Eff es ()
runShutdownActions =
  pure ()
