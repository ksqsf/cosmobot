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
import qualified Bot.Media.Cache as MediaCache
import qualified Bot.Media.Config as MediaConfig
import Bot.Prelude
import qualified Bot.Storage.Lifecycle as LifecycleStorage
import Effectful.FileSystem (FileSystem)

runLifecycle
  :: (Chat.Chat :> es, Storage.Storage :> es, FileSystem :> es, Concurrent :> es, IOE :> es, KatipE :> es)
  => MediaConfig.Config
  -> Eff es a
  -> Eff es a
runLifecycle mediaConfig inner =
  withMediaGc mediaConfig $
    bracket_ runStartupActions runShutdownActions inner

runStartupActions
  :: (Chat.Chat :> es, Storage.Storage :> es, KatipE :> es)
  => Eff es ()
runStartupActions = do
  actions <- LifecycleStorage.loadStartupActions
  for_ actions \action@LifecycleStorage.StartupReply{actionId, message, body} -> do
    result <- trySync $ Chat.replyTo message body `finally` LifecycleStorage.deleteStartupAction action
    case result of
      Right response -> do
        logInfo [i|Ran startup reply lifecycle action #{actionId}; response=#{show response :: Text}|]
      Left err -> do
        logWarning [i|Startup reply lifecycle action #{actionId} failed and was deleted: #{show err :: String}|]

withMediaGc
  :: (Storage.Storage :> es, FileSystem :> es, Concurrent :> es, IOE :> es, KatipE :> es)
  => MediaConfig.Config
  -> Eff es a
  -> Eff es a
withMediaGc mediaConfig inner
  | not mediaConfig.gc.enabled =
      inner
  | otherwise = do
      worker <- forkIO (mediaGcLoop mediaConfig)
      inner `finally` killThread worker

mediaGcLoop
  :: (Storage.Storage :> es, FileSystem :> es, Concurrent :> es, IOE :> es, KatipE :> es)
  => MediaConfig.Config
  -> Eff es ()
mediaGcLoop mediaConfig =
  forever do
    runMediaGc mediaConfig
    threadDelay (hoursToMicroseconds (max 1 mediaConfig.gc.intervalHours))

runMediaGc
  :: (Storage.Storage :> es, FileSystem :> es, IOE :> es, KatipE :> es)
  => MediaConfig.Config
  -> Eff es ()
runMediaGc mediaConfig = do
  let maxAgeSeconds = daysToSeconds (max 0 mediaConfig.gc.olderThanDays)
      cacheConfig = MediaCache.CacheConfig{directory = mediaConfig.cacheDir}
  result <- trySync (MediaCache.gcMediaCache cacheConfig maxAgeSeconds)
  case result of
    Right deleted ->
      when (deleted > 0) $
        logInfo [i|Media cache GC deleted #{deleted} file(s)|]
    Left err ->
      logWarning [i|Media cache GC failed: #{show err :: String}|]

daysToSeconds :: Int -> Int
daysToSeconds days =
  days * 24 * 60 * 60

hoursToMicroseconds :: Int -> Int
hoursToMicroseconds hours =
  hours * 60 * 60 * 1000000

runShutdownActions :: KatipE :> es => Eff es ()
runShutdownActions =
  pure ()
