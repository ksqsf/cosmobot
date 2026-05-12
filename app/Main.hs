{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cosmobot executable wiring configuration, effects, platforms, and routes.
module Main (main) where

import Bot.Config
import Bot.Core.Conversation
import qualified Bot.Chat.Driver as ChatDriver
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Scheduler as Scheduler
import Bot.Core.Route
import Bot.Handler.Ask
import Bot.Handler.Saucenao
import Bot.Handler.Scratchpad
import Bot.Handler.Typing
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Storage.SQLite as SQLiteStorage
import Control.Concurrent (ThreadId, forkIO, killThread)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import Log.Backend.StandardOutput
import qualified Streaming as S
import qualified Streaming.Prelude as S

-- | Start the bot using @config.toml@ from the current working directory.
main :: IO ()
main = do
  cfg <- loadConfig "config.toml"
  sqliteStore <- SQLiteStorage.openSQLiteStore cfg.sqlitePath
  let maybeSQLiteStore = Just sqliteStore
  conversations <- newConversationStore maybeSQLiteStore
  runEff $
    runBotLog cfg.logLevel .
    ChatLog.runChatLog maybeSQLiteStore .
    Memory.runMemory cfg.memory .
    Scheduler.runScheduler $
      LLM.runLLM cfg.llm $
        ChatDriver.runChatDrivers cfg.qq cfg.telegram cfg.matrix \chatMessageStreams -> do
          logInfo_ "Cosmobot stand by!"
          let messageStreams =
                chatMessageStreams <> [Scheduler.scheduledMessages]
          consumeWith
            (routes cfg sqliteStore conversations)
            (recordedIncomingMessages (mergeIncomingMessages messageStreams))

routes
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Memory.Memory :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => BotConfig
  -> SQLiteStorage.SQLiteStore
  -> ConversationStore
  -> [RouteHandler es]
routes cfg sqliteStore conversations =
  scratchpadHandlers sqliteStore
    <> typingHandlers
    <> saucenaoHandlers cfg.saucenao
    <> askHandlers cfg.tool cfg.handlers.ask conversations

recordedIncomingMessages
  :: ChatLog.ChatLog :> es
  => Stream (Of IncomingMessage) (Eff es) ()
  -> Stream (Of IncomingMessage) (Eff es) ()
recordedIncomingMessages =
  S.mapM \message -> do
    ChatLog.recordMessage message
    pure message

mergeIncomingMessages
  :: (Log :> es, IOE :> es)
  => [Stream (Of IncomingMessage) (Eff es) ()]
  -> Stream (Of IncomingMessage) (Eff es) ()
mergeIncomingMessages streams = do
  queue <- S.lift (liftIO (TBQueue.newTBQueueIO incomingMessageQueueCapacity :: IO (TBQueue.TBQueue IncomingMessage)))
  pumpThreads <- S.lift $ withEffToIO (ConcUnlift Persistent Unlimited) $ \runInIO ->
    traverse (forkIO . runInIO . pump queue) streams
  readMerged queue `streamFinally` cleanupPumpThreads pumpThreads

readMerged
  :: IOE :> es
  => TBQueue.TBQueue IncomingMessage
  -> Stream (Of IncomingMessage) (Eff es) ()
readMerged queue =
  forever do
    message <- S.lift (liftIO (STM.atomically (TBQueue.readTBQueue queue)))
    S.yield message

cleanupPumpThreads :: IOE :> es => [ThreadId] -> Eff es ()
cleanupPumpThreads =
  traverse_ (liftIO . killThread)

streamFinally
  :: Stream (Of a) (Eff es) r
  -> Eff es ()
  -> Stream (Of a) (Eff es) r
streamFinally stream cleanup =
  go stream
  where
    go current = do
      next <- S.lift (S.next current `onException` cleanup)
      case next of
        Left result -> do
          S.lift cleanup
          pure result
        Right (item, rest) -> do
          S.yield item
          go rest

pump
  :: (Log :> es, IOE :> es)
  => TBQueue.TBQueue IncomingMessage
  -> Stream (Of IncomingMessage) (Eff es) ()
  -> Eff es ()
pump queue stream =
  S.mapM_ (liftIO . STM.atomically . TBQueue.writeTBQueue queue) stream
    `catch` \(err :: SomeException) ->
      logInfo "Incoming message stream stopped" (show err :: String)

incomingMessageQueueCapacity :: Natural
incomingMessageQueueCapacity =
  1024

runBotLog :: IOE :> es => LogLevel -> Eff (Log : es) a -> Eff es a
runBotLog level inner = withStdOutLogger $ \logger ->
  runLog "cosmobot" logger level $ do
    logExceptions inner
