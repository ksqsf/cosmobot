{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cosmobot executable wiring configuration, effects, platforms, and routes.
module Main (main) where

import Bot.Prelude
import Bot.Config
import Bot.Core.Conversation
import Bot.Core.Route
import qualified Bot.Chat.Driver as ChatDriver
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Scheduler as Scheduler
import Bot.Handler.Ask
import Bot.Handler.Saucenao
import Bot.Handler.Scratchpad
import Bot.Handler.Typing
import qualified Bot.Storage.SQLite as SQLiteStorage
import qualified Bot.Util.Stream as StreamUtil
import Log.Backend.StandardOutput

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
    Scheduler.runScheduler .
    LLM.runLLM cfg.llm .
    ChatDriver.runChatDrivers cfg.qq cfg.telegram cfg.matrix $ do
      logInfo_ "Cosmobot stand by!"
      let allStreams =
            [ ChatDriver.incomingMessages
            , Scheduler.scheduledMessages
            ]
      consumeWith
        (routes cfg sqliteStore conversations)
        (ChatLog.recordIncomingMessages (StreamUtil.mergeStreams allStreams))

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

runBotLog :: IOE :> es => LogLevel -> Eff (Log : es) a -> Eff es a
runBotLog level inner = withStdOutLogger $ \logger ->
  runLog "cosmobot" logger level $ do
    logExceptions inner
