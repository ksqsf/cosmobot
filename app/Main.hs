{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cosmobot executable wiring configuration, effects, platforms, and routes.
module Main (main) where

import Bot.Prelude
import Bot.Config
import Bot.Core.Route
import qualified Bot.Chat.Driver as ChatDriver
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Effect.Typst as Typst
import Bot.Handler.Ask
import Bot.Handler.Audit
import Bot.Handler.Saucenao
import Bot.Handler.Scratchpad
import Bot.Handler.Typing
import Bot.Storage.Conversation
import qualified Bot.Util.Stream as StreamUtil
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import Log.Backend.StandardOutput
import qualified System.Posix.Signals as Signals

-- | Start the bot using @config.toml@ from the current working directory.
main :: IO ()
main = withShutdownSignal \shutdown -> do
  cfg <- loadConfig "config.toml"
  conversations <- newConversationStore
  runEff $
    runBotLog cfg.logLevel .
    Storage.runStorageSQLitePath cfg.sqlitePath .
    AgentAudit.runAgentAudit .
    ChatLog.runChatLog .
    Memory.runMemory cfg.memory .
    Scheduler.runScheduler .
    Typst.runTypst .
    LLM.runLLM cfg.llm .
    ChatDriver.runChatDrivers cfg.qq cfg.telegram cfg.matrix $ do
      runUntilShutdown shutdown do
        logInfo_ "Cosmobot stand by!"
        let allStreams =
              [ ChatDriver.incomingMessages
              , Scheduler.scheduledMessages
              ]
        consumeWith
          (routes cfg conversations)
          (ChatLog.recordIncomingMessages (StreamUtil.mergeStreams allStreams))

withShutdownSignal :: (MVar.MVar () -> IO ()) -> IO ()
withShutdownSignal action =
  Exception.bracket installHandlers restoreHandlers (action . (.shutdown))
  where
    installHandlers = do
      shutdown <- MVar.newEmptyMVar
      termHandler <- install shutdown Signals.sigTERM
      intHandler <- install shutdown Signals.sigINT
      pure ShutdownHandlers{shutdown, termHandler, intHandler}

    install shutdown signal =
      Signals.installHandler signal (Signals.Catch (void (MVar.tryPutMVar shutdown ()))) Nothing

    restoreHandlers ShutdownHandlers{termHandler, intHandler} = do
      void $ Signals.installHandler Signals.sigTERM termHandler Nothing
      void $ Signals.installHandler Signals.sigINT intHandler Nothing

data ShutdownHandlers = ShutdownHandlers
  { shutdown :: !(MVar.MVar ())
  , termHandler :: !Signals.Handler
  , intHandler :: !Signals.Handler
  }

runUntilShutdown :: (Log :> es, IOE :> es) => MVar.MVar () -> Eff es () -> Eff es ()
runUntilShutdown shutdown action =
  withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
    liftIO $
      Async.race_
        (runInIO action)
        (MVar.takeMVar shutdown >> runInIO (logInfo_ "Shutdown requested; stopping cosmobot."))

routes
  :: (Chat.Chat :> es, AgentAudit.AgentAudit :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Memory.Memory :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Typst.Typst :> es, Log :> es, IOE :> es)
  => BotConfig
  -> ConversationStore
  -> [RouteHandler es]
routes cfg conversations =
  auditHandlers conversations
    <> scratchpadHandlers
    <> typingHandlers
    <> saucenaoHandlers cfg.saucenao
    <> askHandlers cfg.tool cfg.handlers.ask conversations

runBotLog :: IOE :> es => LogLevel -> Eff (Log : es) a -> Eff es a
runBotLog level inner = withStdOutLogger $ \logger ->
  runLog "cosmobot" logger level $ do
    logExceptions inner
