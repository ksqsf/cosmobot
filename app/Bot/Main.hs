-- | Cosmobot executable wiring configuration, effects, platforms, and routes.
module Bot.Main
  ( main
  , mainWithConfig
  )
where

import Bot.Prelude
import Bot.Config
import Bot.Core.Route
import qualified Bot.Lifecycle as Lifecycle
import qualified Bot.Chat.Driver as ChatDriver
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Skills as Skills
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Effect.Typst as Typst
import qualified Bot.LLM.OpenAI as OpenAI
import Bot.Handler.Admin
import Bot.Handler.Ask
import Bot.Handler.Audit
import Bot.Handler.Safebooru
import Bot.Handler.Saucenao
import Bot.Handler.ShutUp
import Bot.Handler.Scratchpad
import Bot.Handler.Typing
import Bot.Storage.Conversation
import qualified Bot.Storage.SQLite as StorageSQLite
import qualified Bot.System.Typst.CLI as TypstCLI
import qualified Bot.Util.Stream as StreamUtil
import qualified Effectful.Concurrent.MVar as MVar
import Log.Backend.StandardOutput
import qualified System.Posix.Signals as Signals
import Effectful.Timeout
import Effectful.Process
import Effectful.FileSystem

-- | Start the bot using @config.toml@ from the current working directory.
main :: IO ()
main = mainWithConfig "config.toml"

-- | Start the bot using the given TOML config file.
mainWithConfig :: FilePath -> IO ()
mainWithConfig configPath = runEff . runPrim . runFailIO $ do
  cfg <- loadConfig configPath
  conversations <- newConversationStore
  let runStack = runConcurrent
               . runGracefulTermination
               . runTimeout
               . runFileSystem
               . runProcess
               . runConcurrent
               . runBotLog cfg.logLevel
               . StorageSQLite.runStorageSQLitePath cfg.sqlitePath
               . AgentAudit.runAgentAudit
               . ChatLog.runChatLog
               . Memory.runMemory cfg.memory
               . Skills.runSkills cfg.skills
               . Scheduler.runScheduler
               . TypstCLI.runTypst
               . OpenAI.runLLM cfg.llm
               . ChatDriver.runChatDrivers cfg.qq cfg.telegram cfg.matrix cfg.discord
               . Lifecycle.runLifecycle
  runStack do
    logInfo_ "Cosmobot stand by!"
    let allStreams =
          [ ChatDriver.incomingMessages
          , Scheduler.scheduledMessages
          ]
    consumeWith
      (routes cfg conversations)
      (ChatLog.recordIncomingMessages (StreamUtil.mergeStreams allStreams))

routes
  :: ( Chat.Chat :> es, AgentAudit.AgentAudit :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Memory.Memory :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Typst.Typst :> es, Log :> es, Prim :> es, Concurrent :> es, Fail :> es, Timeout :> es, FileSystem :> es, Process :> es, IOE :> es)
  => BotConfig
  -> ConversationStore
  -> [RouteHandler es]
routes cfg conversations =
  shutUpHandlers cfg.handlers.shutup
    <> auditHandlers conversations
    <> adminHandlers cfg.handlers.admin
    <> scratchpadHandlers
    <> typingHandlers
    <> safebooruHandlers
    <> saucenaoHandlers cfg.saucenao
    <> askHandlers cfg.tool cfg.handlers.ask conversations

runBotLog :: IOE :> es => LogLevel -> Eff (Log : es) a -> Eff es a
runBotLog level inner = withStdOutLogger $ \logger ->
  runLog "cosmobot" logger level $ do
    logInfo_ [i|Log level: #{show level :: String}|]
    logExceptions inner

runGracefulTermination :: (IOE :> es, Concurrent :> es) => Eff es a -> Eff es ()
runGracefulTermination inner = do
  shutdown <- MVar.newEmptyMVar
  _termHandler <- installHandler shutdown Signals.sigTERM
  _intHandler  <- installHandler shutdown Signals.sigINT

  -- inner will be canceled by asynchronous exceptions
  race_ inner (MVar.takeMVar shutdown)

  where
    installHandler shutdown signal = liftIO do
      Signals.installHandler signal (Signals.Catch (notify shutdown)) Nothing

    notify shutdown = void . runEff . runConcurrent $ MVar.tryPutMVar shutdown ()
