-- | Cosmobot executable wiring configuration, effects, platforms, and routes.
module Bot.Main
  ( main
  , mainWithConfig
  )
where

import Bot.Prelude
import Bot.Config
import qualified Bot.Concurrency.Manager as ConcurrencyManager
import Bot.Core.Route
import qualified Bot.Lifecycle as Lifecycle
import qualified Bot.Chat.Driver as ChatDriver
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as MediaEffect
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Skills as Skills
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Effect.Typst as Typst
import qualified Bot.LLM.OpenAI as OpenAI
import qualified Bot.Media.Interpreter as Media
import qualified Bot.RPC.Audit as RPCAudit
import qualified Bot.RPC.Config as RPCConfig
import qualified Bot.RPC.Server as RPCServer
import qualified Bot.RPC.State as RPC
import qualified Data.Aeson as Aeson
import Bot.Handler.Admin
import Bot.Handler.Ask
import Bot.Handler.Audit
import Bot.Handler.Safebooru
import Bot.Handler.Saucenao
import Bot.Handler.ShutUp
import Bot.Handler.Scratchpad
import Bot.Handler.Typing
import qualified Bot.HTTP as HTTP
import Bot.Storage.Thread
import qualified Bot.Storage.SQLite as StorageSQLite
import qualified Bot.System.Typst.CLI as TypstCLI
import qualified Bot.Util.Stream as StreamUtil
import qualified Effectful.Concurrent.MVar as MVar
import qualified System.Posix.Signals as Signals
import Effectful.Timeout
import Effectful.Process
import Effectful.FileSystem
import qualified Effectful.Ki as Ki

-- | Start the bot using @config.toml@ from the current working directory.
main :: IO ()
main = mainWithConfig "config.toml"

-- | Start the bot using the given TOML config file.
mainWithConfig :: FilePath -> IO ()
mainWithConfig configPath = runEff . runPrim . runFailIO $ do
  cfg <- loadConfig configPath
  threads <- newThreadStore
  rpcState <- runConcurrent RPC.newRpcState
  let runStack = runConcurrent
             . Ki.runStructuredConcurrency
             . ConcurrencyManager.runConcurrencyManager
             . runGracefulTermination
             . runTimeout
             . runFileSystem
             . runProcess
             . runConcurrent
             . runBotLog cfg.logLevel
             . StorageSQLite.runStorageSQLitePath cfg.sqlitePath
             . HTTP.runHTTP
             . Media.runMedia cfg.media
             . AgentAudit.runAgentAuditWithObserver (RPC.broadcastAuditRecord rpcState . Aeson.toJSON)
             . ChatLog.runChatLog
             . Memory.runMemory cfg.memory
             . Skills.runSkills cfg.skills
             . Scheduler.runScheduler
             . TypstCLI.runTypst
             . OpenAI.runLLM cfg.llm
             . ChatDriver.runChatDrivers cfg.qq cfg.telegram cfg.matrix cfg.discord cfg.rpc rpcState
             . Lifecycle.runLifecycle cfg.media
  runStack do
    logInfo "Cosmobot stand by!"
    let allStreams =
          [ Chat.incomingMessages
          , Scheduler.scheduledMessages
          ]
        messageConsumer =
          consumeWith
            (routes cfg threads)
            (ChatLog.recordIncomingMessages (StreamUtil.mergeStreams allStreams))

    let RPCConfig.Config{enabled = rpcEnabled} = cfg.rpc
    if rpcEnabled
      then concurrently_ (RPCServer.runRpcServer cfg.rpc rpcState RPCAudit.auditRpcCallbacks) messageConsumer
      else messageConsumer

routes
  :: ( Chat.Chat :> es, AgentAudit.AgentAudit :> es, ChatLog.ChatLog :> es, Concurrency.Concurrency :> es, HTTP.HTTP :> es, LLM.LLM :> es, MediaEffect.Media :> es, Memory.Memory :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Typst.Typst :> es, KatipE :> es, Prim :> es, Concurrent :> es, Fail :> es, Timeout :> es, FileSystem :> es, Process :> es, IOE :> es)
  => BotConfig
  -> ThreadStore
  -> [RouteHandler es]
routes cfg threads =
  shutUpHandlers cfg.handlers.shutup
    <> auditHandlers threads
    <> adminHandlers cfg.handlers.admin
    <> scratchpadHandlers
    <> typingHandlers
    <> safebooruHandlers
    <> saucenaoHandlers cfg.saucenao
    <> askHandlers cfg.tool cfg.handlers.ask threads

runBotLog :: IOE :> es => Severity -> Eff (KatipE : es) a -> Eff es a
runBotLog level inner =
  startKatipE "cosmobot" "production" do
    stdoutScribe <- mkHandleScribe (ColorLog True) stdout (permitItem level) V2
    registerScribe "stdout" stdoutScribe defaultScribeSettings
    logInfo [i|Log level: #{show level :: String}|]
    logExceptionAt ErrorS inner

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
