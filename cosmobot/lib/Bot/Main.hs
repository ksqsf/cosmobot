-- | Cosmobot executable wiring configuration, effects, platforms, and routes.
{-# LANGUAGE TypeApplications #-}

module Bot.Main
  ( main
  , mainWithConfig
  )
where

import Bot.Prelude
import qualified Bot.Agent as Agent
import qualified Bot.Agent.Middleware.Python as PythonMiddleware
import qualified Bot.Agent.Tools as AgentTools
import qualified Bot.Agent.Types as AgentTypes
import qualified Bot.ACP.Client as ACPClient
import qualified Bot.ACP.Config as ACPConfig
import qualified Bot.ACP.Server as ACPServer
import qualified Bot.ACP.State as ACP
import Bot.Config
import qualified Bot.Concurrency.Manager as ConcurrencyManager
import Bot.Core.Message (IncomingMessage (..), MessageDigest (..), inputWithAttachments, incomingMessageLogLine)
import Bot.Core.Transcript (startWithUserInput)
import Bot.Core.Route
import qualified Bot.Lifecycle as Lifecycle
import qualified Bot.Chat.Driver as ChatDriver
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Agent as AgentEffect
import qualified Bot.Effect.ACP as ACPEffect
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Lifecycle as LifecycleEffect
import qualified Bot.Effect.Media as MediaEffect
import qualified Bot.Effect.Matrix as MatrixEffect
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Plugin as PluginEffect
import qualified Bot.Effect.Resource as ResourceEffect
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Skills as Skills
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Effect.Typst as Typst
import qualified Bot.LLM.OpenAI as OpenAI
import qualified Bot.Media.Interpreter as Media
import qualified Bot.Resource as Resource
import qualified Bot.Resource.Python as Python
import qualified Bot.Resource.Sandbox as Sandbox
import qualified Bot.Resource.Workspace as Workspace
import qualified Bot.RPC.Audit as RPCAudit
import qualified Bot.RPC.Config as RPCConfig
import qualified Bot.RPC.Server as RPCServer
import qualified Bot.RPC.State as RPC
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (Builder, fromText)
import Bot.Handler.Admin
import Bot.Handler.Ask
import Bot.Handler.Ask.AgentRun (askSystemPrompt)
import Bot.Handler.Audit
import Bot.Handler.Help
import Bot.Handler.Media
import Bot.Handler.Plugin
import Bot.Handler.Resource
import Bot.Handler.Safebooru
import Bot.Handler.Saucenao
import Bot.Handler.ShutUp
import Bot.Handler.Scratchpad
import Bot.Handler.Typing
import qualified Bot.HTTP as HTTP
import qualified Bot.Plugin.Manager as PluginManager
import Bot.Storage.Thread
import qualified Bot.Storage.SQLite as StorageSQLite
import qualified Bot.System.Typst.CLI as TypstCLI
import qualified Bot.Util.Stream as StreamUtil
import qualified Streaming.Prelude as S
import Effectful.Timeout
import Effectful.Process
import qualified Effectful.Process.Typed as TypedProcess
import Effectful.FileSystem
import qualified Paths_cosmobot as Paths

-- | Start the bot using @config.toml@ from the current working directory.
main :: IO ()
main = mainWithConfig "config.toml"

-- | Start the bot using the given TOML config file.
mainWithConfig :: FilePath -> IO ()
mainWithConfig configPath =
  runOnce configPath >>= \case
    False -> pure ()
    True -> mainWithConfig configPath

runOnce :: FilePath -> IO Bool
runOnce configPath = runEff . runPrim . runFailIO $ do
  cfg <- loadConfig configPath
  restartRequested <- newIORef False
  threads <- newThreadStore
  rpcState <- runConcurrent RPC.newRpcState
  acpState <- runConcurrent ACP.newAcpState
  let runRuntime =
        runConcurrent
          . runTimeout
          . runFileSystem
          . TypedProcess.runTypedProcess
          . runProcess
          . runConcurrent
      runInfrastructure =
        runBotLog cfg.logLevel
          . StorageSQLite.runStorageSQLitePath cfg.sqlitePath
          . HTTP.runHTTP
          . Media.runMedia cfg.media
          . TypstCLI.runTypst
          . OpenAI.runLLM cfg.llm
      runApplication pythonArguments =
        ConcurrencyManager.runConcurrencyManager
          . Resource.runResourceManagerWith
              [ Resource.resourceLoader @Sandbox.Sandbox
              , Resource.resourceLoader @Workspace.Workspace
              ]
          . Agent.runAgent
          . maybe id pythonMiddleware pythonArguments
          . AgentAudit.runAgentAuditWithObserver (RPC.broadcastAuditRecord rpcState . Aeson.toJSON)
          . ChatLog.runChatLog
          . Memory.runMemory cfg.memory
          . Skills.runSkills cfg.skills
          . ACPClient.runACP acpState
          . Scheduler.runScheduler
          . ChatDriver.runChatDrivers cfg.qq cfg.telegram cfg.matrix cfg.discord cfg.rpc rpcState cfg.acp.enabled acpState
          . PluginManager.runPluginManager cfg.plugins.pluginDir cfg.media.cacheDir (pluginHostCallbacks cfg)
          . Lifecycle.runLifecycle cfg.media restartRequested
      pythonMiddleware arguments =
        PythonMiddleware.withPythonMiddleware \runTools message resourceOwner request ->
          Python.runPython resourceOwner ResourceEffect.Init{message, arguments} runTools request
  runRuntime . runInfrastructure $ do
    pythonArguments <- preparePython cfg.tool.python
    runApplication pythonArguments do
      logInfo "Cosmobot stand by!"
      let allStreams =
            [ Chat.incomingMessages
            , Scheduler.scheduledMessages
            ]
          messageConsumer =
            consumeWith
              (zipWith withRouteDebugLogging [1 :: Int ..] (routes cfg threads))
              (ChatLog.recordIncomingMessages (StreamUtil.mergeStreams allStreams))

      runConfiguredServers cfg threads rpcState acpState messageConsumer
  readIORef restartRequested

preparePython
  :: (Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Timeout :> es, Fail :> es, IOE :> es)
  => AgentTypes.PythonConfig
  -> Eff es (Maybe Python.PythonArgs)
preparePython config
  | not config.enabled = pure Nothing
  | otherwise = do
      workerPath <- liftIO (Paths.getDataFileName "python/cosmobot_worker.py")
      Python.preparePythonArgs workerPath config >>= either (fail . toString) (pure . Just)

routes
  :: ( ACPEffect.ACP :> es, AgentEffect.Agent :> es, Chat.Chat :> es, AgentAudit.AgentAudit :> es, ChatLog.ChatLog :> es, Concurrency.Concurrency :> es, HTTP.HTTP :> es, LLM.LLM :> es, LifecycleEffect.Lifecycle :> es, MediaEffect.Media :> es, MatrixEffect.Matrix :> es, Memory.Memory :> es, PluginEffect.Plugin :> es, ResourceEffect.Resource :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Typst.Typst :> es, KatipE :> es, Prim :> es, Concurrent :> es, Fail :> es, Timeout :> es, FileSystem :> es, Process :> es, IOE :> es)
  => BotConfig
  -> ThreadStore
  -> [RouteHandler es]
routes cfg threads =
  helpHandlersWith PluginEffect.helpEntries baseRoutes <> baseRoutes
  where
    baseRoutes =
      builtInRoutes
        <> pluginHandlers
        <> [pluginRouteGateway]
        <> askHandlers cfg.tool (AgentTools.defaultToolsWith AgentTools.acpTools) cfg.handlers.ask threads
    builtInRoutes =
      shutUpHandlers cfg.handlers.shutup
        <> auditHandlers threads
        <> adminHandlers cfg.handlers.admin
        <> mediaHandlers
        <> scratchpadHandlers
        <> typingHandlers
        <> safebooruHandlers
        <> saucenaoHandlers cfg.saucenao
        <> resourceHandlers

pluginHostCallbacks
  :: ( ACPEffect.ACP :> es
     , AgentEffect.Agent :> es
     , AgentAudit.AgentAudit :> es
     , Chat.Chat :> es
     , ChatLog.ChatLog :> es
     , Concurrency.Concurrency :> es
     , HTTP.HTTP :> es
     , LLM.LLM :> es
     , MediaEffect.Media :> es
     , Memory.Memory :> es
     , MatrixEffect.Matrix :> es
     , ResourceEffect.Resource :> es
     , Scheduler.Scheduler :> es
     , Skills.Skills :> es
     , Storage.Storage :> es
     , Typst.Typst :> es
     , KatipE :> es
     , Prim :> es
     , Concurrent :> es
     , Fail :> es
     , Timeout :> es
     , Process :> es
     , FileSystem :> es
     , IOE :> es
     )
  => BotConfig
  -> PluginManager.HostCallbacks es
pluginHostCallbacks cfg = PluginManager.HostCallbacks
  { chatReply = \message body -> Aeson.toJSON <$> Chat.replyTo message body
  , chatReferenced = \message -> case message.replyToMessageId of
      Nothing -> pure Aeson.Null
      Just messageId -> maybe Aeson.Null Aeson.toJSON <$> Chat.getMessageContent message messageId
  , llmComplete = LLM.ask
  , agentRun = runPluginAgent cfg
  , mediaResolve = resolvePluginMedia
  }

runPluginAgent
  :: ( ACPEffect.ACP :> es
     , AgentEffect.Agent :> es
     , AgentAudit.AgentAudit :> es
     , Chat.Chat :> es
     , ChatLog.ChatLog :> es
     , Concurrency.Concurrency :> es
     , HTTP.HTTP :> es
     , LLM.LLM :> es
     , MediaEffect.Media :> es
     , Memory.Memory :> es
     , MatrixEffect.Matrix :> es
     , ResourceEffect.Resource :> es
     , Scheduler.Scheduler :> es
     , Skills.Skills :> es
     , Storage.Storage :> es
     , Typst.Typst :> es
     , KatipE :> es
     , Prim :> es
     , Concurrent :> es
     , Fail :> es
     , Timeout :> es
     , Process :> es
     , FileSystem :> es
     , IOE :> es
     )
  => BotConfig
  -> IncomingMessage
  -> Text
  -> Eff es Text
runPluginAgent cfg message prompt = do
  systemPrompt <- askSystemPrompt cfg.handlers.ask message
  let input = inputWithAttachments prompt message.imageUrls message.files
      context = Agent.Context
        { message
        , input
        , superuser = message.digest.senderIsSuperuser
        , systemContext = systemPrompt
        , askCommand = cfg.handlers.ask.command
        , toolConfig = cfg.tool
        }
      tools = AgentTools.defaultTools
  Agent.withRun
    cfg.handlers.ask.agentMaxTurns
    cfg.handlers.ask.contextStrategy
    (cfg.handlers.ask.contextCompactionThresholdKTokens * 1000)
    context
    tools
    \runtime -> (.finalText) <$> S.effects (Agent.agentStream runtime (startWithUserInput input))

resolvePluginMedia :: (MediaEffect.Media :> es, Fail :> es) => Text -> Eff es Aeson.Value
resolvePluginMedia canonicalReference = do
  info <- MediaEffect.mediaFileInfoByRef canonicalReference >>= \case
    Nothing -> fail "media.resolve accepts only existing canonical media references"
    Just value -> pure value
  publicUrl <- MediaEffect.publicMediaRef info.ref
  localPath <- MediaEffect.localMediaPath info.ref
  pure $ Aeson.object
    [ "canonicalReference" Aeson..= info.ref
    , "mimeType" Aeson..= info.mimeType
    , "size" Aeson..= info.size
    , "publicUrl" Aeson..= publicUrl
    , "localPath" Aeson..= localPath
    ]

withRouteDebugLogging :: KatipE :> es => Int -> Route es -> Route es
withRouteDebugLogging index route =
  route
    { decide = \message -> do
        decision <- route.decide message
        case decision of
          Skip ->
            pure ()
          ContinueWith{} ->
            logMatch ("continue" :: Text) message
          StopWith{} ->
            logMatch ("stop" :: Text) message
        pure decision
    }
  where
    logMatch decision message =
      logDebug
        [i|Route matched: index=#{index} label=#{routeLabel route} decision=#{decision} #{incomingMessageLogLine message}|]

routeLabel :: Route es -> Text
routeLabel route =
  maybe "-" (.label) route.help

runConfiguredServers
  :: ( ACPEffect.ACP :> es, Chat.Chat :> es, AgentAudit.AgentAudit :> es, ChatLog.ChatLog :> es, Concurrency.Concurrency :> es, HTTP.HTTP :> es, LLM.LLM :> es, MediaEffect.Media :> es, Memory.Memory :> es, ResourceEffect.Resource :> es, Skills.Skills :> es, Scheduler.Scheduler :> es, Storage.Storage :> es, Typst.Typst :> es, KatipE :> es, Prim :> es, Concurrent :> es, Fail :> es, Timeout :> es, FileSystem :> es, Process :> es, IOE :> es)
  => BotConfig
  -> ThreadStore
  -> RPC.RpcState
  -> ACP.AcpState
  -> Eff es ()
  -> Eff es ()
runConfiguredServers cfg threads rpcState acpState messageConsumer =
  runWithTaskGroup "servers" (serverTasks cfg threads rpcState acpState) "message.consumer" messageConsumer

serverTasks
  :: ( AgentAudit.AgentAudit :> es, Concurrency.Concurrency :> es, ResourceEffect.Resource :> es, Storage.Storage :> es, MediaEffect.Media :> es, KatipE :> es, Prim :> es, Concurrent :> es, FileSystem :> es, IOE :> es)
  => BotConfig
  -> ThreadStore
  -> RPC.RpcState
  -> ACP.AcpState
  -> [(Text, Eff es ())]
serverTasks cfg threads rpcState acpState =
  enabledTask cfg.rpc.enabled "rpc.server" (RPCServer.runRpcServer cfg.rpc rpcState (RPCServer.withManagerRpcCallbacks RPCAudit.auditRpcCallbacks))
    <> enabledTask cfg.acp.enabled "acp.server" (ACPServer.runAcpServer cfg.acp threads acpState)

enabledTask :: Bool -> Text -> Eff es () -> [(Text, Eff es ())]
enabledTask enabled label action =
  [(label, action) | enabled]

runWithTaskGroup
  :: (Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es)
  => Text
  -> [(Text, Eff es ())]
  -> Text
  -> Eff es ()
  -> Eff es ()
runWithTaskGroup groupLabel tasks innerLabel inner =
  case nonEmpty tasks of
    Nothing ->
      inner
    Just tasks_ ->
      let (taskLabel, task) = collapseTaskGroup groupLabel tasks_
      in Concurrency.raceTasks_ taskLabel task innerLabel inner

collapseTaskGroup
  :: (Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es)
  => Text
  -> NonEmpty (Text, Eff es ())
  -> (Text, Eff es ())
collapseTaskGroup groupLabel tasks =
  case tasks of
    task :| [] ->
      task
    task :| rest ->
      let (_combinedLabel, combinedTask) = foldl' raceTaskPair task rest
      in (groupLabel, combinedTask)

raceTaskPair
  :: (Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es)
  => (Text, Eff es ())
  -> (Text, Eff es ())
  -> (Text, Eff es ())
raceTaskPair (leftLabel, left) (rightLabel, right) =
  ( [i|#{leftLabel}+#{rightLabel}|]
  , Concurrency.raceTasks_ leftLabel left rightLabel right
  )

runBotLog :: IOE :> es => Severity -> Eff (KatipE : es) a -> Eff es a
runBotLog level inner =
  startKatipE "cosmobot" "production" do
    stdoutScribe <- mkHandleScribeWithFormatter journalFormat (ColorLog False) stdout (permitItem level) V2
    registerScribe "stdout" stdoutScribe defaultScribeSettings
    logInfo [i|Log level: #{show level :: String}|]
    logExceptionAt ErrorS inner

journalFormat :: LogItem a => ItemFormatter a
journalFormat _ _ item =
  "[" <> fromText (renderSeverity item._itemSeverity) <> "]"
    <> namespaceField item._itemNamespace
    <> "[ThreadId " <> fromText (getThreadIdText item._itemThread) <> "] "
    <> unLogStr item._itemMessage

namespaceField :: Namespace -> Builder
namespaceField (Namespace (_application : scopes@(_ : _))) =
  "[" <> fromText (Text.intercalate "." scopes) <> "]"
namespaceField _ =
  mempty
