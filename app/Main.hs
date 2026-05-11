{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Cosmobot executable wiring configuration, effects, platforms, and routes.
module Main (main) where

import Bot.Config
import Bot.Core.Conversation
import qualified Bot.Chat.Platform as ChatPlatform
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Chat.QQ as QQ
import qualified Bot.Effect.Chat.Telegram as Telegram
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Scheduler as Scheduler
import Bot.Core.Filter
import Bot.Handler.Ask
import Bot.Handler.Saucenao
import Bot.Handler.Scratchpad
import Bot.Handler.Typing
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Storage.SQLite as SQLiteStorage
import Control.Concurrent (forkIO)
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
    Scheduler.runScheduler .
    Telegram.runTelegram cfg.telegram .
    QQ.runQQ cfg.qq .
    runPlatformChat .
    LLM.runLLM cfg.llm $ do
      logInfo_ "Cosmobot stand by!"
      consumeWith (routes cfg sqliteStore conversations) (recordedIncomingMessages incomingMessages)

runPlatformChat
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Log :> es, IOE :> es)
  => Eff (Chat.Chat : es) a
  -> Eff es a
runPlatformChat =
  Chat.runChatWith
    ChatPlatform.replyToPlatform
    ChatPlatform.editPlatformMessage
    ChatPlatform.platformReplyStreamStyle
    ChatPlatform.getPlatformMessageContent
    ChatPlatform.getPlatformSenderMemberInfo
    ChatPlatform.getPlatformMemberInfo
    ChatPlatform.listPlatformGroupMembers
    ChatPlatform.mentionPlatformUser

routes
  :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => BotConfig
  -> SQLiteStorage.SQLiteStore
  -> ConversationStore
  -> [RouteHandler es]
routes cfg sqliteStore conversations =
  scratchpadHandlers sqliteStore
    <> typingHandlers cfg.handlers.ask
    <> saucenaoHandlers cfg.saucenao cfg.handlers.ask
    <> askHandlers cfg.memory cfg.tool cfg.handlers.ask conversations

incomingMessages
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Scheduler.Scheduler :> es, Log :> es, IOE :> es)
  => Stream (Of IncomingMessage) (Eff es) ()
incomingMessages =
  mergeIncomingMessages
    [ QQ.incomingMessages
    , Telegram.incomingMessages
    , Scheduler.scheduledMessages
    ]

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
  S.lift $ withEffToIO (ConcUnlift Persistent Unlimited) $ \runInIO ->
    traverse_ (forkIO . runInIO . pump queue) streams
  forever do
    message <- S.lift (liftIO (STM.atomically (TBQueue.readTBQueue queue)))
    S.yield message

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
