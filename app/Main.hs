{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Cosmobot executable wiring configuration, effects, platforms, and routes.
module Main (main) where

import Bot.Config
import Bot.Conversation
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Chat.QQ as QQ
import qualified Bot.Effect.Chat.Telegram as Telegram
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Scheduler as Scheduler
import Bot.Filter
import Bot.Handler.Ask
import Bot.Handler.Saucenao
import Bot.Handler.Scratchpad
import Bot.Handler.Typing
import Bot.Message
import Bot.Prelude
import qualified Bot.Storage.SQLite as SQLiteStorage
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Aeson as Aeson
import qualified Data.List as List
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
    Chat.runChatWith replyToPlatform getPlatformMessageContent getPlatformSenderMemberInfo getPlatformMemberInfo listPlatformGroupMembers mentionPlatformUser .
    LLM.runLLM cfg.llm $ do
      logInfo_ "Cosmobot stand by!"
      consumeWith (routes cfg sqliteStore conversations) (recordedIncomingMessages incomingMessages)

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
    <> askHandlers cfg.memory cfg.handlers.ask conversations

data ChatPlatformDriver es = ChatPlatformDriver
  { platform :: !ChatPlatform
  , replyTo :: IncomingMessage -> Text -> Eff es (Maybe Integer)
  , getMessageContent :: IncomingMessage -> Integer -> Eff es (Maybe ReferencedMessage)
  , getSenderMemberInfo :: IncomingMessage -> Eff es (Maybe Aeson.Value)
  , getMemberInfo :: IncomingMessage -> Integer -> Eff es (Maybe Aeson.Value)
  , listGroupMembers :: IncomingMessage -> Eff es (Maybe Aeson.Value)
  , mentionUser :: IncomingMessage -> Integer -> Text -> Eff es (Maybe Integer)
  }

chatPlatformDrivers
  :: (QQ.QQ :> es, Telegram.Telegram :> es, IOE :> es)
  => [ChatPlatformDriver es]
chatPlatformDrivers =
  [ qqDriver
  , telegramDriver
  ]

qqDriver
  :: (QQ.QQ :> es, IOE :> es)
  => ChatPlatformDriver es
qqDriver = ChatPlatformDriver
  { platform = PlatformQQ
  , replyTo = QQ.replyTo
  , getMessageContent = \_ messageId -> QQ.getMessageContent messageId
  , getSenderMemberInfo = \message ->
      case (message.kind, message.chatId, message.senderId) of
        (ChatGroup, Just groupId, Just userId) ->
          QQ.getGroupMemberInfo groupId userId
        _ ->
          pure Nothing
  , getMemberInfo = \message userId ->
      case (message.kind, message.chatId) of
        (ChatGroup, Just groupId) ->
          QQ.getGroupMemberInfo groupId userId
        _ ->
          pure Nothing
  , listGroupMembers = \message ->
      case (message.kind, message.chatId) of
        (ChatGroup, Just groupId) ->
          QQ.getGroupMemberList groupId
        _ ->
          pure Nothing
  , mentionUser = QQ.mentionUser
  }

telegramDriver
  :: (Telegram.Telegram :> es, IOE :> es)
  => ChatPlatformDriver es
telegramDriver = ChatPlatformDriver
  { platform = PlatformTelegram
  , replyTo = Telegram.replyTo
  , getMessageContent = Telegram.getMessageContent
  , getSenderMemberInfo = \message ->
      case (message.kind, message.chatId, message.senderId) of
        (ChatGroup, Just chatId, Just userId) ->
          Just . Aeson.toJSON <$> Telegram.getChatMember chatId userId
        _ ->
          pure Nothing
  , getMemberInfo = \message userId ->
      case (message.kind, message.chatId) of
        (ChatGroup, Just chatId) ->
          Just . Aeson.toJSON <$> Telegram.getChatMember chatId userId
        _ ->
          pure Nothing
  , listGroupMembers = \_ ->
      pure Nothing
  , mentionUser = Telegram.mentionUser
  }

platformDriver
  :: (QQ.QQ :> es, Telegram.Telegram :> es, IOE :> es)
  => IncomingMessage
  -> Maybe (ChatPlatformDriver es)
platformDriver message =
  List.find ((== message.platform) . (.platform)) chatPlatformDrivers

withPlatformDriver
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Text
  -> (ChatPlatformDriver es -> Eff es (Maybe a))
  -> Eff es (Maybe a)
withPlatformDriver message label action =
  case platformDriver message of
    Nothing ->
      pure Nothing
    Just driver ->
      action driver `catch` \(err :: SomeException) -> do
        logInfo [i|#{label} failed|] (message.platform, show err :: String)
        pure Nothing

replyToPlatform
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Text
  -> Eff es (Maybe Integer)
replyToPlatform message body =
  withPlatformDriver message "chat reply" \driver ->
    driver.replyTo message body

getPlatformMessageContent
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Integer
  -> Eff es (Maybe ReferencedMessage)
getPlatformMessageContent message messageId =
  withPlatformDriver message "fetch referenced message" \driver ->
    driver.getMessageContent message messageId

getPlatformSenderMemberInfo
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Eff es (Maybe Aeson.Value)
getPlatformSenderMemberInfo message =
  withPlatformDriver message "fetch sender member info" \driver ->
    driver.getSenderMemberInfo message

getPlatformMemberInfo
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Integer
  -> Eff es (Maybe Aeson.Value)
getPlatformMemberInfo message userId =
  withPlatformDriver message "fetch member info" \driver ->
    driver.getMemberInfo message userId

listPlatformGroupMembers
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Eff es (Maybe Aeson.Value)
listPlatformGroupMembers message =
  withPlatformDriver message "list group members" \driver ->
    driver.listGroupMembers message

mentionPlatformUser
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Integer
  -> Text
  -> Eff es (Maybe Integer)
mentionPlatformUser message userId body =
  withPlatformDriver message "chat mention" \driver ->
    driver.mentionUser message userId body

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
  chan <- S.lift (liftIO Chan.newChan)
  S.lift $ withEffToIO (ConcUnlift Persistent Unlimited) $ \runInIO ->
    traverse_ (forkIO . runInIO . pump chan) streams
  forever do
    message <- S.lift (liftIO (Chan.readChan chan))
    S.yield message

pump
  :: (Log :> es, IOE :> es)
  => Chan.Chan IncomingMessage
  -> Stream (Of IncomingMessage) (Eff es) ()
  -> Eff es ()
pump chan stream =
  S.mapM_ (liftIO . Chan.writeChan chan) stream
    `catch` \(err :: SomeException) ->
      logInfo "Incoming message stream stopped" (show err :: String)

runBotLog :: IOE :> es => LogLevel -> Eff (Log : es) a -> Eff es a
runBotLog level inner = withStdOutLogger $ \logger ->
  runLog "cosmobot" logger level $ do
    logExceptions inner
