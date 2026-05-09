{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main (main) where

import Bot.Config
import Bot.Conversation
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Chat.QQ as QQ
import qualified Bot.Effect.Chat.Telegram as Telegram
import qualified Bot.Effect.LLM as LLM
import Bot.Filter
import Bot.Handler.Ask
import Bot.Message
import Bot.Prelude
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Aeson as Aeson
import Log.Backend.StandardOutput
import qualified Streaming as S
import qualified Streaming.Prelude as S

main :: IO ()
main = do
  cfg <- loadConfig "config.toml"
  conversations <- newConversationStore
  runEff $
    runBotLog cfg.logLevel .
    Telegram.runTelegram cfg.telegram .
    QQ.runQQ cfg.qq .
    Chat.runChatWith platformReplyTo platformGetMessageContent platformGetSenderMemberInfo .
    LLM.runLLM cfg.llm $ do
      logInfo_ "Cosmobot stand by!"
      consumeWith (routes cfg conversations) incomingMessages

routes
  :: (Chat.Chat :> es, LLM.LLM :> es, Log :> es, IOE :> es)
  => BotConfig
  -> ConversationStore
  -> [RouteHandler es]
routes cfg conversations =
  askHandlers cfg.handlers.ask conversations

platformReplyTo
  :: (QQ.QQ :> es, Telegram.Telegram :> es)
  => IncomingMessage
  -> Text
  -> Eff es (Maybe Integer)
platformReplyTo message body =
  case message.platform of
    PlatformQQ       -> QQ.replyTo message body
    PlatformTelegram -> Telegram.replyTo message body

platformGetMessageContent
  :: QQ.QQ :> es
  => IncomingMessage
  -> Integer
  -> Eff es (Maybe ReferencedMessage)
platformGetMessageContent message messageId =
  case message.platform of
    PlatformQQ       -> QQ.getMessageContent messageId
    PlatformTelegram -> pure Nothing

platformGetSenderMemberInfo
  :: (QQ.QQ :> es, Telegram.Telegram :> es)
  => IncomingMessage
  -> Eff es (Maybe Aeson.Value)
platformGetSenderMemberInfo message =
  case (message.platform, message.kind, message.chatId, message.senderId) of
    (PlatformQQ, ChatGroup, Just groupId, Just userId) ->
      QQ.getGroupMemberInfo groupId userId
    (PlatformTelegram, ChatGroup, Just chatId, Just userId) ->
      Just . Aeson.toJSON <$> Telegram.getChatMember chatId userId
    _ ->
      pure Nothing

incomingMessages
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Log :> es, IOE :> es)
  => Stream (Of IncomingMessage) (Eff es) ()
incomingMessages =
  mergeIncomingMessages
    [ QQ.incomingMessages
    , Telegram.incomingMessages
    ]

mergeIncomingMessages
  :: IOE :> es
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
  :: IOE :> es
  => Chan.Chan IncomingMessage
  -> Stream (Of IncomingMessage) (Eff es) ()
  -> Eff es ()
pump chan =
  S.mapM_ (liftIO . Chan.writeChan chan)

runBotLog :: IOE :> es => LogLevel -> Eff (Log : es) a -> Eff es a
runBotLog level inner = withStdOutLogger $ \logger ->
  runLog "cosmobot" logger level $ do
    logExceptions inner
