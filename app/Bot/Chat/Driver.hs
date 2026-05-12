{-|
Module      : Bot.Chat.Driver
Description : Platform-specific chat driver dispatch
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Chat.Driver
  ( runChatDrivers
  , incomingMessages
  )
where

import qualified Bot.Chat.Driver.QQ as QQ
import qualified Bot.Chat.Driver.Matrix as Matrix
import qualified Bot.Chat.Driver.Telegram as Telegram
import Bot.Chat.Driver.Types
import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Util.Stream as StreamUtil
import qualified Data.Aeson as Aeson
import qualified Data.List as List

type ChatDriverEffects es =
  Chat.Chat : QQ.QQ : Telegram.Telegram : Matrix.Matrix : es

chatPlatformDrivers
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, IOE :> es)
  => [ChatPlatformDriver es]
chatPlatformDrivers =
  [ QQ.qqDriver
  , Telegram.telegramDriver
  , Matrix.matrixDriver
  ]

platformDriver
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, IOE :> es)
  => IncomingMessage
  -> Maybe (ChatPlatformDriver es)
platformDriver message =
  List.find ((== message.platform) . (.platform)) chatPlatformDrivers

withPlatformDriver
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, Log :> es, IOE :> es)
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
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Text
  -> Eff es (Maybe Integer)
replyToPlatform message body =
  withPlatformDriver message "chat reply" \driver ->
    driver.replyTo message body

editPlatformMessage
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Integer
  -> Text
  -> Eff es Bool
editPlatformMessage message messageId body =
  fromMaybe False <$> withPlatformDriver message "chat edit" \driver ->
    Just <$> driver.editMessage message messageId body

platformReplyStreamStyle
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Eff es Chat.ReplyStreamStyle
platformReplyStreamStyle message =
  fromMaybe defaultReplyStreamStyle <$> withPlatformDriver message "reply stream style" \driver ->
    Just <$> driver.replyStreamStyle message

defaultReplyStreamStyle :: Chat.ReplyStreamStyle
defaultReplyStreamStyle =
  Chat.ChunkedReply defaultChunkedReplyLimit

defaultChunkedReplyLimit :: Int
defaultChunkedReplyLimit = 4000

getPlatformMessageContent
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Integer
  -> Eff es (Maybe ReferencedMessage)
getPlatformMessageContent message messageId =
  withPlatformDriver message "fetch referenced message" \driver ->
    driver.getMessageContent message messageId

getPlatformSenderMemberInfo
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Eff es (Maybe Aeson.Value)
getPlatformSenderMemberInfo message =
  withPlatformDriver message "fetch sender member info" \driver ->
    driver.getSenderMemberInfo message

getPlatformMemberInfo
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Integer
  -> Eff es (Maybe Aeson.Value)
getPlatformMemberInfo message userId =
  withPlatformDriver message "fetch member info" \driver ->
    driver.getMemberInfo message userId

listPlatformGroupMembers
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Eff es (Maybe Aeson.Value)
listPlatformGroupMembers message =
  withPlatformDriver message "list group members" \driver ->
    driver.listGroupMembers message

mentionPlatformUser
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Integer
  -> Text
  -> Eff es (Maybe Integer)
mentionPlatformUser message userId body =
  withPlatformDriver message "chat mention" \driver ->
    driver.mentionUser message userId body

runChatDrivers
  :: (Log :> es, IOE :> es)
  => QQ.Config
  -> Telegram.Config
  -> Matrix.Config
  -> Eff (ChatDriverEffects es) a
  -> Eff es a
runChatDrivers qqConfig telegramConfig matrixConfig =
  Matrix.runMatrix matrixConfig .
  Telegram.runTelegram telegramConfig .
  QQ.runQQ qqConfig .
  Chat.runChatWith chatHandlers

incomingMessages
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, Log :> es, IOE :> es)
  => Stream (Of IncomingMessage) (Eff es) ()
incomingMessages =
  StreamUtil.mergeStreams
    [ QQ.incomingMessages
    , Telegram.incomingMessages
    , Matrix.incomingMessages
    ]

chatHandlers
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, Log :> es, IOE :> es)
  => Chat.ChatHandlers es
chatHandlers = Chat.ChatHandlers
  { handleReplyTo = replyToPlatform
  , handleEditMessage = editPlatformMessage
  , handleReplyStreamStyle = platformReplyStreamStyle
  , handleGetMessageContent = getPlatformMessageContent
  , handleGetSenderMemberInfo = getPlatformSenderMemberInfo
  , handleGetMemberInfo = getPlatformMemberInfo
  , handleListGroupMembers = listPlatformGroupMembers
  , handleMentionUser = mentionPlatformUser
  }
