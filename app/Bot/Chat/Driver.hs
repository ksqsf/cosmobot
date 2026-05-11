{-|
Module      : Bot.Chat.Driver
Description : Platform-specific chat driver dispatch
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Chat.Driver
  ( runChatDrivers
  , replyToPlatform
  , editPlatformMessage
  , platformReplyStreamStyle
  , getPlatformMessageContent
  , getPlatformSenderMemberInfo
  , getPlatformMemberInfo
  , listPlatformGroupMembers
  , mentionPlatformUser
  )
where

import qualified Bot.Effect.Chat.QQ as QQ
import qualified Bot.Effect.Chat.Telegram as Telegram
import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.List as List

data ChatPlatformDriver es = ChatPlatformDriver
  { platform :: !ChatPlatform
  , replyTo :: IncomingMessage -> Text -> Eff es (Maybe Integer)
  , editMessage :: IncomingMessage -> Integer -> Text -> Eff es Bool
  , replyStreamStyle :: IncomingMessage -> Eff es Chat.ReplyStreamStyle
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
  , editMessage = \_ _ _ -> pure False
  , replyStreamStyle = \_ -> pure (Chat.ChunkedReply qqStreamingMessageLimit)
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
  , editMessage = Telegram.editMessage
  , replyStreamStyle = \_ -> pure (Chat.EditableReply telegramEditChunkChars)
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

editPlatformMessage
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Integer
  -> Text
  -> Eff es Bool
editPlatformMessage message messageId body =
  fromMaybe False <$> withPlatformDriver message "chat edit" \driver ->
    Just <$> driver.editMessage message messageId body

platformReplyStreamStyle
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Log :> es, IOE :> es)
  => IncomingMessage
  -> Eff es Chat.ReplyStreamStyle
platformReplyStreamStyle message =
  fromMaybe defaultReplyStreamStyle <$> withPlatformDriver message "reply stream style" \driver ->
    Just <$> driver.replyStreamStyle message

defaultReplyStreamStyle :: Chat.ReplyStreamStyle
defaultReplyStreamStyle =
  Chat.ChunkedReply qqStreamingMessageLimit

telegramEditChunkChars :: Int
telegramEditChunkChars = 50

qqStreamingMessageLimit :: Int
qqStreamingMessageLimit = 4000

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

runChatDrivers
  :: (QQ.QQ :> es, Telegram.Telegram :> es, Log :> es, IOE :> es)
  => Eff (Chat.Chat : es) a
  -> Eff es a
runChatDrivers =
  Chat.runChatWith Chat.ChatHandlers
    { handleReplyTo = replyToPlatform
    , handleEditMessage = editPlatformMessage
    , handleReplyStreamStyle = platformReplyStreamStyle
    , handleGetMessageContent = getPlatformMessageContent
    , handleGetSenderMemberInfo = getPlatformSenderMemberInfo
    , handleGetMemberInfo = getPlatformMemberInfo
    , handleListGroupMembers = listPlatformGroupMembers
    , handleMentionUser = mentionPlatformUser
    }
