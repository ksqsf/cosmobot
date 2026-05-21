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
import qualified Bot.Chat.Driver.Discord as Discord
import qualified Bot.Chat.Driver.Matrix as Matrix
import qualified Bot.Chat.Driver.Telegram as Telegram
import Bot.Chat.Driver.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Storage as Storage
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Util.Stream as StreamUtil
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import Effectful.FileSystem (FileSystem)
import Effectful.Timeout

type ChatDriverEffects es =
  Chat.Chat : QQ.QQ : Telegram.Telegram : Matrix.Matrix : Discord.Discord : es

type ChatDriverConstraints es =
  (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, Discord.Discord :> es, FileSystem :> es, IOE :> es)

chatPlatformDrivers
  :: ChatDriverConstraints es
  => [ChatPlatformDriver es]
chatPlatformDrivers =
  [ QQ.qqDriver
  , Telegram.telegramDriver
  , Matrix.matrixDriver
  , Discord.discordDriver
  ]

platformDriver
  :: ChatDriverConstraints es
  => IncomingMessage
  -> Maybe (ChatPlatformDriver es)
platformDriver message =
  List.find ((== message.platform) . (.platform)) chatPlatformDrivers

withPlatformDriver
  :: (ChatDriverConstraints es, Log :> es)
  => IncomingMessage
  -> Text
  -> (ChatPlatformDriver es -> Eff es (Maybe a))
  -> Eff es (Maybe a)
withPlatformDriver message label action =
  case platformDriver message of
    Nothing ->
      pure Nothing
    Just driver ->
      action driver `catchSync` \err -> do
        let platformText = show message.platform :: String
        logInfo_ [i|#{label} failed on #{platformText}: #{displayException err}|]
        pure Nothing

replyToPlatform
  :: (ChatDriverConstraints es, Log :> es)
  => IncomingMessage
  -> Text
  -> Eff es (Maybe MessageId)
replyToPlatform message body =
  withPlatformDriver message "chat reply" \driver ->
    driver.replyTo message body

uploadFileToPlatform
  :: (ChatDriverConstraints es, Log :> es)
  => IncomingMessage
  -> FilePath
  -> Eff es (Either Text (Maybe MessageId))
uploadFileToPlatform message path =
  case platformDriver message of
    Nothing ->
      let platformText = show message.platform :: String
      in pure (Left [i|No chat driver is registered for #{platformText}.|])
    Just driver ->
      driver.uploadFile message path `catchSync` \err -> do
        let platformText = show message.platform :: String
            messageText = [i|File upload failed on #{platformText}: #{displayException err}|]
        logInfo_ messageText
        pure (Left messageText)

replyAudioToPlatform
  :: (ChatDriverConstraints es, Log :> es)
  => IncomingMessage
  -> Text
  -> Maybe Text
  -> Eff es (Either Text (Maybe MessageId))
replyAudioToPlatform message audioRef caption =
  case platformDriver message of
    Nothing ->
      let platformText = show message.platform :: String
      in pure (Left [i|No chat driver is registered for #{platformText}.|])
    Just driver ->
      driver.replyAudio message audioRef caption `catchSync` \err -> do
        let platformText = show message.platform :: String
            messageText = [i|Audio send failed on #{platformText}: #{displayException err}|]
        logInfo_ messageText
        pure (Left messageText)

editPlatformMessage
  :: (ChatDriverConstraints es, Log :> es)
  => IncomingMessage
  -> MessageId
  -> Text
  -> Eff es Bool
editPlatformMessage message messageId body =
  fromMaybe False <$> withPlatformDriver message "chat edit" \driver ->
    Just <$> driver.editMessage message messageId body

deletePlatformMessage
  :: (ChatDriverConstraints es, Log :> es)
  => IncomingMessage
  -> MessageId
  -> Eff es Bool
deletePlatformMessage message messageId =
  fromMaybe False <$> withPlatformDriver message "chat delete" \driver ->
    Just <$> driver.deleteMessage message messageId

platformReplyStreamStyle
  :: (ChatDriverConstraints es, Log :> es)
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
  :: (ChatDriverConstraints es, Log :> es)
  => IncomingMessage
  -> MessageId
  -> Eff es (Maybe ReferencedMessage)
getPlatformMessageContent message messageId =
  withPlatformDriver message "fetch referenced message" \driver ->
    driver.getMessageContent message messageId

getPlatformSenderMemberInfo
  :: (ChatDriverConstraints es, Log :> es)
  => IncomingMessage
  -> Eff es (Maybe Aeson.Value)
getPlatformSenderMemberInfo message =
  withPlatformDriver message "fetch sender member info" \driver ->
    driver.getSenderMemberInfo message

getPlatformMemberInfo
  :: (ChatDriverConstraints es, Log :> es)
  => IncomingMessage
  -> Integer
  -> Eff es (Maybe Aeson.Value)
getPlatformMemberInfo message userId =
  withPlatformDriver message "fetch member info" \driver ->
    driver.getMemberInfo message userId

getPlatformUserAvatar
  :: (ChatDriverConstraints es, Log :> es)
  => IncomingMessage
  -> Text
  -> Eff es (Maybe Aeson.Value)
getPlatformUserAvatar message userId =
  withPlatformDriver message "fetch user avatar" \driver ->
    driver.getUserAvatar message userId

listPlatformGroupMembers
  :: (ChatDriverConstraints es, Log :> es)
  => IncomingMessage
  -> Eff es (Maybe Aeson.Value)
listPlatformGroupMembers message =
  withPlatformDriver message "list group members" \driver ->
    driver.listGroupMembers message

mentionPlatformUser
  :: (ChatDriverConstraints es, Log :> es)
  => IncomingMessage
  -> Integer
  -> Text
  -> Eff es (Maybe MessageId)
mentionPlatformUser message userId body =
  withPlatformDriver message "chat mention" \driver ->
    driver.mentionUser message userId body

setPlatformMemberTitle
  :: (ChatDriverConstraints es, Log :> es)
  => IncomingMessage
  -> Integer
  -> Text
  -> Eff es Bool
setPlatformMemberTitle message userId title =
  fromMaybe False <$> withPlatformDriver message "set member title" \driver ->
    Just <$> driver.setMemberTitle message userId title

runChatDrivers
  :: (Log :> es, Timeout :> es, Fail :> es, Concurrent :> es, FileSystem :> es, Prim :> es, Storage.Storage :> es, IOE :> es)
  => QQ.Config
  -> Telegram.Config
  -> Matrix.Config
  -> Discord.Config
  -> Eff (ChatDriverEffects es) a
  -> Eff es a
runChatDrivers qqConfig telegramConfig matrixConfig discordConfig =
  Discord.runDiscord discordConfig .
  Matrix.runMatrix matrixConfig .
  Telegram.runTelegram telegramConfig .
  QQ.runQQ qqConfig .
  Chat.runChatWith chatHandlers

incomingMessages
  :: QQ.QQ :> es
  => Telegram.Telegram :> es
  => Matrix.Matrix :> es
  => Discord.Discord :> es
  => Log :> es
  => Concurrent :> es
  => Fail :> es
  => IOE :> es
  => Stream (Of IncomingMessage) (Eff es) ()
incomingMessages =
  StreamUtil.mergeStreams
    [ QQ.incomingMessages
    , Telegram.incomingMessages
    , Matrix.incomingMessages
    , Discord.incomingMessages
    ]

chatHandlers
  :: (ChatDriverConstraints es, Log :> es)
  => Chat.ChatHandlers es
chatHandlers = Chat.ChatHandlers
  { handleReplyTo = replyToPlatform
  , handleReplyAudio = replyAudioToPlatform
  , handleUploadFile = uploadFileToPlatform
  , handleEditMessage = editPlatformMessage
  , handleDeleteMessage = deletePlatformMessage
  , handleReplyStreamStyle = platformReplyStreamStyle
  , handleGetMessageContent = getPlatformMessageContent
  , handleGetSenderMemberInfo = getPlatformSenderMemberInfo
  , handleGetMemberInfo = getPlatformMemberInfo
  , handleGetUserAvatar = getPlatformUserAvatar
  , handleListGroupMembers = listPlatformGroupMembers
  , handleMentionUser = mentionPlatformUser
  , handleSetMemberTitle = setPlatformMemberTitle
  }
