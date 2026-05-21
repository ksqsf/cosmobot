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
import qualified Bot.RPC.State as RPC
import qualified Bot.Util.Stream as StreamUtil
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import Effectful.FileSystem (FileSystem)
import Effectful.Timeout

type ChatDriverEffects es =
  Chat.Chat : QQ.QQ : Telegram.Telegram : Matrix.Matrix : Discord.Discord : es

type ChatDriverConstraints es =
  (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, Discord.Discord :> es, FileSystem :> es, Concurrent :> es, Storage.Storage :> es, IOE :> es)

chatPlatformDrivers
  :: ChatDriverConstraints es
  => RPC.RpcState
  -> [ChatPlatformDriver es]
chatPlatformDrivers rpcState =
  [ QQ.qqDriver
  , Telegram.telegramDriver
  , Matrix.matrixDriver
  , Discord.discordDriver
  , RPC.rpcChatDriver rpcState
  ]

platformDriver
  :: ChatDriverConstraints es
  => RPC.RpcState
  -> IncomingMessage
  -> Maybe (ChatPlatformDriver es)
platformDriver rpcState message =
  List.find ((== message.platform) . (.platform)) (chatPlatformDrivers rpcState)

withPlatformDriver
  :: (ChatDriverConstraints es, Log :> es)
  => RPC.RpcState
  -> IncomingMessage
  -> Text
  -> (ChatPlatformDriver es -> Eff es (Maybe a))
  -> Eff es (Maybe a)
withPlatformDriver rpcState message label action =
  case platformDriver rpcState message of
    Nothing ->
      pure Nothing
    Just driver ->
      action driver `catchSync` \err -> do
        let platformText = show message.platform :: String
        logInfo_ [i|#{label} failed on #{platformText}: #{displayException err}|]
        pure Nothing

replyToPlatform
  :: (ChatDriverConstraints es, Log :> es)
  => RPC.RpcState
  -> IncomingMessage
  -> Text
  -> Eff es (Maybe MessageId)
replyToPlatform rpcState message body =
  withPlatformDriver rpcState message "chat reply" \driver ->
    driver.replyTo message body

uploadFileToPlatform
  :: (ChatDriverConstraints es, Log :> es)
  => RPC.RpcState
  -> IncomingMessage
  -> FilePath
  -> Eff es (Either Text (Maybe MessageId))
uploadFileToPlatform rpcState message path =
  case platformDriver rpcState message of
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
  => RPC.RpcState
  -> IncomingMessage
  -> Text
  -> Maybe Text
  -> Eff es (Either Text (Maybe MessageId))
replyAudioToPlatform rpcState message audioRef caption =
  case platformDriver rpcState message of
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
  => RPC.RpcState
  -> IncomingMessage
  -> MessageId
  -> Text
  -> Eff es Bool
editPlatformMessage rpcState message messageId body =
  fromMaybe False <$> withPlatformDriver rpcState message "chat edit" \driver ->
    Just <$> driver.editMessage message messageId body

deletePlatformMessage
  :: (ChatDriverConstraints es, Log :> es)
  => RPC.RpcState
  -> IncomingMessage
  -> MessageId
  -> Eff es Bool
deletePlatformMessage rpcState message messageId =
  fromMaybe False <$> withPlatformDriver rpcState message "chat delete" \driver ->
    Just <$> driver.deleteMessage message messageId

platformReplyStreamStyle
  :: (ChatDriverConstraints es, Log :> es)
  => RPC.RpcState
  -> IncomingMessage
  -> Eff es Chat.ReplyStreamStyle
platformReplyStreamStyle rpcState message =
  fromMaybe defaultReplyStreamStyle <$> withPlatformDriver rpcState message "reply stream style" \driver ->
    Just <$> driver.replyStreamStyle message

defaultReplyStreamStyle :: Chat.ReplyStreamStyle
defaultReplyStreamStyle =
  Chat.ChunkedReply defaultChunkedReplyLimit

defaultChunkedReplyLimit :: Int
defaultChunkedReplyLimit = 4000

getPlatformMessageContent
  :: (ChatDriverConstraints es, Log :> es)
  => RPC.RpcState
  -> IncomingMessage
  -> MessageId
  -> Eff es (Maybe ReferencedMessage)
getPlatformMessageContent rpcState message messageId =
  withPlatformDriver rpcState message "fetch referenced message" \driver ->
    driver.getMessageContent message messageId

getPlatformSenderMemberInfo
  :: (ChatDriverConstraints es, Log :> es)
  => RPC.RpcState
  -> IncomingMessage
  -> Eff es (Maybe Aeson.Value)
getPlatformSenderMemberInfo rpcState message =
  withPlatformDriver rpcState message "fetch sender member info" \driver ->
    driver.getSenderMemberInfo message

getPlatformMemberInfo
  :: (ChatDriverConstraints es, Log :> es)
  => RPC.RpcState
  -> IncomingMessage
  -> Integer
  -> Eff es (Maybe Aeson.Value)
getPlatformMemberInfo rpcState message userId =
  withPlatformDriver rpcState message "fetch member info" \driver ->
    driver.getMemberInfo message userId

getPlatformUserAvatar
  :: (ChatDriverConstraints es, Log :> es)
  => RPC.RpcState
  -> IncomingMessage
  -> Text
  -> Eff es (Maybe Aeson.Value)
getPlatformUserAvatar rpcState message userId =
  withPlatformDriver rpcState message "fetch user avatar" \driver ->
    driver.getUserAvatar message userId

listPlatformGroupMembers
  :: (ChatDriverConstraints es, Log :> es)
  => RPC.RpcState
  -> IncomingMessage
  -> Eff es (Maybe Aeson.Value)
listPlatformGroupMembers rpcState message =
  withPlatformDriver rpcState message "list group members" \driver ->
    driver.listGroupMembers message

mentionPlatformUser
  :: (ChatDriverConstraints es, Log :> es)
  => RPC.RpcState
  -> IncomingMessage
  -> Integer
  -> Text
  -> Eff es (Maybe MessageId)
mentionPlatformUser rpcState message userId body =
  withPlatformDriver rpcState message "chat mention" \driver ->
    driver.mentionUser message userId body

setPlatformMemberTitle
  :: (ChatDriverConstraints es, Log :> es)
  => RPC.RpcState
  -> IncomingMessage
  -> Integer
  -> Text
  -> Eff es Bool
setPlatformMemberTitle rpcState message userId title =
  fromMaybe False <$> withPlatformDriver rpcState message "set member title" \driver ->
    Just <$> driver.setMemberTitle message userId title

runChatDrivers
  :: (Log :> es, Timeout :> es, Fail :> es, Concurrent :> es, FileSystem :> es, Prim :> es, Storage.Storage :> es, IOE :> es)
  => QQ.Config
  -> Telegram.Config
  -> Matrix.Config
  -> Discord.Config
  -> RPC.RpcState
  -> Eff (ChatDriverEffects es) a
  -> Eff es a
runChatDrivers qqConfig telegramConfig matrixConfig discordConfig rpcState =
  Discord.runDiscord discordConfig .
  Matrix.runMatrix matrixConfig .
  Telegram.runTelegram telegramConfig .
  QQ.runQQ qqConfig .
  Chat.runChatWith (chatHandlers rpcState)

incomingMessages
  :: QQ.QQ :> es
  => Telegram.Telegram :> es
  => Matrix.Matrix :> es
  => Discord.Discord :> es
  => Log :> es
  => Concurrent :> es
  => Fail :> es
  => IOE :> es
  => RPC.RpcState
  -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages rpcState =
  StreamUtil.mergeStreams
    [ QQ.incomingMessages
    , Telegram.incomingMessages
    , Matrix.incomingMessages
    , Discord.incomingMessages
    , RPC.incomingMessages rpcState
    ]

chatHandlers
  :: (ChatDriverConstraints es, Log :> es)
  => RPC.RpcState
  -> Chat.ChatHandlers es
chatHandlers rpcState = Chat.ChatHandlers
  { handleReplyTo = replyToPlatform rpcState
  , handleReplyAudio = replyAudioToPlatform rpcState
  , handleUploadFile = uploadFileToPlatform rpcState
  , handleEditMessage = editPlatformMessage rpcState
  , handleDeleteMessage = deletePlatformMessage rpcState
  , handleReplyStreamStyle = platformReplyStreamStyle rpcState
  , handleGetMessageContent = getPlatformMessageContent rpcState
  , handleGetSenderMemberInfo = getPlatformSenderMemberInfo rpcState
  , handleGetMemberInfo = getPlatformMemberInfo rpcState
  , handleGetUserAvatar = getPlatformUserAvatar rpcState
  , handleListGroupMembers = listPlatformGroupMembers rpcState
  , handleMentionUser = mentionPlatformUser rpcState
  , handleSetMemberTitle = setPlatformMemberTitle rpcState
  }
