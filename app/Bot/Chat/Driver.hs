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
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Storage as Storage
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.RPC.Config as RPCConfig
import qualified Bot.RPC.State as RPC
import qualified Bot.Util.Stream as StreamUtil
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.List as List
import qualified Data.Vector as Vector
import Effectful.FileSystem (FileSystem)
import Effectful.Timeout

type ChatDriverEffects es =
  Chat.Chat : QQ.QQ : Telegram.Telegram : Matrix.Matrix : Discord.Discord : es

type ChatDriverConstraints es =
  (QQ.QQ :> es, Telegram.Telegram :> es, Matrix.Matrix :> es, Discord.Discord :> es, Media.Media :> es, FileSystem :> es, Concurrent :> es, Storage.Storage :> es, IOE :> es)

chatPlatformDrivers
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> [ChatPlatformDriver es]
chatPlatformDrivers rpcConfig rpcState =
  [ QQ.qqDriver
  , Telegram.telegramDriver
  , Matrix.matrixDriver
  , Discord.discordDriver
  , RPC.rpcChatDriver rpcConfig rpcState
  ]

platformDriver
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> Maybe (ChatPlatformDriver es)
platformDriver rpcConfig rpcState message =
  List.find ((== message.platform) . (.platform)) (chatPlatformDrivers rpcConfig rpcState)

withPlatformDriver
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> Text
  -> (ChatPlatformDriver es -> Eff es (Maybe a))
  -> Eff es (Maybe a)
withPlatformDriver rpcConfig rpcState message label action =
  case platformDriver rpcConfig rpcState message of
    Nothing ->
      pure Nothing
    Just driver ->
      action driver `catchSync` \err -> do
        let platformText = show message.platform :: String
        logInfo [i|#{label} failed on #{platformText}: #{displayException err}|]
        pure Nothing

replyToPlatform
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> Text
  -> Eff es (Maybe MessageId)
replyToPlatform rpcConfig rpcState message body =
  withPlatformDriver rpcConfig rpcState message "chat reply" \driver -> do
    normalizedBody <- Media.normalizeReplyBody body
    driver.replyTo message normalizedBody

uploadFileToPlatform
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> FilePath
  -> Eff es (Either Text (Maybe MessageId))
uploadFileToPlatform rpcConfig rpcState message path =
  case platformDriver rpcConfig rpcState message of
    Nothing ->
      let platformText = show message.platform :: String
      in pure (Left [i|No chat driver is registered for #{platformText}.|])
    Just driver ->
      driver.uploadFile message path `catchSync` \err -> do
        let platformText = show message.platform :: String
            messageText = [i|File upload failed on #{platformText}: #{displayException err}|]
        logInfo messageText
        pure (Left messageText)

replyAudioToPlatform
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> Text
  -> Maybe Text
  -> Eff es (Either Text (Maybe MessageId))
replyAudioToPlatform rpcConfig rpcState message audioRef caption =
  case platformDriver rpcConfig rpcState message of
    Nothing ->
      let platformText = show message.platform :: String
      in pure (Left [i|No chat driver is registered for #{platformText}.|])
    Just driver ->
      driver.replyAudio message audioRef caption `catchSync` \err -> do
        let platformText = show message.platform :: String
            messageText = [i|Audio send failed on #{platformText}: #{displayException err}|]
        logInfo messageText
        pure (Left messageText)

editPlatformMessage
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> MessageId
  -> Text
  -> Eff es Bool
editPlatformMessage rpcConfig rpcState message messageId body =
  fromMaybe False <$> withPlatformDriver rpcConfig rpcState message "chat edit" \driver ->
    Just <$> driver.editMessage message messageId body

deletePlatformMessage
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> MessageId
  -> Eff es Bool
deletePlatformMessage rpcConfig rpcState message messageId =
  fromMaybe False <$> withPlatformDriver rpcConfig rpcState message "chat delete" \driver ->
    Just <$> driver.deleteMessage message messageId

platformReplyStreamStyle
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> Eff es Chat.ReplyStreamStyle
platformReplyStreamStyle rpcConfig rpcState message =
  fromMaybe defaultReplyStreamStyle <$> withPlatformDriver rpcConfig rpcState message "reply stream style" \driver ->
    Just <$> driver.replyStreamStyle message

defaultReplyStreamStyle :: Chat.ReplyStreamStyle
defaultReplyStreamStyle =
  Chat.ChunkedReply defaultChunkedReplyLimit

defaultChunkedReplyLimit :: Int
defaultChunkedReplyLimit = 4000

getPlatformMessageContent
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> MessageId
  -> Eff es (Maybe ReferencedMessage)
getPlatformMessageContent rpcConfig rpcState message messageId =
  withPlatformDriver rpcConfig rpcState message "fetch referenced message" \driver ->
    traverse (normalizeReferencedMessageMedia driver) =<< driver.getMessageContent message messageId

normalizeReferencedMessageMedia :: Media.Media :> es => ChatPlatformDriver es -> ReferencedMessage -> Eff es ReferencedMessage
normalizeReferencedMessageMedia driver message = do
  imageUrls <- traverse (driver.normalizeMediaRef >=> Media.normalizeMediaRef) message.imageUrls
  pure ReferencedMessage
    { messageId = message.messageId
    , senderDisplayName = message.senderDisplayName
    , senderIdentifier = message.senderIdentifier
    , text = message.text
    , imageUrls
    }

getPlatformSenderMemberInfo
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> Eff es (Maybe Aeson.Value)
getPlatformSenderMemberInfo rpcConfig rpcState message =
  withPlatformDriver rpcConfig rpcState message "fetch sender member info" \driver ->
    traverse (normalizeJsonMediaUrls driver.normalizeMediaRef) =<< driver.getSenderMemberInfo message

getPlatformMemberInfo
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> Text
  -> Eff es (Maybe Aeson.Value)
getPlatformMemberInfo rpcConfig rpcState message userId =
  withPlatformDriver rpcConfig rpcState message "fetch member info" \driver ->
    traverse (normalizeJsonMediaUrls driver.normalizeMediaRef) =<< driver.getMemberInfo message userId

getPlatformUserAvatar
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> Text
  -> Eff es (Maybe Aeson.Value)
getPlatformUserAvatar rpcConfig rpcState message userId =
  withPlatformDriver rpcConfig rpcState message "fetch user avatar" \driver ->
    traverse (normalizeJsonMediaUrls driver.normalizeMediaRef) =<< driver.getUserAvatar message userId

listPlatformGroupMembers
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> Eff es (Maybe Aeson.Value)
listPlatformGroupMembers rpcConfig rpcState message =
  withPlatformDriver rpcConfig rpcState message "list group members" \driver ->
    traverse (normalizeJsonMediaUrls driver.normalizeMediaRef) =<< driver.listGroupMembers message

mentionPlatformUser
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> Text
  -> Text
  -> Eff es (Maybe MessageId)
mentionPlatformUser rpcConfig rpcState message userId body =
  withPlatformDriver rpcConfig rpcState message "chat mention" \driver -> do
    normalizedBody <- Media.normalizeReplyBody body
    driver.mentionUser message userId normalizedBody

setPlatformMemberTitle
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> IncomingMessage
  -> Text
  -> Text
  -> Eff es Bool
setPlatformMemberTitle rpcConfig rpcState message userId title =
  fromMaybe False <$> withPlatformDriver rpcConfig rpcState message "set member title" \driver ->
    Just <$> driver.setMemberTitle message userId title

runChatDrivers
  :: (KatipE :> es, Timeout :> es, Fail :> es, Concurrent :> es, Media.Media :> es, FileSystem :> es, Prim :> es, Storage.Storage :> es, IOE :> es)
  => QQ.Config
  -> Telegram.Config
  -> Matrix.Config
  -> Discord.Config
  -> RPCConfig.Config
  -> RPC.RpcState
  -> Eff (ChatDriverEffects es) a
  -> Eff es a
runChatDrivers qqConfig telegramConfig matrixConfig discordConfig rpcConfig rpcState =
  Discord.runDiscord discordConfig .
  Matrix.runMatrix matrixConfig .
  Telegram.runTelegram telegramConfig .
  QQ.runQQ qqConfig .
  Chat.runChatWith (chatHandlers rpcConfig rpcState)

incomingMessages
  :: QQ.QQ :> es
  => Telegram.Telegram :> es
  => Matrix.Matrix :> es
  => Discord.Discord :> es
  => Media.Media :> es
  => KatipE :> es
  => Concurrent :> es
  => Fail :> es
  => IOE :> es
  => RPC.RpcState
  -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages rpcState =
  Media.normalizeIncomingMessages $
    StreamUtil.mergeStreams
    [ QQ.incomingMessages
    , Telegram.incomingMessages
    , Matrix.incomingMessages
    , Discord.incomingMessages
    , RPC.incomingMessages rpcState
    ]

normalizeJsonMediaUrls :: Media.Media :> es => (Text -> Eff es Text) -> Aeson.Value -> Eff es Aeson.Value
normalizeJsonMediaUrls normalizePlatformMediaRef = \case
  Aeson.Object jsonObject ->
    Aeson.Object . AesonKeyMap.fromList <$> traverse normalizePair (AesonKeyMap.toList jsonObject)
  Aeson.Array array ->
    Aeson.Array . Vector.fromList <$> traverse (normalizeJsonMediaUrls normalizePlatformMediaRef) (Vector.toList array)
  value ->
    pure value
  where
    normalizePair (key, Aeson.String ref)
      | AesonKey.toText key `elem` mediaUrlKeys = do
          platformNormalized <- normalizePlatformMediaRef ref
          normalized <- Media.normalizeMediaRef platformNormalized
          pure (key, Aeson.String normalized)
    normalizePair (key, value) =
      (key,) <$> normalizeJsonMediaUrls normalizePlatformMediaRef value

    mediaUrlKeys =
      ["avatar_url", "image_url", "url"]

chatHandlers
  :: (ChatDriverConstraints es, KatipE :> es)
  => RPCConfig.Config
  -> RPC.RpcState
  -> Chat.ChatHandlers es
chatHandlers rpcConfig rpcState = Chat.ChatHandlers
  { handleReplyTo = replyToPlatform rpcConfig rpcState
  , handleReplyAudio = replyAudioToPlatform rpcConfig rpcState
  , handleUploadFile = uploadFileToPlatform rpcConfig rpcState
  , handleEditMessage = editPlatformMessage rpcConfig rpcState
  , handleDeleteMessage = deletePlatformMessage rpcConfig rpcState
  , handleReplyStreamStyle = platformReplyStreamStyle rpcConfig rpcState
  , handleGetMessageContent = getPlatformMessageContent rpcConfig rpcState
  , handleGetSenderMemberInfo = getPlatformSenderMemberInfo rpcConfig rpcState
  , handleGetMemberInfo = getPlatformMemberInfo rpcConfig rpcState
  , handleGetUserAvatar = getPlatformUserAvatar rpcConfig rpcState
  , handleListGroupMembers = listPlatformGroupMembers rpcConfig rpcState
  , handleMentionUser = mentionPlatformUser rpcConfig rpcState
  , handleSetMemberTitle = setPlatformMemberTitle rpcConfig rpcState
  }
