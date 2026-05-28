{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Bot.Chat.Driver
Description : Platform-specific chat driver dispatch
Stability   : experimental
-}

module Bot.Chat.Driver
  ( runChatDrivers
  )
where

import qualified Bot.Chat.Driver.QQ as QQ
import qualified Bot.Chat.Driver.Discord as Discord
import qualified Bot.Chat.Driver.Matrix as Matrix
import qualified Bot.Chat.Driver.RPC as RPCDriver
import qualified Bot.Chat.Driver.Telegram as Telegram
import Bot.Chat.Driver.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.HTTP as HTTP
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
import qualified Data.Vector as Vector
import Effectful.FileSystem (FileSystem)
import Effectful.Timeout
import qualified Streaming as S

data ChatDrivers = ChatDrivers
  { qq :: !QQ.QQDriver
  , telegram :: !Telegram.TelegramDriver
  , matrix :: !Matrix.MatrixDriver
  , discord :: !Discord.DiscordDriver
  , rpc :: !RPCDriver.RpcChatDriver
  , rpcState :: !RPC.RpcState
  }

newtype NormalizingChatDriver driver =
  NormalizingChatDriver driver

instance ChatDriver driver => ChatDriver (NormalizingChatDriver driver) where
  type ChatDriverEffects (NormalizingChatDriver driver) es =
    (ChatDriverEffects driver es, KatipE :> es, Media.Media :> es)

  driverPlatform (NormalizingChatDriver driver) =
    driverPlatform driver

  replyTo (NormalizingChatDriver driver) message body =
    withChatDriverEither message "Chat reply" do
      normalizedBody <- Media.normalizeReplyBody body
      replyTo driver message normalizedBody

  replyAudio (NormalizingChatDriver driver) message audioRef caption =
    withChatDriverEither message "Audio send" do
      replyAudio driver message audioRef caption

  uploadFile (NormalizingChatDriver driver) message path =
    withChatDriverEither message "File upload" do
      uploadFile driver message path

  editMessage (NormalizingChatDriver driver) message messageId body =
    fromMaybe False <$> withChatDriverMaybe message "chat edit" do
      Just <$> editMessage driver message messageId body

  deleteMessage (NormalizingChatDriver driver) message messageId =
    fromMaybe False <$> withChatDriverMaybe message "chat delete" do
      Just <$> deleteMessage driver message messageId

  replyStreamStyle (NormalizingChatDriver driver) message =
    fromMaybe defaultReplyStreamStyle <$> withChatDriverMaybe message "reply stream style" do
      Just <$> replyStreamStyle driver message

  getMessageContent (NormalizingChatDriver driver) message messageId =
    withChatDriverMaybe message "fetch referenced message" do
      traverse (normalizeReferencedMessageMedia driver) =<< getMessageContent driver message messageId

  getSenderMemberInfo (NormalizingChatDriver driver) message =
    withChatDriverMaybe message "fetch sender member info" do
      traverse (normalizeJsonMediaUrls (normalizeMediaRef driver)) =<< getSenderMemberInfo driver message

  getMemberInfo (NormalizingChatDriver driver) message userId =
    withChatDriverMaybe message "fetch member info" do
      traverse (normalizeJsonMediaUrls (normalizeMediaRef driver)) =<< getMemberInfo driver message userId

  getUserAvatar (NormalizingChatDriver driver) message userId =
    withChatDriverMaybe message "fetch user avatar" do
      traverse (normalizeJsonMediaUrls (normalizeMediaRef driver)) =<< getUserAvatar driver message userId

  listGroupMembers (NormalizingChatDriver driver) message =
    withChatDriverMaybe message "list group members" do
      traverse (normalizeJsonMediaUrls (normalizeMediaRef driver)) =<< listGroupMembers driver message

  normalizeMediaRef (NormalizingChatDriver driver) =
    normalizeMediaRef driver

  mentionUser (NormalizingChatDriver driver) message userId body =
    withChatDriverEither message "Chat mention" do
      normalizedBody <- Media.normalizeReplyBody body
      mentionUser driver message userId normalizedBody

  setMemberTitle (NormalizingChatDriver driver) message userId title =
    fromMaybe False <$> withChatDriverMaybe message "set member title" do
      Just <$> setMemberTitle driver message userId title

  setTyping (NormalizingChatDriver driver) message timeoutMillis =
    setTyping driver message timeoutMillis
      `catchSync` \err ->
        logError [i|Failed to set typing: #{show err :: String}|]

instance ChatDriver ChatDrivers where
  type ChatDriverEffects ChatDrivers es =
    ( ChatDriverEffects (NormalizingChatDriver QQ.QQDriver) es
    , ChatDriverEffects (NormalizingChatDriver Telegram.TelegramDriver) es
    , ChatDriverEffects (NormalizingChatDriver Matrix.MatrixDriver) es
    , ChatDriverEffects (NormalizingChatDriver Discord.DiscordDriver) es
    , ChatDriverEffects (NormalizingChatDriver RPCDriver.RpcChatDriver) es
    )

  driverPlatform _ =
    error "ChatDrivers does not have a single platform"

  replyTo drivers message body =
    withMessageDriver drivers message \driver ->
      replyTo driver message body

  replyAudio drivers message audioRef caption =
    withMessageDriver drivers message \driver ->
      replyAudio driver message audioRef caption

  uploadFile drivers message path =
    withMessageDriver drivers message \driver ->
      uploadFile driver message path

  editMessage drivers message messageId body =
    withMessageDriver drivers message \driver ->
      editMessage driver message messageId body

  deleteMessage drivers message messageId =
    withMessageDriver drivers message \driver ->
      deleteMessage driver message messageId

  replyStreamStyle drivers message =
    withMessageDriver drivers message \driver ->
      replyStreamStyle driver message

  getMessageContent drivers message messageId =
    withMessageDriver drivers message \driver ->
      getMessageContent driver message messageId

  getSenderMemberInfo drivers message =
    withMessageDriver drivers message \driver ->
      getSenderMemberInfo driver message

  getMemberInfo drivers message userId =
    withMessageDriver drivers message \driver ->
      getMemberInfo driver message userId

  getUserAvatar drivers message userId =
    withMessageDriver drivers message \driver ->
      getUserAvatar driver message userId

  listGroupMembers drivers message =
    withMessageDriver drivers message \driver ->
      listGroupMembers driver message

  mentionUser drivers message userId body =
    withMessageDriver drivers message \driver ->
      mentionUser driver message userId body

  setMemberTitle drivers message userId title =
    withMessageDriver drivers message \driver ->
      setMemberTitle driver message userId title

  setTyping drivers message timeoutMillis =
    withMessageDriver drivers message \driver ->
      setTyping driver message timeoutMillis

withMessageDriver
  :: ChatDriverEffects ChatDrivers es
  => ChatDrivers
  -> IncomingMessage
  -> (forall driver. (ChatDriver driver, ChatDriverEffects driver es) => driver -> Eff es a)
  -> Eff es a
withMessageDriver drivers message action =
  case message.platform of
    PlatformQQ ->
      action (NormalizingChatDriver drivers.qq)
    PlatformTelegram ->
      action (NormalizingChatDriver drivers.telegram)
    PlatformMatrix ->
      action (NormalizingChatDriver drivers.matrix)
    PlatformDiscord ->
      action (NormalizingChatDriver drivers.discord)
    PlatformRPC ->
      action (NormalizingChatDriver drivers.rpc)

withChatDriverMaybe
  :: (KatipE :> es)
  => IncomingMessage
  -> Text
  -> Eff es (Maybe a)
  -> Eff es (Maybe a)
withChatDriverMaybe message label action =
  action `catchSync` \err -> do
    let platformText = show message.platform :: String
    logInfo [i|#{label} failed on #{platformText}: #{displayException err}|]
    pure Nothing

withChatDriverEither
  :: (KatipE :> es)
  => IncomingMessage
  -> Text
  -> Eff es (Either Text MessageId)
  -> Eff es (Either Text MessageId)
withChatDriverEither message label action =
  action `catchSync` \err -> do
    let platformText = show message.platform :: String
        messageText = [i|#{label} failed on #{platformText}: #{displayException err}|]
    logInfo messageText
    pure (Left messageText)

defaultReplyStreamStyle :: Chat.ReplyStreamStyle
defaultReplyStreamStyle =
  Chat.ChunkedReply defaultChunkedReplyLimit

defaultChunkedReplyLimit :: Int
defaultChunkedReplyLimit = 4000

normalizeReferencedMessageMedia
  :: (Media.Media :> es, ChatDriver driver, ChatDriverEffects driver es)
  => driver
  -> ReferencedMessage
  -> Eff es ReferencedMessage
normalizeReferencedMessageMedia driver message = do
  imageUrls <- traverse (normalizeMediaRef driver >=> Media.normalizeMediaRef) message.imageUrls
  pure ReferencedMessage
    { messageId = message.messageId
    , senderDisplayName = message.senderDisplayName
    , senderIdentifier = message.senderIdentifier
    , text = message.text
    , imageUrls
    }

runChatDrivers
  :: (KatipE :> es, HTTP.HTTP :> es, Timeout :> es, Fail :> es, Concurrent :> es, Media.Media :> es, FileSystem :> es, Prim :> es, Storage.Storage :> es, IOE :> es)
  => QQ.Config
  -> Telegram.Config
  -> Matrix.Config
  -> Discord.Config
  -> RPCConfig.Config
  -> RPC.RpcState
  -> Eff (Chat.Chat : es) a
  -> Eff es a
runChatDrivers qqConfig telegramConfig matrixConfig discordConfig rpcConfig rpcState action = do
  qq <- QQ.newQQDriver qqConfig
  let telegram = Telegram.newTelegramDriver telegramConfig
  matrix <- Matrix.newMatrixDriver matrixConfig
  discord <- Discord.newDiscordDriver discordConfig
  let drivers = ChatDrivers
        { qq
        , telegram
        , matrix
        , discord
        , rpc = RPCDriver.rpcChatDriver rpcConfig rpcState
        , rpcState
        }
  runChatDriversWith drivers action

runChatDriversWith
  :: (KatipE :> es, HTTP.HTTP :> es, Timeout :> es, Fail :> es, Concurrent :> es, Media.Media :> es, FileSystem :> es, Prim :> es, Storage.Storage :> es, IOE :> es)
  => ChatDrivers
  -> Eff (Chat.Chat : es) a
  -> Eff es a
runChatDriversWith drivers inner = do
  Discord.runDiscordDriver drivers.discord $
    QQ.runQQDriver drivers.qq $
      Chat.runChatWithHandler (chatDriversHandler drivers) inner

chatDriversHandler
  :: forall es.
     ( ChatDriverEffects ChatDrivers es
     , HTTP.HTTP :> es
     , Media.Media :> es
     , Storage.Storage :> es
     , KatipE :> es
     , Concurrent :> es
     , Prim :> es
     , Fail :> es
     , IOE :> es
     )
  => ChatDrivers
  -> Chat.ChatHandler es
chatDriversHandler drivers localEnv = \case
  Chat.IncomingMessages -> do
    let stream :: Stream (Of IncomingMessage) (Eff es) ()
        stream = incomingMessages drivers
    localLift localEnv (ConcUnlift Persistent Unlimited) \runLocal ->
      pure (S.hoist runLocal stream)
  op ->
    Chat.chatDriverHandler drivers localEnv op

incomingMessages
  :: HTTP.HTTP :> es
  => Media.Media :> es
  => Storage.Storage :> es
  => KatipE :> es
  => Concurrent :> es
  => Prim :> es
  => Fail :> es
  => IOE :> es
  => ChatDrivers
  -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages drivers =
  Media.normalizeIncomingMessages $
    StreamUtil.mergeStreams
    [ QQ.incomingMessages drivers.qq
    , Telegram.incomingMessages drivers.telegram
    , Matrix.incomingMessages drivers.matrix
    , Discord.incomingMessages drivers.discord
    , RPC.incomingMessages drivers.rpcState
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
