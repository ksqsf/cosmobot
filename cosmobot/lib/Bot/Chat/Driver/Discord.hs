{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.Discord
Description : Discord Gateway and REST chat driver
Stability   : experimental
-}

module Bot.Chat.Driver.Discord
  ( DiscordDriver
  , newDiscordDriver
  , runDiscordDriver
  , Config (..)
  , GatewayEnvelope (..)
  , GatewayHello (..)
  , Message (..)
  , DeletedMessage (..)
  , User (..)
  , Attachment (..)
  , Embed (..)
  , EmbedImage (..)
  , StickerItem (..)
  , Member (..)
  , Reference (..)
  , CreateMessageRequest (..)
  , incomingMessages
  , eventToIncomingMessage
  , eventToIncomingMessageWith
  , deletedEventToIncomingMessageWith
  , formatDiscordMarkdown
  , discordUserAvatarValue
  )
where

import qualified Bot.Chat.Driver.Types as Driver
import Bot.Chat.Driver.Discord.Markdown (formatDiscordMarkdown)
import Bot.Chat.Driver.Discord.Protocol hiding (DiscordDriver, newDiscordDriver, runDiscordDriver)
import qualified Bot.Chat.Driver.Discord.Protocol as Protocol
import Bot.Chat.Driver.Discord.Types (Config (..), defaultConfig)
import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Media as Media
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Bits (shiftR)
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Effectful.FileSystem (FileSystem)
import qualified Streaming as S
import qualified Streaming.Prelude as S

newtype DiscordDriver = DiscordDriver Protocol.DiscordDriver

newDiscordDriver :: IOE :> es => Config -> Eff es DiscordDriver
newDiscordDriver =
  fmap DiscordDriver . Protocol.newDiscordDriver

runDiscordDriver
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => DiscordDriver
  -> Eff es a
  -> Eff es a
runDiscordDriver (DiscordDriver driver) =
  Protocol.runDiscordDriver driver

instance Driver.ChatDriver DiscordDriver where
  type ChatDriverEffects DiscordDriver es = (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Media.Media :> es)

  driverPlatform _ =
    PlatformDiscord

  sendReplyMessage (DiscordDriver driver) =
    replyToDiscord
      driver

  replyAudio (DiscordDriver driver) =
    replyAudioDiscord
      driver

  uploadFile (DiscordDriver driver) =
    uploadFileDiscord
      driver

  editMessage (DiscordDriver driver) message messageId body =
    editMessageDiscord driver message messageId body

  deleteMessage (DiscordDriver driver) =
    deleteMessageDiscord
      driver

  messageOutPolicy _ _ =
    pure (Chat.EditableMessage discordEditChunkChars discordMessageTextLimit)

  getMessageContent (DiscordDriver driver) =
    getMessageContentDiscord
      driver

  getSenderMemberInfo (DiscordDriver driver) message =
      case (discordMessageGuildId message.raw, message.senderId) of
        (Just guildId, Just userId) ->
          Just <$> getGuildMember driver guildId userId
        _ ->
          pure Nothing

  getMemberInfo (DiscordDriver driver) message userId =
      case discordMessageGuildId message.raw of
        Just guildId ->
          Just <$> getGuildMember driver guildId userId
        _ ->
          pure Nothing

  getUserAvatar (DiscordDriver driver) _ userId = do
    value <- getUser driver userId
    pure (discordUserAvatarValue =<< Aeson.parseMaybe Aeson.parseJSON value)

  listGroupMembers (DiscordDriver driver) message =
      case discordMessageGuildId message.raw of
        Just guildId ->
          Just <$> listGuildMembers driver guildId
        _ ->
          pure Nothing

  mentionUser (DiscordDriver driver) =
    mentionUserDiscord
      driver

  setTyping (DiscordDriver driver) message _timeoutMillis =
      case discordChannelId message of
        Just channelId ->
          triggerTyping driver channelId
        _ ->
          pure ()

discordEditChunkChars :: Int
discordEditChunkChars = 512

discordMessageTextLimit :: Int
discordMessageTextLimit = 2000

incomingMessages :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => DiscordDriver -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages (DiscordDriver driver) =
  incomingMessagesProtocol driver

incomingMessagesProtocol :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => Protocol.DiscordDriver -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessagesProtocol driver = do
  if discordEnabled driver.config
    then incomingMessagesLoop driver
    else S.lift $ $(logInfo) "Discord driver disabled: no bot token configured"

incomingMessagesLoop :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => Protocol.DiscordDriver -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessagesLoop driver = do
  event <- S.lift (receiveEvent driver)
  let incoming = case event of
        GatewayMessageCreated message -> eventToIncomingMessageWith driver.config message
        GatewayMessageDeleted deleted -> Just (deletedEventToIncomingMessageWith driver.config deleted)
  case incoming of
    Nothing -> do
      S.lift $ $(logDebug) "Ignoring Discord event"
    Just parsedMessage -> do
      message <- S.lift (resolveDiscordChatDisplayName driver parsedMessage)
      S.lift $ $(logDebug) ("incoming Discord message:\n" <> logJsonText message)
      S.yield message
  incomingMessagesLoop driver

resolveDiscordChatDisplayName
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => Protocol.DiscordDriver
  -> IncomingMessage
  -> Eff es IncomingMessage
resolveDiscordChatDisplayName driver message = case listToMaybe message.chatAliases of
  Nothing -> pure message
  Just channelId -> do
    cached <- liftIO (Map.lookup channelId <$> IORef.readIORef driver.channelDisplayNames)
    displayName <- case cached of
      Just name -> pure (Just name)
      Nothing -> do
        name <- Protocol.discordChannelDisplayName <$> fetchChannel driver channelId
        for_ name \resolved ->
          liftIO $ IORef.atomicModifyIORef' driver.channelDisplayNames (\names -> (Map.insert channelId resolved names, ()))
        pure name
    pure message{chatDisplayName = displayName}

eventToIncomingMessage :: Message -> Maybe IncomingMessage
eventToIncomingMessage =
  eventToIncomingMessageWith defaultConfig

eventToIncomingMessageWith :: Config -> Message -> Maybe IncomingMessage
eventToIncomingMessageWith cfg message = do
  guard (not (isOwnMessage cfg message))
  guard (not message.author.bot)
  guard (not (Text.null content) || not (null message.attachments))
  pure IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformDiscord
    , kind = if isJust message.guildId then ChatGroup else ChatPrivate
    , chatId = Just (textChatId message.channelId)
    , chatAliases = catMaybes [Just message.channelId, message.guildId]
    , chatDisplayName = Nothing
    , digest = discordMessageDigest cfg message
    , senderId = Just message.author.id
    , senderUsername = message.author.username
    , senderDisplayName = (message.member >>= (.nick)) <|> message.author.globalName
    , senderGlobalDisplayName = message.author.globalName
    , messageId = Just (textMessageId message.id)
    , replyToMessageId = message.referencedMessage <&> (.id) <&> textMessageId
    , mentions = map (.id) message.mentions
    , mentionUsernames = mapMaybe (.username) message.mentions
    , imageUrls = messageImageUrls message
    , files = messageFiles message
    , text = content
    , raw = message.raw
    }
  where
    content = Text.unwords . filter (not . Text.null) $
      Text.strip message.content : map (\sticker -> "[sticker: " <> sticker.name <> "]") message.stickerItems

deletedEventToIncomingMessageWith :: Config -> DeletedMessage -> IncomingMessage
deletedEventToIncomingMessageWith cfg deleted =
  IncomingMessage
    { eventKind = IncomingMessageDeleted
    , platform = PlatformDiscord
    , kind = if isJust deleted.guildId then ChatGroup else ChatPrivate
    , chatId = Just (textChatId deleted.channelId)
    , chatAliases = catMaybes [Just deleted.channelId, deleted.guildId]
    , chatDisplayName = Nothing
    , digest = emptyMessageDigest
        { chatIsAllowed =
            discordSnowflakeNumber deleted.channelId `elem` cfg.allowedChannels
              || maybe False ((`elem` cfg.allowedGuilds) . discordSnowflakeNumber) deleted.guildId
        , botId = cfg.botId
        }
    , senderId = Nothing
    , senderUsername = Nothing
    , senderDisplayName = Nothing
    , senderGlobalDisplayName = Nothing
    , messageId = Just (textMessageId deleted.id)
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = ""
    , raw = deleted.raw
    }

discordMessageDigest :: Config -> Message -> MessageDigest
discordMessageDigest cfg message =
  MessageDigest
    { chatIsAllowed = chatAllowed
    , senderIsAllowed = if isNothing message.guildId then (chatAllowed || senderAllowed || senderSuperuser) else senderSuperuser
    , senderIsSuperuser = senderSuperuser
    , mentionsBot = maybe False (`elem` map (.id) message.mentions) cfg.botId
    , botId = cfg.botId
    }
  where
    chatAllowed =
      discordSnowflakeNumber message.channelId `elem` cfg.allowedChannels ||
        maybe False ((`elem` cfg.allowedGuilds) . discordSnowflakeNumber) message.guildId
    senderAllowed =
      message.author.id `elem` cfg.allowedUsers
    senderSuperuser =
      message.author.id `elem` cfg.superusers

replyToDiscord
  :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es, Media.Media :> es)
  => Protocol.DiscordDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Either Text MessageId)
replyToDiscord driver message body =
  case discordChannelId message of
    Just channelId -> do
      let text = formatDiscordMarkdown (Chat.renderReplyBody body)
          imageRefs = Chat.replyImageUrls body
          request = createMessageRequest text (discordReplyReference message)
      sentText <- if Text.null (Text.strip text)
        then pure Nothing
        else Just <$> createMessage driver channelId request
      sentImages <- traverse (sendDiscordImage driver channelId (discordReplyReference message)) imageRefs
      pure case textMessageId . (.id) <$> sentText <|> (textMessageId . (.id) <$> viaNonEmpty head sentImages) of
        Just messageId ->
          Right messageId
        Nothing ->
          Left "Discord reply did not send any message."
    _ ->
      pure (Left "Discord reply requires a Discord channel id.")

sendDiscordImage :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es, Media.Media :> es) => Protocol.DiscordDriver -> Text -> Maybe Reference -> Text -> Eff es Message
sendDiscordImage driver channelId replyReference imageRef = do
  resolvedRef <- discordImageRef imageRef
  case discordRemoteImageRef resolvedRef of
    Just url ->
      createMessage driver channelId (createMessageRequest url replyReference)
    Nothing ->
      withDiscordImageFile resolvedRef \path ->
        uploadDiscordFile driver channelId Nothing path Nothing

editMessageDiscord :: (HTTP.HTTP :> es, KatipE :> es) => Protocol.DiscordDriver -> IncomingMessage -> MessageId -> Text -> Eff es Bool
editMessageDiscord driver message messageId body =
  case discordChannelId message of
    Just channelId -> do
      void $ editDiscordMessage driver channelId (messageIdText messageId) (createMessageRequest (formatDiscordMarkdown (Chat.renderReplyBody body)) Nothing)
      pure True
    _ ->
      pure False

deleteMessageDiscord :: (HTTP.HTTP :> es, KatipE :> es) => Protocol.DiscordDriver -> IncomingMessage -> MessageId -> Eff es Bool
deleteMessageDiscord driver message messageId =
  case discordChannelId message of
    Just channelId -> do
      deleteDiscordMessage driver channelId (messageIdText messageId)
      pure True
    _ ->
      pure False

getMessageContentDiscord :: (HTTP.HTTP :> es, KatipE :> es) => Protocol.DiscordDriver -> IncomingMessage -> MessageId -> Eff es (Maybe ReferencedMessage)
getMessageContentDiscord driver message messageId =
  case discordChannelId message of
    Just channelId -> do
      fetched <- fetchMessage driver channelId (messageIdText messageId)
      pure (Just ReferencedMessage
        { messageId = Just (textMessageId fetched.id)
        , senderDisplayName = fetched.author.globalName <|> fetched.author.username
        , senderIdentifier = Just fetched.author.id
        , senderIsBot = fetched.author.bot
        , text = fetched.content
        , imageUrls = messageImageUrls fetched
        , files = messageFiles fetched
        })
    _ ->
      pure Nothing

mentionUserDiscord :: (HTTP.HTTP :> es, KatipE :> es) => Protocol.DiscordDriver -> IncomingMessage -> Text -> Text -> Eff es (Either Text MessageId)
mentionUserDiscord driver message userId body =
  case discordChannelId message of
    Just channelId -> do
      sent <- createMessage driver channelId (createMessageRequest ([i|<@#{userId}> #{formatDiscordMarkdown body}|]) (discordReplyReference message))
      pure (Right (textMessageId sent.id))
    _ ->
      pure (Left "Discord mention reply requires a Discord channel id.")

replyAudioDiscord :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es) => Protocol.DiscordDriver -> IncomingMessage -> Text -> Maybe Text -> Eff es (Either Text MessageId)
replyAudioDiscord driver message audioRef caption =
  case discordChannelId message of
    Just channelId -> do
      sent <- withDiscordImageFile audioRef \path ->
        uploadDiscordFile driver channelId (formatDiscordMarkdown . Chat.renderReplyBody <$> caption) path Nothing
      pure (Right (textMessageId sent.id))
    _ ->
      pure (Left "Discord audio reply requires a Discord channel id.")

uploadFileDiscord :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => Protocol.DiscordDriver -> IncomingMessage -> FilePath -> Maybe Text -> Eff es (Either Text MessageId)
uploadFileDiscord driver message path fileName =
  case discordChannelId message of
    Just channelId -> do
      sent <- uploadDiscordFile driver channelId Nothing path fileName
      pure (Right (textMessageId sent.id))
    _ ->
      pure (Left "Discord file upload requires a Discord channel id.")

discordChannelId :: IncomingMessage -> Maybe Text
discordChannelId message =
  viaNonEmpty head message.chatAliases

discordReplyReference :: IncomingMessage -> Maybe Reference
discordReplyReference message =
  Reference <$> message.messageId <*> discordChannelId message <*> Just (discordMessageGuildId message.raw)

createMessageRequest :: Text -> Maybe Reference -> CreateMessageRequest
createMessageRequest content messageReference =
  CreateMessageRequest
    { content = nonEmptyDiscordContent content
    , messageReference
    , allowedMentions = AllowedMentions
        { parse = []
        , users = []
        , repliedUser = Just False
        }
    }

nonEmptyDiscordContent :: Text -> Text
nonEmptyDiscordContent content
  | Text.null (Text.strip content) = " "
  | otherwise = Text.take discordMessageTextLimit content

discordUserAvatarValue :: User -> Maybe Aeson.Value
discordUserAvatarValue user = do
  avatarUrl <- discordUserAvatarUrl user
  pure $ Aeson.object
    [ "platform" Aeson..= ("discord" :: Text)
    , "user_id" Aeson..= user.id
    , "username" Aeson..= user.username
    , "global_name" Aeson..= user.globalName
    , "avatar" Aeson..= user.avatar
    , "avatar_url" Aeson..= avatarUrl
    ]

discordUserAvatarUrl :: User -> Maybe Text
discordUserAvatarUrl user =
  case user.avatar of
    Just avatarHash | not (Text.null avatarHash) ->
      let userId = user.id
          extension = discordAvatarExtension avatarHash
      in Just [i|https://cdn.discordapp.com/avatars/#{userId}/#{avatarHash}.#{extension}?size=512|]
    _ ->
      discordDefaultAvatarUrl user.id

discordAvatarExtension :: Text -> Text
discordAvatarExtension avatarHash
  | "a_" `Text.isPrefixOf` avatarHash = "gif"
  | otherwise = "png"

discordDefaultAvatarUrl :: Text -> Maybe Text
discordDefaultAvatarUrl userId = do
  numericUserId <- parseDiscordSnowflake userId
  let index = (numericUserId `shiftR` 22) `mod` 6
  pure [i|https://cdn.discordapp.com/embed/avatars/#{index}.png|]

parseDiscordSnowflake :: Text -> Maybe Integer
parseDiscordSnowflake raw =
  case reads (Text.unpack (Text.strip raw)) of
    [(value, "")] ->
      Just value
    _ ->
      Nothing

discordRemoteImageRef :: Text -> Maybe Text
discordRemoteImageRef ref
  | "http://" `Text.isPrefixOf` ref || "https://" `Text.isPrefixOf` ref = Just ref
  | otherwise = Nothing

discordImageRef :: Media.Media :> es => Text -> Eff es Text
discordImageRef ref
  | "media:" `Text.isPrefixOf` Text.strip ref = do
      publicRef <- Media.publicMediaRef ref
      if "media:" `Text.isPrefixOf` Text.strip publicRef
        then maybe ref (("file://" <>) . Text.pack) <$> Media.localMediaPath ref
        else pure publicRef
  | otherwise =
      pure ref

withDiscordImageFile :: (FileSystem :> es, IOE :> es) => Text -> (FilePath -> Eff es a) -> Eff es a
withDiscordImageFile ref action =
  case Text.stripPrefix "file://" ref of
    Just path -> action (Text.unpack path)
    Nothing -> action (Text.unpack ref)

messageImageUrls :: Message -> [Text]
messageImageUrls message =
  [ attachment.url
  | attachment <- message.attachments
  , attachmentIsImage attachment
  ] <>
  [ embedImage.imageUrl
  | embed <- message.embeds
  , embedImage <- maybeToList embed.image <> maybeToList embed.thumbnail
  ] <>
  contentImageUrls message.content

messageFiles :: Message -> [MessageFile]
messageFiles message =
  [ MessageFile{name = attachment.filename, ref = attachment.url}
  | attachment <- message.attachments
  , not (attachmentIsImage attachment)
  ]

attachmentIsImage :: Attachment -> Bool
attachmentIsImage attachment =
  maybe (imageFileName attachment.filename || imageUrl attachment.url) ("image/" `Text.isPrefixOf`) attachment.contentType

contentImageUrls :: Text -> [Text]
contentImageUrls =
  filter imageUrl . Text.words

imageUrl :: Text -> Bool
imageUrl raw =
  ("http://" `Text.isPrefixOf` stripped || "https://" `Text.isPrefixOf` stripped) && imagePath (Text.toLower withoutQuery)
  where
    stripped =
      Text.dropWhileEnd (`elem` (".,;:!?)" :: String)) raw
    withoutQuery =
      Text.takeWhile (\c -> c /= '?' && c /= '#') stripped

imageFileName :: Text -> Bool
imageFileName =
  imagePath . Text.toLower

imagePath :: Text -> Bool
imagePath path =
  any (`Text.isSuffixOf` path)
    [ ".jpg"
    , ".jpeg"
    , ".png"
    , ".gif"
    , ".webp"
    , ".bmp"
    , ".avif"
    ]

isOwnMessage :: Config -> Message -> Bool
isOwnMessage cfg message =
  Just message.author.id == cfg.botId

discordMessageGuildId :: Aeson.Value -> Maybe Text
discordMessageGuildId =
  join . Aeson.parseMaybe (Aeson.withObject "Discord message" \o -> o Aeson..:? "guild_id")

discordSnowflakeNumber :: Text -> Integer
discordSnowflakeNumber raw =
  fromMaybe (stableTextId raw) (parseIntegerUserId raw)

parseIntegerUserId :: Text -> Maybe Integer
parseIntegerUserId raw =
  readMaybe (Text.unpack (Text.strip raw))

stableTextId :: Text -> Integer
stableTextId =
  Text.foldl' step 14695981039346656037
  where
    step acc char =
      fromIntegral ((fromIntegral acc `xor` fromIntegral (fromEnum char)) * fnvPrime :: Word64)
    fnvPrime :: Word64
    fnvPrime = 1099511628211
