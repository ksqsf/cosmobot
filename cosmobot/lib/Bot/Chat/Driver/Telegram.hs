{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Bot.Chat.Driver.Telegram
Description : Telegram ChatDriver implementation and public facade
Stability   : experimental
-}

module Bot.Chat.Driver.Telegram
  ( TelegramDriver
  , newTelegramDriver
  , Config (..)
  , User (..)
  , Update (..)
  , Chat (..)
  , ChatType (..)
  , ChatMember (..)
  , ChatMemberStatus (..)
  , Message (..)
  , MessageEntity (..)
  , PhotoSize (..)
  , TelegramMedia (..)
  , Sticker (..)
  , ParseMode (..)
  , InputRichMessage (..)
  , InputRichMessageMedia (..)
  , InputRichMedia (..)
  , ReplyParameters (..)
  , SendRichMessageRequest (..)
  , SendMessageRequest (..)
  , EditMessageTextRequest (..)
  , SendPhotoRequest (..)
  , SendDocumentRequest (..)
  , SendVoiceRequest (..)
  , TelegramException (..)
  , TelegramResult
  , parseTelegramResult
  , formatTelegramRichHtml
  , telegramFailureReplyText
  , richMessageFallbackAllowed
  , incomingMessages
  , updateToIncomingMessage
  , updateToIncomingMessageWith
  )
where

import Bot.Core.Message
import Bot.Chat.Driver.Telegram.Markdown (escapeHtml, formatTelegramRichHtml)
import qualified Bot.Chat.Driver.Telegram.Protocol as Protocol
import Bot.Chat.Driver.Telegram.Protocol hiding (TelegramDriver, newTelegramDriver)
import Bot.Chat.Driver.Telegram.Types (Config (..))
import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Chat as ChatEffect
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Media as Media
import qualified Bot.Media.Mime as Mime
import Bot.Util.Multipart
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Effectful.Temporary as Temporary
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.FilePath ((</>), (<.>))

newtype TelegramDriver = TelegramDriver Protocol.TelegramDriver

newTelegramDriver :: Config -> TelegramDriver
newTelegramDriver =
  TelegramDriver . Protocol.newTelegramDriver

instance Driver.ChatDriver TelegramDriver where
  type ChatDriverEffects TelegramDriver es =
    (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es)

  driverPlatform _ =
    PlatformTelegram

  sendReplyMessage (TelegramDriver driver) =
    replyToTelegram driver

  replyAudio (TelegramDriver driver) =
    replyAudioTelegram driver

  uploadFile (TelegramDriver driver) =
    uploadFileTelegram driver

  editMessage (TelegramDriver driver) =
    editMessageTelegram driver

  deleteMessage (TelegramDriver driver) =
    deleteMessageForTelegram driver

  messageOutPolicy _ _ =
    pure (ChatEffect.EditableMessage Protocol.telegramEditChunkChars Protocol.telegramMessageTextLimit)

  getMessageContent (TelegramDriver driver) =
    getMessageContentTelegram driver

  getSenderMemberInfo (TelegramDriver driver) message =
    case (message.kind, message.chatId, message.senderId) of
      (ChatGroup, Just chatId, Just rawUserId)
        | Just userId <- parseIntegerUserId rawUserId ->
            Just . Aeson.toJSON <$> getChatMember driver chatId userId
      _ -> pure Nothing

  getMemberInfo (TelegramDriver driver) message userId =
    case (message.kind, message.chatId) of
      (ChatGroup, Just chatId)
        | Just numericUserId <- parseIntegerUserId userId ->
            Just . Aeson.toJSON <$> getChatMember driver chatId numericUserId
      _ -> pure Nothing

  getUserAvatar (TelegramDriver driver) _ userId =
    maybe (pure Nothing) (getUserAvatar driver) (parseIntegerUserId userId)

  mentionUser (TelegramDriver driver) =
    mentionUserTelegram driver

  setTyping (TelegramDriver driver) message _ =
    for_ message.chatId (Protocol.setTypingTelegram driver)

incomingMessages
  :: (HTTP.HTTP :> es, KatipE :> es, Concurrent :> es, IOE :> es)
  => TelegramDriver
  -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages (TelegramDriver driver) =
  incomingMessagesProtocol driver

incomingMessagesProtocol :: (HTTP.HTTP :> es, KatipE :> es, Concurrent :> es, IOE :> es) => Protocol.TelegramDriver -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessagesProtocol driver = S.for (updatesStream driver) $ \update -> do
  case updateToIncomingMessageWith driver.config update of
    Nothing -> do
      S.lift $ $(logDebug) [i|Ignoring Telegram event|]
    Just parsedMessage -> do
      message <- S.lift $
        resolveIncomingMessageMedia driver parsedMessage `catchSync` \err -> do
          $(logError) [i|Telegram media resolution failed: #{show err :: String}|]
          pure parsedMessage
      S.lift $ $(logDebug) ("incoming Telegram message:\n" <> logJsonText message)
      S.yield message

resolveIncomingMessageMedia :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => Protocol.TelegramDriver -> IncomingMessage -> Eff es IncomingMessage
resolveIncomingMessageMedia driver message = do
  imageUrls <- traverse (fileUrl driver) message.imageUrls
  files <- traverse (traverseMessageFileRef (fileUrl driver)) message.files
  pure (message :: IncomingMessage){imageUrls, files}

updateToIncomingMessage :: Update -> Maybe IncomingMessage
updateToIncomingMessage =
  updateToIncomingMessageWith defaultMessageConfig

updateToIncomingMessageWith :: Config -> Update -> Maybe IncomingMessage
updateToIncomingMessageWith cfg Update{message = telegramMessage} = do
  message <- telegramMessage
  guard (not (isBotMessage message))
  pure IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform  = PlatformTelegram
    , kind      = telegramChatKind message.chat.type_
    , chatId    = Just message.chat.id
    , chatAliases = telegramChatAliases message.chat
    , chatDisplayName = telegramChatDisplayName message.chat
    , digest = telegramMessageDigest cfg message
    , senderId  = Text.pack . show . (.id) <$> message.from
    , senderUsername = message.from >>= (.username)
    , senderDisplayName = telegramUserFullName <$> message.from
    , senderGlobalDisplayName = telegramUserFullName <$> message.from
    , messageId = Just (integerMessageId message.messageId)
    , replyToMessageId = integerMessageId . (.messageId) <$> message.replyToMessage
    , mentions  = messageMentionIds message
    , mentionUsernames = messageMentionUsernames message
    , imageUrls = messageImageFileIds message
    , files = telegramMessageFiles message
    , text      = messageText message
    , raw       = Aeson.toJSON message
    }

defaultMessageConfig :: Config
defaultMessageConfig =
  Config
    { botToken = ""
    , botIds = []
    , botUsernames = []
    , allowedChatIds = []
    , allowedChatAliases = []
    , superusers = []
    }

telegramMessageDigest :: Config -> Message -> MessageDigest
telegramMessageDigest cfg message =
  MessageDigest
    { chatIsAllowed = chatAllowed
    , senderIsAllowed = telegramChatKind message.chat.type_ == ChatPrivate && (chatAllowed || senderSuperuser)
    , senderIsSuperuser = senderSuperuser
    , mentionsBot =
        any (`elem` map show cfg.botIds) (messageMentionIds message) ||
        any (`elem` cfg.botUsernames) (messageMentionUsernames message)
    , botId = listToMaybe (map (Text.pack . show) cfg.botIds <> cfg.botUsernames)
    }
  where
    chatAllowed =
      message.chat.id `elem` cfg.allowedChatIds ||
        any (`elem` cfg.allowedChatAliases) (telegramChatAliases message.chat)
    senderSuperuser =
      maybe False (`elem` cfg.superusers) (normalizeUsername <$> (message.from >>= (.username)))

telegramChatAliases :: Chat -> [Text]
telegramChatAliases chat =
  map normalizeUsername (catMaybes [chat.username, chat.title])

telegramChatDisplayName :: Chat -> Maybe Text
telegramChatDisplayName chat =
  nonEmptyText $ Text.unwords $ catMaybes [chat.title, chat.firstName, chat.lastName, chat.username]

isBotMessage :: Message -> Bool
isBotMessage message =
  maybe False (.isBot) message.from

telegramChatKind :: ChatType -> ChatKind
telegramChatKind = \case
  ChatTypePrivate    -> ChatPrivate
  ChatTypeGroup      -> ChatGroup
  ChatTypeSuperGroup -> ChatGroup
  ChatTypeChannel    -> ChatChannel

messageMentionIds :: Message -> [Text]
messageMentionIds message =
  mapMaybe entityMentionUserId (messageEntities message)

entityMentionUserId :: MessageEntity -> Maybe Text
entityMentionUserId messageEntity =
  show . (.id) <$> messageEntity.user

messageMentionUsernames :: Message -> [Text]
messageMentionUsernames message =
  mapMaybe (entityMentionUsername (messageText message)) (messageEntities message)

messageText :: Message -> Text
messageText message =
  Text.unwords (filter (not . Text.null) [body, sticker])
  where
    body = Text.strip (fromMaybe "" (message.text <|> message.caption))
    sticker = maybe "" telegramStickerText message.sticker

telegramStickerText :: Sticker -> Text
telegramStickerText sticker =
  maybe "[sticker]" (\emoji -> "[sticker: " <> emoji <> "]") (sticker.emoji >>= nonEmptyText)

messageEntities :: Message -> [MessageEntity]
messageEntities message =
  fromMaybe [] (message.entities <|> message.captionEntities)

messageImageFileIds :: Message -> [Text]
messageImageFileIds message =
  maybe [] (maybeToList . largestPhotoFileId) message.photo

telegramMessageFiles :: Message -> [MessageFile]
telegramMessageFiles message =
  catMaybes
    [ telegramMessageFile "document" <$> message.document
    , telegramMessageFile "audio" <$> message.audio
    , telegramMessageFile "voice" <$> message.voice
    ]

telegramMessageFile :: Text -> TelegramMedia -> MessageFile
telegramMessageFile fallback media =
  MessageFile
    { name = fromMaybe (fallback <> maybe "" Mime.extensionFromMime media.mimeType) media.fileName
    , ref = media.fileId
    }

traverseMessageFileRef :: Functor f => (Text -> f Text) -> MessageFile -> f MessageFile
traverseMessageFileRef resolve file =
  (\ref -> MessageFile{name = file.name, ref}) <$> resolve file.ref

largestPhotoFileId :: [PhotoSize] -> Maybe Text
largestPhotoFileId =
  fmap (.fileId) . largestPhoto

largestPhoto :: [PhotoSize] -> Maybe PhotoSize
largestPhoto =
  viaNonEmpty largest
  where
    largest photos =
      let MaxPhotoSize photo = sconcat (fmap MaxPhotoSize photos)
      in photo

newtype MaxPhotoSize = MaxPhotoSize PhotoSize

instance Semigroup MaxPhotoSize where
  MaxPhotoSize a <> MaxPhotoSize b =
    MaxPhotoSize (if photoArea a >= photoArea b then a else b)

photoArea :: PhotoSize -> Integer
photoArea photo =
  photo.width * photo.height

-- | Resolve the content of a replied-to Telegram message when available locally.
getMessageContentTelegram
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => Protocol.TelegramDriver
  -> IncomingMessage
  -> MessageId
  -> Eff es (Maybe ReferencedMessage)
getMessageContentTelegram driver message messageId =
  case messageIdInteger messageId of
    Nothing ->
      pure Nothing
    Just rawMessageId ->
      case Aeson.fromJSON message.raw :: Aeson.Result Message of
        Aeson.Success telegramMessage ->
          case telegramMessage.replyToMessage of
            Just referenced
              | referenced.messageId == rawMessageId -> do
                  imageUrls <- traverse (fileUrl driver) (messageImageFileIds referenced)
                  files <- traverse (traverseMessageFileRef (fileUrl driver)) (telegramMessageFiles referenced)
                  pure $ Just ReferencedMessage
                    { messageId = Just (integerMessageId referenced.messageId)
                    , senderDisplayName = telegramMessageSenderDisplayName referenced
                    , senderIdentifier = telegramMessageSenderIdentifier referenced
                    , senderIsBot = maybe False (.isBot) referenced.from
                    , text = messageText referenced
                    , imageUrls = imageUrls
                    , files
                    }
            _ -> pure Nothing
        Aeson.Error _ ->
          pure Nothing

entityMentionUsername :: Text -> MessageEntity -> Maybe Text
entityMentionUsername text messageEntity
  | Just username <- messageEntity.user >>= (.username) =
      Just (normalizeUsername username)
  | messageEntity.type_ == "mention" =
      normalizeUsername <$> entityText text messageEntity
  | otherwise =
      Nothing

telegramMessageSenderDisplayName :: Message -> Maybe Text
telegramMessageSenderDisplayName message =
  telegramUserFullName <$> message.from

telegramMessageSenderIdentifier :: Message -> Maybe Text
telegramMessageSenderIdentifier message =
  message.from <&> \user ->
    maybe (show user.id) ("@" <>) user.username

telegramUserFullName :: User -> Text
telegramUserFullName user =
  Text.unwords (filter (not . Text.null) [user.firstName, fromMaybe "" user.lastName])

entityText :: Text -> MessageEntity -> Maybe Text
entityText text messageEntity =
  let piece = Text.take (fromInteger messageEntity.length) (Text.drop (fromInteger messageEntity.offset) text)
  in if Text.null piece
    then Nothing
    else Just piece

normalizeUsername :: Text -> Text
normalizeUsername =
  Text.toLower . Text.dropWhile (== '@') . Text.strip


-- | Reply to a Telegram chat, including image directives in the body.
replyToTelegram
  :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es)
  => Protocol.TelegramDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Either Text MessageId)
replyToTelegram driver message body =
  case message.chatId of
    Just chatId -> do
      let replyToMessageId = messageIdInteger =<< message.messageId
      sent <- replyTextAndImages driver chatId replyToMessageId body `catch` \(err :: TelegramException) ->
        sendTelegramFailureReply driver chatId replyToMessageId err
      pure (Right (integerMessageId sent.messageId))
    _ ->
      pure (Left "Telegram reply requires a Telegram chat id.")

sendTelegramFailureReply :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => Protocol.TelegramDriver -> Integer -> Maybe Integer -> TelegramException -> Eff es Message
sendTelegramFailureReply driver chatId replyToMessageId err =
  callTelegram driver SendMessageRequest
    { chatId = chatId
    , messageThreadId = Nothing
    , text = telegramFailureReplyText err
    , parseMode = Nothing
    , entities = Nothing
    , disableNotification = Nothing
    , replyToMessageId = replyToMessageId
    }

telegramFailureReplyText :: TelegramException -> Text
telegramFailureReplyText (TelegramException message) =
  "Telegram request failed: " <> message

-- | Edit a Telegram text message previously sent by this bot.
editMessageTelegram
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => Protocol.TelegramDriver
  -> IncomingMessage
  -> MessageId
  -> Text
  -> Eff es Bool
editMessageTelegram driver message messageId body =
  case (message.chatId, messageIdInteger messageId) of
    (Just chatId, Just rawMessageId) -> do
      let text = ChatEffect.renderReplyBody body
      void $
        withRichMessageFallback text
          (callTelegram driver EditMessageTextRequest
            { chatId = chatId
            , messageId = rawMessageId
            , richMessage = InputRichMessage
                { html = formatTelegramRichHtml text
                , media = Nothing
                }
            })
          (callTelegram driver EditPlainMessageTextRequest
            { chatId = chatId
            , messageId = rawMessageId
            , text = text
            , parseMode = Nothing
            , entities = Nothing
            })
      pure True
    _ ->
      pure False

-- | Delete a Telegram message in the current chat when the bot has permission.
deleteMessageForTelegram
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => Protocol.TelegramDriver
  -> IncomingMessage
  -> MessageId
  -> Eff es Bool
deleteMessageForTelegram driver message messageId =
  case (message.chatId, messageIdInteger messageId) of
    (Just chatId, Just rawMessageId) ->
      deleteMessage driver chatId rawMessageId
    _ ->
      pure False

-- | Reply with an HTML mention for a Telegram user id.
mentionUserTelegram
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => Protocol.TelegramDriver
  -> IncomingMessage
  -> Text
  -> Text
  -> Eff es (Either Text MessageId)
mentionUserTelegram driver message userId body =
  case message.chatId of
    Just chatId
      | Just numericUserId <- parseIntegerUserId userId -> do
      let replyToMessageId = messageIdInteger =<< message.messageId
      sent <- callTelegram driver SendMessageRequest
        { chatId = chatId
        , messageThreadId = Nothing
        , text = telegramMentionHtml numericUserId body
        , parseMode = Just ParseModeHTML
        , entities = Nothing
        , disableNotification = Nothing
        , replyToMessageId = replyToMessageId
        }
      pure (Right (integerMessageId sent.messageId))
    _ ->
      pure (Left "Telegram mention reply requires a Telegram chat id and numeric user id.")

-- | Reply with audio as a Telegram voice message.
replyAudioTelegram
  :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es)
  => Protocol.TelegramDriver
  -> IncomingMessage
  -> Text
  -> Maybe Text
  -> Eff es (Either Text MessageId)
replyAudioTelegram driver message audioRef caption =
  case message.chatId of
    Just chatId -> do
      let replyToMessageId = messageIdInteger =<< message.messageId
      sent <- sendVoiceRequest driver SendVoiceRequest
        { chatId = chatId
        , messageThreadId = Nothing
        , voice = audioRef
        , caption = nonEmptyText . ChatEffect.renderReplyBody =<< caption
        , parseMode = Nothing
        , captionEntities = Nothing
        , disableNotification = Nothing
        , replyToMessageId = replyToMessageId
        }
      pure (Right (integerMessageId sent.messageId))
    _ ->
      pure (Left "Telegram audio reply requires a Telegram chat id.")

-- | Send a file to a Telegram chat as a document.
uploadFileTelegram
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => Protocol.TelegramDriver
  -> IncomingMessage
  -> FilePath
  -> Maybe Text
  -> Eff es (Either Text MessageId)
uploadFileTelegram driver message path fileName =
  case message.chatId of
    Just chatId -> do
      let replyToMessageId = messageIdInteger =<< message.messageId
          baseRequest =
            TelegramUploadRequest
              { chatId = chatId
              , messageThreadId = Nothing
              , caption = Nothing
              , parseMode = Nothing
              , captionEntities = Nothing
              , disableNotification = Nothing
              , replyToMessageId = replyToMessageId
              }
      sent <- uploadTelegramFileByMime driver baseRequest path fileName
      pure (Right (integerMessageId sent.messageId))
    _ ->
      pure (Left "Telegram file upload requires a Telegram chat id.")

data TelegramUploadRequest = TelegramUploadRequest
  { chatId :: !Integer
  , messageThreadId :: !(Maybe Integer)
  , caption :: !(Maybe Text)
  , parseMode :: !(Maybe ParseMode)
  , captionEntities :: !(Maybe [MessageEntity])
  , disableNotification :: !(Maybe Bool)
  , replyToMessageId :: !(Maybe Integer)
  }

uploadTelegramFileByMime :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => Protocol.TelegramDriver -> TelegramUploadRequest -> FilePath -> Maybe Text -> Eff es Message
uploadTelegramFileByMime driver request path fileName =
  case telegramFileKind (Mime.mimeFromName (Text.pack path)) of
    TelegramImageFile ->
      apiMultipartCall driver.config "sendPhoto" (sendPhotoParts (photoRequest request) path fileName)
    TelegramAudioFile ->
      uploadAudio driver (audioRequest request) path fileName
    TelegramVideoFile ->
      uploadVideo driver (videoRequest request) path fileName
    TelegramDocumentFile ->
      uploadDocument driver (documentRequest request) path fileName

data TelegramFileKind
  = TelegramImageFile
  | TelegramAudioFile
  | TelegramVideoFile
  | TelegramDocumentFile

telegramFileKind :: Text -> TelegramFileKind
telegramFileKind mime
  | "image/" `Text.isPrefixOf` clean = TelegramImageFile
  | "audio/" `Text.isPrefixOf` clean = TelegramAudioFile
  | "video/" `Text.isPrefixOf` clean = TelegramVideoFile
  | otherwise = TelegramDocumentFile
  where
    clean = Text.toLower (Text.takeWhile (/= ';') mime)

photoRequest :: TelegramUploadRequest -> SendPhotoRequest
photoRequest TelegramUploadRequest{..} =
  SendPhotoRequest
    { photo = "attach://photo"
    , ..
    }

audioRequest :: TelegramUploadRequest -> SendAudioRequest
audioRequest TelegramUploadRequest{..} =
  SendAudioRequest{..}

videoRequest :: TelegramUploadRequest -> SendVideoRequest
videoRequest TelegramUploadRequest{..} =
  SendVideoRequest{..}

documentRequest :: TelegramUploadRequest -> SendDocumentRequest
documentRequest TelegramUploadRequest{..} =
  SendDocumentRequest{..}

telegramMentionHtml :: Integer -> Text -> Text
telegramMentionHtml userId body =
  [i|<a href="tg://user?id=#{userId}">user</a> #{escapeHtml body}|]

replyTextAndImages :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es) => Protocol.TelegramDriver -> Integer -> Maybe Integer -> Text -> Eff es Message
replyTextAndImages driver chatId replyToMessageId body = do
  let text = ChatEffect.renderReplyBody body
      images = ChatEffect.replyImageUrls body
  sent <- withRichMessageFallback text
    (do
      (richMessage, mediaParts) <- prepareRichMessage driver text images
      sendPreparedRichMessage driver SendRichMessageRequest
        { chatId = chatId
        , messageThreadId = Nothing
        , richMessage = richMessage
        , disableNotification = Nothing
        , replyParameters = ReplyParameters <$> replyToMessageId
        } mediaParts)
    (legacyReply text images)
  traverse_ (Media.recordMediaPlatform PlatformTelegram) images
  pure sent
  where
    legacyReply text = \case
      [] ->
        callTelegram driver SendMessageRequest
            { chatId = chatId
            , messageThreadId = Nothing
            , text = text
            , parseMode = Nothing
            , entities = Nothing
            , disableNotification = Nothing
            , replyToMessageId = replyToMessageId
            }
      firstImage : restImages -> do
        firstSent <- sendImageRequest driver SendPhotoRequest
          { chatId = chatId
          , messageThreadId = Nothing
          , photo = firstImage
          , caption = nonEmptyText text
          , parseMode = Nothing
          , captionEntities = Nothing
          , disableNotification = Nothing
          , replyToMessageId = replyToMessageId
          }
        traverse_ (sendImage Nothing) restImages
        pure firstSent
    sendImage caption photo = void $ sendImageRequest driver SendPhotoRequest
        { chatId = chatId
        , messageThreadId = Nothing
        , photo = photo
        , caption = caption
        , parseMode = Nothing
        , captionEntities = Nothing
        , disableNotification = Nothing
        , replyToMessageId = Nothing
        }

data PreparedRichMedia = PreparedRichMedia
  { block :: !Text
  , input :: !(Maybe InputRichMessageMedia)
  , part :: !(Maybe Multipart.Part)
  }

prepareRichMessage
  :: (Media.Media :> es)
  => Protocol.TelegramDriver
  -> Text
  -> [Text]
  -> Eff es (InputRichMessage, [Multipart.Part])
prepareRichMessage driver text refs = do
  prepared <- zipWithM (prepareRichPhoto driver) [(1 :: Int)..] refs
  let html = Text.intercalate "\n" $ filter (not . Text.null) (formatTelegramRichHtml text : map (.block) prepared)
      media = viaNonEmpty toList (mapMaybe (.input) prepared)
  pure
    ( InputRichMessage{html, media}
    , mapMaybe (.part) prepared
    )

prepareRichPhoto :: Media.Media :> es => Protocol.TelegramDriver -> Int -> Text -> Eff es PreparedRichMedia
prepareRichPhoto driver index ref =
  Media.platformMediaRef "telegram" (telegramMediaScope driver) ref >>= \case
    Just telegramFileId ->
      pure (referenced telegramFileId Nothing)
    Nothing ->
      Media.localMediaPath ref >>= \case
        Just path ->
          let attachment = "rich-media-" <> show index
          in pure (referenced ("attach://" <> attachment) (Just (telegramFilePart attachment path Nothing)))
        Nothing ->
          pure PreparedRichMedia
            { block = [i|<img src="#{escapeHtml ref}"/>|]
            , input = Nothing
            , part = Nothing
            }
  where
    mediaId = "media-" <> show index
    referenced mediaRef part = PreparedRichMedia
      { block = [i|<img src="tg://photo?id=#{mediaId}"/>|]
      , input = Just InputRichMessageMedia
          { id = mediaId
          , media = InputRichMedia
              { type_ = "photo"
              , media = mediaRef
              }
          }
      , part = part
      }

sendPreparedRichMessage
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => Protocol.TelegramDriver
  -> SendRichMessageRequest
  -> [Multipart.Part]
  -> Eff es Message
sendPreparedRichMessage driver request mediaParts
  | null mediaParts =
      callTelegram driver request
  | otherwise =
      apiMultipartCall driver.config "sendRichMessage" (richMessageParts request <> mediaParts)

richMessageParts :: SendRichMessageRequest -> [Multipart.Part]
richMessageParts SendRichMessageRequest{..} =
  [ textPart "chat_id" (show chatId)
  , textPart "rich_message" (jsonText richMessage)
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_parameters" (jsonText <$> replyParameters)

sendImageRequest :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es) => Protocol.TelegramDriver -> SendPhotoRequest -> Eff es Message
sendImageRequest driver request =
  Media.platformMediaRef "telegram" (telegramMediaScope driver) originalPhoto >>= \case
    Just telegramFileId ->
      sendPhoto driver (replacePhoto telegramFileId request)
    Nothing ->
      case localFilePhoto originalPhoto of
        Just path ->
          uploadAndRemember path
        Nothing ->
          Media.localMediaPath originalPhoto >>= \case
            Just path ->
              uploadAndRemember path
            Nothing ->
              case dataImagePhoto originalPhoto of
                Just bytes -> uploadTemporaryPhoto driver request bytes
                Nothing -> do
                  sent <- sendPhoto driver request
                  rememberTelegramPhotoRef originalPhoto sent
                  pure sent
  where
    originalPhoto =
      request.photo

    uploadAndRemember path = do
      sent <- uploadPhoto driver request path
      rememberTelegramPhotoRef originalPhoto sent
      pure sent

    rememberTelegramPhotoRef ref sent =
      when (telegramCacheablePhotoRef ref) do
        for_ (sent.photo >>= largestPhotoFileId) \telegramFileId ->
          Media.storePlatformMediaRef "telegram" (telegramMediaScope driver) ref telegramFileId

telegramMediaScope :: Protocol.TelegramDriver -> Text
telegramMediaScope driver =
  fromMaybe "default" $
    (("id:" <>) . show <$> viaNonEmpty head driver.config.botIds) <|>
      (("username:" <>) <$> viaNonEmpty head driver.config.botUsernames)

telegramCacheablePhotoRef :: Text -> Bool
telegramCacheablePhotoRef ref =
  let stripped = Text.strip ref
      lower = Text.toLower stripped
  in not (Text.null stripped) && not ("data:image/" `Text.isPrefixOf` lower)

replacePhoto :: Text -> SendPhotoRequest -> SendPhotoRequest
replacePhoto newPhoto SendPhotoRequest{..} =
  SendPhotoRequest{photo = newPhoto, ..}

sendVoiceRequest :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es) => Protocol.TelegramDriver -> SendVoiceRequest -> Eff es Message
sendVoiceRequest driver request =
  case localFileRef request.voice of
    Just path -> uploadVoice driver request path
    Nothing ->
      case dataAudioBytes request.voice of
        Just bytes -> uploadTemporaryVoice driver request bytes
        Nothing    -> callTelegram driver request

localFilePhoto :: Text -> Maybe FilePath
localFilePhoto photo =
  localFileRef photo

localFileRef :: Text -> Maybe FilePath
localFileRef ref =
  let stripped = Text.strip ref
  in case Text.stripPrefix "file://" stripped of
    Just path ->
      Just (Text.unpack path)
    Nothing
      | isLocalPathRef stripped ->
          Just (Text.unpack stripped)
      | otherwise ->
          Nothing

isLocalPathRef :: Text -> Bool
isLocalPathRef ref =
  "/" `Text.isPrefixOf` ref || "./" `Text.isPrefixOf` ref || "../" `Text.isPrefixOf` ref

dataImagePhoto :: Text -> Maybe ByteString.ByteString
dataImagePhoto photo = do
  rest <- Text.stripPrefix "data:image/" (Text.strip photo)
  let (_, encodedWithMarker) = Text.breakOn ";base64," rest
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encoded))

dataAudioBytes :: Text -> Maybe ByteString.ByteString
dataAudioBytes ref = do
  rest <- Text.stripPrefix "data:audio/" (Text.strip ref)
  let (_, encodedWithMarker) = Text.breakOn ";base64," rest
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encoded))

uploadTemporaryPhoto :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es) => Protocol.TelegramDriver -> SendPhotoRequest -> ByteString.ByteString -> Eff es Message
uploadTemporaryPhoto driver request bytes = do
  withTemporaryTelegramFile "telegram-photo" "png" bytes (uploadPhoto driver request)

uploadTemporaryVoice :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es) => Protocol.TelegramDriver -> SendVoiceRequest -> ByteString.ByteString -> Eff es Message
uploadTemporaryVoice driver request bytes = do
  withTemporaryTelegramFile "telegram-voice" "ogg" bytes (uploadVoice driver request)

withTemporaryTelegramFile :: (FileSystem :> es, IOE :> es) => FilePath -> FilePath -> ByteString.ByteString -> (FilePath -> Eff es a) -> Eff es a
withTemporaryTelegramFile prefix extension bytes action =
  Temporary.runTemporary $
    Temporary.withSystemTempDirectory "cosmobot-telegram-" \dir -> do
      let path = dir </> (prefix <.> extension)
      FileSystemByteString.writeFile path bytes
      raise (action path)

nonEmptyText :: Text -> Maybe Text
nonEmptyText text
  | Text.null text = Nothing
  | otherwise      = Just text

getUserAvatar :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => Protocol.TelegramDriver -> Integer -> Eff es (Maybe Aeson.Value)
getUserAvatar driver userId = do
  profilePhotos <- callTelegram driver GetUserProfilePhotosRequest
    { userId = userId
    , offset = Nothing
    , limit = Just 1
    }
  case profilePhotos.photos >>= maybeToList . largestPhoto of
    [] ->
      pure Nothing
    photo : _ -> do
      avatarUrl <- fileUrl driver photo.fileId
      pure $ Just $ Aeson.object
        [ "platform" Aeson..= ("telegram" :: Text)
        , "user_id" Aeson..= userId
        , "avatar_url" Aeson..= avatarUrl
        , "file_id" Aeson..= photo.fileId
        , "width" Aeson..= photo.width
        , "height" Aeson..= photo.height
        ]

parseIntegerUserId :: Text -> Maybe Integer
parseIntegerUserId raw =
  case reads (Text.unpack (Text.strip raw)) of
    [(userId, "")] ->
      Just userId
    _ ->
      Nothing
