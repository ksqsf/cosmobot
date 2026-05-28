{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Chat.Driver.Telegram
Description : Telegram chat driver
Stability   : experimental
-}

module Bot.Chat.Driver.Telegram
  ( telegramDriver
  , Telegram
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
  , ParseMode (..)
  , SendMessageRequest (..)
  , EditMessageTextRequest (..)
  , SendPhotoRequest (..)
  , SendDocumentRequest (..)
  , SendVoiceRequest (..)
  , TelegramFormatted (..)
  , TelegramException (..)
  , TelegramResult
  , parseTelegramResult
  , formatTelegramMarkdown
  , telegramFailureReplyText
  , runTelegram
  , incomingMessages
  , updateToIncomingMessage
  , updateToIncomingMessageWith
  , getMe
  , getUpdates
  , sendMessage
  , sendPhoto
  , uploadPhoto
  , replyAudio
  , uploadFile
  , replyTo
  , editMessage
  , deleteMessageFor
  , getMessageContent
  , forwardMessage
  , deleteMessage
  , pinMessage
  , unpinMessage
  , getChat
  , getChatMember
  , banChatMember
  , unbanChatMember
  , leaveChat
  , getUserAvatar
  , mentionUser
  )
where

import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Chat as ChatEffect
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Media.Mime as Mime
import Bot.Util.Multipart
import Bot.Core.Message
import Commonmark hiding (escapeHtml)
import qualified Commonmark.Entity as Commonmark
import Commonmark.Extensions
  ( HasFootnote (..)
  , HasMath (..)
  , HasStrikethrough (..)
  , HasTaskList (..)
  , footnoteSpec
  , mathSpec
  , strikethroughSpec
  , taskListSpec
  )
import Data.List (maximum)
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import qualified Data.Text.Encoding as TextEncoding
import qualified Network.HTTP.Client as Client
import Network.HTTP.Req
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import qualified Network.HTTP.Types.Header as HTTPHeader
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import GHC.Clock (getMonotonicTimeNSec)
import qualified Data.Text as Text
import System.FilePath ((</>), (<.>), takeFileName)

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

-- | Telegram Bot API credentials.
data Config = Config
  { botToken :: !Text
  , botIds :: ![Integer]
  , botUsernames :: ![Text]
  , allowedChatIds :: ![Integer]
  , allowedChatAliases :: ![Text]
  , superusers :: ![Text]
  }
  deriving (Show)

telegramDriver
  :: (Telegram :> es, FileSystem :> es, IOE :> es)
  => Driver.ChatPlatformDriver es
telegramDriver = Driver.ChatPlatformDriver
  { Driver.platform = PlatformTelegram
  , Driver.replyTo = replyTo
  , Driver.replyAudio = replyAudio
  , Driver.uploadFile = uploadFile
  , Driver.editMessage = editMessage
  , Driver.deleteMessage = deleteMessageFor
  , Driver.replyStreamStyle = \_ -> pure (ChatEffect.EditableReply telegramEditChunkChars telegramMessageTextLimit)
  , Driver.getMessageContent = getMessageContent
  , Driver.getSenderMemberInfo = \message ->
      case (message.kind, message.chatId, message.senderId) of
        (ChatGroup, Just chatId, Just rawUserId)
          | Just userId <- parseIntegerUserId rawUserId ->
          Just . Aeson.toJSON <$> getChatMember chatId userId
        _ ->
          pure Nothing
  , Driver.getMemberInfo = \message userId ->
      case (message.kind, message.chatId) of
        (ChatGroup, Just chatId)
          | Just numericUserId <- parseIntegerUserId userId ->
            Just . Aeson.toJSON <$> getChatMember chatId numericUserId
        _ ->
          pure Nothing
  , Driver.getUserAvatar = \message userId ->
      case message.platform of
        PlatformTelegram ->
          maybe (pure Nothing) getUserAvatar (parseIntegerUserId userId)
        _ ->
          pure Nothing
  , Driver.listGroupMembers = \_ ->
      pure Nothing
  , Driver.normalizeMediaRef = pure
  , Driver.mentionUser = mentionUser
  , Driver.setMemberTitle = \_ _ _ -> pure False
  , Driver.setTyping = \message _timeoutMillis ->
      case (message.platform, message.chatId) of
        (PlatformTelegram, Just chatId) -> do
          _ <- callTelegram (SendChatActionRequest chatId ChatActionTyping Nothing Nothing)
          pure ()
        _ ->
          pure ()
  }

telegramEditChunkChars :: Int
telegramEditChunkChars = 512

telegramMessageTextLimit :: Int
telegramMessageTextLimit = 4096

-- ---------------------------------------------------------------------------
-- Typeclass
-- ---------------------------------------------------------------------------

class (Aeson.ToJSON req, Aeson.FromJSON (TelegramResponse req)) => TelegramRequest req where
  type TelegramResponse req
  telegramMethod :: req -> Text

-- ---------------------------------------------------------------------------
-- Effect
-- ---------------------------------------------------------------------------

-- | Telegram Bot API effect.
data Telegram :: Effect where
  TelegramConfig :: Telegram m Config
  CallTelegram
    :: TelegramRequest req
    => req -> Telegram m (TelegramResponse req)
  FileUrl
    :: Text
    -> Telegram m Text
  UploadPhoto
    :: SendPhotoRequest
    -> FilePath
    -> Telegram m Message
  UploadVoice
    :: SendVoiceRequest
    -> FilePath
    -> Telegram m Message
  UploadAudio
    :: SendAudioRequest
    -> FilePath
    -> Telegram m Message
  UploadVideo
    :: SendVideoRequest
    -> FilePath
    -> Telegram m Message
  UploadDocument
    :: SendDocumentRequest
    -> FilePath
    -> Telegram m Message

type instance DispatchOf Telegram = Dynamic

telegramConfig :: Telegram :> es => Eff es Config
telegramConfig = send TelegramConfig

callTelegram
  :: (Telegram :> es, TelegramRequest req)
  => req -> Eff es (TelegramResponse req)
callTelegram = send . CallTelegram

fileUrl
  :: Telegram :> es
  => Text
  -> Eff es Text
fileUrl = send . FileUrl

-- | Interpret Telegram operations with HTTP calls to the Bot API.
runTelegram
  :: IOE :> es
  => KatipE :> es
  => HTTP.HTTP :> es
  => Config
  -> Eff (Telegram : es) a
  -> Eff es a
runTelegram cfg inner = do
  interpret
    ( \_ -> \case
        TelegramConfig ->
          pure cfg
        CallTelegram request ->
          apiCall cfg (telegramMethod request) request
        FileUrl fileId -> do
          file :: File <- apiCall cfg (telegramMethod (GetFileRequest fileId)) (GetFileRequest fileId)
          pure (telegramFileUrl cfg file.filePath)
        UploadPhoto request path ->
          apiMultipartCall cfg "sendPhoto" (sendPhotoParts request path)
        UploadVoice request path ->
          apiMultipartCall cfg "sendVoice" (sendVoiceParts request path)
        UploadAudio request path ->
          apiMultipartCall cfg "sendAudio" (sendAudioParts request path)
        UploadVideo request path ->
          apiMultipartCall cfg "sendVideo" (sendVideoParts request path)
        UploadDocument request path ->
          apiMultipartCall cfg "sendDocument" (sendDocumentParts request path)
    )
    inner

-- ---------------------------------------------------------------------------
-- Streaming
-- ---------------------------------------------------------------------------

updatesStream'
  :: (Telegram :> es, KatipE :> es, IOE :> es)
  => Int
  -> Stream (Of Update) (Eff es) ()
updatesStream' offset = do
  batches <- S.lift (getUpdates offset)
  S.lift $ logDebug [i|Got a batch of #{length batches} messages|]
  S.lift $ logInfo [i|Telegram update batch: #{length batches}|]
  S.each batches
  let nextOffset = case batches of
        [] -> offset
        _  -> 1 + maximum (map (fromInteger . (.updateId)) batches)
  updatesStream' nextOffset

updatesStream :: (Telegram :> es, KatipE :> es, IOE :> es) => Stream (Of Update) (Eff es) ()
updatesStream = updatesStream' 0

-- | Poll Telegram updates and yield platform-independent messages.
incomingMessages :: (Telegram :> es, KatipE :> es, IOE :> es) => Stream (Of IncomingMessage) (Eff es) ()
incomingMessages = S.for updatesStream $ \update -> do
  cfg <- S.lift telegramConfig
  case updateToIncomingMessageWith cfg update of
    Nothing -> do
      S.lift $ logDebug [i|Ignoring Telegram event|]
      S.lift $ logInfo "Ignoring Telegram event"
    Just parsedMessage -> do
      message <- S.lift $
        resolveIncomingMessageImages parsedMessage `catchSync` \err -> do
          logError [i|Telegram image resolution failed: #{show err :: String}|]
          pure parsedMessage
      S.lift $ logDebug [i|incoming Telegram message: #{show message :: String}|]
      S.lift $ logInfo [i|incoming Telegram message: #{incomingMessageLogLine message}|]
      S.yield message

resolveIncomingMessageImages :: Telegram :> es => IncomingMessage -> Eff es IncomingMessage
resolveIncomingMessageImages message = do
  imageUrls <- traverse fileUrl message.imageUrls
  pure IncomingMessage
    { platform = message.platform
    , kind = message.kind
    , chatId = message.chatId
    , chatAliases = message.chatAliases
    , digest = message.digest
    , senderId = message.senderId
    , senderUsername = message.senderUsername
    , messageId = message.messageId
    , replyToMessageId = message.replyToMessageId
    , mentions = message.mentions
    , mentionUsernames = message.mentionUsernames
    , imageUrls = imageUrls
    , text = message.text
    , raw = message.raw
    }

updateToIncomingMessage :: Update -> Maybe IncomingMessage
updateToIncomingMessage =
  updateToIncomingMessageWith defaultMessageConfig

updateToIncomingMessageWith :: Config -> Update -> Maybe IncomingMessage
updateToIncomingMessageWith cfg Update{message = telegramMessage} = do
  message <- telegramMessage
  guard (not (isBotMessage message))
  pure IncomingMessage
    { platform  = PlatformTelegram
    , kind      = telegramChatKind message.chat.type_
    , chatId    = Just message.chat.id
    , chatAliases = telegramChatAliases message.chat
    , digest = telegramMessageDigest cfg message
    , senderId  = Text.pack . show . (.id) <$> message.from
    , senderUsername = message.from >>= (.username)
    , messageId = Just (integerMessageId message.messageId)
    , replyToMessageId = integerMessageId . (.messageId) <$> message.replyToMessage
    , mentions  = messageMentionIds message
    , mentionUsernames = messageMentionUsernames message
    , imageUrls = messageImageFileIds message
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
  Text.strip (fromMaybe "" (message.text <|> message.caption))

messageEntities :: Message -> [MessageEntity]
messageEntities message =
  fromMaybe [] (message.entities <|> message.captionEntities)

messageImageFileIds :: Message -> [Text]
messageImageFileIds message =
  maybe [] (maybeToList . largestPhotoFileId) message.photo

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
getMessageContent :: Telegram :> es => IncomingMessage -> MessageId -> Eff es (Maybe ReferencedMessage)
getMessageContent message messageId =
  case messageIdInteger messageId of
    Nothing ->
      pure Nothing
    Just rawMessageId ->
      case Aeson.fromJSON message.raw :: Aeson.Result Message of
        Aeson.Success telegramMessage ->
          case telegramMessage.replyToMessage of
            Just referenced
              | referenced.messageId == rawMessageId -> do
                  imageUrls <- traverse fileUrl (messageImageFileIds referenced)
                  pure $ Just ReferencedMessage
                    { messageId = Just (integerMessageId referenced.messageId)
                    , senderDisplayName = telegramMessageSenderDisplayName referenced
                    , senderIdentifier = telegramMessageSenderIdentifier referenced
                    , text = messageText referenced
                    , imageUrls = imageUrls
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

-- ---------------------------------------------------------------------------
-- Telegram API
-- ---------------------------------------------------------------------------

apiUrl :: Config -> Text -> Url 'Https
apiUrl cfg method =
  https "api.telegram.org"
    /: ("bot" <> cfg.botToken)
    /: method

telegramFileUrl :: Config -> Text -> Text
telegramFileUrl cfg path =
  [i|https://api.telegram.org/file/bot#{token}/#{path}|]
  where
    token = cfg.botToken

apiCall
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Aeson.ToJSON body, Aeson.FromJSON result)
  => Config
  -> Text
  -> body
  -> Eff es result
apiCall cfg method body = do
  logTelegramApiRequest method
  resp :: TelegramResult <-
    ( HTTP.runReq $
        req POST (apiUrl cfg method) (ReqBodyJson body) jsonResponse (telegramRequestOptions method)
          <&> responseBody
    ) `catch` \(err :: HttpException) ->
      throwIO (TelegramException (telegramExceptionMessage cfg err))
  logTelegramApiResponse method
  parseTelegramResult resp

apiMultipartCall
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Aeson.FromJSON result)
  => Config
  -> Text
  -> [Multipart.Part]
  -> Eff es result
apiMultipartCall cfg method parts = do
  logTelegramApiRequest method
  resp :: TelegramResult <-
    ( HTTP.runReq do
        body <- reqBodyMultipart parts
        req POST (apiUrl cfg method) body jsonResponse (telegramRequestOptions method)
          <&> responseBody
    ) `catch` \(err :: HttpException) ->
      throwIO (TelegramException (telegramExceptionMessage cfg err))
  logTelegramApiResponse method
  parseTelegramResult resp

telegramExceptionMessage :: Config -> HttpException -> Text
telegramExceptionMessage cfg err =
  case err of
    VanillaHttpException (Client.HttpExceptionRequest _ (Client.StatusCodeException _ body)) ->
      case Aeson.eitherDecodeStrict body of
        Right result -> telegramResultError result
        Left _ -> sanitizeTelegramException cfg err
    _ ->
      sanitizeTelegramException cfg err

sanitizeTelegramException :: Show err => Config -> err -> Text
sanitizeTelegramException cfg err =
  Text.replace cfg.botToken "<telegram-token>" (show err)

logTelegramApiRequest :: KatipE :> es => Text -> Eff es ()
logTelegramApiRequest method =
  unless (method == "getUpdates") $
    logInfo [i|Telegram API request: #{method}|]

logTelegramApiResponse :: KatipE :> es => Text -> Eff es ()
logTelegramApiResponse method =
  unless (method == "getUpdates") $
    logInfo [i|Telegram API response: #{method}|]

parseTelegramResult
  :: (IOE :> es, Aeson.FromJSON result)
  => TelegramResult
  -> Eff es result
parseTelegramResult resp =
  case resp of
    Err desc -> throwIO (TelegramException desc)
    Ok value -> case Aeson.fromJSON value of
      Aeson.Success x  -> pure x
      Aeson.Error  err -> throwIO (TelegramException (Text.pack err))

newtype TelegramException = TelegramException Text
  deriving (Show)
instance Exception TelegramException where
  displayException (TelegramException message) = Text.unpack message

data TelegramResult
  = Ok  Aeson.Value
  | Err Text
  deriving (Show, Generic)

instance Aeson.FromJSON TelegramResult where
  parseJSON = Aeson.withObject "TelegramResult" $ \o -> do
    ok <- o Aeson..: "ok"
    if ok
      then Ok <$> o Aeson..: "result"
      else Err <$> o Aeson..: "description"

telegramResultError :: TelegramResult -> Text
telegramResultError = \case
  Ok _ -> "Telegram API returned ok result in an HTTP error response."
  Err desc -> desc

maybeField :: Aeson.ToJSON value => Aeson.Key -> Maybe value -> [Aeson.Pair]
maybeField key =
  maybe [] (\value -> [key Aeson..= value])

telegramLongPollTimeoutSeconds :: Int
telegramLongPollTimeoutSeconds = 30

telegramLongPollResponseTimeoutMicroseconds :: Int
telegramLongPollResponseTimeoutMicroseconds =
  (telegramLongPollTimeoutSeconds + 10) * 1000000

telegramApiResponseTimeoutMicroseconds :: Int
telegramApiResponseTimeoutMicroseconds =
  10 * 1000000

telegramRequestOptions :: Text -> Option 'Https
telegramRequestOptions method =
  responseTimeout $
    if method == "getUpdates"
      then telegramLongPollResponseTimeoutMicroseconds
      else telegramApiResponseTimeoutMicroseconds

sendPhotoParts :: SendPhotoRequest -> FilePath -> [Multipart.Part]
sendPhotoParts SendPhotoRequest{..} path =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "photo" path
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

sendDocumentParts :: SendDocumentRequest -> FilePath -> [Multipart.Part]
sendDocumentParts SendDocumentRequest{..} path =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "document" path
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

sendAudioParts :: SendAudioRequest -> FilePath -> [Multipart.Part]
sendAudioParts SendAudioRequest{..} path =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "audio" path
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

sendVideoParts :: SendVideoRequest -> FilePath -> [Multipart.Part]
sendVideoParts SendVideoRequest{..} path =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "video" path
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

sendVoiceParts :: SendVoiceRequest -> FilePath -> [Multipart.Part]
sendVoiceParts SendVoiceRequest{..} path =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "voice" path
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

telegramUploadFileName :: FilePath -> FilePath
telegramUploadFileName path =
  let name = takeFileName path
  in if null name then "file" else name

telegramFilePart :: Text -> FilePath -> Multipart.Part
telegramFilePart fieldName path =
  Multipart.addPartHeaders
    (Multipart.partFileRequestBodyM fieldName (telegramUploadFileName path) (Client.streamFile path))
    [(HTTPHeader.hContentType, TextEncoding.encodeUtf8 (Mime.mimeFromName (Text.pack path)))]

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

boolText :: Bool -> Text
boolText True  = "true"
boolText False = "false"

parseModeText :: ParseMode -> Text
parseModeText ParseModeMarkdown   = "Markdown"
parseModeText ParseModeMarkdownV2 = "MarkdownV2"
parseModeText ParseModeHTML       = "HTML"

-- ---------------------------------------------------------------------------
-- Formatting
-- ---------------------------------------------------------------------------

data TelegramFormatted = TelegramFormatted
  { formattedText :: !Text
  , formattedEntities :: ![MessageEntity]
  }
  deriving (Show, Typeable)

instance Semigroup TelegramFormatted where
  left <> right =
    TelegramFormatted
      { formattedText = left.formattedText <> right.formattedText
      , formattedEntities = left.formattedEntities <> map (shiftEntity (utf16Length left.formattedText)) right.formattedEntities
      }

instance Monoid TelegramFormatted where
  mempty = TelegramFormatted "" []

instance Rangeable TelegramFormatted where
  ranged _ = id

instance HasAttributes TelegramFormatted where
  addAttributes _ = id

instance ToPlainText TelegramFormatted where
  toPlainText = (.formattedText)

instance IsInline TelegramFormatted where
  lineBreak = formattedTextOnly "\n"
  softBreak = formattedTextOnly "\n"
  str = formattedTextOnly
  entity raw =
    formattedTextOnly (fromMaybe raw (Commonmark.lookupEntity (Text.drop 1 raw)))
  escapedChar = formattedTextOnly . Text.singleton
  emph = wrapFormattedEntity "italic" Nothing Nothing
  strong = wrapFormattedEntity "bold" Nothing Nothing
  link target _title = wrapFormattedEntity "text_link" (Just target) Nothing
  image target _title description =
    if Text.null description.formattedText
      then formattedTextOnly target
      else description
  code text = wrapFormattedEntity "code" Nothing Nothing (formattedTextOnly text)
  rawInline _ text = formattedTextOnly text

instance IsBlock TelegramFormatted TelegramFormatted where
  paragraph body = body <> formattedTextOnly "\n\n"
  plain body = body <> formattedTextOnly "\n"
  thematicBreak = formattedTextOnly "────────\n\n"
  blockQuote body = body
  codeBlock info text =
    wrapFormattedEntity "pre" Nothing (nonEmptyText (Text.takeWhile (not . Char.isSpace) info)) (formattedTextOnly text)
      <> formattedTextOnly "\n\n"
  heading _ body =
    wrapFormattedEntity "bold" Nothing Nothing body <> formattedTextOnly "\n\n"
  rawBlock _ text = formattedTextOnly text
  referenceLinkDefinition _ _ = mempty
  list listType _ items =
    mconcat (zipWith renderItem [(1 :: Int)..] items) <> formattedTextOnly "\n"
    where
      renderItem index item =
        formattedTextOnly (listItemPrefix listType index)
          <> indentFormattedContinuation "  " (trimFormattedEnd item)
          <> formattedTextOnly "\n"

instance HasStrikethrough TelegramFormatted where
  strikethrough = wrapFormattedEntity "strikethrough" Nothing Nothing

instance HasMath TelegramFormatted where
  inlineMath text =
    wrapFormattedEntity "code" Nothing Nothing (formattedTextOnly text)
  displayMath text =
    wrapFormattedEntity "pre" Nothing Nothing (formattedTextOnly text)

instance HasTaskList TelegramFormatted TelegramFormatted where
  taskList _ _ items =
    mconcat (map renderItem items) <> formattedTextOnly "\n"
    where
      renderItem (checked, item) =
        formattedTextOnly (taskListItemPrefix checked)
          <> indentFormattedContinuation "  " (trimFormattedEnd item)
          <> formattedTextOnly "\n"

instance HasFootnote TelegramFormatted TelegramFormatted where
  footnote number _label body =
    formattedTextOnly ("[" <> show number <> "]: ")
      <> indentFormattedContinuation "    " (trimFormattedEnd body)
      <> formattedTextOnly "\n"
  footnoteList =
    mconcat
  footnoteRef number _label _body =
    formattedTextOnly ("[" <> number <> "]")

listItemPrefix :: ListType -> Int -> Text
listItemPrefix (BulletList _) _ =
  "• "
listItemPrefix (OrderedList start _ delimiter) index =
  orderedItemPrefix delimiter (start + index - 1)

orderedItemPrefix :: DelimiterType -> Int -> Text
orderedItemPrefix Period number =
  show number <> ". "
orderedItemPrefix OneParen number =
  show number <> ") "
orderedItemPrefix TwoParens number =
  "(" <> show number <> ") "

taskListItemPrefix :: Bool -> Text
taskListItemPrefix False =
  "☐ "
taskListItemPrefix True =
  "☑ "

telegramMarkdownSyntax :: SyntaxSpec Identity TelegramFormatted TelegramFormatted
telegramMarkdownSyntax =
  strikethroughSpec
    <> mathSpec
    <> taskListSpec
    <> footnoteSpec
    <> defaultSyntaxSpec

formatTelegramMarkdown :: Text -> TelegramFormatted
formatTelegramMarkdown input =
  case runIdentity (commonmarkWith telegramMarkdownSyntax "telegram-message" input) of
    Left _ ->
      formattedTextOnly input
    Right formatted ->
      trimFormattedEnd formatted

formattedTextOnly :: Text -> TelegramFormatted
formattedTextOnly text =
  TelegramFormatted text []

wrapFormattedEntity :: Text -> Maybe Text -> Maybe Text -> TelegramFormatted -> TelegramFormatted
wrapFormattedEntity type_ url language body
  | Text.null body.formattedText = body
  | otherwise =
      body
        { formattedEntities =
            MessageEntity
              { type_ = type_
              , offset = 0
              , length = utf16Length body.formattedText
              , url = url
              , language = language
              , user = Nothing
              }
            : body.formattedEntities
        }

shiftEntity :: Integer -> MessageEntity -> MessageEntity
shiftEntity shift MessageEntity{type_, offset, length = entityLength, url, language, user} =
  MessageEntity{type_, offset = offset + shift, length = entityLength, url, language, user}

trimFormattedEnd :: TelegramFormatted -> TelegramFormatted
trimFormattedEnd formatted =
  let trimmedText = Text.dropWhileEnd Char.isSpace formatted.formattedText
      trimmedLength = utf16Length trimmedText
  in TelegramFormatted
      { formattedText = trimmedText
      , formattedEntities = mapMaybe (trimEntityToLength trimmedLength) formatted.formattedEntities
      }

trimEntityToLength :: Integer -> MessageEntity -> Maybe MessageEntity
trimEntityToLength textLength messageEntity@MessageEntity{type_, offset, length = entityLength, url, language, user}
  | offset >= textLength =
      Nothing
  | entityEnd <= textLength =
      Just messageEntity
  | otherwise =
      let nextLength = textLength - offset
      in if nextLength <= 0
        then Nothing
        else Just MessageEntity{type_, offset, length = nextLength, url, language, user}
  where
    entityEnd = offset + entityLength

indentFormattedContinuation :: Text -> TelegramFormatted -> TelegramFormatted
indentFormattedContinuation indent formatted
  | Text.null indent = formatted
  | otherwise =
      TelegramFormatted
        { formattedText = Text.intercalate ("\n" <> indent) (Text.splitOn "\n" formatted.formattedText)
        , formattedEntities = map (indentEntity formatted.formattedText indentLength) formatted.formattedEntities
        }
  where
    indentLength = utf16Length indent

indentEntity :: Text -> Integer -> MessageEntity -> MessageEntity
indentEntity text indentLength MessageEntity{type_, offset, length = entityLength, url, language, user} =
  MessageEntity
    { type_ = type_
    , offset = offset + indentLength * newlinesBefore
    , length = entityLength + indentLength * newlinesInside
    , url = url
    , language = language
    , user = user
    }
  where
    newlinesBefore = countNewlinesBeforeUtf16Offset offset text
    newlinesInside =
      countNewlinesBeforeUtf16Offset (offset + entityLength) text - newlinesBefore

countNewlinesBeforeUtf16Offset :: Integer -> Text -> Integer
countNewlinesBeforeUtf16Offset limit =
  go 0 0 . Text.unpack
  where
    go _ count [] = count
    go position count (char : rest)
      | nextPosition > limit = count
      | otherwise =
          go nextPosition (if char == '\n' then count + 1 else count) rest
      where
        nextPosition = position + utf16CharLength char

utf16CharLength :: Char -> Integer
utf16CharLength char
  | Char.ord char > 0xffff = 2
  | otherwise = 1

utf16Length :: Text -> Integer
utf16Length =
  (`div` 2) . fromIntegral . ByteString.length . TextEncoding.encodeUtf16LE

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Telegram update envelope returned by long polling.
data Update = Update
  { updateId          :: Integer
  , message           :: Maybe Message
  , editedMessage     :: Maybe Message
  , channelPost       :: Maybe Message
  , editedChannelPost :: Maybe Message
  } deriving (Show, Generic)

instance Aeson.FromJSON Update where
  parseJSON = Aeson.withObject "Update" $ \o -> do
    updateId <- o Aeson..: "update_id"
    message <- o Aeson..:? "message"
    editedMessage <- o Aeson..:? "edited_message"
    channelPost <- o Aeson..:? "channel_post"
    editedChannelPost <- o Aeson..:? "edited_channel_post"
    pure Update{..}

instance Aeson.ToJSON Update where
  toJSON Update{..} = Aeson.object $
    [ "update_id" Aeson..= updateId ]
    <> maybeField "message" message
    <> maybeField "edited_message" editedMessage
    <> maybeField "channel_post" channelPost
    <> maybeField "edited_channel_post" editedChannelPost

-- | Telegram user object fields used by the bot.
data User = User
  { id        :: !Integer
  , isBot     :: !Bool
  , firstName :: !Text
  , lastName  :: !(Maybe Text)
  , username  :: !(Maybe Text)
  } deriving (Show, Generic)

instance Aeson.FromJSON User where
  parseJSON = Aeson.withObject "User" $ \o -> do
    userId <- o Aeson..: "id"
    isBot <- o Aeson..: "is_bot"
    firstName <- o Aeson..: "first_name"
    lastName <- o Aeson..:? "last_name"
    username <- o Aeson..:? "username"
    pure User{id = userId, ..}

instance Aeson.ToJSON User where
  toJSON user = Aeson.object $
    [ "id" Aeson..= user.id
    , "is_bot" Aeson..= isBot
    , "first_name" Aeson..= firstName
    ]
    <> maybeField "last_name" lastName
    <> maybeField "username" username
    where
      User{isBot, firstName, lastName, username} = user

-- | Telegram message object fields consumed by the unified parser.
data Message = Message
  { messageId       :: !Integer
  , messageThreadId :: !(Maybe Integer)
  , from            :: !(Maybe User)
  , senderChat      :: !(Maybe Chat)
  , chat            :: !Chat
  , replyToMessage  :: !(Maybe Message)
  , text            :: !(Maybe Text)
  , entities        :: !(Maybe [MessageEntity])
  , caption         :: !(Maybe Text)
  , captionEntities :: !(Maybe [MessageEntity])
  , photo           :: !(Maybe [PhotoSize])
  } deriving (Show, Generic)

instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject "Message" $ \o -> do
    messageId <- o Aeson..: "message_id"
    messageThreadId <- o Aeson..:? "message_thread_id"
    from <- o Aeson..:? "from"
    senderChat <- o Aeson..:? "sender_chat"
    chat <- o Aeson..: "chat"
    replyToMessage <- o Aeson..:? "reply_to_message"
    text <- o Aeson..:? "text"
    entities <- o Aeson..:? "entities"
    caption <- o Aeson..:? "caption"
    captionEntities <- o Aeson..:? "caption_entities"
    photo <- o Aeson..:? "photo"
    pure Message{..}

instance Aeson.ToJSON Message where
  toJSON Message{..} = Aeson.object $
    [ "message_id" Aeson..= messageId
    , "chat" Aeson..= chat
    ]
    <> maybeField "message_thread_id" messageThreadId
    <> maybeField "from" from
    <> maybeField "sender_chat" senderChat
    <> maybeField "reply_to_message" replyToMessage
    <> maybeField "text" text
    <> maybeField "entities" entities
    <> maybeField "caption" caption
    <> maybeField "caption_entities" captionEntities
    <> maybeField "photo" photo

-- | Telegram photo variant metadata.
data PhotoSize = PhotoSize
  { fileId       :: !Text
  , fileUniqueId :: !Text
  , width        :: !Integer
  , height       :: !Integer
  , fileSize     :: !(Maybe Integer)
  } deriving (Show, Generic)

instance Aeson.FromJSON PhotoSize where
  parseJSON = Aeson.withObject "PhotoSize" $ \o -> do
    fileId <- o Aeson..: "file_id"
    fileUniqueId <- o Aeson..: "file_unique_id"
    width <- o Aeson..: "width"
    height <- o Aeson..: "height"
    fileSize <- o Aeson..:? "file_size"
    pure PhotoSize{..}

instance Aeson.ToJSON PhotoSize where
  toJSON PhotoSize{..} = Aeson.object $
    [ "file_id" Aeson..= fileId
    , "file_unique_id" Aeson..= fileUniqueId
    , "width" Aeson..= width
    , "height" Aeson..= height
    ]
    <> maybeField "file_size" fileSize

-- | Telegram message entity metadata used for mention extraction.
data MessageEntity = MessageEntity
  { type_  :: !Text
  , offset :: !Integer
  , length :: !Integer
  , url    :: !(Maybe Text)
  , language :: !(Maybe Text)
  , user   :: !(Maybe User)
  } deriving (Show, Generic)

instance Aeson.FromJSON MessageEntity where
  parseJSON = Aeson.withObject "MessageEntity" $ \o -> do
    type_ <- o Aeson..: "type"
    offset <- o Aeson..: "offset"
    entityLength <- o Aeson..: "length"
    url <- o Aeson..:? "url"
    language <- o Aeson..:? "language"
    user <- o Aeson..:? "user"
    pure MessageEntity{length = entityLength, ..}

instance Aeson.ToJSON MessageEntity where
  toJSON messageEntity = Aeson.object $
    [ "type" Aeson..= type_
    , "offset" Aeson..= offset
    , "length" Aeson..= messageEntity.length
    ]
    <> maybeField "url" url
    <> maybeField "language" language
    <> maybeField "user" user
    where
      MessageEntity{type_, offset, url, language, user} = messageEntity

-- | Telegram chat kind.
data ChatType
  = ChatTypePrivate
  | ChatTypeGroup
  | ChatTypeSuperGroup
  | ChatTypeChannel
  deriving (Show, Generic)

instance Aeson.ToJSON ChatType where
  toJSON ChatTypePrivate    = Aeson.String "private"
  toJSON ChatTypeGroup      = Aeson.String "group"
  toJSON ChatTypeSuperGroup = Aeson.String "supergroup"
  toJSON ChatTypeChannel    = Aeson.String "channel"

instance Aeson.FromJSON ChatType where
  parseJSON = Aeson.withText "ChatType" $ \case
    "private"    -> pure ChatTypePrivate
    "group"      -> pure ChatTypeGroup
    "supergroup" -> pure ChatTypeSuperGroup
    "channel"    -> pure ChatTypeChannel
    other        -> fail $ "Unknown ChatType: " <> show other

-- | Telegram chat object fields used for routing and metadata.
data Chat = Chat
  { id        :: !Integer
  , type_     :: !ChatType
  , title     :: !(Maybe Text)
  , username  :: !(Maybe Text)
  , firstName :: !(Maybe Text)
  , lastName  :: !(Maybe Text)
  } deriving (Show, Generic)

instance Aeson.FromJSON Chat where
  parseJSON = Aeson.withObject "Chat" $ \o -> do
    chatId <- o Aeson..: "id"
    type_ <- o Aeson..: "type"
    title <- o Aeson..:? "title"
    username <- o Aeson..:? "username"
    firstName <- o Aeson..:? "first_name"
    lastName <- o Aeson..:? "last_name"
    pure Chat{id = chatId, ..}

instance Aeson.ToJSON Chat where
  toJSON chat = Aeson.object $
    [ "id" Aeson..= chat.id
    , "type" Aeson..= type_
    ]
    <> maybeField "title" title
    <> maybeField "username" username
    <> maybeField "first_name" firstName
    <> maybeField "last_name" lastName
    where
      Chat{type_, title, username, firstName, lastName} = chat

-- | Telegram membership status.
data ChatMemberStatus
  = ChatMemberCreator
  | ChatMemberAdministrator
  | ChatMemberMember
  | ChatMemberRestricted
  | ChatMemberLeft
  | ChatMemberKicked
  deriving (Show, Generic)

instance Aeson.FromJSON ChatMemberStatus where
  parseJSON = Aeson.withText "ChatMemberStatus" $ \case
    "creator"       -> pure ChatMemberCreator
    "administrator" -> pure ChatMemberAdministrator
    "member"        -> pure ChatMemberMember
    "restricted"    -> pure ChatMemberRestricted
    "left"          -> pure ChatMemberLeft
    "kicked"        -> pure ChatMemberKicked
    other           -> fail $ "Unknown ChatMemberStatus: " <> show other

instance Aeson.ToJSON ChatMemberStatus where
  toJSON ChatMemberCreator       = Aeson.String "creator"
  toJSON ChatMemberAdministrator = Aeson.String "administrator"
  toJSON ChatMemberMember        = Aeson.String "member"
  toJSON ChatMemberRestricted    = Aeson.String "restricted"
  toJSON ChatMemberLeft          = Aeson.String "left"
  toJSON ChatMemberKicked        = Aeson.String "kicked"

-- | Telegram membership information returned by @getChatMember@.
data ChatMember = ChatMember
  { status :: !ChatMemberStatus
  , user   :: !User
  } deriving (Show, Generic)

instance Aeson.FromJSON ChatMember where
  parseJSON = Aeson.withObject "ChatMember" $ \o -> do
    status <- o Aeson..: "status"
    user <- o Aeson..: "user"
    pure ChatMember{..}

instance Aeson.ToJSON ChatMember where
  toJSON ChatMember{..} = Aeson.object
    [ "status" Aeson..= status
    , "user" Aeson..= user
    ]

-- | Telegram parse mode for outbound formatted text.
data ParseMode
  = ParseModeMarkdown
  | ParseModeMarkdownV2
  | ParseModeHTML
  deriving (Show, Generic)

instance Aeson.ToJSON ParseMode where
  toJSON ParseModeMarkdown   = Aeson.String "Markdown"
  toJSON ParseModeMarkdownV2 = Aeson.String "MarkdownV2"
  toJSON ParseModeHTML       = Aeson.String "HTML"

instance Aeson.FromJSON ParseMode where
  parseJSON = Aeson.withText "ParseMode" $ \case
    "Markdown"   -> pure ParseModeMarkdown
    "MarkdownV2" -> pure ParseModeMarkdownV2
    "HTML"       -> pure ParseModeHTML
    other        -> fail $ "Unknown ParseMode: " <> show other

-- ---------------------------------------------------------------------------
-- Requests
-- ---------------------------------------------------------------------------

data GetMeRequest = GetMeRequest
  deriving (Show)

instance Aeson.ToJSON GetMeRequest where
  toJSON _ = Aeson.object []

instance TelegramRequest GetMeRequest where
  type TelegramResponse GetMeRequest = User
  telegramMethod _ = "getMe"

newtype GetFileRequest = GetFileRequest
  { fileId :: Text
  } deriving (Show, Generic)

instance Aeson.ToJSON GetFileRequest where
  toJSON GetFileRequest{..} = Aeson.object
    [ "file_id" Aeson..= fileId
    ]

instance TelegramRequest GetFileRequest where
  type TelegramResponse GetFileRequest = File
  telegramMethod _ = "getFile"

data GetUserProfilePhotosRequest = GetUserProfilePhotosRequest
  { userId :: !Integer
  , offset :: !(Maybe Int)
  , limit  :: !(Maybe Int)
  } deriving (Show, Generic)

instance Aeson.ToJSON GetUserProfilePhotosRequest where
  toJSON GetUserProfilePhotosRequest{..} = Aeson.object $
    [ "user_id" Aeson..= userId
    ]
    <> maybeField "offset" offset
    <> maybeField "limit" limit

instance TelegramRequest GetUserProfilePhotosRequest where
  type TelegramResponse GetUserProfilePhotosRequest = UserProfilePhotos
  telegramMethod _ = "getUserProfilePhotos"

data UserProfilePhotos = UserProfilePhotos
  { totalCount :: !Integer
  , photos     :: ![[PhotoSize]]
  } deriving (Show, Generic)

instance Aeson.FromJSON UserProfilePhotos where
  parseJSON = Aeson.withObject "UserProfilePhotos" $ \o -> do
    totalCount <- o Aeson..: "total_count"
    photos <- o Aeson..: "photos"
    pure UserProfilePhotos{..}

data File = File
  { fileId       :: !Text
  , fileUniqueId :: !Text
  , fileSize     :: !(Maybe Integer)
  , filePath     :: !Text
  } deriving (Show, Generic)

instance Aeson.FromJSON File where
  parseJSON = Aeson.withObject "File" $ \o -> do
    fileId <- o Aeson..: "file_id"
    fileUniqueId <- o Aeson..: "file_unique_id"
    fileSize <- o Aeson..:? "file_size"
    filePath <- o Aeson..: "file_path"
    pure File{..}

data GetUpdatesRequest = GetUpdatesRequest
  { offset  :: !Int
  , timeout :: !Int
  , limit   :: !Int
  } deriving (Show, Generic)

instance Aeson.ToJSON GetUpdatesRequest where
  toJSON GetUpdatesRequest{..} = Aeson.object
    [ "offset" Aeson..= offset
    , "timeout" Aeson..= timeout
    , "limit" Aeson..= limit
    ]

instance TelegramRequest GetUpdatesRequest where
  type TelegramResponse GetUpdatesRequest = [Update]
  telegramMethod _ = "getUpdates"

-- | Request payload for Telegram @sendMessage@.
data SendMessageRequest = SendMessageRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , text                :: !Text
  , parseMode           :: !(Maybe ParseMode)
  , entities            :: !(Maybe [MessageEntity])
  , disableNotification :: !(Maybe Bool)
  , replyToMessageId    :: !(Maybe Integer)
  } deriving (Show, Generic)

instance Aeson.ToJSON SendMessageRequest where
  toJSON SendMessageRequest{..} = Aeson.object $
    [ "chat_id" Aeson..= chatId
    , "text" Aeson..= text
    ]
    <> maybeField "message_thread_id" messageThreadId
    <> maybeField "parse_mode" parseMode
    <> maybeField "entities" entities
    <> maybeField "disable_notification" disableNotification
    <> maybeField "reply_to_message_id" replyToMessageId

instance Aeson.FromJSON SendMessageRequest where
  parseJSON = Aeson.withObject "SendMessageRequest" $ \o -> do
    chatId <- o Aeson..: "chat_id"
    messageThreadId <- o Aeson..:? "message_thread_id"
    text <- o Aeson..: "text"
    parseMode <- o Aeson..:? "parse_mode"
    entities <- o Aeson..:? "entities"
    disableNotification <- o Aeson..:? "disable_notification"
    replyToMessageId <- o Aeson..:? "reply_to_message_id"
    pure SendMessageRequest{..}

instance TelegramRequest SendMessageRequest where
  type TelegramResponse SendMessageRequest = Message
  telegramMethod _ = "sendMessage"

-- | Request payload for Telegram @editMessageText@.
data EditMessageTextRequest = EditMessageTextRequest
  { chatId                :: !Integer
  , messageId             :: !Integer
  , text                  :: !Text
  , parseMode             :: !(Maybe ParseMode)
  , entities              :: !(Maybe [MessageEntity])
  , disableWebPagePreview :: !(Maybe Bool)
  } deriving (Show, Generic)

instance Aeson.ToJSON EditMessageTextRequest where
  toJSON EditMessageTextRequest{..} = Aeson.object $
    [ "chat_id" Aeson..= chatId
    , "message_id" Aeson..= messageId
    , "text" Aeson..= text
    ]
    <> maybeField "parse_mode" parseMode
    <> maybeField "entities" entities
    <> maybeField "disable_web_page_preview" disableWebPagePreview

instance Aeson.FromJSON EditMessageTextRequest where
  parseJSON = Aeson.withObject "EditMessageTextRequest" $ \o -> do
    chatId <- o Aeson..: "chat_id"
    messageId <- o Aeson..: "message_id"
    text <- o Aeson..: "text"
    parseMode <- o Aeson..:? "parse_mode"
    entities <- o Aeson..:? "entities"
    disableWebPagePreview <- o Aeson..:? "disable_web_page_preview"
    pure EditMessageTextRequest{..}

instance TelegramRequest EditMessageTextRequest where
  type TelegramResponse EditMessageTextRequest = Message
  telegramMethod _ = "editMessageText"

-- | Request payload for Telegram @sendPhoto@.
data SendPhotoRequest = SendPhotoRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , photo               :: !Text
  , caption             :: !(Maybe Text)
  , parseMode           :: !(Maybe ParseMode)
  , captionEntities     :: !(Maybe [MessageEntity])
  , disableNotification :: !(Maybe Bool)
  , replyToMessageId    :: !(Maybe Integer)
  } deriving (Show, Generic)

instance Aeson.ToJSON SendPhotoRequest where
  toJSON SendPhotoRequest{..} = Aeson.object $
    [ "chat_id" Aeson..= chatId
    , "photo" Aeson..= photo
    ]
    <> maybeField "message_thread_id" messageThreadId
    <> maybeField "caption" caption
    <> maybeField "parse_mode" parseMode
    <> maybeField "caption_entities" captionEntities
    <> maybeField "disable_notification" disableNotification
    <> maybeField "reply_to_message_id" replyToMessageId

instance Aeson.FromJSON SendPhotoRequest where
  parseJSON = Aeson.withObject "SendPhotoRequest" $ \o -> do
    chatId <- o Aeson..: "chat_id"
    messageThreadId <- o Aeson..:? "message_thread_id"
    photo <- o Aeson..: "photo"
    caption <- o Aeson..:? "caption"
    parseMode <- o Aeson..:? "parse_mode"
    captionEntities <- o Aeson..:? "caption_entities"
    disableNotification <- o Aeson..:? "disable_notification"
    replyToMessageId <- o Aeson..:? "reply_to_message_id"
    pure SendPhotoRequest{..}

instance TelegramRequest SendPhotoRequest where
  type TelegramResponse SendPhotoRequest = Message
  telegramMethod _ = "sendPhoto"

-- | Request payload for Telegram @sendVoice@.
data SendVoiceRequest = SendVoiceRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , voice               :: !Text
  , caption             :: !(Maybe Text)
  , parseMode           :: !(Maybe ParseMode)
  , captionEntities     :: !(Maybe [MessageEntity])
  , disableNotification :: !(Maybe Bool)
  , replyToMessageId    :: !(Maybe Integer)
  } deriving (Show, Generic)

instance Aeson.ToJSON SendVoiceRequest where
  toJSON SendVoiceRequest{..} = Aeson.object $
    [ "chat_id" Aeson..= chatId
    , "voice" Aeson..= voice
    ]
    <> maybeField "message_thread_id" messageThreadId
    <> maybeField "caption" caption
    <> maybeField "parse_mode" parseMode
    <> maybeField "caption_entities" captionEntities
    <> maybeField "disable_notification" disableNotification
    <> maybeField "reply_to_message_id" replyToMessageId

instance TelegramRequest SendVoiceRequest where
  type TelegramResponse SendVoiceRequest = Message
  telegramMethod _ = "sendVoice"

data SendAudioRequest = SendAudioRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , caption             :: !(Maybe Text)
  , parseMode           :: !(Maybe ParseMode)
  , captionEntities     :: !(Maybe [MessageEntity])
  , disableNotification :: !(Maybe Bool)
  , replyToMessageId    :: !(Maybe Integer)
  } deriving (Show, Generic)

data SendVideoRequest = SendVideoRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , caption             :: !(Maybe Text)
  , parseMode           :: !(Maybe ParseMode)
  , captionEntities     :: !(Maybe [MessageEntity])
  , disableNotification :: !(Maybe Bool)
  , replyToMessageId    :: !(Maybe Integer)
  } deriving (Show, Generic)

-- | Request payload for Telegram @sendDocument@ multipart uploads.
data SendDocumentRequest = SendDocumentRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , caption             :: !(Maybe Text)
  , parseMode           :: !(Maybe ParseMode)
  , captionEntities     :: !(Maybe [MessageEntity])
  , disableNotification :: !(Maybe Bool)
  , replyToMessageId    :: !(Maybe Integer)
  } deriving (Show, Generic)

data ForwardMessageRequest = ForwardMessageRequest
  { chatId     :: !Integer
  , fromChatId :: !Integer
  , messageId  :: !Integer
  } deriving (Show, Generic)

instance Aeson.ToJSON ForwardMessageRequest where
  toJSON ForwardMessageRequest{..} = Aeson.object
    [ "chat_id" Aeson..= chatId
    , "from_chat_id" Aeson..= fromChatId
    , "message_id" Aeson..= messageId
    ]

instance TelegramRequest ForwardMessageRequest where
  type TelegramResponse ForwardMessageRequest = Message
  telegramMethod _ = "forwardMessage"

data DeleteMessageRequest = DeleteMessageRequest
  { chatId    :: !Integer
  , messageId :: !Integer
  } deriving (Show, Generic)

instance Aeson.ToJSON DeleteMessageRequest where
  toJSON DeleteMessageRequest{..} = Aeson.object
    [ "chat_id" Aeson..= chatId
    , "message_id" Aeson..= messageId
    ]

instance TelegramRequest DeleteMessageRequest where
  type TelegramResponse DeleteMessageRequest = Bool
  telegramMethod _ = "deleteMessage"

data PinMessageRequest = PinMessageRequest
  { chatId              :: !Integer
  , messageId           :: !Integer
  , disableNotification :: !Bool
  } deriving (Show, Generic)

instance Aeson.ToJSON PinMessageRequest where
  toJSON PinMessageRequest{..} = Aeson.object
    [ "chat_id" Aeson..= chatId
    , "message_id" Aeson..= messageId
    , "disable_notification" Aeson..= disableNotification
    ]

instance TelegramRequest PinMessageRequest where
  type TelegramResponse PinMessageRequest = Bool
  telegramMethod _ = "pinChatMessage"

data UnpinMessageRequest = UnpinMessageRequest
  { chatId    :: !Integer
  , messageId :: !Integer
  } deriving (Show, Generic)

instance Aeson.ToJSON UnpinMessageRequest where
  toJSON UnpinMessageRequest{..} = Aeson.object
    [ "chat_id" Aeson..= chatId
    , "message_id" Aeson..= messageId
    ]

instance TelegramRequest UnpinMessageRequest where
  type TelegramResponse UnpinMessageRequest = Bool
  telegramMethod _ = "unpinChatMessage"

newtype GetChatRequest = GetChatRequest
  { chatId :: Integer
  } deriving (Show, Generic)

instance Aeson.ToJSON GetChatRequest where
  toJSON GetChatRequest{..} = Aeson.object
    [ "chat_id" Aeson..= chatId
    ]

instance TelegramRequest GetChatRequest where
  type TelegramResponse GetChatRequest = Chat
  telegramMethod _ = "getChat"

data GetChatMemberRequest = GetChatMemberRequest
  { chatId :: !Integer
  , userId :: !Integer
  } deriving (Show, Generic)

instance Aeson.ToJSON GetChatMemberRequest where
  toJSON GetChatMemberRequest{..} = Aeson.object
    [ "chat_id" Aeson..= chatId
    , "user_id" Aeson..= userId
    ]

instance TelegramRequest GetChatMemberRequest where
  type TelegramResponse GetChatMemberRequest = ChatMember
  telegramMethod _ = "getChatMember"

data BanChatMemberRequest = BanChatMemberRequest
  { chatId    :: !Integer
  , userId    :: !Integer
  , untilDate :: !(Maybe Integer)
  } deriving (Show, Generic)

instance Aeson.ToJSON BanChatMemberRequest where
  toJSON BanChatMemberRequest{..} = Aeson.object $
    [ "chat_id" Aeson..= chatId
    , "user_id" Aeson..= userId
    ]
    <> maybeField "until_date" untilDate

instance TelegramRequest BanChatMemberRequest where
  type TelegramResponse BanChatMemberRequest = Bool
  telegramMethod _ = "banChatMember"

data UnbanChatMemberRequest = UnbanChatMemberRequest
  { chatId      :: !Integer
  , userId      :: !Integer
  , onlyIfBanned :: !Bool
  } deriving (Show, Generic)

instance Aeson.ToJSON UnbanChatMemberRequest where
  toJSON UnbanChatMemberRequest{..} = Aeson.object
    [ "chat_id" Aeson..= chatId
    , "user_id" Aeson..= userId
    , "only_if_banned" Aeson..= onlyIfBanned
    ]

instance TelegramRequest UnbanChatMemberRequest where
  type TelegramResponse UnbanChatMemberRequest = Bool
  telegramMethod _ = "unbanChatMember"

newtype LeaveChatRequest = LeaveChatRequest
  { chatId :: Integer
  } deriving (Show, Generic)

instance Aeson.ToJSON LeaveChatRequest where
  toJSON LeaveChatRequest{..} = Aeson.object
    [ "chat_id" Aeson..= chatId
    ]

instance TelegramRequest LeaveChatRequest where
  type TelegramResponse LeaveChatRequest = Bool
  telegramMethod _ = "leaveChat"

data SendChatActionRequest = SendChatActionRequest
  { chatId :: !Integer
  , action :: !ChatAction
  , messageThreadId :: !(Maybe Integer)
  , businessConnectionId :: !(Maybe Text)
  } deriving (Show, Generic)

instance TelegramRequest SendChatActionRequest where
  type TelegramResponse SendChatActionRequest = Bool
  telegramMethod _ = "sendChatAction"

instance Aeson.ToJSON SendChatActionRequest where
  toJSON SendChatActionRequest{..} = Aeson.object $
    [ "chat_id" Aeson..= chatId
    , "action" Aeson..= action
    ]
    <> maybeField "message_thread_id" messageThreadId
    <> maybeField "business_connection_id" businessConnectionId

data ChatAction
  = ChatActionTyping
  | ChatActionUploadPhoto
  | ChatActionUploadVideo
  | ChatActionUploadVoice
  | ChatActionUploadDocument
  | ChatActionChooseSticker
  | ChatActionFindLocation
  | ChatActionUploadVideoNote
  deriving (Show, Generic)

instance Aeson.ToJSON ChatAction where
  toJSON ChatActionTyping = "typing" :: Aeson.Value
  toJSON ChatActionUploadPhoto = "upload_photo" :: Aeson.Value
  toJSON ChatActionUploadVideo = "upload_video" :: Aeson.Value
  toJSON ChatActionUploadVoice = "upload_voice" :: Aeson.Value
  toJSON ChatActionUploadDocument = "upload_document" :: Aeson.Value
  toJSON ChatActionChooseSticker = "choose_sticker" :: Aeson.Value
  toJSON ChatActionFindLocation = "find_location" :: Aeson.Value
  toJSON ChatActionUploadVideoNote = "upload_video_note" :: Aeson.Value

instance Aeson.FromJSON ChatAction where
  parseJSON = Aeson.withText "ChatAction" $ \case
    "typing" -> pure ChatActionTyping
    "upload_photo" -> pure ChatActionUploadPhoto
    "upload_video" -> pure ChatActionUploadVideo
    "upload_voice" -> pure ChatActionUploadVoice
    "upload_document" -> pure ChatActionUploadDocument
    "choose_sticker" -> pure ChatActionChooseSticker
    "find_location" -> pure ChatActionFindLocation
    "upload_video_note" -> pure ChatActionUploadVideoNote
    other -> fail $ "Unknown ChatAction: " <> show other

-- ---------------------------------------------------------------------------
-- Smart constructors
-- ---------------------------------------------------------------------------

-- | Return the bot user associated with the current token.
getMe :: Telegram :> es => Eff es User
getMe =
  callTelegram GetMeRequest

-- | Long-poll Telegram updates from an offset.
getUpdates :: Telegram :> es => Int -> Eff es [Update]
getUpdates offset = callTelegram $ GetUpdatesRequest
  { offset  = offset
  , timeout = telegramLongPollTimeoutSeconds
  , limit   = 100
  }

-- | Call Telegram @sendMessage@.
sendMessage :: Telegram :> es => SendMessageRequest -> Eff es Message
sendMessage = callTelegram

-- | Call Telegram @sendPhoto@ with a remote or already-uploaded photo ref.
sendPhoto :: Telegram :> es => SendPhotoRequest -> Eff es Message
sendPhoto = callTelegram

-- | Upload a local photo file through multipart/form-data.
uploadPhoto :: Telegram :> es => SendPhotoRequest -> FilePath -> Eff es Message
uploadPhoto request path =
  send (UploadPhoto request path)

-- | Upload a local voice file through multipart/form-data.
uploadVoice :: Telegram :> es => SendVoiceRequest -> FilePath -> Eff es Message
uploadVoice request path =
  send (UploadVoice request path)

uploadAudio :: Telegram :> es => SendAudioRequest -> FilePath -> Eff es Message
uploadAudio request path =
  send (UploadAudio request path)

uploadVideo :: Telegram :> es => SendVideoRequest -> FilePath -> Eff es Message
uploadVideo request path =
  send (UploadVideo request path)

-- | Upload a local document file through multipart/form-data.
uploadDocument :: Telegram :> es => SendDocumentRequest -> FilePath -> Eff es Message
uploadDocument request path =
  send (UploadDocument request path)

-- | Reply to a Telegram chat, including image directives in the body.
replyTo :: (Telegram :> es, FileSystem :> es, IOE :> es) => IncomingMessage -> Text -> Eff es (Either Text MessageId)
replyTo message body =
  case (message.platform, message.chatId) of
    (PlatformTelegram, Just chatId) -> do
      let replyToMessageId = messageIdInteger =<< message.messageId
      sent <- replyTextAndImages chatId replyToMessageId body `catch` \(err :: TelegramException) ->
        sendTelegramFailureReply chatId replyToMessageId err
      pure (Right (integerMessageId sent.messageId))
    _ ->
      pure (Left "Telegram reply requires a Telegram chat id.")

sendTelegramFailureReply :: Telegram :> es => Integer -> Maybe Integer -> TelegramException -> Eff es Message
sendTelegramFailureReply chatId replyToMessageId err =
  sendMessage SendMessageRequest
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
editMessage :: Telegram :> es => IncomingMessage -> MessageId -> Text -> Eff es Bool
editMessage message messageId body =
  case (message.platform, message.chatId, messageIdInteger messageId) of
    (PlatformTelegram, Just chatId, Just rawMessageId) -> do
      let formatted = formatTelegramMarkdown body
      void $ callTelegram EditMessageTextRequest
        { chatId = chatId
        , messageId = rawMessageId
        , text = formatted.formattedText
        , parseMode = Nothing
        , entities = Just formatted.formattedEntities
        , disableWebPagePreview = Just True
        }
      pure True
    _ ->
      pure False

-- | Delete a Telegram message in the current chat when the bot has permission.
deleteMessageFor :: Telegram :> es => IncomingMessage -> MessageId -> Eff es Bool
deleteMessageFor message messageId =
  case (message.platform, message.chatId, messageIdInteger messageId) of
    (PlatformTelegram, Just chatId, Just rawMessageId) ->
      deleteMessage chatId rawMessageId
    _ ->
      pure False

-- | Reply with an HTML mention for a Telegram user id.
mentionUser :: Telegram :> es => IncomingMessage -> Text -> Text -> Eff es (Either Text MessageId)
mentionUser message userId body =
  case (message.platform, message.chatId) of
    (PlatformTelegram, Just chatId)
      | Just numericUserId <- parseIntegerUserId userId -> do
      let replyToMessageId = messageIdInteger =<< message.messageId
      sent <- sendMessage SendMessageRequest
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
replyAudio :: (Telegram :> es, FileSystem :> es, IOE :> es) => IncomingMessage -> Text -> Maybe Text -> Eff es (Either Text MessageId)
replyAudio message audioRef caption =
  case (message.platform, message.chatId) of
    (PlatformTelegram, Just chatId) -> do
      let replyToMessageId = messageIdInteger =<< message.messageId
      sent <- sendVoiceRequest SendVoiceRequest
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
uploadFile :: Telegram :> es => IncomingMessage -> FilePath -> Eff es (Either Text MessageId)
uploadFile message path =
  case (message.platform, message.chatId) of
    (PlatformTelegram, Just chatId) -> do
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
      sent <- uploadTelegramFileByMime baseRequest path
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

uploadTelegramFileByMime :: Telegram :> es => TelegramUploadRequest -> FilePath -> Eff es Message
uploadTelegramFileByMime request path =
  case telegramFileKind (Mime.mimeFromName (Text.pack path)) of
    TelegramImageFile ->
      uploadPhoto (photoRequest request) path
    TelegramAudioFile ->
      uploadAudio (audioRequest request) path
    TelegramVideoFile ->
      uploadVideo (videoRequest request) path
    TelegramDocumentFile ->
      uploadDocument (documentRequest request) path

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

escapeHtml :: Text -> Text
escapeHtml =
  Text.concatMap \case
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    c   -> Text.singleton c

replyTextAndImages :: (Telegram :> es, FileSystem :> es, IOE :> es) => Integer -> Maybe Integer -> Text -> Eff es Message
replyTextAndImages chatId replyToMessageId body =
  case ChatEffect.replyImageUrls body of
    [] -> sendText (ChatEffect.renderReplyBody body)
    firstImage : restImages -> do
      let formattedCaption = nonEmptyText (ChatEffect.renderReplyBody body) <&> formatTelegramMarkdown
      firstSent <- sendImageRequest SendPhotoRequest
        { chatId = chatId
        , messageThreadId = Nothing
        , photo = firstImage
        , caption = (.formattedText) <$> formattedCaption
        , parseMode = Nothing
        , captionEntities = (.formattedEntities) <$> formattedCaption
        , disableNotification = Nothing
        , replyToMessageId = replyToMessageId
        }
      traverse_ (sendImage Nothing) restImages
      pure firstSent
  where
    sendText text =
      let formatted = formatTelegramMarkdown text
      in sendMessage SendMessageRequest
      { chatId = chatId
      , messageThreadId = Nothing
      , text = formatted.formattedText
      , parseMode = Nothing
      , entities = Just formatted.formattedEntities
      , disableNotification = Nothing
      , replyToMessageId = replyToMessageId
      }
    sendImage caption photo = void $ sendImageRequest SendPhotoRequest
      { chatId = chatId
      , messageThreadId = Nothing
      , photo = photo
      , caption = caption
      , parseMode = Nothing
      , captionEntities = Nothing
      , disableNotification = Nothing
      , replyToMessageId = Nothing
      }

sendImageRequest :: (Telegram :> es, FileSystem :> es, IOE :> es) => SendPhotoRequest -> Eff es Message
sendImageRequest request =
  case localFilePhoto request.photo of
    Just path -> uploadPhoto request path
    Nothing ->
      case dataImagePhoto request.photo of
        Just bytes -> uploadTemporaryPhoto request bytes
        Nothing    -> sendPhoto request

sendVoiceRequest :: (Telegram :> es, FileSystem :> es, IOE :> es) => SendVoiceRequest -> Eff es Message
sendVoiceRequest request =
  case localFileRef request.voice of
    Just path -> uploadVoice request path
    Nothing ->
      case dataAudioBytes request.voice of
        Just bytes -> uploadTemporaryVoice request bytes
        Nothing    -> callTelegram request

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

uploadTemporaryPhoto :: (Telegram :> es, FileSystem :> es, IOE :> es) => SendPhotoRequest -> ByteString.ByteString -> Eff es Message
uploadTemporaryPhoto request bytes = do
  path <- temporaryTelegramPath "telegram-photo" "png"
  FileSystemByteString.writeFile path bytes
  uploadPhoto request path `finally` cleanup path
  where
    cleanup path =
      FileSystem.removeFile path `catchSync` \_ -> pure ()

uploadTemporaryVoice :: (Telegram :> es, FileSystem :> es, IOE :> es) => SendVoiceRequest -> ByteString.ByteString -> Eff es Message
uploadTemporaryVoice request bytes = do
  path <- temporaryTelegramPath "telegram-voice" "ogg"
  FileSystemByteString.writeFile path bytes
  uploadVoice request path `finally` cleanup path
  where
    cleanup path =
      FileSystem.removeFile path `catchSync` \_ -> pure ()

temporaryTelegramPath :: (FileSystem :> es, IOE :> es) => FilePath -> FilePath -> Eff es FilePath
temporaryTelegramPath prefix extension = do
  FileSystem.createDirectoryIfMissing True telegramTempDir
  nonce <- liftIO getMonotonicTimeNSec
  pure (telegramTempDir </> (prefix <> "-" <> show nonce <.> extension))

telegramTempDir :: FilePath
telegramTempDir =
  "/tmp/cosmobot-telegram"

nonEmptyText :: Text -> Maybe Text
nonEmptyText text
  | Text.null text = Nothing
  | otherwise      = Just text

-- | Forward an existing Telegram message.
forwardMessage :: Telegram :> es => Integer -> Integer -> Integer -> Eff es Message
forwardMessage chatId fromChatId messageId =
  callTelegram $ ForwardMessageRequest{..}

-- | Delete a Telegram message when the bot has permission.
deleteMessage :: Telegram :> es => Integer -> Integer -> Eff es Bool
deleteMessage chatId messageId =
  callTelegram $ DeleteMessageRequest{..}

-- | Pin a Telegram message in a chat.
pinMessage :: Telegram :> es => Integer -> Integer -> Bool -> Eff es Bool
pinMessage chatId messageId disableNotification =
  callTelegram $ PinMessageRequest{..}

-- | Unpin a Telegram message in a chat.
unpinMessage :: Telegram :> es => Integer -> Integer -> Eff es Bool
unpinMessage chatId messageId =
  callTelegram $ UnpinMessageRequest{..}

-- | Fetch Telegram chat metadata.
getChat :: Telegram :> es => Integer -> Eff es Chat
getChat chatId =
  callTelegram $ GetChatRequest{..}

-- | Fetch one user's membership in a Telegram chat.
getChatMember :: Telegram :> es => Integer -> Integer -> Eff es ChatMember
getChatMember chatId userId =
  callTelegram $ GetChatMemberRequest{..}

-- | Ban a Telegram user from a chat.
banChatMember :: Telegram :> es => Integer -> Integer -> Maybe Integer -> Eff es Bool
banChatMember chatId userId untilDate =
  callTelegram $ BanChatMemberRequest{..}

-- | Unban a Telegram user from a chat.
unbanChatMember :: Telegram :> es => Integer -> Integer -> Eff es Bool
unbanChatMember chatId userId =
  callTelegram $ UnbanChatMemberRequest { onlyIfBanned = True, .. }

-- | Leave a Telegram chat.
leaveChat :: Telegram :> es => Integer -> Eff es Bool
leaveChat chatId =
  callTelegram $ LeaveChatRequest{..}

-- | Fetch the latest Telegram profile photo for a user id.
getUserAvatar :: Telegram :> es => Integer -> Eff es (Maybe Aeson.Value)
getUserAvatar userId = do
  profilePhotos <- callTelegram GetUserProfilePhotosRequest
    { userId = userId
    , offset = Nothing
    , limit = Just 1
    }
  case profilePhotos.photos >>= maybeToList . largestPhoto of
    [] ->
      pure Nothing
    photo : _ -> do
      avatarUrl <- fileUrl photo.fileId
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
