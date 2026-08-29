{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.Telegram.Protocol
Description : Telegram Bot API protocol implementation
Stability   : experimental
-}

module Bot.Chat.Driver.Telegram.Protocol where

import qualified Bot.Chat.Driver.Types as Driver
import Bot.Chat.Driver.Telegram.Types (Config (..))
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Media.Mime as Mime
import Bot.Util.Multipart
import Bot.Util.Aeson
import Data.List (maximum)
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Encoding as TextEncoding
import qualified Network.HTTP.Client as Client
import Network.HTTP.Req
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import qualified Network.HTTP.Types.Header as HTTPHeader
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified Data.Text as Text

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

newtype TelegramDriver = TelegramDriver
  { config :: Config
  }

newTelegramDriver :: Config -> TelegramDriver
newTelegramDriver config =
  TelegramDriver{config}

setTypingTelegram
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => TelegramDriver
  -> Integer
  -> Eff es ()
setTypingTelegram driver chatId =
  void (callTelegram driver (SendChatActionRequest chatId ChatActionTyping Nothing Nothing))

telegramEditChunkChars :: Int
telegramEditChunkChars = 512

telegramMessageTextLimit :: Int
telegramMessageTextLimit = 30000

telegramLegacyMessageTextLimit :: Int
telegramLegacyMessageTextLimit = 4096

-- ---------------------------------------------------------------------------
-- Typeclass
-- ---------------------------------------------------------------------------

class (Aeson.ToJSON req, Aeson.FromJSON (TelegramResponse req)) => TelegramRequest req where
  type TelegramResponse req
  telegramMethod :: req -> Text

callTelegram
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, TelegramRequest req)
  => TelegramDriver
  -> req
  -> Eff es (TelegramResponse req)
callTelegram driver request =
  apiCall driver.config (telegramMethod request) request

fileUrl
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => TelegramDriver
  -> Text
  -> Eff es Text
fileUrl driver fileId = do
  file :: File <- apiCall driver.config (telegramMethod (GetFileRequest fileId)) (GetFileRequest fileId)
  pure (telegramFileUrl driver.config file.filePath)

-- ---------------------------------------------------------------------------
-- Streaming
-- ---------------------------------------------------------------------------

updatesStream'
  :: (HTTP.HTTP :> es, KatipE :> es, Concurrent :> es, IOE :> es)
  => TelegramDriver
  -> Int
  -> Stream (Of Update) (Eff es) ()
updatesStream' driver offset = do
  batches <- S.lift (getUpdatesRetrying driver offset)
  S.lift $ $(logDebug) [i|Telegram update batch: #{length batches}|]
  S.each batches
  let nextOffset = case batches of
        [] -> offset
        _  -> 1 + maximum (map (fromInteger . (.updateId)) batches)
  updatesStream' driver nextOffset

updatesStream :: (HTTP.HTTP :> es, KatipE :> es, Concurrent :> es, IOE :> es) => TelegramDriver -> Stream (Of Update) (Eff es) ()
updatesStream driver = updatesStream' driver 0

getUpdatesRetrying
  :: (HTTP.HTTP :> es, KatipE :> es, Concurrent :> es, IOE :> es)
  => TelegramDriver
  -> Int
  -> Eff es [Update]
getUpdatesRetrying driver offset =
  getUpdates driver offset `catchSync` \err -> do
    let summary = Text.takeWhile (/= '\n') (toText (displayException err))
    $(logWarning) [i|Telegram polling failed; retrying in 5 seconds: #{summary}|]
    threadDelay telegramPollingRetryDelayMicroseconds
    getUpdatesRetrying driver offset

telegramPollingRetryDelayMicroseconds :: Int
telegramPollingRetryDelayMicroseconds =
  5 * 1000000

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
apiCall cfg method body = katipAddContext (sl "telegram_method" method) do
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
apiMultipartCall cfg method parts = katipAddContext (sl "telegram_method" method) do
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
    $(logDebug) [i|Telegram API request: #{method}|]

logTelegramApiResponse :: KatipE :> es => Text -> Eff es ()
logTelegramApiResponse method =
  unless (method == "getUpdates") $
    $(logDebug) [i|Telegram API response: #{method}|]

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

withRichMessageFallback :: IOE :> es => Text -> Eff es a -> Eff es a -> Eff es a
withRichMessageFallback text action fallback =
  action `catchSync` \err ->
    case fromException err :: Maybe TelegramException of
      Just telegramError
        | richMessageFallbackAllowed text telegramError -> fallback
      _ -> throwIO err

richMessageFallbackAllowed :: Text -> TelegramException -> Bool
richMessageFallbackAllowed text (TelegramException message) =
  Text.length text <= telegramLegacyMessageTextLimit
    && "Bad Request:" `Text.isPrefixOf` message

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

sendPhotoParts :: SendPhotoRequest -> FilePath -> Maybe Text -> [Multipart.Part]
sendPhotoParts SendPhotoRequest{..} path fileName =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "photo" path fileName
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

sendDocumentParts :: SendDocumentRequest -> FilePath -> Maybe Text -> [Multipart.Part]
sendDocumentParts SendDocumentRequest{..} path fileName =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "document" path fileName
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

sendAudioParts :: SendAudioRequest -> FilePath -> Maybe Text -> [Multipart.Part]
sendAudioParts SendAudioRequest{..} path fileName =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "audio" path fileName
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

sendVideoParts :: SendVideoRequest -> FilePath -> Maybe Text -> [Multipart.Part]
sendVideoParts SendVideoRequest{..} path fileName =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "video" path fileName
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

sendVoiceParts :: SendVoiceRequest -> FilePath -> Maybe Text -> [Multipart.Part]
sendVoiceParts SendVoiceRequest{..} path fileName =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "voice" path fileName
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

telegramFilePart :: Text -> FilePath -> Maybe Text -> Multipart.Part
telegramFilePart fieldName path fileName =
  Multipart.addPartHeaders
    (Multipart.partFileRequestBodyM fieldName (Text.unpack (Driver.uploadFileName path fileName)) (Client.streamFile path))
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
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing Update)

-- | Telegram user object fields used by the bot.
data User = User
  { id        :: !Integer
  , isBot     :: !Bool
  , firstName :: !Text
  , lastName  :: !(Maybe Text)
  , username  :: !(Maybe Text)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing User)

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
  , document        :: !(Maybe TelegramMedia)
  , audio           :: !(Maybe TelegramMedia)
  , voice           :: !(Maybe TelegramMedia)
  , sticker         :: !(Maybe Sticker)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing Message)

-- | Telegram photo variant metadata.
data PhotoSize = PhotoSize
  { fileId       :: !Text
  , fileUniqueId :: !Text
  , width        :: !Integer
  , height       :: !Integer
  , fileSize     :: !(Maybe Integer)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing PhotoSize)

data TelegramMedia = TelegramMedia
  { fileId       :: !Text
  , fileUniqueId :: !Text
  , fileName     :: !(Maybe Text)
  , mimeType     :: !(Maybe Text)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing TelegramMedia)

newtype Sticker = Sticker
  { emoji :: Maybe Text
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing Sticker)

-- | Telegram message entity metadata used for mention extraction.
data MessageEntity = MessageEntity
  { type_  :: !Text
  , offset :: !Integer
  , length :: !Integer
  , url    :: !(Maybe Text)
  , language :: !(Maybe Text)
  , user   :: !(Maybe User)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing MessageEntity)

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
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing Chat)

-- | Telegram membership status.
data ChatMemberStatus
  = ChatMemberCreator
  | ChatMemberAdministrator
  | ChatMemberMember
  | ChatMemberRestricted
  | ChatMemberLeft
  | ChatMemberKicked
  deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (PrefixedEnumJSON "ChatMember" ChatMemberStatus)

-- | Telegram membership information returned by @getChatMember@.
data ChatMember = ChatMember
  { status :: !ChatMemberStatus
  , user   :: !User
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSON ChatMember)

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
    deriving Aeson.ToJSON via (SnakeJSON GetFileRequest)

instance TelegramRequest GetFileRequest where
  type TelegramResponse GetFileRequest = File
  telegramMethod _ = "getFile"

data GetUserProfilePhotosRequest = GetUserProfilePhotosRequest
  { userId :: !Integer
  , offset :: !(Maybe Int)
  , limit  :: !(Maybe Int)
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing GetUserProfilePhotosRequest)

instance TelegramRequest GetUserProfilePhotosRequest where
  type TelegramResponse GetUserProfilePhotosRequest = UserProfilePhotos
  telegramMethod _ = "getUserProfilePhotos"

data UserProfilePhotos = UserProfilePhotos
  { totalCount :: !Integer
  , photos     :: ![[PhotoSize]]
  } deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON UserProfilePhotos)

data File = File
  { fileId       :: !Text
  , fileUniqueId :: !Text
  , fileSize     :: !(Maybe Integer)
  , filePath     :: !Text
  } deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON File)

data GetUpdatesRequest = GetUpdatesRequest
  { offset  :: !Int
  , timeout :: !Int
  , limit   :: !Int
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON GetUpdatesRequest)

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
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing SendMessageRequest)

instance TelegramRequest SendMessageRequest where
  type TelegramResponse SendMessageRequest = Message
  telegramMethod _ = "sendMessage"

data InputRichMedia = InputRichMedia
  { type_ :: !Text
  , media :: !Text
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSON InputRichMedia)

data InputRichMessageMedia = InputRichMessageMedia
  { id :: !Text
  , media :: !InputRichMedia
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSON InputRichMessageMedia)

data InputRichMessage = InputRichMessage
  { html :: !Text
  , media :: !(Maybe [InputRichMessageMedia])
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing InputRichMessage)

-- | Minimal reply target used by outgoing Telegram messages.
newtype ReplyParameters = ReplyParameters
  { messageId :: Integer
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSON ReplyParameters)

-- | Request payload for Telegram @sendRichMessage@.
data SendRichMessageRequest = SendRichMessageRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , richMessage         :: !InputRichMessage
  , disableNotification :: !(Maybe Bool)
  , replyParameters     :: !(Maybe ReplyParameters)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing SendRichMessageRequest)

instance TelegramRequest SendRichMessageRequest where
  type TelegramResponse SendRichMessageRequest = Message
  telegramMethod _ = "sendRichMessage"

-- | Request payload for Telegram @editMessageText@.
data EditMessageTextRequest = EditMessageTextRequest
  { chatId      :: !Integer
  , messageId   :: !Integer
  , richMessage :: !InputRichMessage
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing EditMessageTextRequest)

instance TelegramRequest EditMessageTextRequest where
  type TelegramResponse EditMessageTextRequest = Message
  telegramMethod _ = "editMessageText"

data EditPlainMessageTextRequest = EditPlainMessageTextRequest
  { chatId    :: !Integer
  , messageId :: !Integer
  , text      :: !Text
  , parseMode :: !(Maybe ParseMode)
  , entities  :: !(Maybe [MessageEntity])
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing EditPlainMessageTextRequest)

instance TelegramRequest EditPlainMessageTextRequest where
  type TelegramResponse EditPlainMessageTextRequest = Message
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
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing SendPhotoRequest)

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
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing SendVoiceRequest)

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
    deriving Aeson.ToJSON via (SnakeJSON ForwardMessageRequest)

instance TelegramRequest ForwardMessageRequest where
  type TelegramResponse ForwardMessageRequest = Message
  telegramMethod _ = "forwardMessage"

data DeleteMessageRequest = DeleteMessageRequest
  { chatId    :: !Integer
  , messageId :: !Integer
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON DeleteMessageRequest)

instance TelegramRequest DeleteMessageRequest where
  type TelegramResponse DeleteMessageRequest = Bool
  telegramMethod _ = "deleteMessage"

data PinMessageRequest = PinMessageRequest
  { chatId              :: !Integer
  , messageId           :: !Integer
  , disableNotification :: !Bool
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON PinMessageRequest)

instance TelegramRequest PinMessageRequest where
  type TelegramResponse PinMessageRequest = Bool
  telegramMethod _ = "pinChatMessage"

data UnpinMessageRequest = UnpinMessageRequest
  { chatId    :: !Integer
  , messageId :: !Integer
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON UnpinMessageRequest)

instance TelegramRequest UnpinMessageRequest where
  type TelegramResponse UnpinMessageRequest = Bool
  telegramMethod _ = "unpinChatMessage"

newtype GetChatRequest = GetChatRequest
  { chatId :: Integer
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON GetChatRequest)

instance TelegramRequest GetChatRequest where
  type TelegramResponse GetChatRequest = Chat
  telegramMethod _ = "getChat"

data GetChatMemberRequest = GetChatMemberRequest
  { chatId :: !Integer
  , userId :: !Integer
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON GetChatMemberRequest)

instance TelegramRequest GetChatMemberRequest where
  type TelegramResponse GetChatMemberRequest = ChatMember
  telegramMethod _ = "getChatMember"

data BanChatMemberRequest = BanChatMemberRequest
  { chatId    :: !Integer
  , userId    :: !Integer
  , untilDate :: !(Maybe Integer)
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing BanChatMemberRequest)

instance TelegramRequest BanChatMemberRequest where
  type TelegramResponse BanChatMemberRequest = Bool
  telegramMethod _ = "banChatMember"

data UnbanChatMemberRequest = UnbanChatMemberRequest
  { chatId      :: !Integer
  , userId      :: !Integer
  , onlyIfBanned :: !Bool
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON UnbanChatMemberRequest)

instance TelegramRequest UnbanChatMemberRequest where
  type TelegramResponse UnbanChatMemberRequest = Bool
  telegramMethod _ = "unbanChatMember"

newtype LeaveChatRequest = LeaveChatRequest
  { chatId :: Integer
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON LeaveChatRequest)

instance TelegramRequest LeaveChatRequest where
  type TelegramResponse LeaveChatRequest = Bool
  telegramMethod _ = "leaveChat"

data SendChatActionRequest = SendChatActionRequest
  { chatId :: !Integer
  , action :: !ChatAction
  , messageThreadId :: !(Maybe Integer)
  , businessConnectionId :: !(Maybe Text)
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing SendChatActionRequest)

instance TelegramRequest SendChatActionRequest where
  type TelegramResponse SendChatActionRequest = Bool
  telegramMethod _ = "sendChatAction"

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
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (PrefixedEnumJSON "ChatAction" ChatAction)

getUpdates :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> Int -> Eff es [Update]
getUpdates driver offset = callTelegram driver GetUpdatesRequest
  { offset  = offset
  , timeout = telegramLongPollTimeoutSeconds
  , limit   = 100
  }

sendPhoto :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendPhotoRequest -> Eff es Message
sendPhoto =
  callTelegram

uploadPhoto :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendPhotoRequest -> FilePath -> Eff es Message
uploadPhoto driver request path =
  apiMultipartCall driver.config "sendPhoto" (sendPhotoParts request path Nothing)

uploadVoice :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendVoiceRequest -> FilePath -> Eff es Message
uploadVoice driver request path =
  apiMultipartCall driver.config "sendVoice" (sendVoiceParts request path Nothing)

uploadAudio :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendAudioRequest -> FilePath -> Maybe Text -> Eff es Message
uploadAudio driver request path fileName =
  apiMultipartCall driver.config "sendAudio" (sendAudioParts request path fileName)

uploadVideo :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendVideoRequest -> FilePath -> Maybe Text -> Eff es Message
uploadVideo driver request path fileName =
  apiMultipartCall driver.config "sendVideo" (sendVideoParts request path fileName)

uploadDocument :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendDocumentRequest -> FilePath -> Maybe Text -> Eff es Message
uploadDocument driver request path fileName =
  apiMultipartCall driver.config "sendDocument" (sendDocumentParts request path fileName)
deleteMessage :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> Integer -> Integer -> Eff es Bool
deleteMessage driver chatId messageId =
  callTelegram driver DeleteMessageRequest{..}

getChatMember :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> Integer -> Integer -> Eff es ChatMember
getChatMember driver chatId userId =
  callTelegram driver GetChatMemberRequest{..}
