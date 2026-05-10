{-|
Module      : Bot.Effect.Chat.Telegram
Description : Telegram effects
Stability   : experimental
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Effect.Chat.Telegram
  ( Telegram
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
  , SendPhotoRequest (..)
  , runTelegram
  , incomingMessages
  , getMe
  , getUpdates
  , sendMessage
  , sendPhoto
  , uploadPhoto
  , replyTo
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
  , mentionUser
  )
where

import qualified Bot.Effect.Chat as ChatEffect
import Bot.Message
import Control.Concurrent (threadDelay)
import Data.List (maximum)
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as TextEncoding
import Network.HTTP.Req
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.Directory (createDirectoryIfMissing, removeFile)
import qualified Data.Text as Text
import System.FilePath ((<.>))
import System.IO (hClose, openTempFile)

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

-- | Telegram Bot API credentials.
newtype Config = Config
  { botToken :: Text
  }
  deriving (Show)

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

type instance DispatchOf Telegram = Dynamic

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
  => Log :> es
  => Config
  -> Eff (Telegram : es) a
  -> Eff es a
runTelegram cfg = interpret $ \_ -> \case
  CallTelegram request ->
    apiCall cfg (telegramMethod request) request
  FileUrl fileId -> do
    file :: File <- apiCall cfg (telegramMethod (GetFileRequest fileId)) (GetFileRequest fileId)
    pure (telegramFileUrl cfg file.filePath)
  UploadPhoto request path ->
    apiMultipartCall cfg "sendPhoto" (sendPhotoParts request path)

-- ---------------------------------------------------------------------------
-- Streaming
-- ---------------------------------------------------------------------------

updatesStream'
  :: (Telegram :> es, Log :> es, IOE :> es)
  => Int
  -> Stream (Of Update) (Eff es) ()
updatesStream' offset = do
  result <- S.lift $ (Right <$> getUpdates offset) `catch` \(err :: SomeException) -> do
    logInfo "Telegram getUpdates failed, retrying" (show err :: String)
    liftIO $ threadDelay telegramRetryDelayMicroseconds
    pure (Left ())
  case result of
    Left () ->
      updatesStream' offset
    Right batches -> do
      S.lift $ logTrace_ [i|Got a batch of #{length batches} messages|]
      S.lift $ logInfo "Telegram update batch" (length batches)
      S.each batches
      let nextOffset = case batches of
            [] -> offset
            _  -> 1 + maximum (map (fromInteger . (.updateId)) batches)
      updatesStream' nextOffset

updatesStream :: (Telegram :> es, Log :> es, IOE :> es) => Stream (Of Update) (Eff es) ()
updatesStream = updatesStream' 0

-- | Poll Telegram updates and yield platform-independent messages.
incomingMessages :: (Telegram :> es, Log :> es, IOE :> es) => Stream (Of IncomingMessage) (Eff es) ()
incomingMessages = S.for updatesStream $ \update ->
  case updateToIncomingMessage update of
    Nothing -> do
      S.lift $ logTrace_ [i|Ignoring Telegram event|]
      S.lift $ logInfo_ "Ignoring Telegram event"
    Just parsedMessage -> do
      message <- S.lift (resolveIncomingMessageImages parsedMessage)
      S.lift $ logTrace "incoming Telegram message" message
      S.lift $ logInfo "incoming Telegram message" (incomingMessageLogLine message)
      S.yield message

resolveIncomingMessageImages :: Telegram :> es => IncomingMessage -> Eff es IncomingMessage
resolveIncomingMessageImages message = do
  imageUrls <- traverse fileUrl message.imageUrls
  pure IncomingMessage
    { platform = message.platform
    , kind = message.kind
    , chatId = message.chatId
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
updateToIncomingMessage Update{message = telegramMessage} = do
  message <- telegramMessage
  pure IncomingMessage
    { platform  = PlatformTelegram
    , kind      = telegramChatKind message.chat.type_
    , chatId    = Just message.chat.id
    , senderId  = (.id) <$> message.from
    , senderUsername = message.from >>= (.username)
    , messageId = Just message.messageId
    , replyToMessageId = (.messageId) <$> message.replyToMessage
    , mentions  = messageMentionIds message
    , mentionUsernames = messageMentionUsernames message
    , imageUrls = messageImageFileIds message
    , text      = messageText message
    , raw       = Aeson.toJSON message
    }

telegramChatKind :: ChatType -> ChatKind
telegramChatKind = \case
  ChatTypePrivate    -> ChatPrivate
  ChatTypeGroup      -> ChatGroup
  ChatTypeSuperGroup -> ChatGroup
  ChatTypeChannel    -> ChatChannel

messageMentionIds :: Message -> [Integer]
messageMentionIds message =
  mapMaybe entityMentionUserId (messageEntities message)

entityMentionUserId :: MessageEntity -> Maybe Integer
entityMentionUserId entity =
  (.id) <$> entity.user

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
  fmap (.fileId) . viaNonEmpty largest
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
getMessageContent :: Telegram :> es => IncomingMessage -> Integer -> Eff es (Maybe ReferencedMessage)
getMessageContent message messageId =
  case Aeson.fromJSON message.raw :: Aeson.Result Message of
    Aeson.Success telegramMessage ->
      case telegramMessage.replyToMessage of
        Just referenced
          | referenced.messageId == messageId -> do
              imageUrls <- traverse fileUrl (messageImageFileIds referenced)
              pure $ Just ReferencedMessage
                { messageId = Just referenced.messageId
                , text = messageText referenced
                , imageUrls = imageUrls
                }
        _ -> pure Nothing
    Aeson.Error _ ->
      pure Nothing

entityMentionUsername :: Text -> MessageEntity -> Maybe Text
entityMentionUsername text entity
  | Just username <- entity.user >>= (.username) =
      Just (normalizeUsername username)
  | entity.type_ == "mention" =
      normalizeUsername <$> entityText text entity
  | otherwise =
      Nothing

entityText :: Text -> MessageEntity -> Maybe Text
entityText text entity =
  let piece = Text.take (fromInteger entity.length) (Text.drop (fromInteger entity.offset) text)
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
  :: (IOE :> es, Log :> es, Aeson.ToJSON body, Aeson.FromJSON result)
  => Config
  -> Text
  -> body
  -> Eff es result
apiCall cfg method body = do
  resp :: Response <- liftIO $ runReq defaultHttpConfig $
    req POST (apiUrl cfg method) (ReqBodyJson body) jsonResponse telegramRequestOptions
      <&> responseBody
  decodeResponse resp

apiMultipartCall
  :: (IOE :> es, Log :> es, Aeson.FromJSON result)
  => Config
  -> Text
  -> [Multipart.Part]
  -> Eff es result
apiMultipartCall cfg method parts = do
  resp :: Response <- liftIO $ runReq defaultHttpConfig do
    body <- reqBodyMultipart parts
    req POST (apiUrl cfg method) body jsonResponse telegramRequestOptions
      <&> responseBody
  decodeResponse resp

decodeResponse
  :: (IOE :> es, Aeson.FromJSON result)
  => Response
  -> Eff es result
decodeResponse resp =
  case resp of
    Err desc -> throwIO (APIException desc)
    Ok value -> case Aeson.fromJSON value of
      Aeson.Success x  -> pure x
      Aeson.Error  err -> throwIO (APIException (Text.pack err))

newtype APIException = APIException Text
  deriving (Show)
instance Exception APIException

data Response
  = Ok  Aeson.Value
  | Err Text
  deriving (Show, Generic)

instance Aeson.FromJSON Response where
  parseJSON = Aeson.withObject "Response" $ \o -> do
    ok <- o Aeson..: "ok"
    if ok
      then Ok <$> o Aeson..: "result"
      else Err <$> o Aeson..: "description"

maybeField :: Aeson.ToJSON value => Aeson.Key -> Maybe value -> [Aeson.Pair]
maybeField key =
  maybe [] (\value -> [key Aeson..= value])

telegramLongPollTimeoutSeconds :: Int
telegramLongPollTimeoutSeconds = 30

telegramHttpResponseTimeoutMicroseconds :: Int
telegramHttpResponseTimeoutMicroseconds =
  (telegramLongPollTimeoutSeconds + 10) * 1000000

telegramRetryDelayMicroseconds :: Int
telegramRetryDelayMicroseconds =
  5 * 1000000

telegramRequestOptions :: Option 'Https
telegramRequestOptions =
  responseTimeout telegramHttpResponseTimeoutMicroseconds

sendPhotoParts :: SendPhotoRequest -> FilePath -> [Multipart.Part]
sendPhotoParts SendPhotoRequest{..} path =
  [ textPart "chat_id" (show chatId)
  , Multipart.partFile "photo" path
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

textPart :: Text -> Text -> Multipart.Part
textPart name value =
  Multipart.partBS name (TextEncoding.encodeUtf8 value)

maybePart :: Text -> Maybe Text -> [Multipart.Part]
maybePart name =
  maybe [] \value -> [textPart name value]

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
  , user   :: !(Maybe User)
  } deriving (Show, Generic)

instance Aeson.FromJSON MessageEntity where
  parseJSON = Aeson.withObject "MessageEntity" $ \o -> do
    type_ <- o Aeson..: "type"
    offset <- o Aeson..: "offset"
    entityLength <- o Aeson..: "length"
    user <- o Aeson..:? "user"
    pure MessageEntity{length = entityLength, ..}

instance Aeson.ToJSON MessageEntity where
  toJSON entity = Aeson.object $
    [ "type" Aeson..= type_
    , "offset" Aeson..= offset
    , "length" Aeson..= entity.length
    ]
    <> maybeField "user" user
    where
      MessageEntity{type_, offset, user} = entity

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
    <> maybeField "disable_notification" disableNotification
    <> maybeField "reply_to_message_id" replyToMessageId

instance Aeson.FromJSON SendMessageRequest where
  parseJSON = Aeson.withObject "SendMessageRequest" $ \o -> do
    chatId <- o Aeson..: "chat_id"
    messageThreadId <- o Aeson..:? "message_thread_id"
    text <- o Aeson..: "text"
    parseMode <- o Aeson..:? "parse_mode"
    disableNotification <- o Aeson..:? "disable_notification"
    replyToMessageId <- o Aeson..:? "reply_to_message_id"
    pure SendMessageRequest{..}

instance TelegramRequest SendMessageRequest where
  type TelegramResponse SendMessageRequest = Message
  telegramMethod _ = "sendMessage"

-- | Request payload for Telegram @sendPhoto@.
data SendPhotoRequest = SendPhotoRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , photo               :: !Text
  , caption             :: !(Maybe Text)
  , parseMode           :: !(Maybe ParseMode)
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
    <> maybeField "disable_notification" disableNotification
    <> maybeField "reply_to_message_id" replyToMessageId

instance Aeson.FromJSON SendPhotoRequest where
  parseJSON = Aeson.withObject "SendPhotoRequest" $ \o -> do
    chatId <- o Aeson..: "chat_id"
    messageThreadId <- o Aeson..:? "message_thread_id"
    photo <- o Aeson..: "photo"
    caption <- o Aeson..:? "caption"
    parseMode <- o Aeson..:? "parse_mode"
    disableNotification <- o Aeson..:? "disable_notification"
    replyToMessageId <- o Aeson..:? "reply_to_message_id"
    pure SendPhotoRequest{..}

instance TelegramRequest SendPhotoRequest where
  type TelegramResponse SendPhotoRequest = Message
  telegramMethod _ = "sendPhoto"

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

-- | Reply to a Telegram chat, including image directives in the body.
replyTo :: (Telegram :> es, IOE :> es) => IncomingMessage -> Text -> Eff es (Maybe Integer)
replyTo message body =
  case (message.platform, message.chatId) of
    (PlatformTelegram, Just chatId) -> do
      sent <- replyTextAndImages chatId message.messageId body
      pure (Just sent.messageId)
    _ ->
      pure Nothing

-- | Reply with an HTML mention for a Telegram user id.
mentionUser :: Telegram :> es => IncomingMessage -> Integer -> Text -> Eff es (Maybe Integer)
mentionUser message userId body =
  case (message.platform, message.chatId) of
    (PlatformTelegram, Just chatId) -> do
      sent <- sendMessage SendMessageRequest
        { chatId = chatId
        , messageThreadId = Nothing
        , text = telegramMentionHtml userId body
        , parseMode = Just ParseModeHTML
        , disableNotification = Nothing
        , replyToMessageId = message.messageId
        }
      pure (Just sent.messageId)
    _ ->
      pure Nothing

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

replyTextAndImages :: (Telegram :> es, IOE :> es) => Integer -> Maybe Integer -> Text -> Eff es Message
replyTextAndImages chatId replyToMessageId body =
  case ChatEffect.replyImageUrls body of
    [] -> sendText (ChatEffect.renderReplyBody body)
    firstImage : restImages -> do
      firstSent <- sendImageRequest SendPhotoRequest
        { chatId = chatId
        , messageThreadId = Nothing
        , photo = firstImage
        , caption = nonEmptyText (ChatEffect.renderReplyBody body)
        , parseMode = Nothing
        , disableNotification = Nothing
        , replyToMessageId = replyToMessageId
        }
      traverse_ (sendImage Nothing) restImages
      pure firstSent
  where
    sendText text = sendMessage SendMessageRequest
      { chatId = chatId
      , messageThreadId = Nothing
      , text = text
      , parseMode = Nothing
      , disableNotification = Nothing
      , replyToMessageId = replyToMessageId
      }
    sendImage caption photo = void $ sendImageRequest SendPhotoRequest
      { chatId = chatId
      , messageThreadId = Nothing
      , photo = photo
      , caption = caption
      , parseMode = Nothing
      , disableNotification = Nothing
      , replyToMessageId = Nothing
      }

sendImageRequest :: (Telegram :> es, IOE :> es) => SendPhotoRequest -> Eff es Message
sendImageRequest request =
  case localFilePhoto request.photo of
    Just path -> uploadPhoto request path
    Nothing ->
      case dataImagePhoto request.photo of
        Just bytes -> uploadTemporaryPhoto request bytes
        Nothing    -> sendPhoto request

localFilePhoto :: Text -> Maybe FilePath
localFilePhoto photo =
  Text.unpack <$> Text.stripPrefix "file://" photo

dataImagePhoto :: Text -> Maybe ByteString.ByteString
dataImagePhoto photo = do
  rest <- Text.stripPrefix "data:image/" (Text.strip photo)
  let (_, encodedWithMarker) = Text.breakOn ";base64," rest
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encoded))

uploadTemporaryPhoto :: (Telegram :> es, IOE :> es) => SendPhotoRequest -> ByteString.ByteString -> Eff es Message
uploadTemporaryPhoto request bytes = do
  path <- liftIO do
    createDirectoryIfMissing True telegramTempDir
    (path, fileHandle) <- openTempFile telegramTempDir ("telegram-photo" <.> "png")
    ByteString.hPut fileHandle bytes
    hClose fileHandle
    pure path
  uploadPhoto request path `finally` cleanup path
  where
    cleanup path =
      liftIO (removeFile path) `catch` \(_ :: SomeException) -> pure ()

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
