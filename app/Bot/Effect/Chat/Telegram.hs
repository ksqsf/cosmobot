{-
Module      : Bot.Effect.Chat.Telegram
Description : Telegram effects
Stability   : experimental
-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Effect.Chat.Telegram where

import qualified Bot.Effect.Chat as ChatEffect
import Bot.Message
import Data.List (maximum)
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Network.HTTP.Req
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified Data.Text as Text

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

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

data Telegram :: Effect where
  CallTelegram
    :: TelegramRequest req
    => req -> Telegram m (TelegramResponse req)

type instance DispatchOf Telegram = Dynamic

callTelegram
  :: (Telegram :> es, TelegramRequest req)
  => req -> Eff es (TelegramResponse req)
callTelegram = send . CallTelegram

runTelegram
  :: IOE :> es
  => Log :> es
  => Config
  -> Eff (Telegram : es) a
  -> Eff es a
runTelegram cfg = interpret $ \_ -> \case
  CallTelegram request ->
    apiCall cfg (telegramMethod request) request

-- ---------------------------------------------------------------------------
-- Streaming
-- ---------------------------------------------------------------------------

updatesStream'
  :: (Telegram :> es, Log :> es)
  => Int
  -> Stream (Of Update) (Eff es) ()
updatesStream' offset = do
  batches <- S.lift (getUpdates offset)
  S.lift $ logTrace_ [i|Got a batch of #{length batches} messages|]
  S.lift $ logInfo "Telegram update batch" (length batches)
  S.each batches
  let nextOffset = case batches of
        [] -> offset
        _  -> 1 + maximum (map (fromInteger . (.updateId)) batches)
  updatesStream' nextOffset

updatesStream :: (Telegram :> es, Log :> es) => Stream (Of Update) (Eff es) ()
updatesStream = updatesStream' (-1)

incomingMessages :: (Telegram :> es, Log :> es) => Stream (Of IncomingMessage) (Eff es) ()
incomingMessages = S.for updatesStream $ \update ->
  case updateToIncomingMessage update of
    Nothing -> do
      S.lift $ logTrace_ [i|Ignoring Telegram event|]
      S.lift $ logInfo_ "Ignoring Telegram event"
    Just message -> do
      S.lift $ logTrace "incoming Telegram message" message
      S.lift $ logInfo "incoming Telegram message" (incomingMessageLog message)
      S.yield message

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
    , imageUrls = []
    , text      = Text.strip (fromMaybe "" message.text)
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
  mapMaybe entityMentionUserId (fromMaybe [] message.entities)

entityMentionUserId :: MessageEntity -> Maybe Integer
entityMentionUserId entity =
  (.id) <$> entity.user

messageMentionUsernames :: Message -> [Text]
messageMentionUsernames message =
  mapMaybe (entityMentionUsername (fromMaybe "" message.text)) (fromMaybe [] message.entities)

entityMentionUsername :: Text -> MessageEntity -> Maybe Text
entityMentionUsername messageText entity
  | Just username <- entity.user >>= (.username) =
      Just (normalizeUsername username)
  | entity.type_ == "mention" =
      normalizeUsername <$> entityText messageText entity
  | otherwise =
      Nothing

entityText :: Text -> MessageEntity -> Maybe Text
entityText messageText entity =
  let piece = Text.take (fromInteger entity.length) (Text.drop (fromInteger entity.offset) messageText)
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

apiCall
  :: (IOE :> es, Log :> es, Aeson.ToJSON body, Aeson.FromJSON result)
  => Config
  -> Text
  -> body
  -> Eff es result
apiCall cfg method body = do
  resp :: Response <- liftIO $ runReq defaultHttpConfig $
    req POST (apiUrl cfg method) (ReqBodyJson body) jsonResponse mempty
      <&> responseBody
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

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

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

data Message = Message
  { messageId       :: !Integer
  , messageThreadId :: !(Maybe Integer)
  , from            :: !(Maybe User)
  , senderChat      :: !(Maybe Chat)
  , chat            :: !Chat
  , replyToMessage  :: !(Maybe Message)
  , text            :: !(Maybe Text)
  , entities        :: !(Maybe [MessageEntity])
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

getMe :: Telegram :> es => Eff es User
getMe =
  callTelegram GetMeRequest

getUpdates :: Telegram :> es => Int -> Eff es [Update]
getUpdates offset = callTelegram $ GetUpdatesRequest
  { offset  = offset
  , timeout = 30
  , limit   = 100
  }

sendMessage :: Telegram :> es => SendMessageRequest -> Eff es Message
sendMessage = callTelegram

sendPhoto :: Telegram :> es => SendPhotoRequest -> Eff es Message
sendPhoto = callTelegram

replyTo :: Telegram :> es => IncomingMessage -> Text -> Eff es (Maybe Integer)
replyTo message body =
  case (message.platform, message.chatId) of
    (PlatformTelegram, Just chatId) -> do
      sent <- replyTextAndImages chatId message.messageId body
      pure (Just sent.messageId)
    _ ->
      pure Nothing

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

replyTextAndImages :: Telegram :> es => Integer -> Maybe Integer -> Text -> Eff es Message
replyTextAndImages chatId replyToMessageId body =
  case ChatEffect.replyImageUrls body of
    [] -> sendText (ChatEffect.renderReplyBody body)
    firstImage : restImages -> do
      firstSent <- sendPhoto SendPhotoRequest
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
    sendImage caption photo = void $ sendPhoto SendPhotoRequest
      { chatId = chatId
      , messageThreadId = Nothing
      , photo = photo
      , caption = caption
      , parseMode = Nothing
      , disableNotification = Nothing
      , replyToMessageId = Nothing
      }

nonEmptyText :: Text -> Maybe Text
nonEmptyText text
  | Text.null text = Nothing
  | otherwise      = Just text

forwardMessage :: Telegram :> es => Integer -> Integer -> Integer -> Eff es Message
forwardMessage chatId fromChatId messageId =
  callTelegram $ ForwardMessageRequest{..}

deleteMessage :: Telegram :> es => Integer -> Integer -> Eff es Bool
deleteMessage chatId messageId =
  callTelegram $ DeleteMessageRequest{..}

pinMessage :: Telegram :> es => Integer -> Integer -> Bool -> Eff es Bool
pinMessage chatId messageId disableNotification =
  callTelegram $ PinMessageRequest{..}

unpinMessage :: Telegram :> es => Integer -> Integer -> Eff es Bool
unpinMessage chatId messageId =
  callTelegram $ UnpinMessageRequest{..}

getChat :: Telegram :> es => Integer -> Eff es Chat
getChat chatId =
  callTelegram $ GetChatRequest{..}

getChatMember :: Telegram :> es => Integer -> Integer -> Eff es ChatMember
getChatMember chatId userId =
  callTelegram $ GetChatMemberRequest{..}

banChatMember :: Telegram :> es => Integer -> Integer -> Maybe Integer -> Eff es Bool
banChatMember chatId userId untilDate =
  callTelegram $ BanChatMemberRequest{..}

unbanChatMember :: Telegram :> es => Integer -> Integer -> Eff es Bool
unbanChatMember chatId userId =
  callTelegram $ UnbanChatMemberRequest { onlyIfBanned = True, .. }

leaveChat :: Telegram :> es => Integer -> Eff es Bool
leaveChat chatId =
  callTelegram $ LeaveChatRequest{..}
