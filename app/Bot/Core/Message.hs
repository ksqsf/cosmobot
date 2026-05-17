{-|
Module      : Bot.Core.Message
Description : Unified incoming message types
Stability   : experimental
-}

module Bot.Core.Message
  ( -- * Chat identity
    ChatPlatform (..)
  , chatPlatformKey
  , ChatKind (..)
  , MessageDigest (..)
  , emptyMessageDigest

    -- * Incoming messages
  , IncomingMessage (..)
  , incomingMessageLogLine
  , MessageInput (..)
  , MessageInputAttachment (..)
  , inputWithImages
  , messageInputImageUrls

    -- * Referenced messages
  , ReferencedMessage (..)
  )
where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

-- | Chat platform backends supported by the unified message layer.
data ChatPlatform
  = PlatformQQ
  -- ^ Tencent QQ via a OneBot-compatible gateway.
  | PlatformTelegram
  -- ^ Telegram Bot API.
  | PlatformMatrix
  -- ^ Matrix Client-Server API.
  deriving (Eq, Ord, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

chatPlatformKey :: ChatPlatform -> Text
chatPlatformKey = \case
  PlatformQQ ->
    "qq"
  PlatformTelegram ->
    "telegram"
  PlatformMatrix ->
    "matrix"

-- | Coarse chat shape shared across platforms.
data ChatKind
  = ChatPrivate
  | ChatGroup
  | ChatChannel
  | ChatUnknown Text
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | Driver-provided facts that are not inherent in the raw message payload.
data MessageDigest = MessageDigest
  { chatIsAllowed :: !Bool
  , senderIsAllowed :: !Bool
  , senderIsSuperuser :: !Bool
  , mentionsBot :: !Bool
  , botId :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON MessageDigest where
  toJSON MessageDigest{chatIsAllowed, senderIsAllowed, senderIsSuperuser, mentionsBot, botId} =
    Aeson.object
      [ "chatIsAllowed" Aeson..= chatIsAllowed
      , "senderIsAllowed" Aeson..= senderIsAllowed
      , "senderIsSuperuser" Aeson..= senderIsSuperuser
      , "mentionsBot" Aeson..= mentionsBot
      , "botId" Aeson..= botId
      ]

instance Aeson.FromJSON MessageDigest where
  parseJSON = Aeson.withObject "MessageDigest" \o ->
    MessageDigest
      <$> o Aeson..: "chatIsAllowed"
      <*> o Aeson..: "senderIsAllowed"
      <*> o Aeson..: "senderIsSuperuser"
      <*> o Aeson..: "mentionsBot"
      <*> o Aeson..:? "botId"

emptyMessageDigest :: MessageDigest
emptyMessageDigest =
  MessageDigest
    { chatIsAllowed = False
    , senderIsAllowed = False
    , senderIsSuperuser = False
    , mentionsBot = False
    , botId = Nothing
    }

-- | Platform-normalized message consumed by handlers.
data IncomingMessage = IncomingMessage
  { platform  :: !ChatPlatform
  , kind      :: !ChatKind
  , chatId    :: !(Maybe Integer)
  , chatAliases :: ![Text]
  , digest    :: !MessageDigest
  , senderId  :: !(Maybe Text)
  , senderUsername :: !(Maybe Text)
  , messageId :: !(Maybe Integer)
  , replyToMessageId :: !(Maybe Integer)
  , mentions  :: ![Integer]
  , mentionUsernames :: ![Text]
  , imageUrls :: ![Text]
  , text      :: !Text
  , raw       :: !Aeson.Value
  }
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | Normalized user-provided input for one handler/agent turn.
--
-- The attachment type is intentionally algebraic so non-image inputs such as
-- documents can be added without threading another parallel field through the
-- handler and agent layers.
data MessageInput = MessageInput
  { text :: !Text
  , attachments :: ![MessageInputAttachment]
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data MessageInputAttachment
  = MessageInputImageUrl !Text
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

inputWithImages :: Text -> [Text] -> MessageInput
inputWithImages text imageUrls =
  MessageInput
    { text = text
    , attachments = map MessageInputImageUrl imageUrls
    }

messageInputImageUrls :: MessageInput -> [Text]
messageInputImageUrls MessageInput{attachments} =
  [ url
  | MessageInputImageUrl rawUrl <- attachments
  , let url = Text.strip rawUrl
  , not (Text.null url)
  ]

-- | Compact one-line representation for info-level logs.
incomingMessageLogLine :: IncomingMessage -> Text
incomingMessageLogLine message =
  Text.unwords
    [ "platform=" <> show message.platform
    , "kind=" <> show message.kind
    , "chat=" <> showMaybe message.chatId
    , "chat_allowed=" <> show message.digest.chatIsAllowed
    , "sender_allowed=" <> show message.digest.senderIsAllowed
    , "sender=" <> showMaybe message.senderId
    , "username=" <> fromMaybe "-" message.senderUsername
    , "superuser=" <> show message.digest.senderIsSuperuser
    , "bot=" <> showMaybe message.digest.botId
    , "message=" <> showMaybe message.messageId
    , "reply_to=" <> showMaybe message.replyToMessageId
    , "mentions=" <> show (length message.mentions + length message.mentionUsernames)
    , "mentions_bot=" <> show message.digest.mentionsBot
    , "images=" <> show (length message.imageUrls)
    , "text=" <> previewText 80 message.text
    ]

previewText :: Int -> Text -> Text
previewText maxChars text =
  let oneLine = Text.unwords (Text.words text)
      shortened = Text.take maxChars oneLine
  in if Text.length oneLine > maxChars
    then shortened <> "..."
    else shortened

showMaybe :: Show a => Maybe a -> Text
showMaybe =
  maybe "-" show

-- | Minimal content fetched for a message referenced by reply.
data ReferencedMessage = ReferencedMessage
  { messageId :: !(Maybe Integer)
  , senderDisplayName :: !(Maybe Text)
  , senderIdentifier :: !(Maybe Text)
  , text      :: !Text
  , imageUrls :: ![Text]
  }
  deriving (Show, Generic, Aeson.ToJSON)
