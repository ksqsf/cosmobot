{-
Module      : Bot.Message
Description : Unified incoming message types
Stability   : experimental
-}

module Bot.Message where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

data ChatPlatform
  = PlatformQQ
  | PlatformTelegram
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data ChatKind
  = ChatPrivate
  | ChatGroup
  | ChatChannel
  | ChatUnknown Text
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data IncomingMessage = IncomingMessage
  { platform  :: !ChatPlatform
  , kind      :: !ChatKind
  , chatId    :: !(Maybe Integer)
  , senderId  :: !(Maybe Integer)
  , senderUsername :: !(Maybe Text)
  , messageId :: !(Maybe Integer)
  , replyToMessageId :: !(Maybe Integer)
  , mentions  :: ![Integer]
  , mentionUsernames :: ![Text]
  , imageUrls :: ![Text]
  , text      :: !Text
  , raw       :: !Aeson.Value
  }
  deriving (Show, Generic, Aeson.ToJSON)

data IncomingMessageLog = IncomingMessageLog
  { platform :: !ChatPlatform
  , kind     :: !ChatKind
  , chatId   :: !(Maybe Integer)
  , senderId :: !(Maybe Integer)
  , senderUsername :: !(Maybe Text)
  , messageId :: !(Maybe Integer)
  , text     :: !Text
  , imageCount :: !Int
  }
  deriving (Show, Generic, Aeson.ToJSON)

incomingMessageLog :: IncomingMessage -> IncomingMessageLog
incomingMessageLog message =
  IncomingMessageLog
    { platform = message.platform
    , kind = message.kind
    , chatId = message.chatId
    , senderId = message.senderId
    , senderUsername = message.senderUsername
    , messageId = message.messageId
    , text = message.text
    , imageCount = length message.imageUrls
    }

incomingMessageLogLine :: IncomingMessage -> Text
incomingMessageLogLine message =
  Text.unwords
    [ "platform=" <> show message.platform
    , "kind=" <> show message.kind
    , "chat=" <> showMaybe message.chatId
    , "sender=" <> showMaybe message.senderId
    , "username=" <> fromMaybe "-" message.senderUsername
    , "message=" <> showMaybe message.messageId
    , "reply_to=" <> showMaybe message.replyToMessageId
    , "mentions=" <> show (length message.mentions + length message.mentionUsernames)
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

data ReferencedMessage = ReferencedMessage
  { messageId :: !(Maybe Integer)
  , text      :: !Text
  , imageUrls :: ![Text]
  }
  deriving (Show, Generic, Aeson.ToJSON)
