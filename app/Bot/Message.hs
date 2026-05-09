{-
Module      : Bot.Message
Description : Unified incoming message types
Stability   : experimental
-}

module Bot.Message where

import Bot.Prelude
import qualified Data.Aeson as Aeson

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

data ReferencedMessage = ReferencedMessage
  { messageId :: !(Maybe Integer)
  , text      :: !Text
  , imageUrls :: ![Text]
  }
  deriving (Show, Generic, Aeson.ToJSON)
