{-|
Module      : Bot.ChatLog.Types
Description : Chat log domain types
Stability   : experimental
-}

module Bot.ChatLog.Types
  ( ChatLogEntry (..)
  , ChatLogScope (..)
  , ChatLogSummary (..)
  , ChatLogItem (..)
  , ChatLogWindow (..)
  , ChatLogWindowAnchor (..)
  , SenderChatLogScope (..)
  , ChatLogTimeRange (..)
  , unboundedChatLogTimeRange
  )
where

import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import Data.Time (UTCTime)

data SenderChatLogScope
  = SenderChatLogChat
  | SenderChatLogGlobal
  deriving (Eq, Show)

data ChatLogTimeRange = ChatLogTimeRange
  { since :: !(Maybe UTCTime)
  , before :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

unboundedChatLogTimeRange :: ChatLogTimeRange
unboundedChatLogTimeRange =
  ChatLogTimeRange Nothing Nothing

-- | Sanitized message record exposed to agent tools.
data ChatLogEntry = ChatLogEntry
  { recordedAt :: !(Maybe UTCTime)
  , platform :: !ChatPlatform
  , kind :: !ChatKind
  , chatId :: !(Maybe Integer)
  , senderId :: !(Maybe Text)
  , senderUsername :: !(Maybe Text)
  , messageId :: !(Maybe MessageId)
  , replyToMessageId :: !(Maybe MessageId)
  , isBot :: !Bool
  , mentions :: ![Text]
  , mentionUsernames :: ![Text]
  , imageUrls :: ![Text]
  , files :: ![MessageFile]
  , text :: !Text
  }
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data ChatLogScope = ChatLogScope
  { platform :: !ChatPlatform
  , kind :: !ChatKind
  , chatId :: !(Maybe Integer)
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data ChatLogSummary = ChatLogSummary
  { scope :: !ChatLogScope
  , messageCount :: !Int
  , latestAt :: !(Maybe UTCTime)
  }
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data ChatLogItem = ChatLogItem
  { rowId :: !Integer
  , entry :: !ChatLogEntry
  }
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data ChatLogWindow = ChatLogWindow
  { scope :: !ChatLogScope
  , entries :: ![ChatLogItem]
  , hasOlder :: !Bool
  , hasNewer :: !Bool
  , anchorFound :: !Bool
  , anchorMessageId :: !(Maybe MessageId)
  }
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data ChatLogWindowAnchor
  = LatestChatLogWindow
  | BeforeChatLogRow !Integer
  | AfterChatLogRow !Integer
  | AroundChatLogMessage !MessageId
  deriving (Eq, Show)
