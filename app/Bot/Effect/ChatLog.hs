{-|
Module      : Bot.Effect.ChatLog
Description : Chat log effect
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Effect.ChatLog
  ( ChatLog
  , ChatLogEntry (..)
  , recordMessage
  , recordBotMessage
  , recordIncomingMessages
  , queryChat
  , runChatLog
  )
where

import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Storage as Storage
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Streaming.Prelude as S

-- | Append-only chat log used by agent tools for local context.
data ChatLog :: Effect where
  RecordMessage
    :: IncomingMessage
    -> ChatLog m ()
  RecordBotMessage
    :: IncomingMessage
    -> Maybe Integer
    -> Text
    -> ChatLog m ()
  QueryChat
    :: IncomingMessage
    -> Int
    -> Bool
    -> ChatLog m [ChatLogEntry]

type instance DispatchOf ChatLog = Dynamic

-- | Sanitized message record exposed to agent tools.
data ChatLogEntry = ChatLogEntry
  { platform :: !ChatPlatform
  , kind :: !ChatKind
  , chatId :: !(Maybe Integer)
  , senderId :: !(Maybe Integer)
  , senderUsername :: !(Maybe Text)
  , messageId :: !(Maybe Integer)
  , replyToMessageId :: !(Maybe Integer)
  , isBot :: !Bool
  , mentions :: ![Integer]
  , mentionUsernames :: ![Text]
  , imageUrls :: ![Text]
  , text :: !Text
  }
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | Record a user/platform message.
recordMessage :: ChatLog :> es => IncomingMessage -> Eff es ()
recordMessage message =
  send (RecordMessage message)

-- | Record every incoming message passing through a stream.
recordIncomingMessages
  :: ChatLog :> es
  => Stream (Of IncomingMessage) (Eff es) ()
  -> Stream (Of IncomingMessage) (Eff es) ()
recordIncomingMessages =
  S.mapM \message -> do
    recordMessage message
    pure message

-- | Record a bot reply in the same chat as its triggering message.
recordBotMessage :: ChatLog :> es => IncomingMessage -> Maybe Integer -> Text -> Eff es ()
recordBotMessage context messageId body =
  send (RecordBotMessage context messageId body)

-- | Query recent messages from the current chat in chronological order.
queryChat :: ChatLog :> es => IncomingMessage -> Int -> Bool -> Eff es [ChatLogEntry]
queryChat message limit includeBotMessages =
  send (QueryChat message limit includeBotMessages)

-- | Interpret chat logging through the storage capability.
runChatLog
  :: (IOE :> es, Log :> es, Storage.Storage :> es)
  => Eff (ChatLog : es) a
  -> Eff es a
runChatLog inner =
  interpret
    (\_ -> \case
      RecordMessage message ->
        persistRecord (userRecord message)
      RecordBotMessage context messageId body ->
        persistRecord (botRecord context messageId body)
      QueryChat message limit includeBotMessages ->
        queryStored message limit includeBotMessages
    )
    inner

data ChatLogRecord = ChatLogRecord
  { message :: !IncomingMessage
  , isBot :: !Bool
  }

userRecord :: IncomingMessage -> ChatLogRecord
userRecord message =
  ChatLogRecord message False

botRecord :: IncomingMessage -> Maybe Integer -> Text -> ChatLogRecord
botRecord context messageId body =
  ChatLogRecord
    (botMessage context messageId body)
    True

botMessage :: IncomingMessage -> Maybe Integer -> Text -> IncomingMessage
botMessage context messageId body =
  IncomingMessage
    { platform = context.platform
    , kind = context.kind
    , chatId = context.chatId
    , chatAliases = context.chatAliases
    , digest = emptyMessageDigest
    , senderId = Nothing
    , senderUsername = Nothing
    , messageId = messageId
    , replyToMessageId = context.messageId
    , mentions = []
    , mentionUsernames = []
    , imageUrls = Chat.replyImageUrls body
    , text = Chat.renderReplyBody body
    , raw = Aeson.object
        [ "type" Aeson..= Aeson.String "bot_message"
        , "body" Aeson..= body
        ]
    }

persistRecord :: (IOE :> es, Log :> es, Storage.Storage :> es) => ChatLogRecord -> Eff es ()
persistRecord record =
  Storage.saveChatLogEntry
    (platformKey record.message)
    (kindKey record.message)
    record.message.chatId
    record.isBot
    (Aeson.toJSON (sanitizeChatLogEntry (chatLogEntry record)))
    `catch` \(err :: SomeException) ->
      logInfo_ [i|Failed to persist chat log entry: #{show err :: String}|]

queryStored :: Storage.Storage :> es => IncomingMessage -> Int -> Bool -> Eff es [ChatLogEntry]
queryStored message limit includeBotMessages = do
  values <- Storage.queryChatLogEntries (platformKey message) (kindKey message) message.chatId includeBotMessages limit
  pure (map sanitizeChatLogEntry (mapMaybe (AesonTypes.parseMaybe Aeson.parseJSON) values))

platformKey :: IncomingMessage -> Text
platformKey =
  jsonText . (.platform)

kindKey :: IncomingMessage -> Text
kindKey =
  jsonText . (.kind)

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

chatLogEntry :: ChatLogRecord -> ChatLogEntry
chatLogEntry record =
  ChatLogEntry
    { platform = record.message.platform
    , kind = record.message.kind
    , chatId = record.message.chatId
    , senderId = record.message.senderId
    , senderUsername = record.message.senderUsername
    , messageId = record.message.messageId
    , replyToMessageId = record.message.replyToMessageId
    , isBot = record.isBot
    , mentions = record.message.mentions
    , mentionUsernames = record.message.mentionUsernames
    , imageUrls = record.message.imageUrls
    , text = record.message.text
    }

sanitizeChatLogEntry :: ChatLogEntry -> ChatLogEntry
sanitizeChatLogEntry ChatLogEntry{..} =
  ChatLogEntry
    { imageUrls = map sanitizeImageRef imageUrls
    , text = sanitizeImageText text
    , ..
    }

sanitizeImageRef :: Text -> Text
sanitizeImageRef ref
  | isBase64ImageRef ref = "[Picture]"
  | otherwise = ref

sanitizeImageText :: Text -> Text
sanitizeImageText text =
  Text.intercalate "\n" (map sanitizeImageLine (Text.lines text))

sanitizeImageLine :: Text -> Text
sanitizeImageLine line
  | isBase64ImageRef stripped = "[Picture]"
  | Just ref <- Text.stripPrefix "[image] " stripped
  , isBase64ImageRef ref = "[image] [Picture]"
  | isBase64ImageRefInfix stripped = "[Picture]"
  | otherwise = line
  where
    stripped = Text.strip line

isBase64ImageRef :: Text -> Bool
isBase64ImageRef ref =
  "data:image/" `Text.isPrefixOf` Text.strip ref &&
    ";base64," `Text.isInfixOf` ref

isBase64ImageRefInfix :: Text -> Bool
isBase64ImageRefInfix text =
  "data:image/" `Text.isInfixOf` text &&
    ";base64," `Text.isInfixOf` text
