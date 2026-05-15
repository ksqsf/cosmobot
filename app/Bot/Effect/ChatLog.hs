{-|
Module      : Bot.Effect.ChatLog
Description : Chat log effect
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}

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
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Int as Int
import qualified Data.Text as Text
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
  , senderId :: !(Maybe Text)
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

data ChatLogRow = ChatLogRow
  { id :: ID ChatLogRow
  , platform_key :: Text
  , kind_key :: Text
  , chat_id :: Maybe Int.Int64
  , sender_id :: Maybe Text
  , sender_username :: Maybe Text
  , message_id :: Maybe Int.Int64
  , reply_to_message_id :: Maybe Int.Int64
  , is_bot :: Bool
  , mentions :: Text
  , mention_usernames :: Text
  , image_urls :: Text
  , body_text :: Text
  }
  deriving (Generic)

instance SqlRow ChatLogRow

chatLogRows :: Table ChatLogRow
chatLogRows =
  table "chat_log_entries"
    [ #id :- autoPrimary
    , #platform_key :- index
    , #kind_key :- index
    , #chat_id :- index
    ]

ensureChatLogTable :: Storage.Storage :> es => Eff es ()
ensureChatLogTable =
  runSelda (tryCreateTable chatLogRows)

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
persistRecord record = do
  ensureChatLogTable
  runSelda (insert_ chatLogRows [chatLogRow (sanitizeChatLogEntry (chatLogEntry record))])
    `catch` \(err :: SomeException) ->
      logInfo_ [i|Failed to persist chat log entry: #{show err :: String}|]

queryStored :: Storage.Storage :> es => IncomingMessage -> Int -> Bool -> Eff es [ChatLogEntry]
queryStored message limitCount includeBotMessages = do
  ensureChatLogTable
  rows <- runSelda $
    query $
      queryLimit 0 (max 0 limitCount) do
        row <- select chatLogRows
        restrict (chatLogMatches message includeBotMessages row)
        order (row ! #id) descending
        pure row
  pure (map (chatLogEntryFromRow message) (reverse rows))

chatLogRow :: ChatLogEntry -> ChatLogRow
chatLogRow entry =
  ChatLogRow
    { id = def
    , platform_key = platformKey entry.platform
    , kind_key = kindKey entry.kind
    , chat_id = fromIntegral <$> entry.chatId
    , sender_id = entry.senderId
    , sender_username = entry.senderUsername
    , message_id = fromIntegral <$> entry.messageId
    , reply_to_message_id = fromIntegral <$> entry.replyToMessageId
    , is_bot = entry.isBot
    , mentions = encodeIntegerList entry.mentions
    , mention_usernames = encodeTextList entry.mentionUsernames
    , image_urls = encodeTextList entry.imageUrls
    , body_text = entry.text
    }

chatLogEntryFromRow :: IncomingMessage -> ChatLogRow -> ChatLogEntry
chatLogEntryFromRow context row =
  ChatLogEntry
    { platform = context.platform
    , kind = context.kind
    , chatId = fromIntegral <$> row.chat_id
    , senderId = row.sender_id
    , senderUsername = row.sender_username
    , messageId = fromIntegral <$> row.message_id
    , replyToMessageId = fromIntegral <$> row.reply_to_message_id
    , isBot = row.is_bot
    , mentions = decodeIntegerList row.mentions
    , mentionUsernames = decodeTextList row.mention_usernames
    , imageUrls = decodeTextList row.image_urls
    , text = row.body_text
    }

chatLogMatches :: forall (backend :: Type). IncomingMessage -> Bool -> Row backend ChatLogRow -> Col backend Bool
chatLogMatches message includeBotMessages row =
  row ! #platform_key .== literal (platformKey message.platform)
    .&& row ! #kind_key .== literal (kindKey message.kind)
    .&& chatIdMatches message.chatId row
    .&& botVisibilityMatches includeBotMessages row

chatIdMatches :: forall (backend :: Type). Maybe Integer -> Row backend ChatLogRow -> Col backend Bool
chatIdMatches Nothing row =
  isNull (row ! #chat_id)
chatIdMatches (Just chatId) row =
  row ! #chat_id .== literal (Just (fromIntegral chatId :: Int.Int64))

botVisibilityMatches :: forall (backend :: Type). Bool -> Row backend ChatLogRow -> Col backend Bool
botVisibilityMatches True _ =
  true
botVisibilityMatches False row =
  row ! #is_bot .== literal False

platformKey :: ChatPlatform -> Text
platformKey =
  show

kindKey :: ChatKind -> Text
kindKey =
  show

encodeIntegerList :: [Integer] -> Text
encodeIntegerList =
  Text.intercalate "," . map show

decodeIntegerList :: Text -> [Integer]
decodeIntegerList value
  | Text.null value = []
  | otherwise = mapMaybe (readMaybe . toString) (Text.splitOn "," value)

encodeTextList :: [Text] -> Text
encodeTextList =
  Text.intercalate "\n"

decodeTextList :: Text -> [Text]
decodeTextList value
  | Text.null value = []
  | otherwise = Text.splitOn "\n" value

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
