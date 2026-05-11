{-|
Module      : Bot.Effect.ChatLog
Description : In-memory chat log
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Effect.ChatLog
  ( ChatLog
  , ChatLogEntry (..)
  , recordMessage
  , recordBotMessage
  , queryChat
  , runChatLog
  )
where

import Bot.Message
import Bot.Prelude
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Storage.SQLite as Storage
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

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

-- | Record a bot reply in the same chat as its triggering message.
recordBotMessage :: ChatLog :> es => IncomingMessage -> Maybe Integer -> Text -> Eff es ()
recordBotMessage context messageId body =
  send (RecordBotMessage context messageId body)

-- | Query recent messages from the current chat in chronological order.
queryChat :: ChatLog :> es => IncomingMessage -> Int -> Bool -> Eff es [ChatLogEntry]
queryChat message limit includeBotMessages =
  send (QueryChat message limit includeBotMessages)

-- | Interpret chat logging in memory, optionally backed by SQLite.
runChatLog
  :: (IOE :> es, Log :> es)
  => Maybe Storage.SQLiteStore
  -> Eff (ChatLog : es) a
  -> Eff es a
runChatLog sqliteStore inner = do
  memoryRef <- case sqliteStore of
    Nothing ->
      Just <$> liftIO (IORef.newIORef [] :: IO (IORef.IORef [ChatLogRecord]))
    Just _ ->
      pure Nothing
  interpret
    (\_ -> \case
      RecordMessage message -> do
        let record = userRecord message
        traverse_ (appendMemoryRecord record) memoryRef
        persistRecord sqliteStore record
      RecordBotMessage context messageId body -> do
        let record = botRecord context messageId body
        traverse_ (appendMemoryRecord record) memoryRef
        persistRecord sqliteStore record
      QueryChat message limit includeBotMessages -> do
        case sqliteStore of
          Just store -> liftIO (queryStored store message limit includeBotMessages)
          Nothing -> do
            records <- maybe (pure []) (liftIO . IORef.readIORef) memoryRef
            pure
              $ map (sanitizeChatLogEntry . chatLogEntry)
              $ reverse
              $ take (max 0 limit)
              $ filter (visible includeBotMessages)
              $ filter (sameChat message) records)
    inner

appendMemoryRecord :: IOE :> es => ChatLogRecord -> IORef.IORef [ChatLogRecord] -> Eff es ()
appendMemoryRecord record ref =
  liftIO $ IORef.modifyIORef' ref (take maxInMemoryChatLogRecords . (record :))

maxInMemoryChatLogRecords :: Int
maxInMemoryChatLogRecords =
  5000

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

sameChat :: IncomingMessage -> ChatLogRecord -> Bool
sameChat left right =
  left.platform == right.message.platform &&
    left.kind == right.message.kind &&
    left.chatId == right.message.chatId

visible :: Bool -> ChatLogRecord -> Bool
visible includeBotMessages record =
  includeBotMessages || not record.isBot

persistRecord :: (IOE :> es, Log :> es) => Maybe Storage.SQLiteStore -> ChatLogRecord -> Eff es ()
persistRecord Nothing _ =
  pure ()
persistRecord (Just store) record =
  liftIO
    ( Storage.saveChatLogEntry
        store
        (platformKey record.message)
        (kindKey record.message)
        record.message.chatId
        record.isBot
        (Aeson.toJSON (sanitizeChatLogEntry (chatLogEntry record)))
    )
    `catch` \(err :: SomeException) ->
      logInfo "Failed to persist chat log entry" (show err :: String)

queryStored :: Storage.SQLiteStore -> IncomingMessage -> Int -> Bool -> IO [ChatLogEntry]
queryStored store message limit includeBotMessages = do
  values <- Storage.queryChatLogEntries store (platformKey message) (kindKey message) message.chatId includeBotMessages limit
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
