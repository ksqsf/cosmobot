{-|
Module      : Bot.Storage.ChatLog
Description : Persistent chat log storage
Stability   : experimental
-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Storage.ChatLog
  ( persistRecord
  , queryStored
  , queryCurrentSenderStored
  )
where

import Bot.ChatLog.Record
import Bot.ChatLog.Types
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Effect.Storage as Storage
import Bot.Storage.Prelude
import qualified Data.Int as Int
import qualified Data.Text as Text

data ChatLogRow = ChatLogRow
  { id :: ID ChatLogRow
  , platform_key :: Text
  , kind_key :: Text
  , chat_id :: Maybe Int.Int64
  , sender_id :: Maybe Text
  , sender_username :: Maybe Text
  , message_id :: Maybe Text
  , reply_to_message_id :: Maybe Text
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
    , #sender_id :- index
    ]

ensureChatLogTable :: Storage.Storage :> es => Eff es ()
ensureChatLogTable =
  runSelda (tryCreateTable chatLogRows)

persistRecord :: (IOE :> es, Log :> es, Storage.Storage :> es) => ChatLogRecord -> Eff es ()
persistRecord record = do
  ensureChatLogTable
  runSelda (insert_ chatLogRows [chatLogRow (sanitizeChatLogEntry (chatLogEntry record))])
    `catchSync` \err ->
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

queryCurrentSenderStored :: Storage.Storage :> es => IncomingMessage -> [[Text]] -> Int -> Eff es [ChatLogEntry]
queryCurrentSenderStored message keywords limitCount = do
  ensureChatLogTable
  case (message.chatId, message.senderId, keywordLikePatterns keywords) of
    (Just _, Just _, patterns@(_ : _)) -> do
      rows <- runSelda $
        query $
          queryLimit 0 (boundedChatLogLimit limitCount) do
            row <- select chatLogRows
            restrict (currentSenderChatLogMatches message patterns row)
            order (row ! #id) descending
            pure row
      pure (map (chatLogEntryFromRow message) rows)
    _ ->
      pure []

boundedChatLogLimit :: Int -> Int
boundedChatLogLimit =
  min 100 . max 0

chatLogRow :: ChatLogEntry -> ChatLogRow
chatLogRow entry =
  ChatLogRow
    { id = def
    , platform_key = platformKey entry.platform
    , kind_key = kindKey entry.kind
    , chat_id = fromIntegral <$> entry.chatId
    , sender_id = entry.senderId
    , sender_username = entry.senderUsername
    , message_id = messageIdText <$> entry.messageId
    , reply_to_message_id = messageIdText <$> entry.replyToMessageId
    , is_bot = entry.isBot
    , mentions = encodeTextList entry.mentions
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
    , messageId = textMessageId <$> row.message_id
    , replyToMessageId = textMessageId <$> row.reply_to_message_id
    , isBot = row.is_bot
    , mentions = decodeTextList row.mentions
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

currentSenderChatLogMatches :: forall (backend :: Type). IncomingMessage -> [Text] -> Row backend ChatLogRow -> Col backend Bool
currentSenderChatLogMatches message patterns row =
  chatLogMatches message False row
    .&& senderIdMatches message.senderId row
    .&& keywordPatternsMatch patterns row

senderIdMatches :: forall (backend :: Type). Maybe Text -> Row backend ChatLogRow -> Col backend Bool
senderIdMatches Nothing _ =
  false
senderIdMatches (Just senderId) row =
  row ! #sender_id .== literal (Just senderId)

keywordPatternsMatch :: forall (backend :: Type). [Text] -> Row backend ChatLogRow -> Col backend Bool
keywordPatternsMatch [] _ =
  false
keywordPatternsMatch (pattern : rest) row =
  foldl' (.||) (row ! #body_text `like` literal pattern)
    [ row ! #body_text `like` literal nextPattern
    | nextPattern <- rest
    ]

keywordLikePatterns :: [[Text]] -> [Text]
keywordLikePatterns =
  map \keyword -> "%" <> Text.intercalate "%" keyword <> "%"

platformKey :: ChatPlatform -> Text
platformKey =
  show

kindKey :: ChatKind -> Text
kindKey =
  show

encodeTextList :: [Text] -> Text
encodeTextList =
  Text.intercalate "\n"

decodeTextList :: Text -> [Text]
decodeTextList value
  | Text.null value = []
  | otherwise = Text.splitOn "\n" value
