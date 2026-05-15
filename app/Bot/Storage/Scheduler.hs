{-|
Module      : Bot.Storage.Scheduler
Description : Persistent scheduler queue
Stability   : experimental
-}
{-# LANGUAGE OverloadedLabels #-}

module Bot.Storage.Scheduler
  ( StoredScheduledMessage (..)
  , loadScheduledMessages
  , loadNextScheduleId
  , saveScheduledMessage
  , deleteScheduledMessage
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.Int as Int
import qualified Data.Text.Encoding as TextEncoding

data StoredScheduledMessage = StoredScheduledMessage
  { scheduleId :: !Integer
  -- | Absolute due time as Unix epoch seconds.
  --
  -- This is deliberately not a local timestamp string, so persisted schedules
  -- survive timezone and daylight-saving changes without reinterpretation.
  , dueAtUnixSeconds :: !Integer
  , message :: !IncomingMessage
  }
  deriving (Show)

data ScheduledMessageRow = ScheduledMessageRow
  { id :: ID ScheduledMessageRow
  , schedule_id :: Int.Int64
  , due_at_unix_seconds :: Int.Int64
  , platform_key :: Text
  , chat_id :: Maybe Int.Int64
  , sender_id :: Maybe Text
  , sender_username :: Maybe Text
  , message_json :: Text
  }
  deriving (Generic)

instance SqlRow ScheduledMessageRow

scheduledMessages :: Table ScheduledMessageRow
scheduledMessages =
  table "scheduled_messages"
    [ #id :- autoPrimary
    , #schedule_id :- unique
    , #due_at_unix_seconds :- index
    , #platform_key :- index
    , #chat_id :- index
    , #sender_id :- index
    , #sender_username :- index
    ]

loadScheduledMessages :: Storage.Storage :> es => Eff es [StoredScheduledMessage]
loadScheduledMessages = do
  ensureScheduledMessagesTable
  rows <- runSelda $
    query do
      row <- select scheduledMessages
      order (row ! #due_at_unix_seconds) ascending
      order (row ! #schedule_id) ascending
      pure row
  pure (mapMaybe storedScheduledMessageFromRow rows)

loadNextScheduleId :: Storage.Storage :> es => Eff es Integer
loadNextScheduleId = do
  rows <- loadScheduledMessages
  pure (Foldable.maximum (1 : [row.scheduleId + 1 | row <- rows]))

saveScheduledMessage :: Storage.Storage :> es => StoredScheduledMessage -> Eff es ()
saveScheduledMessage scheduled = do
  ensureScheduledMessagesTable
  runSelda do
    deleteFrom_ scheduledMessages \row ->
      row ! #schedule_id .== literal (fromIntegral scheduled.scheduleId :: Int.Int64)
    insert_
      scheduledMessages
      [ ScheduledMessageRow
          { id = def
          , schedule_id = fromIntegral scheduled.scheduleId
          , due_at_unix_seconds = fromIntegral scheduled.dueAtUnixSeconds
          , platform_key = chatPlatformKey scheduled.message.platform
          , chat_id = fromIntegral <$> scheduled.message.chatId
          , sender_id = scheduled.message.senderId
          , sender_username = scheduled.message.senderUsername
          , message_json = encodeMessage scheduled.message
          }
      ]

deleteScheduledMessage :: Storage.Storage :> es => Integer -> Eff es ()
deleteScheduledMessage scheduleId = do
  ensureScheduledMessagesTable
  runSelda $
    deleteFrom_ scheduledMessages \row ->
      row ! #schedule_id .== literal (fromIntegral scheduleId :: Int.Int64)

ensureScheduledMessagesTable :: Storage.Storage :> es => Eff es ()
ensureScheduledMessagesTable =
  runSelda (tryCreateTable scheduledMessages)

storedScheduledMessageFromRow :: ScheduledMessageRow -> Maybe StoredScheduledMessage
storedScheduledMessageFromRow row = do
  message <- decodeMessage row.message_json
  pure StoredScheduledMessage
    { scheduleId = fromIntegral row.schedule_id
    , dueAtUnixSeconds = fromIntegral row.due_at_unix_seconds
    , message
    }

encodeMessage :: IncomingMessage -> Text
encodeMessage =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

decodeMessage :: Text -> Maybe IncomingMessage
decodeMessage =
  either (const Nothing) Just . Aeson.eitherDecodeStrict' . TextEncoding.encodeUtf8
