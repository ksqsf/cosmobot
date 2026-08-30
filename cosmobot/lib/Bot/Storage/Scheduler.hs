{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.Storage.Scheduler
Description : Persistent scheduler queue
Stability   : experimental
-}

module Bot.Storage.Scheduler
  ( StoredScheduledMessage (..)
  , loadScheduledMessages
  , createScheduledMessage
  , deleteScheduledMessage
  , rescheduleScheduledMessages
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Text.Encoding as TextEncoding

data StoredScheduledMessage = StoredScheduledMessage
  { scheduleId :: !Integer
  -- | Absolute due time as Unix epoch seconds.
  --
  -- This is deliberately not a local timestamp string, so persisted schedules
  -- survive timezone and daylight-saving changes without reinterpretation.
  , dueAtUnixSeconds :: !Integer
  , recurringIntervalSeconds :: !(Maybe Int)
  , message :: !IncomingMessage
  }
  deriving (Show)

data ScheduledMessageRow = ScheduledMessageRow
  { id :: RowID
  , due_at_unix_seconds :: Int.Int64
  , recurring_interval_seconds :: Maybe Int.Int64
  , platform_key :: Text
  , chat_id :: Maybe Text
  , sender_id :: Maybe Text
  , sender_username :: Maybe Text
  , message_json :: Text
  }
  deriving (Generic)

instance SqlRow ScheduledMessageRow

scheduledMessages :: Table ScheduledMessageRow
scheduledMessages =
  table "scheduled_messages"
    [ #id :- untypedAutoPrimary
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
      order (row ! #id) ascending
      pure row
  pure (mapMaybe storedScheduledMessageFromRow rows)

createScheduledMessage :: Storage.Storage :> es => Integer -> Maybe Int -> IncomingMessage -> Eff es StoredScheduledMessage
createScheduledMessage dueAtUnixSeconds recurringIntervalSeconds message = do
  ensureScheduledMessagesTable
  scheduleId <- fromIntegral . fromId <$> runSelda
    ( insertWithPK
      scheduledMessages
      [ ScheduledMessageRow
          { id = def
          , due_at_unix_seconds = fromIntegral dueAtUnixSeconds
          , recurring_interval_seconds = fromIntegral <$> recurringIntervalSeconds
          , platform_key = chatPlatformKey message.platform
          , chat_id = chatIdText <$> message.chatId
          , sender_id = message.senderId
          , sender_username = message.senderUsername
          , message_json = encodeMessage message
          }
      ]
    )
  pure StoredScheduledMessage{scheduleId, dueAtUnixSeconds, recurringIntervalSeconds, message}

rescheduleScheduledMessages :: Storage.Storage :> es => [(Integer, Integer)] -> Eff es ()
rescheduleScheduledMessages schedules = do
  ensureScheduledMessagesTable
  runSelda . transaction $
    for_ schedules \(scheduleId, dueAtUnixSeconds) ->
      update_ scheduledMessages
        (\row -> row ! #id .== literal (toRowId (fromIntegral scheduleId :: Int.Int64)))
        (\row -> row `with` [#due_at_unix_seconds := literal (fromIntegral dueAtUnixSeconds)])

deleteScheduledMessage :: Storage.Storage :> es => Integer -> Eff es ()
deleteScheduledMessage scheduleId = do
  ensureScheduledMessagesTable
  runSelda $
    deleteFrom_ scheduledMessages \row ->
      row ! #id .== literal (toRowId (fromIntegral scheduleId :: Int.Int64))

ensureScheduledMessagesTable :: Storage.Storage :> es => Eff es ()
ensureScheduledMessagesTable =
  runSelda (tryCreateTable scheduledMessages)

storedScheduledMessageFromRow :: ScheduledMessageRow -> Maybe StoredScheduledMessage
storedScheduledMessageFromRow row = do
  message <- decodeMessage row.message_json
  pure StoredScheduledMessage
    { scheduleId = fromIntegral (fromRowId row.id)
    , dueAtUnixSeconds = fromIntegral row.due_at_unix_seconds
    , recurringIntervalSeconds = fromIntegral <$> row.recurring_interval_seconds
    , message
    }

encodeMessage :: IncomingMessage -> Text
encodeMessage =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

decodeMessage :: Text -> Maybe IncomingMessage
decodeMessage =
  either (const Nothing) Just . Aeson.eitherDecodeStrict' . TextEncoding.encodeUtf8
