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
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Database.Selda.Migrations as SeldaMigrations
import qualified Database.Selda.Unsafe as SeldaUnsafe

data LegacyScheduledMessageRow = LegacyScheduledMessageRow
  { legacy_id :: RowID
  , legacy_schedule_id :: Int.Int64
  , legacy_due_at_unix_seconds :: Int.Int64
  , legacy_platform_key :: Text
  , legacy_chat_id :: Maybe Int.Int64
  , legacy_sender_id :: Maybe Text
  , legacy_sender_username :: Maybe Text
  , legacy_message_json :: Text
  }
  deriving (Generic)

instance SqlRow LegacyScheduledMessageRow

legacyScheduledMessages :: Table LegacyScheduledMessageRow
legacyScheduledMessages =
  tableFieldMod "scheduled_messages"
    [ #legacy_id :- untypedAutoPrimary
    , #legacy_schedule_id :- unique
    , #legacy_due_at_unix_seconds :- index
    , #legacy_platform_key :- index
    , #legacy_chat_id :- index
    , #legacy_sender_id :- index
    , #legacy_sender_username :- index
    ]
    (fromMaybe "" . Text.stripPrefix "legacy_")

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
  { id :: RowID
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

createScheduledMessage :: Storage.Storage :> es => Integer -> IncomingMessage -> Eff es StoredScheduledMessage
createScheduledMessage dueAtUnixSeconds message = do
  ensureScheduledMessagesTable
  scheduleId <- fromIntegral . fromId <$> runSelda
    ( insertWithPK
      scheduledMessages
      [ ScheduledMessageRow
          { id = def
          , due_at_unix_seconds = fromIntegral dueAtUnixSeconds
          , platform_key = chatPlatformKey message.platform
          , chat_id = fromIntegral <$> message.chatId
          , sender_id = message.senderId
          , sender_username = message.senderUsername
          , message_json = encodeMessage message
          }
      ]
    )
  pure StoredScheduledMessage{scheduleId, dueAtUnixSeconds, message}

deleteScheduledMessage :: Storage.Storage :> es => Integer -> Eff es ()
deleteScheduledMessage scheduleId = do
  ensureScheduledMessagesTable
  runSelda $
    deleteFrom_ scheduledMessages \row ->
      row ! #id .== literal (toRowId (fromIntegral scheduleId :: Int.Int64))

ensureScheduledMessagesTable :: Storage.Storage :> es => Eff es ()
ensureScheduledMessagesTable =
  runSelda do
    tryCreateTable legacyScheduledMessages
    SeldaMigrations.autoMigrate True
      [ [ SeldaMigrations.Migration
            legacyScheduledMessages
            scheduledMessages
            (pure . migrateLegacyScheduledMessageRow)
        ]
      ]

migrateLegacyScheduledMessageRow :: Row (s :: Type) LegacyScheduledMessageRow -> Row s ScheduledMessageRow
migrateLegacyScheduledMessageRow legacy =
  new
    [ #id := SeldaUnsafe.cast (legacy ! #legacy_schedule_id)
    , #due_at_unix_seconds := legacy ! #legacy_due_at_unix_seconds
    , #platform_key := legacy ! #legacy_platform_key
    , #chat_id := legacy ! #legacy_chat_id
    , #sender_id := legacy ! #legacy_sender_id
    , #sender_username := legacy ! #legacy_sender_username
    , #message_json := legacy ! #legacy_message_json
    ]

storedScheduledMessageFromRow :: ScheduledMessageRow -> Maybe StoredScheduledMessage
storedScheduledMessageFromRow row = do
  message <- decodeMessage row.message_json
  pure StoredScheduledMessage
    { scheduleId = fromIntegral (fromRowId row.id)
    , dueAtUnixSeconds = fromIntegral row.due_at_unix_seconds
    , message
    }

encodeMessage :: IncomingMessage -> Text
encodeMessage =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

decodeMessage :: Text -> Maybe IncomingMessage
decodeMessage =
  either (const Nothing) Just . Aeson.eitherDecodeStrict' . TextEncoding.encodeUtf8
