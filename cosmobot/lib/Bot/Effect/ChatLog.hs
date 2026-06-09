{-|
Module      : Bot.Effect.ChatLog
Description : Chat log capability facade
Stability   : experimental
-}

module Bot.Effect.ChatLog
  ( ChatLog
  , ChatLogEntry (..)
  , recordMessage
  , recordSelfMessage
  , recordIncomingMessages
  , queryChat
  , queryCurrentSenderChatLog
  , runChatLog
  )
where

import Bot.ChatLog.Record
import Bot.ChatLog.Types
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Storage.ChatLog as ChatLogStorage
import qualified Effectful.Timeout as Timeout
import qualified Streaming.Prelude as S

chatLogRecordTimeoutMicroseconds :: Int
chatLogRecordTimeoutMicroseconds =
  1_000_000

-- | Append-only chat log used by agent tools for local context.
data ChatLog :: Effect where
  RecordMessage
    :: IncomingMessage
    -> ChatLog m ()
  RecordSelfMessage
    :: IncomingMessage
    -> Text
    -> ChatLog m ()
  QueryChat
    :: IncomingMessage
    -> Int
    -> Bool
    -> ChatLog m [ChatLogEntry]
  QueryCurrentSenderChatLog
    :: IncomingMessage
    -> [[Text]]
    -> Int
    -> ChatLog m [ChatLogEntry]

type instance DispatchOf ChatLog = Dynamic

-- | Record a user/platform message.
recordMessage :: ChatLog :> es => IncomingMessage -> Eff es ()
recordMessage message =
  send (RecordMessage message)

-- | Record every incoming message passing through a stream.
recordIncomingMessages
  :: (ChatLog :> es, KatipE :> es, Timeout.Timeout :> es)
  => Stream (Of IncomingMessage) (Eff es) ()
  -> Stream (Of IncomingMessage) (Eff es) ()
recordIncomingMessages =
  S.mapM \message -> do
    Timeout.timeout chatLogRecordTimeoutMicroseconds (recordMessage message) >>= \case
      Just () ->
        pure ()
      Nothing ->
        logWarning [i|chat log record timed out; continuing route dispatch: #{incomingMessageLogLine message}|]
    pure message

-- | Record a logical self reply in the same chat as its triggering message.
recordSelfMessage :: ChatLog :> es => IncomingMessage -> Text -> Eff es ()
recordSelfMessage context body =
  send (RecordSelfMessage context body)

-- | Query recent messages from the current chat in chronological order.
queryChat :: ChatLog :> es => IncomingMessage -> Int -> Bool -> Eff es [ChatLogEntry]
queryChat message limit includeBotMessages =
  send (QueryChat message limit includeBotMessages)

-- | Query current sender's messages in the current chat, newest first.
queryCurrentSenderChatLog :: ChatLog :> es => IncomingMessage -> [[Text]] -> Int -> Eff es [ChatLogEntry]
queryCurrentSenderChatLog message keywords limit =
  send (QueryCurrentSenderChatLog message keywords limit)

-- | Interpret chat logging through the storage capability.
runChatLog
  :: (IOE :> es, KatipE :> es, Storage.Storage :> es)
  => Eff (ChatLog : es) a
  -> Eff es a
runChatLog inner =
  interpret
    (\_ -> \case
      RecordMessage message ->
        ChatLogStorage.persistRecord (userRecord message)
      RecordSelfMessage context body ->
        ChatLogStorage.persistRecord (selfRecord context body)
      QueryChat message limit includeBotMessages ->
        ChatLogStorage.queryStored message limit includeBotMessages
      QueryCurrentSenderChatLog message keywords limit ->
        ChatLogStorage.queryCurrentSenderStored message keywords limit
    )
    inner
