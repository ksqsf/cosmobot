{-|
Module      : Bot.Effect.ChatLog
Description : Chat log capability facade
Stability   : experimental
-}

module Bot.Effect.ChatLog
  ( ChatLog
  , ChatLogEntry (..)
  , ChatLogScope (..)
  , ChatLogSummary (..)
  , ChatLogItem (..)
  , ChatLogWindow (..)
  , ChatLogWindowAnchor (..)
  , SenderChatLogScope (..)
  , ChatLogTimeRange (..)
  , unboundedChatLogTimeRange
  , recordMessage
  , recordSelfMessage
  , recordSelfMessageWithFiles
  , recordIncomingMessages
  , queryChat
  , queryCurrentSenderChatLog
  , listChats
  , queryWindow
  , findLegacyReplyAnchor
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
    -> Maybe MessageId
    -> Text
    -> [MessageFile]
    -> ChatLog m ()
  QueryChat
    :: IncomingMessage
    -> Maybe Text
    -> Int
    -> Bool
    -> ChatLogTimeRange
    -> ChatLog m [ChatLogEntry]
  QueryCurrentSenderChatLog
    :: IncomingMessage
    -> SenderChatLogScope
    -> [[Text]]
    -> Int
    -> ChatLogTimeRange
    -> ChatLog m [ChatLogEntry]
  ListChats :: ChatLog m [ChatLogSummary]
  QueryWindow :: ChatLogScope -> ChatLogWindowAnchor -> Int -> ChatLog m ChatLogWindow
  FindLegacyReplyAnchor :: ChatLogScope -> Text -> ChatLog m (Maybe MessageId)

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
    when (message.eventKind == IncomingMessageCreated) $
      Timeout.timeout chatLogRecordTimeoutMicroseconds (recordMessage message) >>= \case
        Just () ->
          pure ()
        Nothing ->
          $(logWarning) [i|chat log record timed out; continuing route dispatch: #{incomingMessageLog message}|]
    pure message

-- | Record a logical self reply in the same chat as its triggering message.
recordSelfMessage :: ChatLog :> es => IncomingMessage -> Maybe MessageId -> Text -> Eff es ()
recordSelfMessage context messageId body =
  recordSelfMessageWithFiles context messageId body []

recordSelfMessageWithFiles :: ChatLog :> es => IncomingMessage -> Maybe MessageId -> Text -> [MessageFile] -> Eff es ()
recordSelfMessageWithFiles context messageId body files =
  send (RecordSelfMessage context messageId body files)

-- | Query recent messages from the current chat in chronological order.
queryChat :: ChatLog :> es => IncomingMessage -> Maybe Text -> Int -> Bool -> ChatLogTimeRange -> Eff es [ChatLogEntry]
queryChat message sender limit includeBotMessages timeRange =
  send (QueryChat message sender limit includeBotMessages timeRange)

-- | Query current sender's messages in the requested scope, newest first.
queryCurrentSenderChatLog :: ChatLog :> es => IncomingMessage -> SenderChatLogScope -> [[Text]] -> Int -> ChatLogTimeRange -> Eff es [ChatLogEntry]
queryCurrentSenderChatLog message scope keywords limit timeRange =
  send (QueryCurrentSenderChatLog message scope keywords limit timeRange)

listChats :: ChatLog :> es => Eff es [ChatLogSummary]
listChats =
  send ListChats

queryWindow :: ChatLog :> es => ChatLogScope -> ChatLogWindowAnchor -> Int -> Eff es ChatLogWindow
queryWindow scope anchor limit =
  send (QueryWindow scope anchor limit)

findLegacyReplyAnchor :: ChatLog :> es => ChatLogScope -> Text -> Eff es (Maybe MessageId)
findLegacyReplyAnchor scope body =
  send (FindLegacyReplyAnchor scope body)

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
      RecordSelfMessage context messageId body files ->
        ChatLogStorage.persistRecord (selfRecordWithFiles context messageId body files)
      QueryChat message sender limit includeBotMessages timeRange ->
        ChatLogStorage.queryStored message sender limit includeBotMessages timeRange
      QueryCurrentSenderChatLog message scope keywords limit timeRange ->
        ChatLogStorage.queryCurrentSenderStored message scope keywords limit timeRange
      ListChats ->
        ChatLogStorage.listStoredChats
      QueryWindow scope anchor limit ->
        ChatLogStorage.queryStoredWindow scope anchor limit
      FindLegacyReplyAnchor scope body ->
        ChatLogStorage.findLegacyReplyAnchor scope body
    )
    inner
