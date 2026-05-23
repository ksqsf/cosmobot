{-|
Module      : Bot.AgentAudit.Storage
Description : Persistent agent audit records
Stability   : experimental
-}
{-# LANGUAGE OverloadedLabels #-}

module Bot.AgentAudit.Storage
  ( ensureAgentAuditTable
  , loadStoredAuditRecords
  , persistEvent
  , queryStoredRecent
  , queryStoredRecord
  , queryStoredConversationAudit
  , queryStoredConversationMessagesAudit
  )
where

import Bot.AgentAudit.Projection
import Bot.AgentAudit.Types
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Effect.Storage as Storage
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Text.Encoding as TextEncoding

data AgentAuditRow = AgentAuditRow
  { id :: ID AgentAuditRow
  , run_id :: Text
  , occurred_at :: UTCTime
  , linked_message_id :: Maybe Text
  , parent_message_id :: Maybe Text
  , event_json :: Text
  }
  deriving (Generic)

instance SqlRow AgentAuditRow

agentAuditRows :: Table AgentAuditRow
agentAuditRows =
  table "agent_trace"
    [ #id :- autoPrimary
    , #run_id :- index
    , #linked_message_id :- index
    , #parent_message_id :- index
    ]

persistEvent :: (IOE :> es, KatipE :> es, Storage.Storage :> es) => UTCTime -> AgentAuditEvent -> Eff es (Maybe Integer)
persistEvent occurredAt event = do
  (Just . fromIntegral . fromId <$> runSelda
    ( insertWithPK
        agentAuditRows
        [ AgentAuditRow
            { id = def
            , run_id = eventRunId event
            , occurred_at = occurredAt
            , linked_message_id = messageIdText <$> maybeLinkedMessageId
            , parent_message_id = messageIdText <$> maybeParentMessageId
            , event_json = jsonText event
            }
        ]
    ))
    `catchSync` \err ->
      logInfo [i|Failed to persist agent audit event: #{show err :: String}|] $> Nothing
  where
    (maybeLinkedMessageId, maybeParentMessageId) =
      case event of
        AgentConversationLinked{linkedMessageId = eventLinkedMessageId, parentMessageId = eventParentMessageId} ->
          (Just eventLinkedMessageId, eventParentMessageId)
        _ ->
          (Nothing, Nothing)

loadStoredAuditRecords :: Storage.Storage :> es => Eff es [AgentAuditRecord]
loadStoredAuditRecords =
  queryStoredRecent maxInMemoryAgentAuditEvents

queryStoredRecent :: Storage.Storage :> es => Int -> Eff es [AgentAuditRecord]
queryStoredRecent limit = do
  rows <- runSelda $
    query $
      queryLimit 0 (max 0 limit) do
        row <- select agentAuditRows
        order (row ! #id) descending
        pure row
  pure (mapMaybe storedAuditRecord (reverse rows))

queryStoredRecord :: Storage.Storage :> es => Integer -> Eff es (Maybe AgentAuditRecord)
queryStoredRecord auditId = do
  rows <- runSelda $
    query $
      queryLimit 0 1 do
        row <- select agentAuditRows
        restrict (row ! #id .== literal (toId (fromIntegral auditId :: Int.Int64)))
        pure row
  pure (viaNonEmpty head (mapMaybe storedAuditRecord rows))

queryStoredConversationAudit :: Storage.Storage :> es => MessageId -> Eff es [AgentAuditRecord]
queryStoredConversationAudit messageId =
  queryStoredConversationMessagesAudit [messageId]

queryStoredConversationMessagesAudit :: Storage.Storage :> es => [MessageId] -> Eff es [AgentAuditRecord]
queryStoredConversationMessagesAudit [] =
  pure []
queryStoredConversationMessagesAudit messageIds = do
  runIds <- linkedRunIds messageIds
  concat <$> traverse queryStoredRun runIds

ensureAgentAuditTable :: Storage.Storage :> es => Eff es ()
ensureAgentAuditTable =
  runSelda (tryCreateTable agentAuditRows)

queryStoredRun :: Storage.Storage :> es => Text -> Eff es [AgentAuditRecord]
queryStoredRun runId = do
  rows <- runSelda $
    query do
      row <- select agentAuditRows
      restrict (row ! #run_id .== literal runId)
      order (row ! #id) ascending
      pure row
  pure (mapMaybe storedAuditRecord rows)

linkedRunIds :: Storage.Storage :> es => [MessageId] -> Eff es [Text]
linkedRunIds messageIds = do
  rows <- runSelda $
    query do
      row <- select agentAuditRows
      restrict (row ! #linked_message_id `isIn` ids .|| row ! #parent_message_id `isIn` ids)
      order (row ! #run_id) ascending
      pure (row ! #run_id)
  pure (ordNub rows)
  where
    ids =
      map (literal . Just . messageIdText) (ordNub messageIds)

storedAuditRecord :: AgentAuditRow -> Maybe AgentAuditRecord
storedAuditRecord row = do
  event <- decodeJsonText row.event_json
  pure AgentAuditRecord
    { id = Just (fromIntegral (fromId row.id))
    , occurredAt = row.occurred_at
    , event = event
    }

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

decodeJsonText :: Aeson.FromJSON a => Text -> Maybe a
decodeJsonText =
  either (const Nothing) Just . Aeson.eitherDecodeStrict' . TextEncoding.encodeUtf8
