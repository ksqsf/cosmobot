{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.AgentAudit.Storage
Description : Persistent agent audit records
Stability   : experimental
-}

module Bot.AgentAudit.Storage
  ( ensureAgentAuditTable
  , loadStoredAuditRecords
  , persistEvent
  , queryStoredRecent
  , queryStoredRecord
  , queryStoredThreadAudit
  , queryStoredThreadMessagesAudit
  )
where

import Bot.AgentAudit.Projection
import Bot.AgentAudit.Types
import Bot.Core.Message
import Bot.Core.Thread
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
  table "audit_log"
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
      logError [i|Failed to persist agent audit event: #{show err :: String}|] $> Nothing
  where
    (maybeLinkedMessageId, maybeParentMessageId) =
      case event of
        AgentThreadLinked{linkedMessageId = eventLinkedMessageId, parentMessageId = eventParentMessageId} ->
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

queryStoredThreadAudit :: Storage.Storage :> es => ThreadMessageKey -> Eff es [AgentAuditRecord]
queryStoredThreadAudit messageKey =
  queryStoredThreadMessagesAudit [messageKey]

queryStoredThreadMessagesAudit :: Storage.Storage :> es => [ThreadMessageKey] -> Eff es [AgentAuditRecord]
queryStoredThreadMessagesAudit [] =
  pure []
queryStoredThreadMessagesAudit messageKeys = do
  linkedRuns <- linkedRunOccurrences messageKeys
  ordNubOn (.id) . concat <$> traverse (uncurry queryStoredRunOccurrence) linkedRuns

ensureAgentAuditTable :: Storage.Storage :> es => Eff es ()
ensureAgentAuditTable =
  runSelda (tryCreateTable agentAuditRows)

queryStoredRunOccurrence :: Storage.Storage :> es => Text -> ID AgentAuditRow -> Eff es [AgentAuditRecord]
queryStoredRunOccurrence runId linkedAuditId = do
  rows <- runSelda $
    query do
      row <- select agentAuditRows
      restrict (row ! #run_id .== literal runId)
      restrict (row ! #id .<= literal linkedAuditId)
      order (row ! #id) ascending
      pure row
  pure $ case reverse rows of
    [] -> []
    linked : earlier ->
      mapMaybe storedAuditRecord (reverse (linked : takeWhile (isNothing . (.linked_message_id)) earlier))

linkedRunOccurrences :: Storage.Storage :> es => [ThreadMessageKey] -> Eff es [(Text, ID AgentAuditRow)]
linkedRunOccurrences messageKeys = do
  rows <- runSelda $
    query do
      row <- select agentAuditRows
      restrict (row ! #linked_message_id `isIn` ids .|| row ! #parent_message_id `isIn` ids)
      order (row ! #id) ascending
      pure row
  pure
    [ (eventRunId record.event, row.id)
    | row <- rows
    , Just record <- [storedAuditRecord row]
    , matchesConversation messageKeys record.event
    ]
  where
    ids =
      map (literal . Just . messageIdText . (.messageId)) (ordNub messageKeys)

matchesConversation :: [ThreadMessageKey] -> AgentAuditEvent -> Bool
matchesConversation messageKeys = \case
  AgentThreadLinked{linkedMessageId, linkedMessageKey = Just linkedKey, parentMessageId} ->
    any (matches linkedKey linkedMessageId parentMessageId) messageKeys
  _ -> False
  where
    matches linkedKey linkedMessageId parentMessageId messageKey =
      sameConversation linkedKey messageKey
        && messageKey.messageId `elem` (linkedMessageId : maybeToList parentMessageId)

    sameConversation left right =
      left.platform == right.platform && left.chatId == right.chatId

storedAuditRecord :: AgentAuditRow -> Maybe AgentAuditRecord
storedAuditRecord row = do
  event <- decodeJsonText row.event_json
  pure AgentAuditRecord
    { id = fromIntegral (fromId row.id)
    , occurredAt = row.occurred_at
    , event = event
    }

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

decodeJsonText :: Aeson.FromJSON a => Text -> Maybe a
decodeJsonText =
  either (const Nothing) Just . Aeson.eitherDecodeStrict' . TextEncoding.encodeUtf8
