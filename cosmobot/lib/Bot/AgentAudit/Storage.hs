{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.AgentAudit.Storage
Description : Persistent agent audit records
Stability   : experimental
-}

module Bot.AgentAudit.Storage
  ( ensureAgentAuditTable
  , persistEvent
  , queryStoredRecent
  , queryStoredRecord
  , queryStoredRecentToolUses
  , queryStoredToolUse
  , queryStoredRunAudit
  , queryStoredThreadAudit
  , queryStoredThreadMessagesAudit
  )
where

import Bot.AgentAudit.Types
import Bot.Core.Message
import Bot.Core.Thread
import Bot.Prelude
import qualified Bot.Effect.Storage as Storage
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Database.Selda.Migrations as SeldaMigrations
import qualified Database.Selda.SQLite as SeldaSQLite

data LegacyAgentAuditRow = LegacyAgentAuditRow
  { legacy_id :: RowID
  , legacy_run_id :: Text
  , legacy_occurred_at :: UTCTime
  , legacy_linked_message_id :: Maybe Text
  , legacy_parent_message_id :: Maybe Text
  , legacy_event_json :: Text
  }
  deriving (Generic)

instance SqlRow LegacyAgentAuditRow

legacyAgentAuditRows :: Table LegacyAgentAuditRow
legacyAgentAuditRows =
  tableFieldMod "audit_log"
    [ #legacy_id :- untypedAutoPrimary
    , #legacy_run_id :- index
    , #legacy_linked_message_id :- index
    , #legacy_parent_message_id :- index
    ]
    (fromMaybe "" . Text.stripPrefix "legacy_")

data AgentAuditRow = AgentAuditRow
  { id :: RowID
  , run_id :: Text
  , occurred_at :: UTCTime
  , linked_message_id :: Maybe Text
  , parent_message_id :: Maybe Text
  , event_json :: Text
  , event_kind :: Text
  , tool_call_id :: Maybe Text
  }
  deriving (Generic)

instance SqlRow AgentAuditRow

agentAuditRows :: Table AgentAuditRow
agentAuditRows =
  table "audit_log"
    [ #id :- untypedAutoPrimary
    , #run_id :- index
    , #linked_message_id :- index
    , #parent_message_id :- index
    , #event_kind :- index
    , #tool_call_id :- index
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
            , event_kind = auditEventKind event
            , tool_call_id = auditEventToolCallId event
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
        restrict (row ! #id .== literal (toRowId (fromIntegral auditId :: Int.Int64)))
        pure row
  pure (viaNonEmpty head (mapMaybe storedAuditRecord rows))

queryStoredRecentToolUses :: Storage.Storage :> es => Int -> Eff es [AgentAuditRecord]
queryStoredRecentToolUses limit = do
  starts <- queryStoredRecentToolStarts limit
  related <- queryStoredRelatedToolEvents starts
  pure (starts <> related)

queryStoredToolUse :: Storage.Storage :> es => Integer -> Eff es [AgentAuditRecord]
queryStoredToolUse auditId = do
  start <- queryStoredRecord auditId
  case start of
    Just record@AgentAuditRecord{event = ToolCallStarted{}} ->
      (record :) <$> queryStoredRelatedToolEvents [record]
    _ ->
      pure []

queryStoredRunAudit :: Storage.Storage :> es => Text -> Eff es [AgentAuditRecord]
queryStoredRunAudit runId = do
  rows <- runSelda $
    query do
      row <- select agentAuditRows
      restrict (row ! #run_id .== literal runId)
      order (row ! #id) ascending
      pure row
  pure (mapMaybe storedAuditRecord rows)

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
  runSelda do
    tryCreateTable legacyAgentAuditRows
    SeldaMigrations.autoMigrate True
      [ [ SeldaMigrations.Migration
            legacyAgentAuditRows
            agentAuditRows
            (pure . migrateLegacyAgentAuditRow)
        ]
      ]
    transaction backfillAuditIndexColumns

queryStoredRecentToolStarts :: Storage.Storage :> es => Int -> Eff es [AgentAuditRecord]
queryStoredRecentToolStarts limit = do
  rows <- runSelda $
    query $
      queryLimit 0 (max 0 limit) do
        row <- select agentAuditRows
        restrict (row ! #event_kind .== literal toolCallStartedKind)
        order (row ! #id) descending
        pure row
  pure (mapMaybe storedAuditRecord (reverse rows))

queryStoredRelatedToolEvents :: Storage.Storage :> es => [AgentAuditRecord] -> Eff es [AgentAuditRecord]
queryStoredRelatedToolEvents [] =
  pure []
queryStoredRelatedToolEvents starts = do
  rows <- runSelda $
    query do
      row <- select agentAuditRows
      restrict $
        ( row ! #event_kind .== literal toolCallFinishedKind
            .&& row ! #tool_call_id `isIn` toolCallIds
            .&& row ! #run_id `isIn` runIds
        )
          .|| ( row ! #event_kind .== literal agentRunInterruptedKind
                  .&& row ! #run_id `isIn` runIds
              )
      pure row
  pure (mapMaybe storedAuditRecord rows)
  where
    toolCallIds =
      [ literal (Just toolCall.id)
      | AgentAuditRecord{event = ToolCallStarted{toolCall}} <- starts
      ]
    runIds =
      [ literal runId
      | AgentAuditRecord{event = ToolCallStarted{runId}} <- starts
      ]

migrateLegacyAgentAuditRow :: Row (s :: Type) LegacyAgentAuditRow -> Row s AgentAuditRow
migrateLegacyAgentAuditRow legacy =
  new
    [ #id := legacy ! #legacy_id
    , #run_id := legacy ! #legacy_run_id
    , #occurred_at := legacy ! #legacy_occurred_at
    , #linked_message_id := legacy ! #legacy_linked_message_id
    , #parent_message_id := legacy ! #legacy_parent_message_id
    , #event_json := legacy ! #legacy_event_json
    , #event_kind := literal ""
    , #tool_call_id := literal Nothing
    ]

backfillAuditIndexColumns :: SeldaT SeldaSQLite.SQLite IO ()
backfillAuditIndexColumns = do
  rows <- query do
    row <- select agentAuditRows
    restrict (row ! #event_kind .== literal "")
    pure row
  for_ rows \row ->
    for_ (decodeJsonText row.event_json) \event ->
      update_
        agentAuditRows
        (\candidate -> candidate ! #id .== literal row.id)
        (\candidate -> candidate `with`
          [ #event_kind := literal (auditEventKind event)
          , #tool_call_id := literal (auditEventToolCallId event)
          ])

toolCallStartedKind :: Text
toolCallStartedKind =
  "tool_call_started"

toolCallFinishedKind :: Text
toolCallFinishedKind =
  "tool_call_finished"

agentRunInterruptedKind :: Text
agentRunInterruptedKind =
  "agent_run_interrupted"

auditEventKind :: AgentAuditEvent -> Text
auditEventKind = \case
  AgentRunStarted{} -> "agent_run_started"
  ModelTurnStarted{} -> "model_turn_started"
  ModelTurnFinished{} -> "model_turn_finished"
  ContextCompacted{} -> "context_compacted"
  SubAgentRunStarted{} -> "subagent_run_started"
  ToolCallStarted{} -> toolCallStartedKind
  ToolCallFinished{} -> toolCallFinishedKind
  AgentRunFinished{} -> "agent_run_finished"
  AgentRunInterrupted{} -> agentRunInterruptedKind
  AgentThreadLinked{} -> "agent_thread_linked"

auditEventToolCallId :: AgentAuditEvent -> Maybe Text
auditEventToolCallId = \case
  ToolCallStarted{toolCall} -> Just toolCall.id
  ToolCallFinished{toolCallId} -> Just toolCallId
  _ -> Nothing

queryStoredRunOccurrence :: Storage.Storage :> es => Text -> RowID -> Eff es [AgentAuditRecord]
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

linkedRunOccurrences :: Storage.Storage :> es => [ThreadMessageKey] -> Eff es [(Text, RowID)]
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
    { id = fromIntegral (fromRowId row.id)
    , occurredAt = row.occurred_at
    , event = event
    }

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

decodeJsonText :: Aeson.FromJSON a => Text -> Maybe a
decodeJsonText =
  either (const Nothing) Just . Aeson.eitherDecodeStrict' . TextEncoding.encodeUtf8
