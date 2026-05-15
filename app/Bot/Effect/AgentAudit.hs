{-|
Module      : Bot.Effect.AgentAudit
Description : Agent tool audit log
Stability   : experimental
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}

module Bot.Effect.AgentAudit
  ( AgentAudit
  , AgentAuditEvent (..)
  , AgentAuditRecord (..)
  , ToolCallTrace (..)
  , ToolUseDetail (..)
  , ToolUseStatus (..)
  , agentAuditObserver
  , toolUsesFromAuditRecords
  , queryRecentToolUses
  , queryToolUse
  , queryConversationAudit
  , queryConversationMessagesAudit
  , runAgentAudit
  )
where

import qualified Bot.Agent.Middleware.Observation.Types as Observation
import qualified Bot.Agent.Types as Agent
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Effect.Storage as Storage
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TextEncoding
import Data.Time (diffUTCTime, getCurrentTime)

data AgentAudit :: Effect where
  RecordEvent :: AgentAuditEvent -> AgentAudit m (Maybe Integer)
  QueryRecentToolUses :: Int -> AgentAudit m [ToolUseDetail]
  QueryToolUse :: Integer -> AgentAudit m (Maybe ToolUseDetail)
  QueryConversationAudit :: Integer -> AgentAudit m [AgentAuditRecord]
  QueryConversationMessagesAudit :: [Integer] -> AgentAudit m [AgentAuditRecord]

type instance DispatchOf AgentAudit = Dynamic

data ToolCallTrace = ToolCallTrace
  { id :: !Text
  , name :: !Text
  , arguments :: !Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data AgentAuditEvent
  = ToolCallStarted
      { runId :: !Text
      , turn :: !Int
      , toolCall :: !ToolCallTrace
      }
  | ToolCallFinished
      { runId :: !Text
      , turn :: !Int
      , toolCallId :: !Text
      , toolName :: !Text
      , status :: !Text
      , result :: !Text
      , resultLength :: !Int
      , messageIds :: ![Maybe Integer]
      }
  | AgentRunInterrupted
      { runId :: !Text
      , reason :: !Text
      }
  | AgentConversationLinked
      { runId :: !Text
      , linkedMessageId :: !Integer
      , parentMessageId :: !(Maybe Integer)
      }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data AgentAuditRecord = AgentAuditRecord
  { id :: !(Maybe Integer)
  , occurredAt :: !UTCTime
  , event :: !AgentAuditEvent
  }
  deriving (Eq, Show)

data ToolUseStatus
  = ToolUseInProgress
  | ToolUseFinished
      { status :: !Text
      , durationMilliseconds :: !Integer
      }
  | ToolUseInterrupted
      { reason :: !Text
      , durationMilliseconds :: !Integer
      }
  deriving (Eq, Show)

data ToolUseDetail = ToolUseDetail
  { auditId :: !Integer
  , occurredAt :: !UTCTime
  , finishedAt :: !(Maybe UTCTime)
  , runId :: !Text
  , turn :: !Int
  , toolName :: !Text
  , toolCallId :: !Text
  , arguments :: !Text
  , status :: !ToolUseStatus
  , result :: !(Maybe Text)
  , messageIds :: ![Maybe Integer]
  }
  deriving (Eq, Show)

data AgentAuditRow = AgentAuditRow
  { id :: ID AgentAuditRow
  , run_id :: Text
  , occurred_at :: UTCTime
  , linked_message_id :: Maybe Int.Int64
  , parent_message_id :: Maybe Int.Int64
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

recordEvent :: AgentAudit :> es => AgentAuditEvent -> Eff es (Maybe Integer)
recordEvent event =
  send (RecordEvent event)

agentAuditObserver :: AgentAudit :> es => Agent.AgentObserver Observation.ObservationContext es
agentAuditObserver =
  Agent.AgentObserver{Agent.observe = recordAgentEvent}

recordAgentEvent :: AgentAudit :> es => Agent.AgentEvent -> Eff es Observation.ObservationContext
recordAgentEvent event =
  observationContext <$> maybe (pure Nothing) recordEvent (agentAuditEvent event)

agentAuditEvent :: Agent.AgentEvent -> Maybe AgentAuditEvent
agentAuditEvent = \case
  Agent.ToolCallStarted{runId, turn, toolCall} ->
    Just ToolCallStarted
      { runId
      , turn
      , toolCall = toolCallTrace toolCall
      }
  Agent.ToolCallFinished{runId, turn, toolCallId, toolName, status, result, resultLength, messageIds} ->
    Just ToolCallFinished{runId, turn, toolCallId, toolName, status, result, resultLength, messageIds}
  Agent.AgentRunInterrupted{runId, reason} ->
    Just AgentRunInterrupted{runId, reason}
  Agent.AgentConversationLinked{runId, linkedMessageId, parentMessageId} ->
    Just AgentConversationLinked{runId, linkedMessageId, parentMessageId}
  Agent.AgentRunStarted{} ->
    Nothing
  Agent.ModelTurnStarted{} ->
    Nothing
  Agent.ModelTurnFinished{} ->
    Nothing
  Agent.AgentRunFinished{} ->
    Nothing

toolCallTrace :: LLM.ToolCall -> ToolCallTrace
toolCallTrace call =
  ToolCallTrace
    { id = call.id
    , name = call.name
    , arguments = call.arguments
    }

queryRecentToolUses :: AgentAudit :> es => Int -> Eff es [ToolUseDetail]
queryRecentToolUses limit =
  send (QueryRecentToolUses limit)

queryToolUse :: AgentAudit :> es => Integer -> Eff es (Maybe ToolUseDetail)
queryToolUse auditId =
  send (QueryToolUse auditId)

queryConversationAudit :: AgentAudit :> es => Integer -> Eff es [AgentAuditRecord]
queryConversationAudit messageId =
  send (QueryConversationAudit messageId)

queryConversationMessagesAudit :: AgentAudit :> es => [Integer] -> Eff es [AgentAuditRecord]
queryConversationMessagesAudit messageIds =
  send (QueryConversationMessagesAudit messageIds)

runAgentAudit
  :: (IOE :> es, Log :> es, Storage.Storage :> es)
  => Eff (AgentAudit : es) a
  -> Eff es a
runAgentAudit inner = do
  processStartedAt <- liftIO getCurrentTime
  ensureAgentAuditTable
  interpret
    (\_ -> \case
      RecordEvent event -> do
        occurredAt <- liftIO getCurrentTime
        persistEvent occurredAt event
      QueryRecentToolUses limit -> do
        records <- loadStoredAuditRecords
        pure (toolUsesFromRecords limit (markStaleRunningToolUses processStartedAt records))
      QueryToolUse auditId -> do
        records <- loadStoredAuditRecords
        pure (find ((== auditId) . (.auditId)) (toolUsesFromRecords maxInMemoryAgentAuditEvents (markStaleRunningToolUses processStartedAt records)))
      QueryConversationAudit messageId ->
        markStaleRunningToolUses processStartedAt <$> queryStoredConversationAudit messageId
      QueryConversationMessagesAudit messageIds ->
        markStaleRunningToolUses processStartedAt <$> queryStoredConversationMessagesAudit messageIds
    )
    inner

maxInMemoryAgentAuditEvents :: Int
maxInMemoryAgentAuditEvents =
  5000

persistEvent :: (IOE :> es, Log :> es, Storage.Storage :> es) => UTCTime -> AgentAuditEvent -> Eff es (Maybe Integer)
persistEvent occurredAt event =
  (Just . fromIntegral . fromId <$> runSelda
    ( insertWithPK
        agentAuditRows
        [ AgentAuditRow
            { id = def
            , run_id = eventRunId event
            , occurred_at = occurredAt
            , linked_message_id = fromIntegral <$> maybeLinkedMessageId
            , parent_message_id = fromIntegral <$> maybeParentMessageId
            , event_json = jsonText event
            }
        ]
    ))
    `catch` \(err :: SomeException) ->
      logInfo_ [i|Failed to persist agent audit event: #{show err :: String}|] $> Nothing
  where
    (maybeLinkedMessageId, maybeParentMessageId) =
      case event of
        AgentConversationLinked{linkedMessageId = eventLinkedMessageId, parentMessageId = eventParentMessageId} ->
          (Just eventLinkedMessageId, eventParentMessageId)
        _ ->
          (Nothing, Nothing)

observationContext :: Maybe Integer -> Observation.ObservationContext
observationContext auditId =
  Observation.ObservationContext{Observation.auditToolUseId = auditId}

markStaleRunningToolUses :: UTCTime -> [AgentAuditRecord] -> [AgentAuditRecord]
markStaleRunningToolUses processStartedAt records =
  let restartedRunIds =
        ordNub
          [ toolUse.runId
          | toolUse <- toolUsesFromAuditRecords records
          , toolUse.status == ToolUseInProgress
          , toolUse.occurredAt < processStartedAt
          ]
      restartedRecords =
        [ AgentAuditRecord
            { id = Nothing
            , occurredAt = processStartedAt
            , event = AgentRunInterrupted{runId, reason = "restarted"}
            }
        | runId <- restartedRunIds
        ]
  in records <> restartedRecords

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

queryStoredConversationAudit :: Storage.Storage :> es => Integer -> Eff es [AgentAuditRecord]
queryStoredConversationAudit messageId =
  queryStoredConversationMessagesAudit [messageId]

queryStoredConversationMessagesAudit :: Storage.Storage :> es => [Integer] -> Eff es [AgentAuditRecord]
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

linkedRunIds :: Storage.Storage :> es => [Integer] -> Eff es [Text]
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
      map (literal . Just . toInt64) (ordNub messageIds)

    toInt64 :: Integer -> Int.Int64
    toInt64 =
      fromIntegral

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

toolUsesFromRecords :: Int -> [AgentAuditRecord] -> [ToolUseDetail]
toolUsesFromRecords limit records =
  takeLast (max 0 limit) (toolUsesFromAuditRecords records)

toolUsesFromAuditRecords :: [AgentAuditRecord] -> [ToolUseDetail]
toolUsesFromAuditRecords records =
  mapMaybe (toolUseDetail finishes interruptions) (filter isToolStart records)
  where
    finishes =
      Map.fromList
        [ ((event.runId, event.toolCallId), record)
        | record@AgentAuditRecord{event = event@ToolCallFinished{}} <- records
        ]

    interruptions =
      Map.fromList
        [ (event.runId, record)
        | record@AgentAuditRecord{event = event@AgentRunInterrupted{}} <- records
        ]

    toolUseDetail finishesByCall interruptionsByRun AgentAuditRecord{id = Just auditId, occurredAt, event = ToolCallStarted{runId, turn, toolCall}} =
      let finished = Map.lookup (runId, toolCall.id) finishesByCall
          interrupted = Map.lookup runId interruptionsByRun
      in Just ToolUseDetail
        { auditId = auditId
        , occurredAt = occurredAt
        , finishedAt = (.occurredAt) <$> (finished <|> interrupted)
        , runId = runId
        , turn = turn
        , toolName = toolCall.name
        , toolCallId = toolCall.id
        , arguments = toolCall.arguments
        , status = toolUseStatus occurredAt finished interrupted
        , result = finished >>= finishedResult
        , messageIds = maybe [] finishedMessageIds finished
        }
    toolUseDetail _ _ _ =
      Nothing

    finishedStatus startedAt AgentAuditRecord{occurredAt = finishedAt, event = ToolCallFinished{status}} =
      ToolUseFinished
        { status = status
        , durationMilliseconds = floor (diffUTCTime finishedAt startedAt * 1000)
        }
    finishedStatus _ _ =
      ToolUseInProgress

    interruptedStatus startedAt AgentAuditRecord{occurredAt = interruptedAt, event = AgentRunInterrupted{reason}} =
      ToolUseInterrupted
        { reason = reason
        , durationMilliseconds = floor (diffUTCTime interruptedAt startedAt * 1000)
        }
    interruptedStatus _ _ =
      ToolUseInProgress

    toolUseStatus startedAt (Just finished) _ =
      finishedStatus startedAt finished
    toolUseStatus startedAt Nothing (Just interrupted) =
      interruptedStatus startedAt interrupted
    toolUseStatus _ Nothing Nothing =
      ToolUseInProgress

    finishedResult AgentAuditRecord{event = ToolCallFinished{result}} =
      Just result
    finishedResult _ =
      Nothing

    finishedMessageIds AgentAuditRecord{event = ToolCallFinished{messageIds}} =
      messageIds
    finishedMessageIds _ =
      []

    isToolStart AgentAuditRecord{event = ToolCallStarted{}} =
      True
    isToolStart _ =
      False

takeLast :: Int -> [a] -> [a]
takeLast n values =
  drop (max 0 (length values - n)) values

eventRunId :: AgentAuditEvent -> Text
eventRunId = \case
  ToolCallStarted{runId} -> runId
  ToolCallFinished{runId} -> runId
  AgentRunInterrupted{runId} -> runId
  AgentConversationLinked{runId} -> runId
