{-|
Module      : Bot.Effect.AgentAudit
Description : Agent tool audit log
Stability   : experimental
-}
{-# LANGUAGE RecordWildCards #-}

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

import qualified Bot.Agent.Types as Agent
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Storage.SQLite as Storage
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import Data.Time (diffUTCTime, getCurrentTime)

data AgentAudit :: Effect where
  RecordEvent :: AgentAuditEvent -> AgentAudit m ()
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

recordEvent :: AgentAudit :> es => AgentAuditEvent -> Eff es ()
recordEvent event =
  send (RecordEvent event)

agentAuditObserver :: AgentAudit :> es => Agent.AgentObserver es
agentAuditObserver =
  Agent.AgentObserver{Agent.observe = recordAgentEvent}

recordAgentEvent :: AgentAudit :> es => Agent.AgentEvent -> Eff es ()
recordAgentEvent event =
  traverse_ recordEvent (agentAuditEvent event)

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
  :: (IOE :> es, Log :> es)
  => Maybe Storage.SQLiteStore
  -> Eff (AgentAudit : es) a
  -> Eff es a
runAgentAudit sqliteStore inner = do
  memoryRef <- case sqliteStore of
    Nothing ->
      Just <$> liftIO (IORef.newIORef [] :: IO (IORef.IORef [AgentAuditRecord]))
    Just _ ->
      pure Nothing
  interpret
    (\_ -> \case
      RecordEvent event -> do
        occurredAt <- liftIO getCurrentTime
        traverse_ (appendMemoryEvent occurredAt event) memoryRef
        persistEvent sqliteStore occurredAt event
      QueryRecentToolUses limit ->
        toolUsesFromRecords limit <$> loadAuditRecords sqliteStore memoryRef
      QueryToolUse auditId ->
        find ((== auditId) . (.auditId)) . toolUsesFromRecords maxInMemoryAgentAuditEvents <$> loadAuditRecords sqliteStore memoryRef
      QueryConversationAudit messageId ->
        case sqliteStore of
          Just store -> liftIO (queryStoredConversationAudit store messageId)
          Nothing -> conversationAudit messageId <$> loadAuditRecords sqliteStore memoryRef
      QueryConversationMessagesAudit messageIds ->
        case sqliteStore of
          Just store -> liftIO (queryStoredConversationMessagesAudit store messageIds)
          Nothing -> conversationMessagesAudit messageIds <$> loadAuditRecords sqliteStore memoryRef
    )
    inner

appendMemoryEvent :: IOE :> es => UTCTime -> AgentAuditEvent -> IORef.IORef [AgentAuditRecord] -> Eff es ()
appendMemoryEvent occurredAt event ref =
  liftIO $ IORef.modifyIORef' ref \records ->
    let nextId = maybe 1 ((+ 1) . fromMaybe 0 . (.id)) (viaNonEmpty last records)
        record = AgentAuditRecord{occurredAt, event, id = Just nextId}
    in take maxInMemoryAgentAuditEvents (records <> [record])

maxInMemoryAgentAuditEvents :: Int
maxInMemoryAgentAuditEvents =
  5000

persistEvent :: (IOE :> es, Log :> es) => Maybe Storage.SQLiteStore -> UTCTime -> AgentAuditEvent -> Eff es ()
persistEvent Nothing _ _ =
  pure ()
persistEvent (Just store) occurredAt event =
  liftIO (Storage.saveAgentAuditEvent store (eventRunId event) occurredAt maybeLinkedMessageId maybeParentMessageId (Aeson.toJSON event))
    `catch` \(err :: SomeException) ->
      logInfo_ [i|Failed to persist agent audit event: #{show err :: String}|]
  where
    (maybeLinkedMessageId, maybeParentMessageId) =
      case event of
        AgentConversationLinked{linkedMessageId = eventLinkedMessageId, parentMessageId = eventParentMessageId} ->
          (Just eventLinkedMessageId, eventParentMessageId)
        _ ->
          (Nothing, Nothing)

loadAuditRecords :: IOE :> es => Maybe Storage.SQLiteStore -> Maybe (IORef.IORef [AgentAuditRecord]) -> Eff es [AgentAuditRecord]
loadAuditRecords sqliteStore memoryRef =
  case sqliteStore of
    Just store -> liftIO (queryStoredRecent store maxInMemoryAgentAuditEvents)
    Nothing -> maybe (pure []) (liftIO . IORef.readIORef) memoryRef

queryStoredRecent :: Storage.SQLiteStore -> Int -> IO [AgentAuditRecord]
queryStoredRecent store limit =
  map storedAuditRecord <$> Storage.queryRecentAgentAuditEvents store limit

queryStoredConversationAudit :: Storage.SQLiteStore -> Integer -> IO [AgentAuditRecord]
queryStoredConversationAudit store messageId =
  map storedAuditRecord <$> Storage.queryAgentAuditEventsForMessage store messageId

queryStoredConversationMessagesAudit :: Storage.SQLiteStore -> [Integer] -> IO [AgentAuditRecord]
queryStoredConversationMessagesAudit store messageIds =
  map storedAuditRecord <$> Storage.queryAgentAuditEventsForMessages store messageIds

storedAuditRecord :: Storage.AgentAuditRow AgentAuditEvent -> AgentAuditRecord
storedAuditRecord row =
  AgentAuditRecord
    { id = Just row.id
    , occurredAt = row.occurredAt
    , event = row.event
    }

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

conversationAudit :: Integer -> [AgentAuditRecord] -> [AgentAuditRecord]
conversationAudit messageId records =
  filter ((`elem` linkedRunIds) . eventRunId . (.event)) records
  where
    linkedRunIds =
      ordNub
        [ runId
        | AgentAuditRecord{event = AgentConversationLinked{runId, linkedMessageId, parentMessageId}} <- records
        , linkedMessageId == messageId || parentMessageId == Just messageId
        ]

conversationMessagesAudit :: [Integer] -> [AgentAuditRecord] -> [AgentAuditRecord]
conversationMessagesAudit messageIds records =
  filter ((`elem` linkedRunIds) . eventRunId . (.event)) records
  where
    messageIdSet =
      ordNub messageIds
    linkedRunIds =
      ordNub
        [ runId
        | AgentAuditRecord{event = AgentConversationLinked{runId, linkedMessageId, parentMessageId}} <- records
        , linkedMessageId `elem` messageIdSet || maybe False (`elem` messageIdSet) parentMessageId
        ]

eventRunId :: AgentAuditEvent -> Text
eventRunId = \case
  ToolCallStarted{runId} -> runId
  ToolCallFinished{runId} -> runId
  AgentRunInterrupted{runId} -> runId
  AgentConversationLinked{runId} -> runId
