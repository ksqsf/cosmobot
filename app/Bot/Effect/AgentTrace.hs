{-|
Module      : Bot.Effect.AgentTrace
Description : Agent trace event log
Stability   : experimental
-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Effect.AgentTrace
  ( AgentTrace
  , AgentTraceEvent (..)
  , AgentTraceRecord (..)
  , ToolCallTrace (..)
  , ToolUseDetail (..)
  , ToolUseStatus (..)
  , toolUsesFromTraceRecords
  , recordEvent
  , queryRun
  , queryAll
  , queryRecentToolUses
  , queryToolUse
  , queryConversationTrace
  , queryConversationMessagesTrace
  , runAgentTrace
  )
where

import Bot.Prelude
import qualified Bot.Storage.SQLite as Storage
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import Data.Time (diffUTCTime, getCurrentTime)

data AgentTrace :: Effect where
  RecordEvent :: AgentTraceEvent -> AgentTrace m ()
  QueryRun :: Text -> AgentTrace m [AgentTraceEvent]
  QueryAll :: AgentTrace m [AgentTraceEvent]
  QueryRecentToolUses :: Int -> AgentTrace m [ToolUseDetail]
  QueryToolUse :: Integer -> AgentTrace m (Maybe ToolUseDetail)
  QueryConversationTrace :: Integer -> AgentTrace m [AgentTraceRecord]
  QueryConversationMessagesTrace :: [Integer] -> AgentTrace m [AgentTraceRecord]

type instance DispatchOf AgentTrace = Dynamic

data ToolCallTrace = ToolCallTrace
  { id :: !Text
  , name :: !Text
  , arguments :: !Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data AgentTraceEvent
  = AgentRunStarted
      { runId :: !Text
      , messageId :: !(Maybe Integer)
      , maxTurns :: !Int
      , exposedTools :: ![Text]
      }
  | ModelTurnStarted
      { runId :: !Text
      , turn :: !Int
      , messageCount :: !Int
      , exposedTools :: ![Text]
      }
  | ModelTurnFinished
      { runId :: !Text
      , turn :: !Int
      , answerKind :: !Text
      , contentLength :: !Int
      , toolCalls :: ![ToolCallTrace]
      }
  | ToolCallStarted
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
  | AgentRunFinished
      { runId :: !Text
      , status :: !Text
      , finalLength :: !Int
      , turnsUsed :: !Int
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

data AgentTraceRecord = AgentTraceRecord
  { id :: !(Maybe Integer)
  , occurredAt :: !UTCTime
  , event :: !AgentTraceEvent
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

recordEvent :: AgentTrace :> es => AgentTraceEvent -> Eff es ()
recordEvent event =
  send (RecordEvent event)

queryRun :: AgentTrace :> es => Text -> Eff es [AgentTraceEvent]
queryRun runId =
  send (QueryRun runId)

queryAll :: AgentTrace :> es => Eff es [AgentTraceEvent]
queryAll =
  send QueryAll

queryRecentToolUses :: AgentTrace :> es => Int -> Eff es [ToolUseDetail]
queryRecentToolUses limit =
  send (QueryRecentToolUses limit)

queryToolUse :: AgentTrace :> es => Integer -> Eff es (Maybe ToolUseDetail)
queryToolUse auditId =
  send (QueryToolUse auditId)

queryConversationTrace :: AgentTrace :> es => Integer -> Eff es [AgentTraceRecord]
queryConversationTrace messageId =
  send (QueryConversationTrace messageId)

queryConversationMessagesTrace :: AgentTrace :> es => [Integer] -> Eff es [AgentTraceRecord]
queryConversationMessagesTrace messageIds =
  send (QueryConversationMessagesTrace messageIds)

runAgentTrace
  :: (IOE :> es, Log :> es)
  => Maybe Storage.SQLiteStore
  -> Eff (AgentTrace : es) a
  -> Eff es a
runAgentTrace sqliteStore inner = do
  memoryRef <- case sqliteStore of
    Nothing ->
      Just <$> liftIO (IORef.newIORef [] :: IO (IORef.IORef [AgentTraceRecord]))
    Just _ ->
      pure Nothing
  interpret
    (\_ -> \case
      RecordEvent event -> do
        occurredAt <- liftIO getCurrentTime
        traverse_ (appendMemoryEvent occurredAt event) memoryRef
        persistEvent sqliteStore occurredAt event
      QueryRun targetRunId ->
        case sqliteStore of
          Just store -> map (.event) <$> liftIO (queryStoredRun store targetRunId)
          Nothing -> do
            events <- maybe (pure []) (liftIO . IORef.readIORef) memoryRef
            pure (map (.event) (filter ((== targetRunId) . eventRunId . (.event)) events))
      QueryAll ->
        case sqliteStore of
          Just store -> map (.event) <$> liftIO (queryStoredRecent store maxInMemoryAgentTraceEvents)
          Nothing -> map (.event) <$> maybe (pure []) (liftIO . IORef.readIORef) memoryRef
      QueryRecentToolUses limit ->
        toolUsesFromRecords limit <$> loadTraceRecords sqliteStore memoryRef
      QueryToolUse auditId ->
        find ((== auditId) . (.auditId)) . toolUsesFromRecords maxInMemoryAgentTraceEvents <$> loadTraceRecords sqliteStore memoryRef
      QueryConversationTrace messageId ->
        case sqliteStore of
          Just store -> liftIO (queryStoredConversationTrace store messageId)
          Nothing -> conversationTrace messageId <$> loadTraceRecords sqliteStore memoryRef
      QueryConversationMessagesTrace messageIds ->
        case sqliteStore of
          Just store -> liftIO (queryStoredConversationMessagesTrace store messageIds)
          Nothing -> conversationMessagesTrace messageIds <$> loadTraceRecords sqliteStore memoryRef
    )
    inner

appendMemoryEvent :: IOE :> es => UTCTime -> AgentTraceEvent -> IORef.IORef [AgentTraceRecord] -> Eff es ()
appendMemoryEvent occurredAt event ref =
  liftIO $ IORef.modifyIORef' ref \records ->
    let nextId = maybe 1 ((+ 1) . fromMaybe 0 . (.id)) (viaNonEmpty last records)
        record = AgentTraceRecord{occurredAt, event, id = Just nextId}
    in take maxInMemoryAgentTraceEvents (records <> [record])

maxInMemoryAgentTraceEvents :: Int
maxInMemoryAgentTraceEvents =
  5000

persistEvent :: (IOE :> es, Log :> es) => Maybe Storage.SQLiteStore -> UTCTime -> AgentTraceEvent -> Eff es ()
persistEvent Nothing _ _ =
  pure ()
persistEvent (Just store) occurredAt event =
  liftIO (Storage.saveAgentTraceEvent store (eventRunId event) occurredAt maybeLinkedMessageId maybeParentMessageId (Aeson.toJSON event))
    `catch` \(err :: SomeException) ->
      logInfo "Failed to persist agent trace event" (show err :: String)
  where
    (maybeLinkedMessageId, maybeParentMessageId) =
      case event of
        AgentConversationLinked{linkedMessageId = eventLinkedMessageId, parentMessageId = eventParentMessageId} ->
          (Just eventLinkedMessageId, eventParentMessageId)
        _ ->
          (Nothing, Nothing)

loadTraceRecords :: IOE :> es => Maybe Storage.SQLiteStore -> Maybe (IORef.IORef [AgentTraceRecord]) -> Eff es [AgentTraceRecord]
loadTraceRecords sqliteStore memoryRef =
  case sqliteStore of
    Just store -> liftIO (queryStoredRecent store maxInMemoryAgentTraceEvents)
    Nothing -> maybe (pure []) (liftIO . IORef.readIORef) memoryRef

queryStoredRun :: Storage.SQLiteStore -> Text -> IO [AgentTraceRecord]
queryStoredRun store runId =
  map storedTraceRecord <$> Storage.queryAgentTraceEvents store runId

queryStoredRecent :: Storage.SQLiteStore -> Int -> IO [AgentTraceRecord]
queryStoredRecent store limit =
  map storedTraceRecord <$> Storage.queryRecentAgentTraceEvents store limit

queryStoredConversationTrace :: Storage.SQLiteStore -> Integer -> IO [AgentTraceRecord]
queryStoredConversationTrace store messageId =
  map storedTraceRecord <$> Storage.queryAgentTraceEventsForMessage store messageId

queryStoredConversationMessagesTrace :: Storage.SQLiteStore -> [Integer] -> IO [AgentTraceRecord]
queryStoredConversationMessagesTrace store messageIds =
  map storedTraceRecord <$> Storage.queryAgentTraceEventsForMessages store messageIds

storedTraceRecord :: Storage.AgentTraceRow AgentTraceEvent -> AgentTraceRecord
storedTraceRecord row =
  AgentTraceRecord
    { id = Just row.id
    , occurredAt = row.occurredAt
    , event = row.event
    }

toolUsesFromRecords :: Int -> [AgentTraceRecord] -> [ToolUseDetail]
toolUsesFromRecords limit records =
  takeLast (max 0 limit) (toolUsesFromTraceRecords records)

toolUsesFromTraceRecords :: [AgentTraceRecord] -> [ToolUseDetail]
toolUsesFromTraceRecords records =
  mapMaybe (toolUseDetail finishes interruptions) (filter isToolStart records)
  where
    finishes =
      Map.fromList
        [ ((event.runId, event.toolCallId), record)
        | record@AgentTraceRecord{event = event@ToolCallFinished{}} <- records
        ]

    interruptions =
      Map.fromList
        [ (event.runId, record)
        | record@AgentTraceRecord{event = event@AgentRunInterrupted{}} <- records
        ]

    toolUseDetail finishesByCall interruptionsByRun AgentTraceRecord{id = Just auditId, occurredAt, event = ToolCallStarted{runId, turn, toolCall}} =
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

    finishedStatus startedAt AgentTraceRecord{occurredAt = finishedAt, event = ToolCallFinished{status}} =
      ToolUseFinished
        { status = status
        , durationMilliseconds = floor (diffUTCTime finishedAt startedAt * 1000)
        }
    finishedStatus _ _ =
      ToolUseInProgress

    interruptedStatus startedAt AgentTraceRecord{occurredAt = interruptedAt, event = AgentRunInterrupted{reason}} =
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

    finishedResult AgentTraceRecord{event = ToolCallFinished{result}} =
      Just result
    finishedResult _ =
      Nothing

    finishedMessageIds AgentTraceRecord{event = ToolCallFinished{messageIds}} =
      messageIds
    finishedMessageIds _ =
      []

    isToolStart AgentTraceRecord{event = ToolCallStarted{}} =
      True
    isToolStart _ =
      False

takeLast :: Int -> [a] -> [a]
takeLast n values =
  drop (max 0 (length values - n)) values

conversationTrace :: Integer -> [AgentTraceRecord] -> [AgentTraceRecord]
conversationTrace messageId records =
  filter ((`elem` linkedRunIds) . eventRunId . (.event)) records
  where
    linkedRunIds =
      ordNub
        [ runId
        | AgentTraceRecord{event = AgentConversationLinked{runId, linkedMessageId, parentMessageId}} <- records
        , linkedMessageId == messageId || parentMessageId == Just messageId
        ]

conversationMessagesTrace :: [Integer] -> [AgentTraceRecord] -> [AgentTraceRecord]
conversationMessagesTrace messageIds records =
  filter ((`elem` linkedRunIds) . eventRunId . (.event)) records
  where
    messageIdSet =
      ordNub messageIds
    linkedRunIds =
      ordNub
        [ runId
        | AgentTraceRecord{event = AgentConversationLinked{runId, linkedMessageId, parentMessageId}} <- records
        , linkedMessageId `elem` messageIdSet || maybe False (`elem` messageIdSet) parentMessageId
        ]

eventRunId :: AgentTraceEvent -> Text
eventRunId = \case
  AgentRunStarted{runId} -> runId
  ModelTurnStarted{runId} -> runId
  ModelTurnFinished{runId} -> runId
  ToolCallStarted{runId} -> runId
  ToolCallFinished{runId} -> runId
  AgentRunFinished{runId} -> runId
  AgentRunInterrupted{runId} -> runId
  AgentConversationLinked{runId} -> runId
