{-|
Module      : Bot.Effect.AgentAudit
Description : Agent audit capability facade
Stability   : experimental
-}

module Bot.Effect.AgentAudit
  ( AgentAudit
  , AgentAuditEvent (..)
  , AgentAuditRecord (..)
  , ToolCallTrace (..)
  , ToolUseDetail (..)
  , ToolUseStatus (..)
  , agentAuditObserver
  , toolUsesFromAuditRecords
  , queryRecentAuditRecords
  , queryAuditRecord
  , queryRecentToolUses
  , queryToolUse
  , queryConversationAudit
  , queryConversationMessagesAudit
  , runAgentAudit
  , runAgentAuditWithObserver
  )
where

import qualified Bot.Agent.Middleware.Observation.Types as Observation
import qualified Bot.Agent.Types as Agent
import qualified Bot.AgentAudit.Observation as ObservationAdapter
import Bot.AgentAudit.Projection
import qualified Bot.AgentAudit.Storage as AgentAuditStorage
import Bot.AgentAudit.Types
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Effect.Storage as Storage
import Data.Time (getCurrentTime)

data AgentAudit :: Effect where
  RecordEvent :: AgentAuditEvent -> AgentAudit m (Maybe Integer)
  QueryRecentAuditRecords :: Int -> AgentAudit m [AgentAuditRecord]
  QueryAuditRecord :: Integer -> AgentAudit m (Maybe AgentAuditRecord)
  QueryRecentToolUses :: Int -> AgentAudit m [ToolUseDetail]
  QueryToolUse :: Integer -> AgentAudit m (Maybe ToolUseDetail)
  QueryConversationAudit :: MessageId -> AgentAudit m [AgentAuditRecord]
  QueryConversationMessagesAudit :: [MessageId] -> AgentAudit m [AgentAuditRecord]

type instance DispatchOf AgentAudit = Dynamic

recordEvent :: AgentAudit :> es => AgentAuditEvent -> Eff es (Maybe Integer)
recordEvent event =
  send (RecordEvent event)

agentAuditObserver :: AgentAudit :> es => Agent.AgentObserver Observation.ObservationContext es
agentAuditObserver =
  ObservationAdapter.agentAuditObserverWith recordEvent

queryRecentAuditRecords :: AgentAudit :> es => Int -> Eff es [AgentAuditRecord]
queryRecentAuditRecords limit =
  send (QueryRecentAuditRecords limit)

queryAuditRecord :: AgentAudit :> es => Integer -> Eff es (Maybe AgentAuditRecord)
queryAuditRecord auditId =
  send (QueryAuditRecord auditId)

queryRecentToolUses :: AgentAudit :> es => Int -> Eff es [ToolUseDetail]
queryRecentToolUses limit =
  send (QueryRecentToolUses limit)

queryToolUse :: AgentAudit :> es => Integer -> Eff es (Maybe ToolUseDetail)
queryToolUse auditId =
  send (QueryToolUse auditId)

queryConversationAudit :: AgentAudit :> es => MessageId -> Eff es [AgentAuditRecord]
queryConversationAudit messageId =
  send (QueryConversationAudit messageId)

queryConversationMessagesAudit :: AgentAudit :> es => [MessageId] -> Eff es [AgentAuditRecord]
queryConversationMessagesAudit messageIds =
  send (QueryConversationMessagesAudit messageIds)

runAgentAudit
  :: (IOE :> es, Log :> es, Storage.Storage :> es)
  => Eff (AgentAudit : es) a
  -> Eff es a
runAgentAudit =
  runAgentAuditWithObserver (const (pure ()))

runAgentAuditWithObserver
  :: (IOE :> es, Log :> es, Storage.Storage :> es)
  => (AgentAuditRecord -> Eff es ())
  -> Eff (AgentAudit : es) a
  -> Eff es a
runAgentAuditWithObserver observer inner = do
  processStartedAt <- liftIO getCurrentTime
  AgentAuditStorage.ensureAgentAuditTable
  interpret
    (\_ -> \case
      RecordEvent event -> do
        occurredAt <- liftIO getCurrentTime
        persistedId <- AgentAuditStorage.persistEvent occurredAt event
        for_ persistedId \auditId ->
          observer AgentAuditRecord
            { id = Just auditId
            , occurredAt = occurredAt
            , event = event
            }
        pure persistedId
      QueryRecentAuditRecords limit -> do
        records <- AgentAuditStorage.queryStoredRecent maxInMemoryAgentAuditEvents
        pure (takeRecentAuditRecords limit (markStaleRunningToolUses processStartedAt records))
      QueryAuditRecord auditId ->
        AgentAuditStorage.queryStoredRecord auditId
      QueryRecentToolUses limit -> do
        records <- AgentAuditStorage.loadStoredAuditRecords
        pure (toolUsesFromRecords limit (markStaleRunningToolUses processStartedAt records))
      QueryToolUse auditId -> do
        records <- AgentAuditStorage.loadStoredAuditRecords
        pure (find ((== auditId) . (.auditId)) (toolUsesFromRecords maxInMemoryAgentAuditEvents (markStaleRunningToolUses processStartedAt records)))
      QueryConversationAudit messageId ->
        markStaleRunningToolUses processStartedAt <$> AgentAuditStorage.queryStoredConversationAudit messageId
      QueryConversationMessagesAudit messageIds ->
        markStaleRunningToolUses processStartedAt <$> AgentAuditStorage.queryStoredConversationMessagesAudit messageIds
    )
    inner

takeRecentAuditRecords :: Int -> [AgentAuditRecord] -> [AgentAuditRecord]
takeRecentAuditRecords limit records =
  drop (max 0 (length records - max 0 limit)) records
