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
  , queryThreadAudit
  , queryThreadMessagesAudit
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
  QueryThreadAudit :: MessageId -> AgentAudit m [AgentAuditRecord]
  QueryThreadMessagesAudit :: [MessageId] -> AgentAudit m [AgentAuditRecord]

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

queryThreadAudit :: AgentAudit :> es => MessageId -> Eff es [AgentAuditRecord]
queryThreadAudit messageId =
  send (QueryThreadAudit messageId)

queryThreadMessagesAudit :: AgentAudit :> es => [MessageId] -> Eff es [AgentAuditRecord]
queryThreadMessagesAudit messageIds =
  send (QueryThreadMessagesAudit messageIds)

runAgentAudit
  :: (IOE :> es, KatipE :> es, Storage.Storage :> es)
  => Eff (AgentAudit : es) a
  -> Eff es a
runAgentAudit =
  runAgentAuditWithObserver (const (pure ()))

runAgentAuditWithObserver
  :: (IOE :> es, KatipE :> es, Storage.Storage :> es)
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
            { id = auditId
            , occurredAt = occurredAt
            , event = event
            }
        pure persistedId
      QueryRecentAuditRecords limit -> do
        records <- AgentAuditStorage.queryStoredRecent maxInMemoryAgentAuditEvents
        pure (takeRecentAuditRecords limit records)
      QueryAuditRecord auditId ->
        AgentAuditStorage.queryStoredRecord auditId
      QueryRecentToolUses limit -> do
        records <- AgentAuditStorage.loadStoredAuditRecords
        pure (toolUsesFromRecords processStartedAt limit records)
      QueryToolUse auditId -> do
        records <- AgentAuditStorage.loadStoredAuditRecords
        pure (find ((== auditId) . (.auditId)) (toolUsesFromRecords processStartedAt maxInMemoryAgentAuditEvents records))
      QueryThreadAudit messageId ->
        AgentAuditStorage.queryStoredThreadAudit messageId
      QueryThreadMessagesAudit messageIds ->
        AgentAuditStorage.queryStoredThreadMessagesAudit messageIds
    )
    inner

takeRecentAuditRecords :: Int -> [AgentAuditRecord] -> [AgentAuditRecord]
takeRecentAuditRecords limit records =
  drop (max 0 (length records - max 0 limit)) records
