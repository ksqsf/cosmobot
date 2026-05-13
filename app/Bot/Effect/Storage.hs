{-|
Module      : Bot.Effect.Storage
Description : Application storage capability
Stability   : experimental
-}

module Bot.Effect.Storage
  ( Storage
  , SQLite.SQLiteStore
  , SQLite.JsonCollection (..)
  , SQLite.StoredState (..)
  , SQLite.ConversationRow (..)
  , SQLite.ConversationPayloadKind (..)
  , SQLite.AgentAuditRow (..)
  , runStorageSQLite
  , loadJsonCollection
  , appendJsonCollection
  , replaceJsonCollectionItem
  , deleteJsonCollectionRows
  , clearJsonCollection
  , saveChatLogEntry
  , queryChatLogEntries
  , saveAgentAuditEvent
  , queryAgentAuditEvents
  , queryRecentAgentAuditEvents
  , queryAgentAuditEventsForMessage
  , queryAgentAuditEventsForMessages
  )
where

import Bot.Prelude
import qualified Bot.Storage.SQLite as SQLite
import qualified Data.Aeson as Aeson

data Storage :: Effect where
  LoadJsonCollection
    :: Aeson.FromJSON a
    => SQLite.JsonCollection a
    -> [Text]
    -> Storage m [SQLite.StoredState a]
  AppendJsonCollection
    :: Aeson.ToJSON a
    => SQLite.JsonCollection a
    -> [Text]
    -> a
    -> Storage m ()
  ReplaceJsonCollectionItem
    :: Aeson.ToJSON a
    => SQLite.JsonCollection a
    -> [Text]
    -> Integer
    -> a
    -> Storage m ()
  DeleteJsonCollectionRows
    :: SQLite.JsonCollection a
    -> [Text]
    -> [Integer]
    -> Storage m ()
  ClearJsonCollection
    :: SQLite.JsonCollection a
    -> [Text]
    -> Storage m ()
  SaveChatLogEntry
    :: Text
    -> Text
    -> Maybe Integer
    -> Bool
    -> Aeson.Value
    -> Storage m ()
  QueryChatLogEntries
    :: Text
    -> Text
    -> Maybe Integer
    -> Bool
    -> Int
    -> Storage m [Aeson.Value]
  SaveAgentAuditEvent
    :: Aeson.ToJSON a
    => Text
    -> UTCTime
    -> Maybe Integer
    -> Maybe Integer
    -> a
    -> Storage m ()
  QueryAgentAuditEvents
    :: Aeson.FromJSON a
    => Text
    -> Storage m [SQLite.AgentAuditRow a]
  QueryRecentAgentAuditEvents
    :: Aeson.FromJSON a
    => Int
    -> Storage m [SQLite.AgentAuditRow a]
  QueryAgentAuditEventsForMessage
    :: Aeson.FromJSON a
    => Integer
    -> Storage m [SQLite.AgentAuditRow a]
  QueryAgentAuditEventsForMessages
    :: Aeson.FromJSON a
    => [Integer]
    -> Storage m [SQLite.AgentAuditRow a]

type instance DispatchOf Storage = Dynamic

runStorageSQLite
  :: IOE :> es
  => SQLite.SQLiteStore
  -> Eff (Storage : es) a
  -> Eff es a
runStorageSQLite store =
  interpret \_ -> \case
    LoadJsonCollection collection scopeValues ->
      liftIO (SQLite.loadJsonCollection store collection scopeValues)
    AppendJsonCollection collection scopeValues value ->
      liftIO (SQLite.appendJsonCollection store collection scopeValues value)
    ReplaceJsonCollectionItem collection scopeValues rowId value ->
      liftIO (SQLite.replaceJsonCollectionItem store collection scopeValues rowId value)
    DeleteJsonCollectionRows collection scopeValues rowIds ->
      liftIO (SQLite.deleteJsonCollectionRows store collection scopeValues rowIds)
    ClearJsonCollection collection scopeValues ->
      liftIO (SQLite.clearJsonCollection store collection scopeValues)
    SaveChatLogEntry platformKey kindKey chatId isBot entry ->
      liftIO (SQLite.saveChatLogEntry store platformKey kindKey chatId isBot entry)
    QueryChatLogEntries platformKey kindKey chatId includeBotMessages limit ->
      liftIO (SQLite.queryChatLogEntries store platformKey kindKey chatId includeBotMessages limit)
    SaveAgentAuditEvent runId occurredAt linkedMessageId parentMessageId event ->
      liftIO (SQLite.saveAgentAuditEvent store runId occurredAt linkedMessageId parentMessageId (Aeson.toJSON event))
    QueryAgentAuditEvents runId ->
      liftIO (SQLite.queryAgentAuditEvents store runId)
    QueryRecentAgentAuditEvents limit ->
      liftIO (SQLite.queryRecentAgentAuditEvents store limit)
    QueryAgentAuditEventsForMessage messageId ->
      liftIO (SQLite.queryAgentAuditEventsForMessage store messageId)
    QueryAgentAuditEventsForMessages messageIds ->
      liftIO (SQLite.queryAgentAuditEventsForMessages store messageIds)

loadJsonCollection :: (Storage :> es, Aeson.FromJSON a) => SQLite.JsonCollection a -> [Text] -> Eff es [SQLite.StoredState a]
loadJsonCollection collection scopeValues =
  send (LoadJsonCollection collection scopeValues)

appendJsonCollection :: (Storage :> es, Aeson.ToJSON a) => SQLite.JsonCollection a -> [Text] -> a -> Eff es ()
appendJsonCollection collection scopeValues value =
  send (AppendJsonCollection collection scopeValues value)

replaceJsonCollectionItem :: (Storage :> es, Aeson.ToJSON a) => SQLite.JsonCollection a -> [Text] -> Integer -> a -> Eff es ()
replaceJsonCollectionItem collection scopeValues rowId value =
  send (ReplaceJsonCollectionItem collection scopeValues rowId value)

deleteJsonCollectionRows :: Storage :> es => SQLite.JsonCollection a -> [Text] -> [Integer] -> Eff es ()
deleteJsonCollectionRows collection scopeValues rowIds =
  send (DeleteJsonCollectionRows collection scopeValues rowIds)

clearJsonCollection :: Storage :> es => SQLite.JsonCollection a -> [Text] -> Eff es ()
clearJsonCollection collection scopeValues =
  send (ClearJsonCollection collection scopeValues)

saveChatLogEntry :: Storage :> es => Text -> Text -> Maybe Integer -> Bool -> Aeson.Value -> Eff es ()
saveChatLogEntry platformKey kindKey chatId isBot entry =
  send (SaveChatLogEntry platformKey kindKey chatId isBot entry)

queryChatLogEntries :: Storage :> es => Text -> Text -> Maybe Integer -> Bool -> Int -> Eff es [Aeson.Value]
queryChatLogEntries platformKey kindKey chatId includeBotMessages limit =
  send (QueryChatLogEntries platformKey kindKey chatId includeBotMessages limit)

saveAgentAuditEvent :: (Storage :> es, Aeson.ToJSON a) => Text -> UTCTime -> Maybe Integer -> Maybe Integer -> a -> Eff es ()
saveAgentAuditEvent runId occurredAt linkedMessageId parentMessageId event =
  send (SaveAgentAuditEvent runId occurredAt linkedMessageId parentMessageId event)

queryAgentAuditEvents :: (Storage :> es, Aeson.FromJSON a) => Text -> Eff es [SQLite.AgentAuditRow a]
queryAgentAuditEvents runId =
  send (QueryAgentAuditEvents runId)

queryRecentAgentAuditEvents :: (Storage :> es, Aeson.FromJSON a) => Int -> Eff es [SQLite.AgentAuditRow a]
queryRecentAgentAuditEvents limit =
  send (QueryRecentAgentAuditEvents limit)

queryAgentAuditEventsForMessage :: (Storage :> es, Aeson.FromJSON a) => Integer -> Eff es [SQLite.AgentAuditRow a]
queryAgentAuditEventsForMessage messageId =
  send (QueryAgentAuditEventsForMessage messageId)

queryAgentAuditEventsForMessages :: (Storage :> es, Aeson.FromJSON a) => [Integer] -> Eff es [SQLite.AgentAuditRow a]
queryAgentAuditEventsForMessages messageIds =
  send (QueryAgentAuditEventsForMessages messageIds)
