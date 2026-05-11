{-|
Module      : Bot.Storage.SQLite
Description : SQLite persistence
Stability   : experimental
-}

module Bot.Storage.SQLite
  ( SQLiteStore
  , openSQLiteStore
  , JsonCollection (..)
  , StoredState (..)
  , ConversationRow (..)
  , ConversationPayloadKind (..)
  , declareJsonCollection
  , loadJsonCollection
  , appendJsonCollection
  , replaceJsonCollectionItem
  , deleteJsonCollectionRows
  , clearJsonCollection
  , loadConversationRows
  , loadConversationRow
  , loadNextConversationId
  , saveConversationMessages
  , saveChatLogEntry
  , queryChatLogEntries
  )
where

import Bot.Prelude
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import Data.Char (isAlpha, isAlphaNum, isAscii)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Database.SQLite3 as SQLite
import System.IO.Error (userError)

-- | Thread-safe handle for the bot's SQLite database.
data SQLiteStore = SQLiteStore
  { database :: !SQLite.Database
  , lock :: !(MVar.MVar ())
  }

data JsonCollection a = JsonCollection
  { tableName :: !Text
  , scopeColumns :: ![Text]
  }

data StoredState a = StoredState
  { rowId :: !Integer
  , value :: !a
  }
  deriving (Eq, Show)

data ConversationRow = ConversationRow
  { messageId :: !Integer
  , conversationId :: !(Maybe Integer)
  , parentMessageId :: !(Maybe Integer)
  , payloadJson :: !Text
  , payloadKind :: !ConversationPayloadKind
  }
  deriving (Eq, Show)

data ConversationPayloadKind
  = ConversationPayloadMessages
  | ConversationPayloadSnapshot
  deriving (Eq, Show)

-- | Open the database, configure connection pragmas, and run migrations.
openSQLiteStore :: FilePath -> IO SQLiteStore
openSQLiteStore path = do
  database <- SQLite.open (toText path)
  lock <- MVar.newMVar ()
  let store = SQLiteStore{database, lock}
  configure database
  migrate store
  pure store

configure :: SQLite.Database -> IO ()
configure db = do
  SQLite.exec db [i|PRAGMA busy_timeout = #{sqliteBusyTimeoutMilliseconds}|]
  retrySQLiteBusy sqliteBusyRetries $
    SQLite.exec db "PRAGMA journal_mode = WAL"
  SQLite.exec db "PRAGMA synchronous = NORMAL"

migrate :: SQLiteStore -> IO ()
migrate store = withStore store \db -> do
  SQLite.exec db
    "CREATE TABLE IF NOT EXISTS conversation_nodes (message_id INTEGER PRIMARY KEY, conversation_id INTEGER, parent_message_id INTEGER, messages_json TEXT NOT NULL)"
  SQLite.exec db
    "CREATE INDEX IF NOT EXISTS conversation_nodes_parent_idx ON conversation_nodes(parent_message_id)"
  whenM (tableExists db "conversations") do
    ensureColumn db "conversations" "conversation_id" "INTEGER"
    ensureColumn db "conversations" "parent_message_id" "INTEGER"
  SQLite.exec db
    "CREATE TABLE IF NOT EXISTS chat_log (id INTEGER PRIMARY KEY AUTOINCREMENT, platform_key TEXT NOT NULL, kind_key TEXT NOT NULL, chat_id INTEGER, is_bot INTEGER NOT NULL, entry_json TEXT NOT NULL)"
  SQLite.exec db
    "CREATE INDEX IF NOT EXISTS chat_log_chat_idx ON chat_log(platform_key, kind_key, chat_id, id)"
  SQLite.exec db
    "DROP INDEX IF EXISTS chat_log_visible_idx"

ensureColumn :: SQLite.Database -> Text -> Text -> Text -> IO ()
ensureColumn db table column definition = do
  columns <- tableColumnNames db table
  unless (column `elem` columns) $
    SQLite.exec db [i|ALTER TABLE #{table} ADD COLUMN #{column} #{definition}|]

tableColumnNames :: SQLite.Database -> Text -> IO [Text]
tableColumnNames db table =
  withStatement db [i|PRAGMA table_info(#{table})|] [] (rows [])
  where
    rows acc stmt =
      SQLite.step stmt >>= \case
        SQLite.Done ->
          pure (reverse acc)
        SQLite.Row -> do
          name <- columnText stmt 1
          rows (name : acc) stmt

tableExists :: SQLite.Database -> Text -> IO Bool
tableExists db table =
  withStatement db "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ? LIMIT 1" [SQLite.SQLText table] \stmt ->
    SQLite.step stmt >>= \case
      SQLite.Row ->
        pure True
      SQLite.Done ->
        pure False

declareJsonCollection :: SQLiteStore -> JsonCollection a -> IO ()
declareJsonCollection store collection = do
  validateCollection collection
  withStore store \db -> do
    SQLite.exec db
      [i|CREATE TABLE IF NOT EXISTS #{tableName} (id INTEGER PRIMARY KEY AUTOINCREMENT#{scopeDefinitions}, item_json TEXT NOT NULL)|]
    SQLite.exec db
      [i|CREATE INDEX IF NOT EXISTS #{tableName}_scope_idx ON #{tableName}(#{indexColumns})|]
  where
    tableName =
      collection.tableName
    scopeDefinitions =
      Text.concat (map (", " <>) (map (<> " TEXT NOT NULL") collection.scopeColumns))
    indexColumns =
      Text.intercalate ", " (collection.scopeColumns <> ["id"])

loadJsonCollection :: Aeson.FromJSON a => SQLiteStore -> JsonCollection a -> [Text] -> IO [StoredState a]
loadJsonCollection store collection scopeValues = do
  declareJsonCollection store collection
  validateScope collection scopeValues
  withStore store \db ->
    withStatement db
      [i|SELECT id, item_json FROM #{tableName} WHERE #{scopePredicate collection} ORDER BY id ASC|]
      (map SQLite.SQLText scopeValues)
      (rows [])
  where
    tableName =
      collection.tableName
    rows acc stmt =
      SQLite.step stmt >>= \case
        SQLite.Done ->
          pure (reverse acc)
        SQLite.Row -> do
          rowId <- columnInteger stmt 0
          itemJson <- columnText stmt 1
          case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 itemJson) of
            Right value ->
              rows (StoredState{rowId, value} : acc) stmt
            Left err ->
              Exception.throwIO (userError [i|Could not decode #{tableName} row #{rowId}: #{err}|])

appendJsonCollection :: Aeson.ToJSON a => SQLiteStore -> JsonCollection a -> [Text] -> a -> IO ()
appendJsonCollection store collection scopeValues value = do
  declareJsonCollection store collection
  validateScope collection scopeValues
  withStore store \db ->
    withStatement db
      [i|INSERT INTO #{tableName}(#{insertColumns}) VALUES (#{insertPlaceholders})|]
      (map SQLite.SQLText scopeValues <> [SQLite.SQLText (jsonText value)])
      stepDone
  where
    tableName =
      collection.tableName
    insertColumns =
      Text.intercalate ", " (collection.scopeColumns <> ["item_json"])
    insertPlaceholders =
      Text.intercalate ", " (replicate (length collection.scopeColumns + 1) "?")

replaceJsonCollectionItem :: Aeson.ToJSON a => SQLiteStore -> JsonCollection a -> [Text] -> Integer -> a -> IO ()
replaceJsonCollectionItem store collection scopeValues rowId value = do
  declareJsonCollection store collection
  validateScope collection scopeValues
  withStore store \db ->
    withStatement db
      [i|UPDATE #{tableName} SET item_json = ? WHERE #{scopePredicate collection} AND id = ?|]
      ([SQLite.SQLText (jsonText value)] <> map SQLite.SQLText scopeValues <> [SQLite.SQLInteger (fromIntegral rowId)])
      stepDone
  where
    tableName =
      collection.tableName

deleteJsonCollectionRows :: SQLiteStore -> JsonCollection a -> [Text] -> [Integer] -> IO ()
deleteJsonCollectionRows store collection scopeValues rowIds =
  traverse_ (deleteJsonCollectionRow store collection scopeValues) rowIds

deleteJsonCollectionRow :: SQLiteStore -> JsonCollection a -> [Text] -> Integer -> IO ()
deleteJsonCollectionRow store collection scopeValues rowId = do
  declareJsonCollection store collection
  validateScope collection scopeValues
  withStore store \db ->
    withStatement db
      [i|DELETE FROM #{tableName} WHERE #{scopePredicate collection} AND id = ?|]
      (map SQLite.SQLText scopeValues <> [SQLite.SQLInteger (fromIntegral rowId)])
      stepDone
  where
    tableName =
      collection.tableName

clearJsonCollection :: SQLiteStore -> JsonCollection a -> [Text] -> IO ()
clearJsonCollection store collection scopeValues = do
  declareJsonCollection store collection
  validateScope collection scopeValues
  withStore store \db ->
    withStatement db
      [i|DELETE FROM #{tableName} WHERE #{scopePredicate collection}|]
      (map SQLite.SQLText scopeValues)
      stepDone
  where
    tableName =
      collection.tableName

scopePredicate :: JsonCollection a -> Text
scopePredicate collection =
  Text.intercalate " AND " (map (<> " = ?") collection.scopeColumns)

validateCollection :: JsonCollection a -> IO ()
validateCollection collection = do
  unless (validIdentifier collection.tableName) $
    Exception.throwIO (userError [i|Invalid SQLite table name: #{tableName}|])
  traverse_ validateScopeColumn collection.scopeColumns
  when (null collection.scopeColumns) $
    Exception.throwIO (userError [i|JSON collection #{tableName} must define at least one scope column|])
  where
    tableName =
      collection.tableName
    validateScopeColumn column =
      unless (validIdentifier column) $
        Exception.throwIO (userError [i|Invalid SQLite scope column: #{column}|])

validateScope :: JsonCollection a -> [Text] -> IO ()
validateScope collection scopeValues =
  unless (length collection.scopeColumns == length scopeValues) $
    Exception.throwIO (userError [i|JSON collection #{tableName} expected #{scopeColumnCount} scope values, got #{scopeValueCount}|])
  where
    tableName =
      collection.tableName
    scopeColumnCount =
      length collection.scopeColumns
    scopeValueCount =
      length scopeValues

validIdentifier :: Text -> Bool
validIdentifier identifier =
  case Text.uncons identifier of
    Nothing ->
      False
    Just (firstChar, rest) ->
      validFirst firstChar && Text.all validRest rest
  where
    validFirst char =
      char == '_' || (isAscii char && isAlpha char)
    validRest char =
      char == '_' || (isAscii char && isAlphaNum char)

-- | Load persisted conversation nodes keyed by bot message id.
loadConversationRows :: SQLiteStore -> IO [ConversationRow]
loadConversationRows store = withStore store \db -> do
  nodeRows <- loadConversationNodeRows db
  legacyRows <-
    ifM (tableExists db "conversations")
      (loadLegacyConversationRows db)
      (pure [])
  let nodeMessageIds = map (.messageId) nodeRows
  pure (sortOn (.messageId) (nodeRows <> filter (\row -> row.messageId `notElem` nodeMessageIds) legacyRows))

loadConversationRow :: SQLiteStore -> Integer -> IO (Maybe ConversationRow)
loadConversationRow store messageId = withStore store \db -> do
  nodeRow <- loadConversationNodeRow db messageId
  case nodeRow of
    Just row ->
      pure (Just row)
    Nothing ->
      ifM (tableExists db "conversations")
        (loadLegacyConversationRow db messageId)
        (pure Nothing)

loadNextConversationId :: SQLiteStore -> IO Integer
loadNextConversationId store = withStore store \db -> do
  nodeMax <- maxConversationId db "conversation_nodes"
  legacyMax <-
    ifM (tableExists db "conversations")
      (maxConversationId db "conversations")
      (pure 0)
  pure (max nodeMax legacyMax + 1)

loadConversationNodeRows :: SQLite.Database -> IO [ConversationRow]
loadConversationNodeRows db =
  withStatement db "SELECT message_id, conversation_id, parent_message_id, messages_json FROM conversation_nodes ORDER BY message_id ASC" [] \stmt ->
    rows [] stmt
  where
    rows acc stmt =
      SQLite.step stmt >>= \case
        SQLite.Done ->
          pure (reverse acc)
        SQLite.Row -> do
          messageId <- columnInteger stmt 0
          conversationId <- columnMaybeInteger stmt 1
          parentMessageId <- columnMaybeInteger stmt 2
          payloadJson <- columnText stmt 3
          rows (ConversationRow{messageId, conversationId, parentMessageId, payloadJson, payloadKind = ConversationPayloadMessages} : acc) stmt

loadConversationNodeRow :: SQLite.Database -> Integer -> IO (Maybe ConversationRow)
loadConversationNodeRow db targetMessageId =
  withStatement db "SELECT message_id, conversation_id, parent_message_id, messages_json FROM conversation_nodes WHERE message_id = ? LIMIT 1" [SQLite.SQLInteger (fromIntegral targetMessageId)] \stmt ->
    SQLite.step stmt >>= \case
      SQLite.Done ->
        pure Nothing
      SQLite.Row -> do
        messageId <- columnInteger stmt 0
        conversationId <- columnMaybeInteger stmt 1
        parentMessageId <- columnMaybeInteger stmt 2
        payloadJson <- columnText stmt 3
        pure (Just ConversationRow{messageId, conversationId, parentMessageId, payloadJson, payloadKind = ConversationPayloadMessages})

loadLegacyConversationRows :: SQLite.Database -> IO [ConversationRow]
loadLegacyConversationRows db =
  withStatement db "SELECT message_id, conversation_id, parent_message_id, conversation_json FROM conversations ORDER BY message_id ASC" [] \stmt ->
    rows [] stmt
  where
    rows acc stmt =
      SQLite.step stmt >>= \case
        SQLite.Done ->
          pure (reverse acc)
        SQLite.Row -> do
          messageId <- columnInteger stmt 0
          conversationId <- columnMaybeInteger stmt 1
          parentMessageId <- columnMaybeInteger stmt 2
          payloadJson <- columnText stmt 3
          rows (ConversationRow{messageId, conversationId, parentMessageId, payloadJson, payloadKind = ConversationPayloadSnapshot} : acc) stmt

loadLegacyConversationRow :: SQLite.Database -> Integer -> IO (Maybe ConversationRow)
loadLegacyConversationRow db targetMessageId =
  withStatement db "SELECT message_id, conversation_id, parent_message_id, conversation_json FROM conversations WHERE message_id = ? LIMIT 1" [SQLite.SQLInteger (fromIntegral targetMessageId)] \stmt ->
    SQLite.step stmt >>= \case
      SQLite.Done ->
        pure Nothing
      SQLite.Row -> do
        messageId <- columnInteger stmt 0
        conversationId <- columnMaybeInteger stmt 1
        parentMessageId <- columnMaybeInteger stmt 2
        payloadJson <- columnText stmt 3
        pure (Just ConversationRow{messageId, conversationId, parentMessageId, payloadJson, payloadKind = ConversationPayloadSnapshot})

maxConversationId :: SQLite.Database -> Text -> IO Integer
maxConversationId db table =
  withStatement db [i|SELECT COALESCE(MAX(conversation_id), 0) FROM #{table}|] [] \stmt ->
    SQLite.step stmt >>= \case
      SQLite.Done ->
        pure 0
      SQLite.Row ->
        columnInteger stmt 0

-- | Upsert serialized messages for one conversation node.
saveConversationMessages :: SQLiteStore -> Integer -> Integer -> Maybe Integer -> Text -> IO ()
saveConversationMessages store messageId conversationId parentMessageId messagesJson = withStore store \db ->
  withStatement db
    "INSERT OR REPLACE INTO conversation_nodes(message_id, conversation_id, parent_message_id, messages_json) VALUES (?, ?, ?, ?)"
    [ SQLite.SQLInteger (fromIntegral messageId)
    , SQLite.SQLInteger (fromIntegral conversationId)
    , maybe SQLite.SQLNull (SQLite.SQLInteger . fromIntegral) parentMessageId
    , SQLite.SQLText messagesJson
    ]
    stepDone

-- | Persist one sanitized chat log entry.
saveChatLogEntry :: SQLiteStore -> Text -> Text -> Maybe Integer -> Bool -> Aeson.Value -> IO ()
saveChatLogEntry store platformKey kindKey chatId isBot entry = withStore store \db ->
  withStatement db
    "INSERT INTO chat_log(platform_key, kind_key, chat_id, is_bot, entry_json) VALUES (?, ?, ?, ?, ?)"
    [ SQLite.SQLText platformKey
    , SQLite.SQLText kindKey
    , maybe SQLite.SQLNull (SQLite.SQLInteger . fromIntegral) chatId
    , SQLite.SQLInteger (if isBot then 1 else 0)
    , SQLite.SQLText (jsonText entry)
    ]
    stepDone

-- | Return recent chat log entries in chronological order.
queryChatLogEntries :: SQLiteStore -> Text -> Text -> Maybe Integer -> Bool -> Int -> IO [Aeson.Value]
queryChatLogEntries store platformKey kindKey chatId includeBotMessages limit = withStore store \db ->
  withStatement db sql params \stmt ->
    reverse <$> rows [] stmt
  where
    botClause
      | includeBotMessages = ""
      | otherwise = " AND is_bot = 0"
    sql =
      "SELECT entry_json FROM chat_log WHERE platform_key = ? AND kind_key = ? AND "
        <> chatIdClause
        <> botClause
        <> " ORDER BY id DESC LIMIT ?"
    chatIdClause =
      case chatId of
        Nothing -> "chat_id IS NULL"
        Just _ -> "chat_id = ?"
    params =
      [ SQLite.SQLText platformKey
      , SQLite.SQLText kindKey
      ]
        <> maybe [] (\value -> [SQLite.SQLInteger (fromIntegral value)]) chatId
        <> [SQLite.SQLInteger (fromIntegral (max 0 limit))]
    rows acc stmt =
      SQLite.step stmt >>= \case
        SQLite.Done ->
          pure acc
        SQLite.Row -> do
          json <- columnText stmt 0
          let acc' =
                case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 json) of
                  Right value -> value : acc
                  Left _ -> acc
          rows acc' stmt

withStore :: SQLiteStore -> (SQLite.Database -> IO a) -> IO a
withStore SQLiteStore{database, lock} action =
  MVar.withMVar lock \_ ->
    retrySQLiteBusy sqliteBusyRetries (action database)

withStatement :: SQLite.Database -> Text -> [SQLite.SQLData] -> (SQLite.Statement -> IO a) -> IO a
withStatement db sql params action =
  Exception.bracket (SQLite.prepare db sql) SQLite.finalize \stmt -> do
    SQLite.bind stmt params
    action stmt

stepDone :: SQLite.Statement -> IO ()
stepDone stmt =
  SQLite.step stmt >>= \case
    SQLite.Done -> pure ()
    SQLite.Row -> pure ()

columnText :: SQLite.Statement -> Int -> IO Text
columnText stmt index =
  SQLite.column stmt (SQLite.ColumnIndex index) >>= \case
    SQLite.SQLText value -> pure value
    SQLite.SQLInteger value -> pure (show value)
    SQLite.SQLNull -> pure ""
    other -> pure (show other)

columnInteger :: SQLite.Statement -> Int -> IO Integer
columnInteger stmt index =
  SQLite.column stmt (SQLite.ColumnIndex index) >>= \case
    SQLite.SQLInteger value -> pure (fromIntegral value)
    SQLite.SQLText value -> maybe (pure 0) pure (readMaybe (toString value))
    _ -> pure 0

columnMaybeInteger :: SQLite.Statement -> Int -> IO (Maybe Integer)
columnMaybeInteger stmt index =
  SQLite.column stmt (SQLite.ColumnIndex index) >>= \case
    SQLite.SQLInteger value -> pure (Just (fromIntegral value))
    SQLite.SQLText value -> pure (readMaybe (toString value))
    _ -> pure Nothing

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

sqliteBusyTimeoutMilliseconds :: Int
sqliteBusyTimeoutMilliseconds =
  5000

sqliteBusyRetries :: Int
sqliteBusyRetries =
  3

sqliteBusyRetryDelayMicroseconds :: Int
sqliteBusyRetryDelayMicroseconds =
  200000

retrySQLiteBusy :: Int -> IO a -> IO a
retrySQLiteBusy retries action =
  action `Exception.catch` \(err :: SQLite.SQLError) ->
    if retries > 0 && isBusyError (SQLite.sqlError err)
      then do
        threadDelay sqliteBusyRetryDelayMicroseconds
        retrySQLiteBusy (retries - 1) action
      else Exception.throwIO err

isBusyError :: SQLite.Error -> Bool
isBusyError = \case
  SQLite.ErrorBusy        -> True
  SQLite.ErrorLocked      -> True
  SQLite.ErrorBusyTimeout -> True
  SQLite.ErrorBusyRecovery -> True
  SQLite.ErrorBusySnapshot -> True
  _                       -> False
