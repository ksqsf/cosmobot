{-|
Module      : Bot.Storage.SQLite
Description : SQLite persistence
Stability   : experimental
-}

module Bot.Storage.SQLite
  ( SQLiteStore
  , openSQLiteStore
  , loadConversationRows
  , saveConversationJson
  , saveChatLogEntry
  , queryChatLogEntries
  , StoredTodo (..)
  , loadTodoList
  , addTodoItem
  , markTodoDone
  , deleteTodoRows
  , clearTodoList
  )
where

import Bot.Prelude
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TextEncoding
import qualified Database.SQLite3 as SQLite

-- | Thread-safe handle for the bot's SQLite database.
data SQLiteStore = SQLiteStore
  { database :: !SQLite.Database
  , lock :: !(MVar.MVar ())
  }

data StoredTodo = StoredTodo
  { rowId :: !Integer
  , body :: !Text
  , done :: !Bool
  }
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
    "CREATE TABLE IF NOT EXISTS conversations (message_id INTEGER PRIMARY KEY, conversation_json TEXT NOT NULL)"
  SQLite.exec db
    "CREATE TABLE IF NOT EXISTS chat_log (id INTEGER PRIMARY KEY AUTOINCREMENT, platform_key TEXT NOT NULL, kind_key TEXT NOT NULL, chat_id INTEGER, is_bot INTEGER NOT NULL, entry_json TEXT NOT NULL)"
  SQLite.exec db
    "CREATE INDEX IF NOT EXISTS chat_log_chat_idx ON chat_log(platform_key, kind_key, chat_id, id)"
  SQLite.exec db
    "CREATE TABLE IF NOT EXISTS scratchpad_todos (id INTEGER PRIMARY KEY AUTOINCREMENT, platform_key TEXT NOT NULL, sender_key TEXT NOT NULL, body TEXT NOT NULL, done INTEGER NOT NULL DEFAULT 0)"
  SQLite.exec db
    "CREATE INDEX IF NOT EXISTS scratchpad_todos_sender_idx ON scratchpad_todos(platform_key, sender_key, id)"

-- | Load persisted conversation JSON keyed by bot message id.
loadConversationRows :: SQLiteStore -> IO (Map Integer Text)
loadConversationRows store = withStore store \db ->
  withStatement db "SELECT message_id, conversation_json FROM conversations" [] \stmt ->
    rows Map.empty stmt
  where
    rows acc stmt =
      SQLite.step stmt >>= \case
        SQLite.Done ->
          pure acc
        SQLite.Row -> do
          messageId <- columnInteger stmt 0
          json <- columnText stmt 1
          rows (Map.insert messageId json acc) stmt

-- | Upsert a serialized conversation for a bot message id.
saveConversationJson :: SQLiteStore -> Integer -> Text -> IO ()
saveConversationJson store messageId conversationJson = withStore store \db ->
  withStatement db
    "INSERT OR REPLACE INTO conversations(message_id, conversation_json) VALUES (?, ?)"
    [ SQLite.SQLInteger (fromIntegral messageId)
    , SQLite.SQLText conversationJson
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

loadTodoList :: SQLiteStore -> Text -> Text -> IO [StoredTodo]
loadTodoList store platformKey senderKey = withStore store \db ->
  withStatement db
    "SELECT id, body, done FROM scratchpad_todos WHERE platform_key = ? AND sender_key = ? ORDER BY id ASC"
    [ SQLite.SQLText platformKey
    , SQLite.SQLText senderKey
    ]
    (rows [])
  where
    rows acc stmt =
      SQLite.step stmt >>= \case
        SQLite.Done ->
          pure (reverse acc)
        SQLite.Row -> do
          rowId <- columnInteger stmt 0
          body <- columnText stmt 1
          done <- columnBool stmt 2
          rows (StoredTodo{rowId, body, done} : acc) stmt

addTodoItem :: SQLiteStore -> Text -> Text -> Text -> IO ()
addTodoItem store platformKey senderKey body = withStore store \db ->
  withStatement db
    "INSERT INTO scratchpad_todos(platform_key, sender_key, body, done) VALUES (?, ?, ?, 0)"
    [ SQLite.SQLText platformKey
    , SQLite.SQLText senderKey
    , SQLite.SQLText body
    ]
    stepDone

markTodoDone :: SQLiteStore -> Text -> Text -> Integer -> IO ()
markTodoDone store platformKey senderKey rowId = withStore store \db ->
  withStatement db
    "UPDATE scratchpad_todos SET done = 1 WHERE platform_key = ? AND sender_key = ? AND id = ?"
    [ SQLite.SQLText platformKey
    , SQLite.SQLText senderKey
    , SQLite.SQLInteger (fromIntegral rowId)
    ]
    stepDone

deleteTodoRows :: SQLiteStore -> Text -> Text -> [Integer] -> IO ()
deleteTodoRows store platformKey senderKey rowIds =
  traverse_ (deleteTodoRow store platformKey senderKey) rowIds

deleteTodoRow :: SQLiteStore -> Text -> Text -> Integer -> IO ()
deleteTodoRow store platformKey senderKey rowId = withStore store \db ->
  withStatement db
    "DELETE FROM scratchpad_todos WHERE platform_key = ? AND sender_key = ? AND id = ?"
    [ SQLite.SQLText platformKey
    , SQLite.SQLText senderKey
    , SQLite.SQLInteger (fromIntegral rowId)
    ]
    stepDone

clearTodoList :: SQLiteStore -> Text -> Text -> IO ()
clearTodoList store platformKey senderKey = withStore store \db ->
  withStatement db
    "DELETE FROM scratchpad_todos WHERE platform_key = ? AND sender_key = ?"
    [ SQLite.SQLText platformKey
    , SQLite.SQLText senderKey
    ]
    stepDone

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

columnBool :: SQLite.Statement -> Int -> IO Bool
columnBool stmt index =
  SQLite.column stmt (SQLite.ColumnIndex index) >>= \case
    SQLite.SQLInteger value -> pure (value /= 0)
    SQLite.SQLText value -> pure (value == "1" || value == "true")
    _ -> pure False

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
