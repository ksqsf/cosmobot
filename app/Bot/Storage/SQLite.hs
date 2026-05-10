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
  , declareJsonCollection
  , loadJsonCollection
  , appendJsonCollection
  , replaceJsonCollectionItem
  , deleteJsonCollectionRows
  , clearJsonCollection
  , loadConversationRows
  , saveConversationJson
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
import qualified Data.Map.Strict as Map
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
