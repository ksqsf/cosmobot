{-|
Module      : Bot.Handler.Scratchpad
Description : Per-sender scratchpad todo commands
Stability   : experimental
-}

module Bot.Handler.Scratchpad
  ( scratchpadHandlers
  , renderTodoList
  )
where

import qualified Bot.Effect.Chat as Chat
import Bot.Filter
import Bot.Message
import Bot.Prelude
import qualified Bot.Storage.SQLite as Storage
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

scratchpadHandlers
  :: (Chat.Chat :> es, IOE :> es)
  => Storage.SQLiteStore
  -> [RouteHandler es]
scratchpadHandlers store =
  [ scratchpadRoute store "!todo" TodoCommand
  , scratchpadRoute store "!done" DoneCommand
  , scratchpadRoute store "!list" ListCommand
  , scratchpadRoute store "!clear" ClearCommand
  , scratchpadRoute store "!rm" RemoveCommand
  ]

data ScratchpadCommand
  = TodoCommand
  | DoneCommand
  | ListCommand
  | ClearCommand
  | RemoveCommand

data TodoItem = TodoItem
  { body :: !Text
  , done :: !Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

type StoredTodo = Storage.StoredState TodoItem

todoItems :: Storage.JsonCollection TodoItem
todoItems =
  Storage.JsonCollection
    { tableName = "scratchpad_todo_items"
    , scopeColumns = ["platform_key", "sender_key"]
    }

scratchpadRoute
  :: (Chat.Chat :> es, IOE :> es)
  => Storage.SQLiteStore
  -> Text
  -> ScratchpadCommand
  -> RouteHandler es
scratchpadRoute store commandText commandKind =
  routeStop (command commandText) \message args ->
    handleScratchpadCommand store commandKind message args

handleScratchpadCommand
  :: (Chat.Chat :> es, IOE :> es)
  => Storage.SQLiteStore
  -> ScratchpadCommand
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleScratchpadCommand store commandKind message args =
  case senderScope message of
    Nothing ->
      void $ Chat.replyTo message "无法识别发送者，不能保存 todo。"
    Just (platform, sender) ->
      case commandKind of
        TodoCommand -> handleTodo store platform sender message args
        DoneCommand -> handleDone store platform sender message args
        ListCommand -> replyWithList store platform sender message
        ClearCommand -> handleClear store platform sender message
        RemoveCommand -> handleRemove store platform sender message args

handleTodo
  :: (Chat.Chat :> es, IOE :> es)
  => Storage.SQLiteStore
  -> Text
  -> Text
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleTodo store platformKey senderKey message args
  | Text.null body =
      replyWithList store platformKey senderKey message
  | otherwise = do
      liftIO (addTodoItem store platformKey senderKey body)
      todos <- liftIO (loadTodoList store platformKey senderKey)
      void $ Chat.replyTo message ("已添加 #" <> show (length todos) <> ": " <> body)
  where
    body = Text.strip args

handleDone
  :: (Chat.Chat :> es, IOE :> es)
  => Storage.SQLiteStore
  -> Text
  -> Text
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleDone store platformKey senderKey message args =
  case parseSingleIndex args of
    Nothing ->
      void $ Chat.replyTo message "用法：!done <todo编号>"
    Just index -> do
      todos <- liftIO (loadTodoList store platformKey senderKey)
      case todoAt index todos of
        Nothing ->
          void $ Chat.replyTo message [i|没有编号 #{index} 的 todo。|]
        Just todo -> do
          liftIO (markTodoDone store platformKey senderKey todo)
          void $ Chat.replyTo message ("已完成 #" <> show index <> ": " <> todo.value.body)

handleClear
  :: (Chat.Chat :> es, IOE :> es)
  => Storage.SQLiteStore
  -> Text
  -> Text
  -> IncomingMessage
  -> Eff es ()
handleClear store platformKey senderKey message = do
  liftIO (clearTodoList store platformKey senderKey)
  void $ Chat.replyTo message "已清空 todo list。"

handleRemove
  :: (Chat.Chat :> es, IOE :> es)
  => Storage.SQLiteStore
  -> Text
  -> Text
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleRemove store platformKey senderKey message args =
  case parseIndices args of
    [] ->
      void $ Chat.replyTo message "用法：!rm <编号1> <编号2> ..."
    indices -> do
      todos <- liftIO (loadTodoList store platformKey senderKey)
      let rows = mapMaybe (`todoAt` todos) indices
      liftIO (deleteTodoRows store platformKey senderKey (map (.rowId) rows))
      void $ Chat.replyTo message [i|已删除 #{length rows} 项。|]

replyWithList
  :: (Chat.Chat :> es, IOE :> es)
  => Storage.SQLiteStore
  -> Text
  -> Text
  -> IncomingMessage
  -> Eff es ()
replyWithList store platformKey senderKey message = do
  todos <- liftIO (loadTodoList store platformKey senderKey)
  void $ Chat.replyTo message (renderTodoList todos)

renderTodoList :: [StoredTodo] -> Text
renderTodoList [] =
  "todo list 为空。"
renderTodoList todos =
  Text.unlines (zipWith render [1 :: Int ..] todos)
  where
    render index todo =
      "- [" <> status todo.value <> "] " <> show index <> ". " <> todo.value.body
    status todo
      | todo.done = "X"
      | otherwise = " "

loadTodoList :: Storage.SQLiteStore -> Text -> Text -> IO [StoredTodo]
loadTodoList store platformKey senderKey =
  Storage.loadJsonCollection store todoItems (todoScope platformKey senderKey)

addTodoItem :: Storage.SQLiteStore -> Text -> Text -> Text -> IO ()
addTodoItem store platformKey senderKey body =
  Storage.appendJsonCollection store todoItems (todoScope platformKey senderKey) TodoItem{body, done = False}

markTodoDone :: Storage.SQLiteStore -> Text -> Text -> StoredTodo -> IO ()
markTodoDone store platformKey senderKey todo = do
  let item = todo.value
  Storage.replaceJsonCollectionItem store todoItems (todoScope platformKey senderKey) todo.rowId item{done = True}

deleteTodoRows :: Storage.SQLiteStore -> Text -> Text -> [Integer] -> IO ()
deleteTodoRows store platformKey senderKey =
  Storage.deleteJsonCollectionRows store todoItems (todoScope platformKey senderKey)

clearTodoList :: Storage.SQLiteStore -> Text -> Text -> IO ()
clearTodoList store platformKey senderKey =
  Storage.clearJsonCollection store todoItems (todoScope platformKey senderKey)

todoScope :: Text -> Text -> [Text]
todoScope platformKey senderKey =
  [platformKey, senderKey]

todoAt :: Int -> [StoredTodo] -> Maybe StoredTodo
todoAt index todos
  | index <= 0 = Nothing
  | otherwise = viaNonEmpty head (drop (index - 1) todos)

parseSingleIndex :: Text -> Maybe Int
parseSingleIndex args =
  case parseIndices args of
    [index] -> Just index
    _ -> Nothing

parseIndices :: Text -> [Int]
parseIndices =
  mapMaybe (readMaybe . toString) . Text.words

senderScope :: IncomingMessage -> Maybe (Text, Text)
senderScope message =
  (messagePlatformKey message,) <$> messageSenderKey message

messagePlatformKey :: IncomingMessage -> Text
messagePlatformKey message =
  show message.platform

messageSenderKey :: IncomingMessage -> Maybe Text
messageSenderKey message =
  case message.senderId of
    Just senderId ->
      Just ("id:" <> show senderId)
    Nothing ->
      ("username:" <>) <$> message.senderUsername
