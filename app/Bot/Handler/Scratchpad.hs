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
import qualified Bot.Effect.Storage as Storage
import Bot.Core.Route
import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

scratchpadHandlers
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => [RouteHandler es]
scratchpadHandlers =
  [ scratchpadRoute "!todo" TodoCommand
  , scratchpadRoute "!done" DoneCommand
  , scratchpadRoute "!list" ListCommand
  , scratchpadRoute "!clear" ClearCommand
  , scratchpadRoute "!rm" RemoveCommand
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
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => Text
  -> ScratchpadCommand
  -> RouteHandler es
scratchpadRoute commandText commandKind =
  stopOn (command commandText) \message args ->
    handleScratchpadCommand commandKind message args

handleScratchpadCommand
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => ScratchpadCommand
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleScratchpadCommand commandKind message args =
  case senderScope message of
    Nothing ->
      void $ Chat.replyTo message "无法识别发送者，不能保存 todo。"
    Just (platform, sender) ->
      case commandKind of
        TodoCommand -> handleTodo platform sender message args
        DoneCommand -> handleDone platform sender message args
        ListCommand -> replyWithList platform sender message
        ClearCommand -> handleClear platform sender message
        RemoveCommand -> handleRemove platform sender message args

handleTodo
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => Text
  -> Text
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleTodo platformKey senderKey message args
  | Text.null body =
      replyWithList platformKey senderKey message
  | otherwise = do
      addTodoItem platformKey senderKey body
      todos <- loadTodoList platformKey senderKey
      void $ Chat.replyTo message ("已添加 #" <> show (length todos) <> ": " <> body)
  where
    body = Text.strip args

handleDone
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => Text
  -> Text
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleDone platformKey senderKey message args =
  case parseSingleIndex args of
    Nothing ->
      void $ Chat.replyTo message "用法：!done <todo编号>"
    Just index -> do
      todos <- loadTodoList platformKey senderKey
      case todoAt index todos of
        Nothing ->
          void $ Chat.replyTo message [i|没有编号 #{index} 的 todo。|]
        Just todo -> do
          markTodoDone platformKey senderKey todo
          void $ Chat.replyTo message ("已完成 #" <> show index <> ": " <> todo.value.body)

handleClear
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => Text
  -> Text
  -> IncomingMessage
  -> Eff es ()
handleClear platformKey senderKey message = do
  clearTodoList platformKey senderKey
  void $ Chat.replyTo message "已清空 todo list。"

handleRemove
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => Text
  -> Text
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleRemove platformKey senderKey message args =
  case parseIndices args of
    [] ->
      void $ Chat.replyTo message "用法：!rm <编号1> <编号2> ..."
    indices -> do
      todos <- loadTodoList platformKey senderKey
      let rows = mapMaybe (`todoAt` todos) indices
      deleteTodoRows platformKey senderKey (map (.rowId) rows)
      void $ Chat.replyTo message [i|已删除 #{length rows} 项。|]

replyWithList
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => Text
  -> Text
  -> IncomingMessage
  -> Eff es ()
replyWithList platformKey senderKey message = do
  todos <- loadTodoList platformKey senderKey
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

loadTodoList :: Storage.Storage :> es => Text -> Text -> Eff es [StoredTodo]
loadTodoList platformKey senderKey =
  Storage.loadJsonCollection todoItems (todoScope platformKey senderKey)

addTodoItem :: Storage.Storage :> es => Text -> Text -> Text -> Eff es ()
addTodoItem platformKey senderKey body =
  Storage.appendJsonCollection todoItems (todoScope platformKey senderKey) TodoItem{body, done = False}

markTodoDone :: Storage.Storage :> es => Text -> Text -> StoredTodo -> Eff es ()
markTodoDone platformKey senderKey todo = do
  let item = todo.value
  Storage.replaceJsonCollectionItem todoItems (todoScope platformKey senderKey) todo.rowId item{done = True}

deleteTodoRows :: Storage.Storage :> es => Text -> Text -> [Integer] -> Eff es ()
deleteTodoRows platformKey senderKey =
  Storage.deleteJsonCollectionRows todoItems (todoScope platformKey senderKey)

clearTodoList :: Storage.Storage :> es => Text -> Text -> Eff es ()
clearTodoList platformKey senderKey =
  Storage.clearJsonCollection todoItems (todoScope platformKey senderKey)

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
