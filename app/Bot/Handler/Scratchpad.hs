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
      liftIO (Storage.addTodoItem store platformKey senderKey body)
      todos <- liftIO (Storage.loadTodoList store platformKey senderKey)
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
      todos <- liftIO (Storage.loadTodoList store platformKey senderKey)
      case todoAt index todos of
        Nothing ->
          void $ Chat.replyTo message [i|没有编号 #{index} 的 todo。|]
        Just todo -> do
          liftIO (Storage.markTodoDone store platformKey senderKey todo.rowId)
          void $ Chat.replyTo message ("已完成 #" <> show index <> ": " <> todo.body)

handleClear
  :: (Chat.Chat :> es, IOE :> es)
  => Storage.SQLiteStore
  -> Text
  -> Text
  -> IncomingMessage
  -> Eff es ()
handleClear store platformKey senderKey message = do
  liftIO (Storage.clearTodoList store platformKey senderKey)
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
      todos <- liftIO (Storage.loadTodoList store platformKey senderKey)
      let rows = mapMaybe (`todoAt` todos) indices
      liftIO (Storage.deleteTodoRows store platformKey senderKey (map (.rowId) rows))
      void $ Chat.replyTo message [i|已删除 #{length rows} 项。|]

replyWithList
  :: (Chat.Chat :> es, IOE :> es)
  => Storage.SQLiteStore
  -> Text
  -> Text
  -> IncomingMessage
  -> Eff es ()
replyWithList store platformKey senderKey message = do
  todos <- liftIO (Storage.loadTodoList store platformKey senderKey)
  void $ Chat.replyTo message (renderTodoList todos)

renderTodoList :: [Storage.StoredTodo] -> Text
renderTodoList [] =
  "todo list 为空。"
renderTodoList todos =
  Text.unlines (zipWith render [1 :: Int ..] todos)
  where
    render index todo =
      "- [" <> status todo <> "] " <> show index <> ". " <> todo.body
    status todo
      | todo.done = "X"
      | otherwise = " "

todoAt :: Int -> [Storage.StoredTodo] -> Maybe Storage.StoredTodo
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
