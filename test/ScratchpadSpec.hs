module Main (main) where

import qualified Bot.Effect.Chat as Chat
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import Bot.Filter
import Bot.Handler.Scratchpad
import Bot.Message
import Bot.Prelude
import qualified Bot.Storage.SQLite as Storage
import qualified Control.Exception as Exception
import qualified System.Directory as Directory
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "scratchpad"
      [ testCase "todo command flow" testScratchpadTodoFlow
      , testCase "todo lists are scoped by sender" testScratchpadSenderIsolation
      , testCase "todo list persists through SQLite" testScratchpadPersistence
      , testCase "invalid commands reply with usage or missing item" testScratchpadInvalidCommands
      , testCase "messages without sender are rejected" testScratchpadMissingSender
      ]

testScratchpadTodoFlow :: IO ()
testScratchpadTodoFlow = withScratchpadStore "flow" \store -> do
  replies <- IORef.newIORef ([] :: [Text])
  runScratchpad store replies (message "!todo buy milk")
  runScratchpad store replies (message "!todo write tests")
  runScratchpad store replies (message "!done 1")
  runScratchpad store replies (message "!list")
  runScratchpad store replies (message "!rm 1")
  runScratchpad store replies (message "!todo")
  runScratchpad store replies (message "!clear")
  runScratchpad store replies (message "!list")
  IORef.readIORef replies
    >>= (@?=)
    [ "已添加 #1: buy milk"
    , "已添加 #2: write tests"
    , "已完成 #1: buy milk"
    , "- [X] 1. buy milk\n- [ ] 2. write tests\n"
    , "已删除 1 项。"
    , "- [ ] 1. write tests\n"
    , "已清空 todo list。"
    , "todo list 为空。"
    ]

testScratchpadSenderIsolation :: IO ()
testScratchpadSenderIsolation = withScratchpadStore "sender-isolation" \store -> do
  replies <- IORef.newIORef ([] :: [Text])
  runScratchpad store replies (messageFrom 200 "!todo alice task")
  runScratchpad store replies (messageFrom 201 "!todo bob task")
  runScratchpad store replies (messageFrom 200 "!list")
  runScratchpad store replies (messageFrom 201 "!list")
  IORef.readIORef replies
    >>= (@?=)
    [ "已添加 #1: alice task"
    , "已添加 #1: bob task"
    , "- [ ] 1. alice task\n"
    , "- [ ] 1. bob task\n"
    ]

testScratchpadPersistence :: IO ()
testScratchpadPersistence = withScratchpadPath "persistence" \path -> do
  writeReplies <- IORef.newIORef ([] :: [Text])
  writeStore <- Storage.openSQLiteStore path
  runScratchpad writeStore writeReplies (message "!todo persists")
  readReplies <- IORef.newIORef ([] :: [Text])
  readStore <- Storage.openSQLiteStore path
  runScratchpad readStore readReplies (message "!list")
  IORef.readIORef readReplies >>= (@?= ["- [ ] 1. persists\n"])

testScratchpadInvalidCommands :: IO ()
testScratchpadInvalidCommands = withScratchpadStore "invalid-commands" \store -> do
  replies <- IORef.newIORef ([] :: [Text])
  runScratchpad store replies (message "!done")
  runScratchpad store replies (message "!done nope")
  runScratchpad store replies (message "!todo only task")
  runScratchpad store replies (message "!done 2")
  runScratchpad store replies (message "!rm")
  IORef.readIORef replies
    >>= (@?=)
      [ "用法：!done <todo编号>"
      , "用法：!done <todo编号>"
      , "已添加 #1: only task"
      , "没有编号 2 的 todo。"
      , "用法：!rm <编号1> <编号2> ..."
      ]

testScratchpadMissingSender :: IO ()
testScratchpadMissingSender = withScratchpadStore "missing-sender" \store -> do
  replies <- IORef.newIORef ([] :: [Text])
  runScratchpad store replies (messageWithoutSender "!todo unsaved")
  IORef.readIORef replies >>= (@?= ["无法识别发送者，不能保存 todo。"])

withScratchpadStore :: String -> (Storage.SQLiteStore -> IO ()) -> IO ()
withScratchpadStore label action =
  withScratchpadPath label \path -> do
    store <- Storage.openSQLiteStore path
    action store

withScratchpadPath :: String -> (FilePath -> IO ()) -> IO ()
withScratchpadPath label action = do
  tmp <- Directory.getTemporaryDirectory
  let path = tmp </> ("cosmobot-scratchpad-spec-" <> label <> ".sqlite")
  Directory.removeFile path `Exception.catch` \(_ :: IOException) -> pure ()
  action path

runScratchpad :: Storage.SQLiteStore -> IORef.IORef [Text] -> IncomingMessage -> IO ()
runScratchpad store replies incoming =
  runEff $
    Chat.runChatWith reply edit replyStreamStyle fetch fetchSenderMember fetchMember listMembers mention $
      runHandlers (scratchpadHandlers store) incoming
  where
    reply _ body = do
      liftIO $ IORef.modifyIORef' replies (<> [body])
      pure (Just 1)
    fetch _ _ =
      pure Nothing
    edit _ _ _ =
      pure False
    replyStreamStyle _ =
      pure (Chat.ChunkedReply 1800)
    fetchSenderMember _ =
      pure Nothing
    fetchMember _ _ =
      pure Nothing
    listMembers _ =
      pure Nothing
    mention _ _ _ =
      pure Nothing

message :: Text -> IncomingMessage
message =
  messageFrom 200

messageFrom :: Integer -> Text -> IncomingMessage
messageFrom senderId text =
  IncomingMessage
    { platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just 100
    , senderId = Just senderId
    , senderUsername = Just "alice"
    , messageId = Just 300
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , text = text
    , raw = Aeson.Null
    }

messageWithoutSender :: Text -> IncomingMessage
messageWithoutSender text =
  (message text)
    { senderId = Nothing
    , senderUsername = Nothing
    }
