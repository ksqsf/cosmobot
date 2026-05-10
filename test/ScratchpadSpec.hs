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

main :: IO ()
main = do
  testScratchpadTodoFlow
  testScratchpadSenderIsolation

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
  assertEqual
    "scratchpad commands reply with expected todo state"
    [ "已添加 #1: buy milk"
    , "已添加 #2: write tests"
    , "已完成 #1: buy milk"
    , "- [X] 1. buy milk\n- [ ] 2. write tests\n"
    , "已删除 1 项。"
    , "- [ ] 1. write tests\n"
    , "已清空 todo list。"
    , "todo list 为空。"
    ]
    =<< IORef.readIORef replies

testScratchpadSenderIsolation :: IO ()
testScratchpadSenderIsolation = withScratchpadStore "sender-isolation" \store -> do
  replies <- IORef.newIORef ([] :: [Text])
  runScratchpad store replies (messageFrom 200 "!todo alice task")
  runScratchpad store replies (messageFrom 201 "!todo bob task")
  runScratchpad store replies (messageFrom 200 "!list")
  runScratchpad store replies (messageFrom 201 "!list")
  assertEqual
    "scratchpad todo lists are scoped by sender"
    [ "已添加 #1: alice task"
    , "已添加 #1: bob task"
    , "- [ ] 1. alice task\n"
    , "- [ ] 1. bob task\n"
    ]
    =<< IORef.readIORef replies

withScratchpadStore :: String -> (Storage.SQLiteStore -> IO ()) -> IO ()
withScratchpadStore label action = do
  tmp <- Directory.getTemporaryDirectory
  let path = tmp </> ("cosmobot-scratchpad-spec-" <> label <> ".sqlite")
  Directory.removeFile path `Exception.catch` \(_ :: IOException) -> pure ()
  store <- Storage.openSQLiteStore path
  action store

runScratchpad :: Storage.SQLiteStore -> IORef.IORef [Text] -> IncomingMessage -> IO ()
runScratchpad store replies incoming =
  runEff $
    Chat.runChatWith reply fetch fetchSenderMember fetchMember listMembers mention $
      runHandlers (scratchpadHandlers store) incoming
  where
    reply _ body = do
      liftIO $ IORef.modifyIORef' replies (<> [body])
      pure (Just 1)
    fetch _ _ =
      pure Nothing
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

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  unless (expected == actual) $
    fail (label <> ": expected " <> show expected <> ", got " <> show actual)
