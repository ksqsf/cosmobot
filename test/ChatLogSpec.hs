module Main (main) where

import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Storage.SQLite as StorageSQLite
import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "chat log"
      [ testCase "queries current chat in chronological order" testQueryCurrentChat
      , testCase "queries current sender chat log newest first by keyword groups" testQueryCurrentSenderChatLog
      , testCase "bot messages are hidden unless requested" testBotMessageVisibility
      , testCase "base64 image references are sanitized" testImageSanitization
      ]

testQueryCurrentChat :: IO ()
testQueryCurrentChat = runChatLogTest do
  ChatLog.recordMessage (messageFromChat 100 200 "first")
  ChatLog.recordMessage (messageFromChat 101 200 "second")
  ChatLog.recordMessage (messageFromChat 102 201 "other chat")
  entries <- ChatLog.queryChat (messageFromChat 999 200 "query") 10 False
  liftIO $ map (.text) entries @?= ["first", "second"]

testQueryCurrentSenderChatLog :: IO ()
testQueryCurrentSenderChatLog = runChatLogTest do
  ChatLog.recordMessage (messageFromChat 100 200 "older alpha beta")
  ChatLog.recordMessage (messageFromSenderInChat "201" 101 200 "other sender alpha beta")
  ChatLog.recordMessage (messageFromChat 102 201 "other chat alpha beta")
  ChatLog.recordMessage (messageFromChat 103 200 "middle alpha then beta")
  ChatLog.recordMessage (messageFromChat 104 200 "new beta then alpha")
  ChatLog.recordMessage (messageFromChat 105 200 "new alpha gamma")
  entries <- ChatLog.queryCurrentSenderChatLog (messageFromChat 999 200 "query") [["alpha", "beta"], ["gamma"]] 10
  limited <- ChatLog.queryCurrentSenderChatLog (messageFromChat 999 200 "query") [["alpha", "beta"], ["gamma"]] 2
  liftIO $ map (.text) entries @?= ["new alpha gamma", "middle alpha then beta", "older alpha beta"]
  liftIO $ map (.text) limited @?= ["new alpha gamma", "middle alpha then beta"]

testBotMessageVisibility :: IO ()
testBotMessageVisibility = runChatLogTest do
  let context = messageFromChat 100 200 "user"
  ChatLog.recordMessage context
  ChatLog.recordSelfMessage context "bot reply"
  userOnly <- ChatLog.queryChat context 10 False
  withBot <- ChatLog.queryChat context 10 True
  liftIO $ map (.text) userOnly @?= ["user"]
  liftIO $ map (.text) withBot @?= ["user", "bot reply"]
  liftIO $ map (.isBot) withBot @?= [False, True]
  liftIO $ map (.messageId) withBot @?= [Just "100", Nothing]

testImageSanitization :: IO ()
testImageSanitization = runChatLogTest do
  ChatLog.recordMessage (messageFromChatWithImages 100 200 "look" [base64Image])
  ChatLog.recordSelfMessage (messageFromChat 100 200 "user") ("[image] " <> base64Image)
  entries <- ChatLog.queryChat (messageFromChat 999 200 "query") 10 True
  liftIO $ map (.imageUrls) entries @?= [["[Picture]"], ["[Picture]"]]
  liftIO $ map (.text) entries @?= ["look", ""]

runChatLogTest :: Eff '[ChatLog.ChatLog, Storage.Storage, Log, IOE] a -> IO a
runChatLogTest action =
  runEff $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ ChatLog.runChatLog action

runTestLog :: IOE :> es => Eff (Log : es) a -> Eff es a
runTestLog action = do
  logger <- liftIO $ mkLogger "chat-log-spec" \_ -> pure ()
  runLog "chat-log-spec" logger LogTrace action

messageFromChat :: Integer -> Integer -> Text -> IncomingMessage
messageFromChat messageId chatId text =
  messageFromChatWithImages messageId chatId text []

messageFromChatWithImages :: Integer -> Integer -> Text -> [Text] -> IncomingMessage
messageFromChatWithImages messageId chatId text imageUrls =
  IncomingMessage
    { platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just chatId
    , chatAliases = []
    , digest = emptyMessageDigest
    , senderId = Just "200"
    , senderUsername = Just "alice"
    , messageId = Just (integerMessageId messageId)
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = imageUrls
    , text = text
    , raw = Aeson.Null
    }

messageFromSenderInChat :: Text -> Integer -> Integer -> Text -> IncomingMessage
messageFromSenderInChat sender messageId chatId text =
  (messageFromChat messageId chatId text)
    { senderId = Just sender
    , senderUsername = Nothing
    }

base64Image :: Text
base64Image =
  "data:image/png;base64,AAAA"
