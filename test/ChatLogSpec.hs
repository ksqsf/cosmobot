module Main (main) where

import qualified Bot.Effect.ChatLog as ChatLog
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

testBotMessageVisibility :: IO ()
testBotMessageVisibility = runChatLogTest do
  let context = messageFromChat 100 200 "user"
  ChatLog.recordMessage context
  ChatLog.recordBotMessage context (Just 300) "bot reply"
  userOnly <- ChatLog.queryChat context 10 False
  withBot <- ChatLog.queryChat context 10 True
  liftIO $ map (.text) userOnly @?= ["user"]
  liftIO $ map (.text) withBot @?= ["user", "bot reply"]
  liftIO $ map (.isBot) withBot @?= [False, True]

testImageSanitization :: IO ()
testImageSanitization = runChatLogTest do
  ChatLog.recordMessage (messageFromChatWithImages 100 200 "look" [base64Image])
  ChatLog.recordBotMessage (messageFromChat 100 200 "user") (Just 300) ("[image] " <> base64Image)
  entries <- ChatLog.queryChat (messageFromChat 999 200 "query") 10 True
  liftIO $ map (.imageUrls) entries @?= [["[Picture]"], ["[Picture]"]]
  liftIO $ map (.text) entries @?= ["look", ""]

runChatLogTest :: Eff '[ChatLog.ChatLog, Log, IOE] a -> IO a
runChatLogTest action =
  runEff $ runTestLog $ ChatLog.runChatLog Nothing action

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
    , senderId = Just 200
    , senderUsername = Just "alice"
    , messageId = Just messageId
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = imageUrls
    , text = text
    , raw = Aeson.Null
    }

base64Image :: Text
base64Image =
  "data:image/png;base64,AAAA"
