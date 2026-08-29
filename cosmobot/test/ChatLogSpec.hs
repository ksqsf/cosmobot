module Main (main) where

import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Storage.Identity as Identity
import qualified Bot.Storage.SQLite as StorageSQLite
import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "chat log"
      [ testCase "queries current chat in chronological order" testQueryCurrentChat
      , testCase "filters current chat by sender" testQueryCurrentChatBySender
      , testCase "queries current sender chat log newest first by keyword groups" testQueryCurrentSenderChatLog
      , testCase "queries current sender across chats in global scope" testQueryCurrentSenderGlobalChatLog
      , testCase "queries a bounded chat-log time window" testQueryTimeWindow
      , testCase "bot messages are hidden unless requested" testBotMessageVisibility
      , testCase "base64 image references are sanitized" testImageSanitization
      , testCase "lists chats and queries a message window" testInspectionWindow
      , testCase "records sender and chat identity with the message" testIdentityDirectory
      ]

testQueryCurrentChat :: IO ()
testQueryCurrentChat = runChatLogTest do
  ChatLog.recordMessage (messageFromChat 100 200 "first")
  ChatLog.recordMessage (messageFromChat 101 200 "second")
  ChatLog.recordMessage (messageFromChat 102 201 "other chat")
  entries <- ChatLog.queryChat (messageFromChat 999 200 "query") Nothing 10 False ChatLog.unboundedChatLogTimeRange
  liftIO $ map (.text) entries @?= ["first", "second"]

testQueryCurrentChatBySender :: IO ()
testQueryCurrentChatBySender = runChatLogTest do
  ChatLog.recordMessage (messageFromChat 100 200 "alice")
  ChatLog.recordMessage (messageFromSenderInChat "201" 101 200 "bob")
  ChatLog.recordMessage (messageFromSenderInChat "201" 102 201 "bob elsewhere")
  entries <- ChatLog.queryChat (messageFromChat 999 200 "query") (Just "201") 10 False ChatLog.unboundedChatLogTimeRange
  liftIO $ map (.text) entries @?= ["bob"]

testQueryCurrentSenderChatLog :: IO ()
testQueryCurrentSenderChatLog = runChatLogTest do
  ChatLog.recordMessage (messageFromChat 100 200 "older alpha beta")
  ChatLog.recordMessage (messageFromSenderInChat "201" 101 200 "other sender alpha beta")
  ChatLog.recordMessage (messageFromChat 102 201 "other chat alpha beta")
  ChatLog.recordMessage (messageFromChat 103 200 "middle alpha then beta")
  ChatLog.recordMessage (messageFromChat 104 200 "new beta then alpha")
  ChatLog.recordMessage (messageFromChat 105 200 "new alpha gamma")
  entries <- ChatLog.queryCurrentSenderChatLog (messageFromChat 999 200 "query") ChatLog.SenderChatLogChat [["alpha", "beta"], ["gamma"]] 10 ChatLog.unboundedChatLogTimeRange
  limited <- ChatLog.queryCurrentSenderChatLog (messageFromChat 999 200 "query") ChatLog.SenderChatLogChat [["alpha", "beta"], ["gamma"]] 2 ChatLog.unboundedChatLogTimeRange
  liftIO $ map (.text) entries @?= ["new alpha gamma", "middle alpha then beta", "older alpha beta"]
  liftIO $ map (.text) limited @?= ["new alpha gamma", "middle alpha then beta"]

testQueryCurrentSenderGlobalChatLog :: IO ()
testQueryCurrentSenderGlobalChatLog = runChatLogTest do
  ChatLog.recordMessage (messageFromChat 100 200 "same chat needle")
  ChatLog.recordMessage (messageFromChat 101 201 "other chat needle")
  ChatLog.recordMessage (messageFromSenderInChat "201" 102 202 "other sender needle")
  ChatLog.recordMessage ((messageFromChat 103 203 "other platform needle"){platform = PlatformQQ})
  entries <- ChatLog.queryCurrentSenderChatLog (messageFromChat 999 200 "query") ChatLog.SenderChatLogGlobal [["needle"]] 10 ChatLog.unboundedChatLogTimeRange
  liftIO $ map (.text) entries @?= ["other chat needle", "same chat needle"]

testQueryTimeWindow :: IO ()
testQueryTimeWindow = runChatLogTest do
  let context = messageFromChat 999 200 "query"
  ChatLog.recordMessage (messageFromChat 100 200 "older")
  boundary <- liftIO getCurrentTime
  ChatLog.recordMessage (messageFromChat 101 200 "newer")
  newer <- ChatLog.queryChat context Nothing 10 False ChatLog.ChatLogTimeRange{since = Just boundary, before = Nothing}
  older <- ChatLog.queryChat context Nothing 10 False ChatLog.ChatLogTimeRange{since = Nothing, before = Just boundary}
  liftIO $ map (.text) newer @?= ["newer"]
  liftIO $ map (.text) older @?= ["older"]
  liftIO $ assertBool "queried messages include timestamps" (all (isJust . (.recordedAt)) (newer <> older))

testBotMessageVisibility :: IO ()
testBotMessageVisibility = runChatLogTest do
  let context = messageFromChat 100 200 "user"
  ChatLog.recordMessage context
  ChatLog.recordSelfMessage context (Just "bot-1") "bot reply"
  userOnly <- ChatLog.queryChat context Nothing 10 False ChatLog.unboundedChatLogTimeRange
  withBot <- ChatLog.queryChat context Nothing 10 True ChatLog.unboundedChatLogTimeRange
  liftIO $ map (.text) userOnly @?= ["user"]
  liftIO $ map (.text) withBot @?= ["user", "bot reply"]
  liftIO $ map (.isBot) withBot @?= [False, True]
  liftIO $ map (.messageId) withBot @?= [Just "100", Just "bot-1"]

testImageSanitization :: IO ()
testImageSanitization = runChatLogTest do
  ChatLog.recordMessage (messageFromChatWithFiles 100 200 "look" [base64Image] [MessageFile "notes.txt" "https://example.com/notes.txt"])
  ChatLog.recordSelfMessageWithFiles (messageFromChat 100 200 "user") Nothing ("[image] " <> base64Image) [MessageFile "image.png" base64Image]
  entries <- ChatLog.queryChat (messageFromChat 999 200 "query") Nothing 10 True ChatLog.unboundedChatLogTimeRange
  liftIO $ map (.imageUrls) entries @?= [["[Picture]"], ["[Picture]"]]
  liftIO $ map (.files) entries @?= [[MessageFile "notes.txt" "https://example.com/notes.txt"], [MessageFile "image.png" "[Picture]"]]
  liftIO $ map (.text) entries @?= ["look", ""]

testInspectionWindow :: IO ()
testInspectionWindow = runChatLogTest do
  traverse_ (\messageId -> ChatLog.recordMessage (messageFromChat messageId 200 [i|message #{messageId}|])) [100 .. 104]
  ChatLog.recordMessage ((messageFromChat 200 201 "other chat"){platform = PlatformQQ})
  summaries <- ChatLog.listChats
  let scope = ChatLog.ChatLogScope PlatformTelegram ChatPrivate (Just 200)
  window <- ChatLog.queryWindow scope (ChatLog.AroundChatLogMessage "102") 3
  missing <- ChatLog.queryWindow scope (ChatLog.AroundChatLogMessage "missing") 3
  liftIO $ do
    map (.messageCount) summaries @?= [1, 5]
    map ((.text) . (.entry)) window.entries @?= ["message 101", "message 102", "message 103"]
    window.hasOlder @? "message window has an older page"
    window.hasNewer @? "message window has a newer page"
    window.anchorFound @? "message anchor is present"
    assertBool "missing anchor is reported" (not missing.anchorFound)

testIdentityDirectory :: IO ()
testIdentityDirectory = runChatLogTest do
  ChatLog.recordMessage (messageFromChat 100 200 "hello")
  ChatLog.recordMessage ((messageFromChat 101 300 "group")
    { platform = PlatformQQ
    , kind = ChatGroup
    , chatDisplayName = Just "QQ group"
    })
  ChatLog.recordMessage ((messageFromChat 102 300 "private")
    { platform = PlatformQQ
    , chatDisplayName = Just "QQ user"
    })
  senders <- Identity.loadSenderInfos [(PlatformTelegram, "200")]
  chats <- Identity.loadChatInfos [(PlatformTelegram, 200)]
  qqChats <- Identity.loadScopedChatInfos
    [ (PlatformQQ, ChatGroup, 300)
    , (PlatformQQ, ChatPrivate, 300)
    ]
  let bulkScopes = [(PlatformQQ, ChatGroup, chatId) | chatId <- [400 .. 439]]
  traverse_ (\(_, _, chatId) -> ChatLog.recordMessage ((messageFromChat chatId chatId "bulk")
    { platform = PlatformQQ
    , kind = ChatGroup
    , chatDisplayName = Just [i|Group #{chatId}|]
    })) bulkScopes
  bulkChats <- Identity.loadScopedChatInfos bulkScopes
  liftIO $ do
    Map.lookup (PlatformTelegram, "200") senders @?= Just Identity.SenderInfo
      { displayName = Just "Alice"
      , username = Just "alice"
      }
    Map.lookup (PlatformTelegram, 200) chats @?= Just (Just "Example chat")
    Map.lookup (PlatformQQ, ChatGroup, 300) qqChats @?= Just (Just "QQ group")
    Map.lookup (PlatformQQ, ChatPrivate, 300) qqChats @?= Just (Just "QQ user")
    Map.size bulkChats @?= length bulkScopes

runChatLogTest :: Eff '[ChatLog.ChatLog, Storage.Storage, KatipE, Concurrent, IOE] a -> IO a
runChatLogTest action =
  runEff $ runConcurrent $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ ChatLog.runChatLog action

runTestLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runTestLog action = startKatipE "chat-log-spec" "test" action

messageFromChat :: Integer -> Integer -> Text -> IncomingMessage
messageFromChat messageId chatId text =
  messageFromChatWithImages messageId chatId text []

messageFromChatWithImages :: Integer -> Integer -> Text -> [Text] -> IncomingMessage
messageFromChatWithImages messageId chatId text imageUrls =
  messageFromChatWithFiles messageId chatId text imageUrls []

messageFromChatWithFiles :: Integer -> Integer -> Text -> [Text] -> [MessageFile] -> IncomingMessage
messageFromChatWithFiles messageId chatId text imageUrls files =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just chatId
    , chatAliases = []
    , chatDisplayName = Just "Example chat"
    , digest = emptyMessageDigest
    , senderId = Just "200"
    , senderUsername = Just "alice"
    , senderDisplayName = Just "Room Alice"
    , senderGlobalDisplayName = Just "Alice"
    , messageId = Just (integerMessageId messageId)
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = imageUrls
    , files
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
