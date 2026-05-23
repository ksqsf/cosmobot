module Main (main) where

import qualified Bot.Effect.Chat as Chat
import qualified Bot.Storage.SQLite as StorageSQLite
import Bot.Core.Message
import Bot.Core.Route
import Bot.Handler.Admin
import Bot.Handler.Admin.Config
import qualified Bot.Lifecycle as Lifecycle
import Bot.Prelude
import qualified Bot.Storage.Lifecycle as LifecycleStorage
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import Effectful.FileSystem (runFileSystem)
import Effectful.Process (runProcess)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "admin"
      [ testCase "ping replies pong for any sender" testPingRepliesPong
      , testCase "title rejects non-superusers" testTitleRejectsNonSuperuser
      , testCase "title validates arguments" testTitleValidatesArguments
      , testCase "title sets group member title" testTitleSetsGroupMemberTitle
      , testCase "title reports unsupported platform failure" testTitleReportsUnsupportedPlatformFailure
      , testCase "upgrade rejects non-superusers" testUpgradeRejectsNonSuperuser
      , testCase "upgrade reports script exit status" testUpgradeReportsScriptExitStatus
      , testCase "upgrade reports nonzero script exit status" testUpgradeReportsNonzeroScriptExitStatus
      , testCase "lifecycle startup replies are deleted after drain" testLifecycleStartupRepliesAreDeletedAfterDrain
      ]

testPingRepliesPong :: IO ()
testPingRepliesPong = do
  replies <- IORef.newIORef ([] :: [Text])
  actions <- runAdmin defaultAdminConfig replies message
  IORef.readIORef replies >>= (@?= ["pong"])
  assertBool "no startup actions queued" (null actions)

testTitleRejectsNonSuperuser :: IO ()
testTitleRejectsNonSuperuser = do
  replies <- IORef.newIORef ([] :: [Text])
  titleCalls <- IORef.newIORef ([] :: [(Maybe Integer, Text, Text)])
  actions <- runAdminWithTitle defaultAdminConfig replies titleCalls True (groupMessageWith "!title 200 cool" emptyMessageDigest)
  IORef.readIORef replies >>= (@?= ["只有 superuser 可以设置 title。"])
  IORef.readIORef titleCalls >>= (@?= [])
  assertBool "no startup actions queued" (null actions)

testTitleValidatesArguments :: IO ()
testTitleValidatesArguments = do
  replies <- IORef.newIORef ([] :: [Text])
  titleCalls <- IORef.newIORef ([] :: [(Maybe Integer, Text, Text)])
  actions <- runAdminWithTitle defaultAdminConfig replies titleCalls True (groupMessageWith "!title 0 cool" emptyMessageDigest{senderIsSuperuser = True})
  IORef.readIORef replies >>= (@?= ["用法：!title <id> <title>"])
  IORef.readIORef titleCalls >>= (@?= [])
  assertBool "no startup actions queued" (null actions)

testTitleSetsGroupMemberTitle :: IO ()
testTitleSetsGroupMemberTitle = do
  replies <- IORef.newIORef ([] :: [Text])
  titleCalls <- IORef.newIORef ([] :: [(Maybe Integer, Text, Text)])
  actions <- runAdminWithTitle defaultAdminConfig replies titleCalls True (groupMessageWith "!title 200 very cool" emptyMessageDigest{senderIsSuperuser = True})
  IORef.readIORef replies >>= (@?= ["已设置 200 的 title：very cool"])
  IORef.readIORef titleCalls >>= (@?= [(Just 100, "200", "very cool")])
  assertBool "no startup actions queued" (null actions)

testTitleReportsUnsupportedPlatformFailure :: IO ()
testTitleReportsUnsupportedPlatformFailure = do
  replies <- IORef.newIORef ([] :: [Text])
  titleCalls <- IORef.newIORef ([] :: [(Maybe Integer, Text, Text)])
  actions <- runAdminWithTitle defaultAdminConfig replies titleCalls False (groupMessageWith "!title 200 very cool" emptyMessageDigest{senderIsSuperuser = True})
  IORef.readIORef replies >>= (@?= ["设置 title 失败：当前平台可能不支持，或 bot 权限不足。"])
  IORef.readIORef titleCalls >>= (@?= [(Just 100, "200", "very cool")])
  assertBool "no startup actions queued" (null actions)

testUpgradeRejectsNonSuperuser :: IO ()
testUpgradeRejectsNonSuperuser = do
  replies <- IORef.newIORef ([] :: [Text])
  actions <- runAdmin upgradeConfig replies (messageWith "!upgrade" emptyMessageDigest)
  IORef.readIORef replies >>= (@?= ["只有 superuser 可以执行 upgrade。"])
  assertBool "no startup actions queued" (null actions)

testUpgradeReportsScriptExitStatus :: IO ()
testUpgradeReportsScriptExitStatus = do
  replies <- IORef.newIORef ([] :: [Text])
  actions <- runAdminSettled upgradeConfig replies (messageWith "!upgrade" emptyMessageDigest{senderIsSuperuser = True})
  captured <- IORef.readIORef replies
  case captured of
    [started, exited] -> do
      started @?= "已启动 upgrade 脚本：/bin/true"
      exited @?= "upgrade 脚本已退出，exitcode=0。"
    _ ->
      assertFailure [i|unexpected replies: #{show captured :: String}|]
  assertBool "startup action deleted after script return" (null actions)

testUpgradeReportsNonzeroScriptExitStatus :: IO ()
testUpgradeReportsNonzeroScriptExitStatus = do
  replies <- IORef.newIORef ([] :: [Text])
  actions <- runAdminSettled (upgradeConfigFor "/bin/false") replies (messageWith "!upgrade" emptyMessageDigest{senderIsSuperuser = True})
  captured <- IORef.readIORef replies
  case captured of
    [started, exited] -> do
      started @?= "已启动 upgrade 脚本：/bin/false"
      exited @?= "upgrade 脚本已退出，exitcode=1。"
    _ ->
      assertFailure [i|unexpected replies: #{show captured :: String}|]
  assertBool "startup action deleted after script failure" (null actions)

testLifecycleStartupRepliesAreDeletedAfterDrain :: IO ()
testLifecycleStartupRepliesAreDeletedAfterDrain = do
  replies <- IORef.newIORef ([] :: [Text])
  remaining <- runEff $
    runConcurrent $
    runTestLog $
      StorageSQLite.runStorageSQLitePath ":memory:" $
        Chat.runChatWith (chatHandlers replies Nothing False) do
          void $ LifecycleStorage.enqueueStartupReply "test-startup-reply" message "cosmobot 重启完成啦 (｡•̀ᴗ-)✧"
          Lifecycle.runLifecycle (pure ())
          LifecycleStorage.loadStartupActions
  IORef.readIORef replies >>= (@?= ["cosmobot 重启完成啦 (｡•̀ᴗ-)✧"])
  assertBool "startup actions deleted after drain" (null remaining)

upgradeConfig :: AdminConfig
upgradeConfig =
  upgradeConfigFor "/bin/true"

upgradeConfigFor :: FilePath -> AdminConfig
upgradeConfigFor script =
  defaultAdminConfig
    { upgrade = Just UpgradeConfig{script}
    }

runAdmin :: AdminConfig -> IORef.IORef [Text] -> IncomingMessage -> IO [LifecycleStorage.StoredStartupAction]
runAdmin cfg replies incoming =
  runAdminWithDelay 0 cfg replies incoming

runAdminSettled :: AdminConfig -> IORef.IORef [Text] -> IncomingMessage -> IO [LifecycleStorage.StoredStartupAction]
runAdminSettled cfg replies incoming =
  runAdminWithDelay 100_000 cfg replies incoming

runAdminWithTitle
  :: AdminConfig
  -> IORef.IORef [Text]
  -> IORef.IORef [(Maybe Integer, Text, Text)]
  -> Bool
  -> IncomingMessage
  -> IO [LifecycleStorage.StoredStartupAction]
runAdminWithTitle cfg replies titleCalls titleResult incoming =
  runAdminWithDelayAndTitle 0 cfg replies (Just titleCalls) titleResult incoming

runAdminWithDelay :: Int -> AdminConfig -> IORef.IORef [Text] -> IncomingMessage -> IO [LifecycleStorage.StoredStartupAction]
runAdminWithDelay delayMicros cfg replies incoming =
  runAdminWithDelayAndTitle delayMicros cfg replies Nothing False incoming

runAdminWithDelayAndTitle
  :: Int
  -> AdminConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [(Maybe Integer, Text, Text)])
  -> Bool
  -> IncomingMessage
  -> IO [LifecycleStorage.StoredStartupAction]
runAdminWithDelayAndTitle delayMicros cfg replies titleCalls titleResult incoming =
  runEff $
    runConcurrent $
    runTestLog $
      StorageSQLite.runStorageSQLitePath ":memory:" $
        Chat.runChatWith (chatHandlers replies titleCalls titleResult) do
          runAdminStack do
            runHandlers (adminHandlers cfg) incoming
            when (delayMicros > 0) (threadDelay delayMicros)
          LifecycleStorage.loadStartupActions
  where
    runAdminStack =
      runFileSystem
        . runProcess
        . runConcurrent

chatHandlers
  :: IOE :> es
  => IORef.IORef [Text]
  -> Maybe (IORef.IORef [(Maybe Integer, Text, Text)])
  -> Bool
  -> Chat.ChatHandlers es
chatHandlers replies titleCalls titleResult =
  Chat.ChatHandlers
    { handleReplyTo = reply
    , handleUploadFile = upload
    , handleReplyAudio = replyAudio
    , handleEditMessage = edit
    , handleDeleteMessage = delete
    , handleReplyStreamStyle = replyStreamStyle
    , handleGetMessageContent = fetch
    , handleGetSenderMemberInfo = fetchSenderMember
    , handleGetMemberInfo = fetchMember
    , handleGetUserAvatar = fetchUserAvatar
    , handleListGroupMembers = listMembers
    , handleMentionUser = mention
    , handleSetMemberTitle = setMemberTitle
    }
  where
    reply _ body = do
      liftIO $ IORef.modifyIORef' replies (<> [body])
      pure (Just "1")
    upload _ _ =
      pure (Right Nothing)
    replyAudio _ _ _ =
      pure (Right Nothing)
    edit _ _ _ =
      pure False
    delete _ _ =
      pure False
    replyStreamStyle _ =
      pure (Chat.ChunkedReply 1800)
    fetch _ _ =
      pure Nothing
    fetchSenderMember _ =
      pure Nothing
    fetchMember _ _ =
      pure Nothing
    fetchUserAvatar _ _ =
      pure Nothing
    listMembers _ =
      pure Nothing
    mention _ _ _ =
      pure Nothing
    setMemberTitle incoming userId title = do
      traverse_ (\ref -> liftIO $ IORef.modifyIORef' ref (<> [(incoming.chatId, userId, title)])) titleCalls
      pure titleResult

runTestLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runTestLog action = startKatipE "admin-spec" "test" action

message :: IncomingMessage
message =
  messageWith "!ping" emptyMessageDigest

messageWith :: Text -> MessageDigest -> IncomingMessage
messageWith body digest =
  IncomingMessage
    { platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just 100
    , chatAliases = []
    , digest = digest
    , senderId = Just "200"
    , senderUsername = Just "alice"
    , messageId = Just "300"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , text = body
    , raw = Aeson.Null
    }

groupMessageWith :: Text -> MessageDigest -> IncomingMessage
groupMessageWith body digest =
  (messageWith body digest)
    { platform = PlatformQQ
    , kind = ChatGroup
    , chatId = Just 100
    }
