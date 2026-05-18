module Main (main) where

import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Storage as StorageEffect
import Bot.Core.Message
import Bot.Core.Route
import Bot.Handler.Admin
import Bot.Handler.Admin.Config
import qualified Bot.Lifecycle as Lifecycle
import Bot.Prelude
import qualified Bot.Storage.Lifecycle as LifecycleStorage
import Control.Concurrent (threadDelay)
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "admin"
      [ testCase "ping replies pong for any sender" testPingRepliesPong
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
    runTestLog $
      StorageEffect.runStorageSQLitePath ":memory:" $
        Chat.runChatWith (chatHandlers replies) do
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

runAdminWithDelay :: Int -> AdminConfig -> IORef.IORef [Text] -> IncomingMessage -> IO [LifecycleStorage.StoredStartupAction]
runAdminWithDelay delayMicros cfg replies incoming =
  runEff $
    runTestLog $
      StorageEffect.runStorageSQLitePath ":memory:" $
        Chat.runChatWith (chatHandlers replies) do
            runHandlers (adminHandlers cfg) incoming
            liftIO $ when (delayMicros > 0) (threadDelay delayMicros)
            LifecycleStorage.loadStartupActions

chatHandlers :: IOE :> es => IORef.IORef [Text] -> Chat.ChatHandlers es
chatHandlers replies =
  Chat.ChatHandlers
    { handleReplyTo = reply
    , handleUploadFile = upload
    , handleEditMessage = edit
    , handleDeleteMessage = delete
    , handleReplyStreamStyle = replyStreamStyle
    , handleGetMessageContent = fetch
    , handleGetSenderMemberInfo = fetchSenderMember
    , handleGetMemberInfo = fetchMember
    , handleGetUserAvatar = fetchUserAvatar
    , handleListGroupMembers = listMembers
    , handleMentionUser = mention
    }
  where
    reply _ body = do
      liftIO $ IORef.modifyIORef' replies (<> [body])
      pure (Just "1")
    upload _ _ =
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

runTestLog :: IOE :> es => Eff (Log : es) a -> Eff es a
runTestLog action = do
  logger <- liftIO $ mkLogger "admin-spec" \_ -> pure ()
  runLog "admin-spec" logger LogTrace action

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
