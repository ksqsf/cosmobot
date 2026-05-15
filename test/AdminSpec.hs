module Main (main) where

import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Core.Route
import Bot.Handler.Admin
import Bot.Handler.Admin.Config
import Bot.Prelude
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
      , testCase "upgrade runs configured script for superusers" testUpgradeRunsConfiguredScript
      ]

testPingRepliesPong :: IO ()
testPingRepliesPong = do
  replies <- IORef.newIORef ([] :: [Text])
  runAdmin defaultAdminConfig replies message
  IORef.readIORef replies >>= (@?= ["pong"])

testUpgradeRejectsNonSuperuser :: IO ()
testUpgradeRejectsNonSuperuser = do
  replies <- IORef.newIORef ([] :: [Text])
  runAdmin upgradeConfig replies (message{text = "!upgrade"} :: IncomingMessage)
  IORef.readIORef replies >>= (@?= ["只有 superuser 可以执行 upgrade。"])

testUpgradeRunsConfiguredScript :: IO ()
testUpgradeRunsConfiguredScript = do
  replies <- IORef.newIORef ([] :: [Text])
  runAdmin upgradeConfig replies ((message{text = "!upgrade", digest = emptyMessageDigest{senderIsSuperuser = True}}) :: IncomingMessage)
  captured <- IORef.readIORef replies
  case captured of
    [started, finished] -> do
      started @?= "开始执行 upgrade 脚本：/bin/true"
      finished @?= "upgrade 脚本执行成功。"
    _ ->
      assertFailure [i|unexpected replies: #{show captured :: String}|]

upgradeConfig :: AdminConfig
upgradeConfig =
  defaultAdminConfig
    { upgrade = Just UpgradeConfig{script = "/bin/true"}
    }

runAdmin :: AdminConfig -> IORef.IORef [Text] -> IncomingMessage -> IO ()
runAdmin cfg replies incoming =
  runEff $
    Chat.runChatWith Chat.ChatHandlers
      { handleReplyTo = reply
      , handleEditMessage = edit
      , handleDeleteMessage = delete
      , handleReplyStreamStyle = replyStreamStyle
      , handleGetMessageContent = fetch
      , handleGetSenderMemberInfo = fetchSenderMember
      , handleGetMemberInfo = fetchMember
      , handleGetUserAvatar = fetchUserAvatar
      , handleListGroupMembers = listMembers
      , handleMentionUser = mention
      } $
      runHandlers (adminHandlers cfg) incoming
  where
    reply _ body = do
      liftIO $ IORef.modifyIORef' replies (<> [body])
      pure (Just 1)
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

message :: IncomingMessage
message =
  IncomingMessage
    { platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just 100
    , chatAliases = []
    , digest = emptyMessageDigest
    , senderId = Just "200"
    , senderUsername = Just "alice"
    , messageId = Just 300
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , text = "!ping"
    , raw = Aeson.Null
    }
