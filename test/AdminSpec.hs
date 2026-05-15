module Main (main) where

import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Core.Route
import Bot.Handler.Admin
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
      ]

testPingRepliesPong :: IO ()
testPingRepliesPong = do
  replies <- IORef.newIORef ([] :: [Text])
  runAdmin replies message
  IORef.readIORef replies >>= (@?= ["pong"])

runAdmin :: IORef.IORef [Text] -> IncomingMessage -> IO ()
runAdmin replies incoming =
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
      runHandlers adminHandlers incoming
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
