module Main (main) where

import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Core.Route
import Bot.Handler.ShutUp
import Bot.Handler.ShutUp.Config
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import Test.Tasty
import Test.Tasty.HUnit
import Text.Regex.TDFA
  ( defaultCompOpt
  , defaultExecOpt
  , makeRegexOptsM
  )

main :: IO ()
main =
  defaultMain $
    testGroup "shutup"
      [ testCase "matching message is deleted and routing stops" testMatchingMessageIsDeleted
      , testCase "nonmatching message continues routing" testNonmatchingMessageContinues
      ]

testMatchingMessageIsDeleted :: IO ()
testMatchingMessageIsDeleted = do
  deleted <- IORef.newIORef ([] :: [MessageId])
  later <- IORef.newIORef False
  cfg <- either assertFailure pure (compiledConfig ["spam[[:space:]]+message"])
  runShutUp deleted later cfg (messageWithText "this is a spam message")
  IORef.readIORef deleted >>= (@?= ["300"])
  IORef.readIORef later >>= (@?= False)

testNonmatchingMessageContinues :: IO ()
testNonmatchingMessageContinues = do
  deleted <- IORef.newIORef ([] :: [MessageId])
  later <- IORef.newIORef False
  cfg <- either assertFailure pure (compiledConfig ["spam[[:space:]]+message"])
  runShutUp deleted later cfg (messageWithText "ordinary message")
  IORef.readIORef deleted >>= (@?= [])
  IORef.readIORef later >>= (@?= True)

runShutUp :: IORef.IORef [MessageId] -> IORef.IORef Bool -> ShutUpConfig -> IncomingMessage -> IO ()
runShutUp deleted later cfg incoming =
  runEff $
    Chat.runChatWith Chat.ChatHandlers
      { handleReplyTo = \_ _ -> pure Nothing
      , handleEditMessage = \_ _ _ -> pure False
      , handleDeleteMessage = \_ messageId -> do
          liftIO $ IORef.modifyIORef' deleted (<> [messageId])
          pure True
      , handleReplyStreamStyle = \_ -> pure (Chat.ChunkedReply 1800)
      , handleGetMessageContent = \_ _ -> pure Nothing
      , handleGetSenderMemberInfo = \_ -> pure Nothing
      , handleGetMemberInfo = \_ _ -> pure Nothing
      , handleGetUserAvatar = \_ _ -> pure Nothing
      , handleListGroupMembers = \_ -> pure Nothing
      , handleMentionUser = \_ _ _ -> pure Nothing
      } $
      runHandlers (shutUpHandlers cfg <> [laterRoute]) incoming
  where
    laterRoute =
      continueOn anything \_ _ ->
        liftIO $ IORef.writeIORef later True

compiledConfig :: [Text] -> Either String ShutUpConfig
compiledConfig patterns =
  ShutUpConfig <$> traverse compile patterns
  where
    compile source =
      case makeRegexOptsM defaultCompOpt defaultExecOpt source of
        Left err -> Left err
        Right regex -> Right DeletePattern{source, regex}

messageWithText :: Text -> IncomingMessage
messageWithText body =
  IncomingMessage
    { platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just 100
    , chatAliases = []
    , digest = emptyMessageDigest
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
