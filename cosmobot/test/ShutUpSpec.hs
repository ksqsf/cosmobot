module Main (main) where

import Bot.Chat.Driver.Types (ChatDriverEffects)
import qualified Bot.Chat.Driver.Types as Driver
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

newtype DeleteChatDriver es =
  DeleteChatDriver (IncomingMessage -> MessageId -> Eff es Bool)

instance Driver.ChatDriver (DeleteChatDriver es0) where
  type ChatDriverEffects (DeleteChatDriver es0) es = es ~ es0
  driverPlatform _ = PlatformTelegram
  deleteMessage (DeleteChatDriver delete) =
    delete

main :: IO ()
main =
  defaultMain $
    testGroup "shutup"
      [ testCase "matching message is deleted and routing stops" testMatchingMessageIsDeleted
      , testCase "nonmatching message continues routing" testNonmatchingMessageContinues
      , testCase "third repeat and containing suffixes are deleted" $
          checkStream (map messageWithText ["a", "a", "a", "a", "xa", "ax", "b", "b", "b", "b"]) [3,4,5,6,9,10]
      , testCase "arming requires exact consecutive text" $
          checkStream (map messageWithText ["a", "xa", "a", "a", "b", "a", "a", "a", "a"]) [8,9]
      , testCase "empty messages reset without arming" $
          checkStream (map messageWithText ["", "", "", "hello", "a", "a", "", "a"]) []
      , testCase "pattern is literal and preserved after substring matches" $
          checkStream (map messageWithText [".*", ".*", ".*", "x.*", ".*", "anything"]) [3,4,5]
      , testCase "chat and platform scope are independent" $
          let a = messageWithText "a"
              b = a{chatId = Just "101"}
              c = a{platform = PlatformMatrix}
          in checkStream [a,b,c,a,b,c,a,b,c,a,b,c] [7,8,9,10,11,12]
      , testCase "missing chat identity does not contribute" $
          let a = messageWithText "a"
          in checkStream [a,a{chatId = Nothing},a,a,a] [4,5]
      , testCase "deletion events do not contribute or reset" $
          let a = messageWithText "a"
          in checkStream [a,a,a{eventKind = IncomingMessageDeleted},a,(messageWithText "b"){eventKind = IncomingMessageDeleted},a] [4,6]
      ]

checkStream :: [IncomingMessage] -> [Int] -> IO ()
checkStream messages expected = do
  deleted <- IORef.newIORef []
  continued <- IORef.newIORef []
  runEff $ runPrim $ Chat.runChatWith
    (DeleteChatDriver \_ messageId -> liftIO (IORef.modifyIORef' deleted (<> [messageId])) $> True) do
      handlers <- shutUpHandlers defaultShutUpConfig
      let later = continueOn anything \message _ ->
            liftIO $ IORef.modifyIORef' continued (<> [message.messageId])
      forM_ (zip [1 :: Int ..] messages) \(index, message) ->
        runHandlers (handlers <> [later]) message{Bot.Core.Message.messageId = Just (fromString (show index))}
  IORef.readIORef deleted >>= (@?= map (fromString . show) expected)
  IORef.readIORef continued >>= (@?= [Just (fromString (show index)) | index <- [1..length messages], index `notElem` expected])

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
  runEff $ runPrim $
    Chat.runChatWith
      ( DeleteChatDriver \_ messageId -> do
          liftIO $ IORef.modifyIORef' deleted (<> [messageId])
          pure True
      ) $
      do
        handlers <- shutUpHandlers cfg
        runHandlers (handlers <> [laterRoute]) incoming
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
    { eventKind = IncomingMessageCreated
    , replyQuote = Nothing
    , timestamp = Nothing
    , platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just "100"
    , chatAliases = []
    , chatDisplayName = Nothing
    , digest = emptyMessageDigest
    , senderId = Just "200"
    , senderUsername = Just "alice"
    , senderDisplayName = Nothing
    , senderGlobalDisplayName = Nothing
    , messageId = Just "300"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = body
    , raw = Aeson.Null
    }
