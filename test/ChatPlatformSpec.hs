module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Bot.Effect.Chat.QQ as QQ
import qualified Bot.Effect.Chat.Telegram as Telegram
import Bot.Message
import Bot.Prelude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "chat platforms"
      [ testCase "QQ user message converts to incoming message" testQqUserMessageConvertsToIncomingMessage
      , testCase "QQ self message is ignored" testQqSelfMessageIsIgnored
      , testCase "Telegram user message converts to incoming message" testTelegramUserMessageConvertsToIncomingMessage
      , testCase "Telegram bot message is ignored" testTelegramBotMessageIsIgnored
      ]

testQqUserMessageConvertsToIncomingMessage :: IO ()
testQqUserMessageConvertsToIncomingMessage = do
  let incoming = QQ.eventToIncomingMessage (qqMessageEvent 10001)
  ((.platform) <$> incoming) @?= Just PlatformQQ
  ((.text) <$> incoming) @?= Just "hello"

testQqSelfMessageIsIgnored :: IO ()
testQqSelfMessageIsIgnored =
  assertBool
    "QQ messages sent by the bot itself are ignored"
    (isNothing (QQ.eventToIncomingMessage (qqMessageEvent qqBotUserId)))

testTelegramUserMessageConvertsToIncomingMessage :: IO ()
testTelegramUserMessageConvertsToIncomingMessage = do
  let incoming = Telegram.updateToIncomingMessage (telegramUpdate False)
  ((.platform) <$> incoming) @?= Just PlatformTelegram
  ((.text) <$> incoming) @?= Just "hello"

testTelegramBotMessageIsIgnored :: IO ()
testTelegramBotMessageIsIgnored =
  assertBool
    "Telegram bot messages are ignored"
    (isNothing (Telegram.updateToIncomingMessage (telegramUpdate True)))

qqMessageEvent :: Integer -> QQ.Event
qqMessageEvent userId =
  QQ.Event
    { time = Just 1
    , selfId = Just qqBotUserId
    , postType = "message"
    , messageType = Just "group"
    , subType = Just "normal"
    , messageId = Just 80001
    , userId = Just userId
    , groupId = Just 90001
    , message = Just (Aeson.String "hello")
    , rawMessage = Just "hello"
    , sender = Nothing
    , rawEvent = Aeson.Null
    }

qqBotUserId :: Integer
qqBotUserId =
  424242

telegramUpdate :: Bool -> Telegram.Update
telegramUpdate fromBot =
  Telegram.Update
    { updateId = 1
    , message = Just (telegramMessage fromBot)
    , editedMessage = Nothing
    , channelPost = Nothing
    , editedChannelPost = Nothing
    }

telegramMessage :: Bool -> Telegram.Message
telegramMessage fromBot =
  Telegram.Message
    { messageId = 80001
    , messageThreadId = Nothing
    , from = Just (telegramUser fromBot)
    , senderChat = Nothing
    , chat = telegramChat
    , replyToMessage = Nothing
    , text = Just "hello"
    , entities = Nothing
    , caption = Nothing
    , captionEntities = Nothing
    , photo = Nothing
    }

telegramUser :: Bool -> Telegram.User
telegramUser fromBot =
  Telegram.User
    { id = 10001
    , isBot = fromBot
    , firstName = "Alice"
    , lastName = Nothing
    , username = Just "alice"
    }

telegramChat :: Telegram.Chat
telegramChat =
  Telegram.Chat
    { id = 90001
    , type_ = Telegram.ChatTypeGroup
    , title = Just "group"
    , username = Nothing
    , firstName = Nothing
    , lastName = Nothing
    }
