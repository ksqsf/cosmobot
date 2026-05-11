module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Bot.Chat.Driver.QQ as QQ
import qualified Bot.Chat.Driver.Telegram as Telegram
import Bot.Core.Message
import Bot.Prelude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "chat platforms"
      [ testCase "QQ user message converts to incoming message" testQqUserMessageConvertsToIncomingMessage
      , testCase "QQ self message is ignored" testQqSelfMessageIsIgnored
      , testCase "QQ forwarded messages merge all node text" testQqForwardedMessagesMergeAllNodeText
      , testCase "Telegram user message converts to incoming message" testTelegramUserMessageConvertsToIncomingMessage
      , testCase "Telegram bot message is ignored" testTelegramBotMessageIsIgnored
      , testCase "Telegram referenced message includes sender identity" testTelegramReferencedMessageIncludesSenderIdentity
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

testQqForwardedMessagesMergeAllNodeText :: IO ()
testQqForwardedMessagesMergeAllNodeText =
  QQ.forwardedMessagesText forwardedMessages @?= "first\nsecond\nthird"
  where
    forwardedMessages = Aeson.object
      [ "messages" Aeson..=
          [ Aeson.object
              [ "type" Aeson..= ("node" :: Text)
              , "data" Aeson..= Aeson.object
                  [ "content" Aeson..=
                      [ textSegment "first"
                      , imageSegment "https://example.test/ignored.png"
                      ]
                  ]
              ]
          , Aeson.object
              [ "content" Aeson..=
                  [ textSegment "second"
                  ]
              ]
          , Aeson.object
              [ "raw_message" Aeson..= ("third" :: Text)
              ]
          ]
      ]

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

testTelegramReferencedMessageIncludesSenderIdentity :: IO ()
testTelegramReferencedMessageIncludesSenderIdentity = do
  let referencedSender = Telegram.User
        { Telegram.id = 10001
        , Telegram.isBot = False
        , Telegram.firstName = "Bob"
        , Telegram.lastName = Just "Smith"
        , Telegram.username = Just "bob"
        }
  let referenced = (telegramMessage False)
        { Telegram.messageId = 70001
        , Telegram.from = Just referencedSender
        , Telegram.text = Just "quoted"
        }
      messageWithReply = (telegramMessage False){Telegram.replyToMessage = Just referenced}
      incoming = fromMaybe (error "expected incoming Telegram message") $
        Telegram.updateToIncomingMessage (telegramUpdateWithMessage messageWithReply)
  fetched <- runEff $ runTestLog $
    Telegram.runTelegram (Telegram.Config "dummy-token") $
      Telegram.getMessageContent incoming 70001
  (fetched <&> (.senderDisplayName)) @?= Just (Just "Bob Smith")
  (fetched <&> (.senderIdentifier)) @?= Just (Just "@bob")
  (fetched <&> (.text)) @?= Just "quoted"

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

textSegment :: Text -> Aeson.Value
textSegment text =
  Aeson.object
    [ "type" Aeson..= ("text" :: Text)
    , "data" Aeson..= Aeson.object
        [ "text" Aeson..= text
        ]
    ]

imageSegment :: Text -> Aeson.Value
imageSegment url =
  Aeson.object
    [ "type" Aeson..= ("image" :: Text)
    , "data" Aeson..= Aeson.object
        [ "url" Aeson..= url
        ]
    ]

telegramUpdate :: Bool -> Telegram.Update
telegramUpdate fromBot =
  telegramUpdateWithMessage (telegramMessage fromBot)

telegramUpdateWithMessage :: Telegram.Message -> Telegram.Update
telegramUpdateWithMessage message =
  Telegram.Update
    { updateId = 1
    , message = Just message
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

runTestLog :: IOE :> es => Eff (Log : es) a -> Eff es a
runTestLog action = do
  logger <- liftIO $ mkLogger "chat-platform-spec" \_ -> pure ()
  runLog "chat-platform-spec" logger LogTrace action
