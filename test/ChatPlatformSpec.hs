module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Bot.Chat.Driver.Matrix as Matrix
import qualified Bot.Chat.Driver.QQ as QQ
import qualified Bot.Chat.Driver.Telegram as Telegram
import Bot.Core.Message
import Bot.Prelude
import qualified Data.ByteString.Char8 as ByteStringChar8
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "chat platforms"
      [ testCase "QQ user message converts to incoming message" testQqUserMessageConvertsToIncomingMessage
      , testCase "QQ superuser is also allowed sender" testQqSuperuserIsAlsoAllowedSender
      , testCase "QQ self message is ignored" testQqSelfMessageIsIgnored
      , testCase "QQ CQ mention string keeps mentioned user ids" testQqCQMentionStringKeepsMentionedUserIds
      , testCase "QQ forwarded messages merge all node text" testQqForwardedMessagesMergeAllNodeText
      , testCase "Telegram user message converts to incoming message" testTelegramUserMessageConvertsToIncomingMessage
      , testCase "Telegram superuser is also allowed private sender" testTelegramSuperuserIsAlsoAllowedPrivateSender
      , testCase "Telegram bot message is ignored" testTelegramBotMessageIsIgnored
      , testCase "Telegram referenced message includes sender identity" testTelegramReferencedMessageIncludesSenderIdentity
      , testCase "Telegram CommonMark formatting emits UTF-16 entities" testTelegramCommonMarkFormattingEmitsUtf16Entities
      , testCase "Telegram CommonMark extensions render supported Telegram formatting" testTelegramCommonMarkExtensionsRenderSupportedTelegramFormatting
      , testCase "Telegram CommonMark extensions render footnotes and math" testTelegramCommonMarkExtensionsRenderFootnotesAndMath
      , testCase "Telegram CommonMark list items keep line breaks" testTelegramCommonMarkListItemsKeepLineBreaks
      , testCase "Telegram CommonMark nested lists keep continuation indentation" testTelegramCommonMarkNestedListsKeepContinuationIndentation
      , testCase "Telegram ok false becomes TelegramException description" testTelegramOkFalseBecomesTelegramExceptionDescription
      , testCase "Telegram failure reply is concise" testTelegramFailureReplyIsConcise
      , testCase "Matrix message converts to incoming message" testMatrixMessageConvertsToIncomingMessage
      , testCase "Matrix superuser is marked in digest" testMatrixSuperuserIsMarkedInDigest
      ]

testQqUserMessageConvertsToIncomingMessage :: IO ()
testQqUserMessageConvertsToIncomingMessage = do
  let incoming = QQ.eventToIncomingMessage (qqMessageEvent 10001)
  ((.platform) <$> incoming) @?= Just PlatformQQ
  ((.text) <$> incoming) @?= Just "hello"
  ((.digest.botId) <$> incoming) @?= Just (Just "424242")

testQqSuperuserIsAlsoAllowedSender :: IO ()
testQqSuperuserIsAlsoAllowedSender = do
  let cfg = QQ.Config
        { QQ.host = ""
        , QQ.port = 0
        , QQ.path = ""
        , QQ.token = Nothing
        , QQ.botQQ = Nothing
        , QQ.allowedGroups = []
        , QQ.allowedUsers = []
        , QQ.superusers = [10001]
        }
      incoming = fromMaybe (error "expected incoming QQ message") $
        QQ.eventToIncomingMessageWith cfg (qqMessageEvent 10001)
  incoming.digest.senderIsAllowed @?= True
  incoming.digest.senderIsSuperuser @?= True

testQqSelfMessageIsIgnored :: IO ()
testQqSelfMessageIsIgnored =
  assertBool
    "QQ messages sent by the bot itself are ignored"
    (isNothing (QQ.eventToIncomingMessage (qqMessageEvent qqBotUserId)))

testQqCQMentionStringKeepsMentionedUserIds :: IO ()
testQqCQMentionStringKeepsMentionedUserIds = do
  let cfg = QQ.Config
        { QQ.host = ""
        , QQ.port = 0
        , QQ.path = ""
        , QQ.token = Nothing
        , QQ.botQQ = Just qqBotUserId
        , QQ.allowedGroups = []
        , QQ.allowedUsers = []
        , QQ.superusers = []
        }
      event = (qqMessageEvent 10001)
        { QQ.message = Just (Aeson.String "[CQ:at,qq=123456] hi [CQ:at,qq=424242]")
        , QQ.rawMessage = Just "[CQ:at,qq=123456] hi [CQ:at,qq=424242]"
        }
      incoming = fromMaybe (error "expected incoming QQ message") $
        QQ.eventToIncomingMessageWith cfg event
  incoming.mentions @?= [123456, qqBotUserId]
  incoming.text @?= "@123456 hi @424242"
  incoming.digest.mentionsBot @?= True

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

testTelegramSuperuserIsAlsoAllowedPrivateSender :: IO ()
testTelegramSuperuserIsAlsoAllowedPrivateSender = do
  let cfg = Telegram.Config
        { Telegram.botToken = ""
        , Telegram.botIds = []
        , Telegram.botUsernames = []
        , Telegram.allowedChatIds = []
        , Telegram.allowedChatAliases = []
        , Telegram.superusers = ["alice"]
        }
      incoming = fromMaybe (error "expected incoming Telegram message") $
        Telegram.updateToIncomingMessageWith cfg (telegramUpdateWithMessage privateTelegramMessage)
  incoming.digest.senderIsAllowed @?= True
  incoming.digest.senderIsSuperuser @?= True

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
    Telegram.runTelegram (Telegram.Config "dummy-token" [] [] [] [] []) $
      Telegram.getMessageContent incoming ("70001")
  (fetched <&> (.senderDisplayName)) @?= Just (Just "Bob Smith")
  (fetched <&> (.senderIdentifier)) @?= Just (Just "@bob")
  (fetched <&> (.text)) @?= Just "quoted"

testTelegramCommonMarkFormattingEmitsUtf16Entities :: IO ()
testTelegramCommonMarkFormattingEmitsUtf16Entities = do
  let formatted = Telegram.formatTelegramMarkdown "**你👍あ** and [リンク](https://example.test) `值`"
  formatted.formattedText @?= "你👍あ and リンク 值"
  assertEntity formatted "bold" 0 4 Nothing Nothing
  assertEntity formatted "text_link" 9 3 (Just "https://example.test") Nothing
  assertEntity formatted "code" 13 1 Nothing Nothing

  let pre = Telegram.formatTelegramMarkdown "```haskell\nmain = putStrLn \"こんにちは👍\"\n```"
  pre.formattedText @?= "main = putStrLn \"こんにちは👍\""
  assertEntity pre "pre" 0 25 Nothing (Just "haskell")

testTelegramCommonMarkExtensionsRenderSupportedTelegramFormatting :: IO ()
testTelegramCommonMarkExtensionsRenderSupportedTelegramFormatting = do
  let strike = Telegram.formatTelegramMarkdown "~~old~~ and **new**"
  strike.formattedText @?= "old and new"
  assertEntity strike "strikethrough" 0 3 Nothing Nothing
  assertEntity strike "bold" 8 3 Nothing Nothing

  let tasks = Telegram.formatTelegramMarkdown "- [ ] todo\n- [x] **done**"
  tasks.formattedText @?= "☐ todo\n☑ done"
  assertEntity tasks "bold" 9 4 Nothing Nothing

testTelegramCommonMarkExtensionsRenderFootnotesAndMath :: IO ()
testTelegramCommonMarkExtensionsRenderFootnotesAndMath = do
  let footnotes = Telegram.formatTelegramMarkdown "note[^a]\n\n[^a]: **detail**"
  footnotes.formattedText @?= "note[1]\n\n[1]: detail"
  assertEntity footnotes "bold" 14 6 Nothing Nothing

  let math = Telegram.formatTelegramMarkdown "Use $x^2$ and $$y$$"
  math.formattedText @?= "Use x^2 and y"
  assertEntity math "code" 4 3 Nothing Nothing
  assertEntity math "pre" 12 1 Nothing Nothing

  let rule = Telegram.formatTelegramMarkdown "a\n\n---\n\nb"
  rule.formattedText @?= "a\n\n────────\n\nb"

testTelegramCommonMarkListItemsKeepLineBreaks :: IO ()
testTelegramCommonMarkListItemsKeepLineBreaks = do
  let formatted = Telegram.formatTelegramMarkdown "- first\n- second\n- **third**"
  formatted.formattedText @?= "• first\n• second\n• third"
  assertEntity formatted "bold" 19 5 Nothing Nothing

  let ordered = Telegram.formatTelegramMarkdown "3. first\n4. second\n5. **third**"
  ordered.formattedText @?= "3. first\n4. second\n5. third"
  assertEntity ordered "bold" 22 5 Nothing Nothing

testTelegramCommonMarkNestedListsKeepContinuationIndentation :: IO ()
testTelegramCommonMarkNestedListsKeepContinuationIndentation = do
  let formatted = Telegram.formatTelegramMarkdown "- parent\n  - child\n  - **bold**\n- next"
  formatted.formattedText @?= "• parent\n  • child\n  • bold\n• next"
  assertEntity formatted "bold" 23 4 Nothing Nothing

testTelegramOkFalseBecomesTelegramExceptionDescription :: IO ()
testTelegramOkFalseBecomesTelegramExceptionDescription = do
  let raw = ByteStringChar8.pack "{\"ok\":false,\"error_code\":400,\"description\":\"Bad Request: can't parse entities\"}"
      parsed = either (error . toText) id (Aeson.eitherDecodeStrict raw :: Either String Telegram.TelegramResult)
  result <- runEff (trySync (Telegram.parseTelegramResult parsed)) :: IO (Either SomeException Telegram.Message)
  case result of
    Left err ->
      toText (displayException err) @?= "Bad Request: can't parse entities"
    Right _ ->
      assertFailure "expected TelegramException"

testTelegramFailureReplyIsConcise :: IO ()
testTelegramFailureReplyIsConcise =
  Telegram.telegramFailureReplyText (Telegram.TelegramException "Bad Request: message is too long")
    @?= "Telegram request failed: Bad Request: message is too long"

assertEntity :: Telegram.TelegramFormatted -> Text -> Integer -> Integer -> Maybe Text -> Maybe Text -> Assertion
assertEntity formatted type_ offset entityLength url language =
  case find matches formatted.formattedEntities of
    Just _ ->
      pure ()
    Nothing ->
      let actual = formatted.formattedEntities
      in assertFailure [i|expected Telegram entity #{show (type_, offset, entityLength, url, language) :: String}, got #{show actual :: String}|]
  where
    matches entity =
      entity.type_ == type_
        && entity.offset == offset
        && entity.length == entityLength
        && entity.url == url
        && entity.language == language

testMatrixMessageConvertsToIncomingMessage :: IO ()
testMatrixMessageConvertsToIncomingMessage = do
  let incoming = Matrix.eventToIncomingMessage matrixRoomEvent
  ((.platform) <$> incoming) @?= Just PlatformMatrix
  ((.chatAliases) <$> incoming) @?= Just ["!room:example.org"]
  ((.senderUsername) <$> incoming) @?= Just (Just "@alice:example.org")
  ((.text) <$> incoming) @?= Just "hello"

testMatrixSuperuserIsMarkedInDigest :: IO ()
testMatrixSuperuserIsMarkedInDigest = do
  let cfg = Matrix.Config
        { Matrix.homeserver = "https://matrix.example.org"
        , Matrix.loginUser = Nothing
        , Matrix.loginPassword = Nothing
        , Matrix.deviceId = Nothing
        , Matrix.userId = Just "@bot:example.org"
        , Matrix.allowedRooms = ["!room:example.org"]
        , Matrix.superusers = ["@alice:example.org"]
        }
      incoming = fromMaybe (error "expected incoming Matrix message") $
        Matrix.eventToIncomingMessageWith cfg matrixMentionRoomEvent
  incoming.digest.chatIsAllowed @?= True
  incoming.digest.senderIsAllowed @?= True
  incoming.digest.senderIsSuperuser @?= True
  incoming.digest.mentionsBot @?= True

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

privateTelegramMessage :: Telegram.Message
privateTelegramMessage =
  (telegramMessage False)
    { Telegram.chat = Telegram.Chat
        { id = 10001
        , type_ = Telegram.ChatTypePrivate
        , title = Nothing
        , username = Just "alice"
        , firstName = Just "Alice"
        , lastName = Nothing
        }
    }

matrixRoomEvent :: Matrix.RoomEvent
matrixRoomEvent =
  Matrix.RoomEvent
    { Matrix.roomId = "!room:example.org"
    , Matrix.event = Matrix.Event
        { Matrix.type_ = "m.room.message"
        , Matrix.sender = "@alice:example.org"
        , Matrix.eventId = Just "$event:example.org"
        , Matrix.content = Matrix.EventContent
            { Matrix.msgtype = Just "m.text"
            , Matrix.body = Just "hello"
            , Matrix.mentions = []
            }
        , Matrix.raw = Aeson.Null
        }
    }

matrixMentionRoomEvent :: Matrix.RoomEvent
matrixMentionRoomEvent =
  Matrix.RoomEvent
    { Matrix.roomId = "!room:example.org"
    , Matrix.event = Matrix.Event
        { Matrix.content = Matrix.EventContent
            { Matrix.msgtype = Just "m.text"
            , Matrix.body = Just "hello @bot:example.org"
            , Matrix.mentions = []
            }
        , Matrix.type_ = "m.room.message"
        , Matrix.sender = "@alice:example.org"
        , Matrix.eventId = Just "$event:example.org"
        , Matrix.raw = Aeson.Null
        }
    }

runTestLog :: IOE :> es => Eff (Log : es) a -> Eff es a
runTestLog action = do
  logger <- liftIO $ mkLogger "chat-platform-spec" \_ -> pure ()
  runLog "chat-platform-spec" logger LogTrace action
