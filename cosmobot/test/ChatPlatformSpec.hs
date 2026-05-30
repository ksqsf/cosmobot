module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Bot.Chat.Driver.Discord as Discord
import qualified Bot.Chat.Driver.Matrix as Matrix
import qualified Bot.Chat.Driver.QQ as QQ
import qualified Bot.Chat.Driver.Telegram as Telegram
import Bot.Core.Message
import Bot.Prelude
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
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
      , testCase "Matrix direct room converts to private message" testMatrixDirectRoomConvertsToPrivateMessage
      , testCase "Matrix image message includes media URL" testMatrixImageMessageIncludesMediaUrl
      , testCase "Matrix encrypted image message includes media URL" testMatrixEncryptedImageMessageIncludesMediaUrl
      , testCase "Matrix reply relation converts to reply message id" testMatrixReplyRelationConvertsToReplyMessageId
      , testCase "Matrix edit event is ignored" testMatrixEditEventIsIgnored
      , testCase "Matrix superuser is marked in digest" testMatrixSuperuserIsMarkedInDigest
      , testCase "Matrix bot mention uses mentions field only" testMatrixBotMentionUsesMentionsFieldOnly
      , testCase "Matrix Markdown renders custom HTML" testMatrixMarkdownRendersCustomHtml
      , testCase "Matrix Markdown renders user ids as mention links" testMatrixMarkdownRendersUserIdsAsMentionLinks
      , testCase "Discord message converts to incoming message" testDiscordMessageConvertsToIncomingMessage
      , testCase "Discord self message is ignored" testDiscordSelfMessageIsIgnored
      , testCase "Discord superuser and bot mention are marked" testDiscordSuperuserAndBotMentionAreMarked
      , testCase "Discord CommonMark extensions render Discord Markdown" testDiscordCommonMarkExtensionsRenderDiscordMarkdown
      , testCase "Discord avatar value includes avatar URL" testDiscordAvatarValueIncludesAvatarUrl
      , testCase "Discord image context includes embeds and image links" testDiscordImageContextIncludesEmbedsAndImageLinks
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
  incoming.mentions @?= ["123456", show qqBotUserId]
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
      rawMessage :: Telegram.Message
      rawMessage = case Aeson.fromJSON incoming.raw of
        Aeson.Success message -> message
        Aeson.Error err -> error (toText err)
      fetched = rawMessage.replyToMessage
  ((\user -> Text.unwords [user.firstName, fromMaybe "" user.lastName]) <$> (fetched >>= (.from))) @?= Just "Bob Smith"
  ((\user -> maybe (Text.pack (show user.id :: String)) ("@" <>) user.username) <$> (fetched >>= (.from))) @?= Just "@bob"
  (fetched >>= (.text)) @?= Just "quoted"

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
      exceptionFirstLine err @?= "Bad Request: can't parse entities"
    Right _ ->
      assertFailure "expected TelegramException"

testTelegramFailureReplyIsConcise :: IO ()
testTelegramFailureReplyIsConcise =
  Telegram.telegramFailureReplyText (Telegram.TelegramException "Bad Request: message is too long")
    @?= "Telegram request failed: Bad Request: message is too long"

exceptionFirstLine :: Exception err => err -> Text
exceptionFirstLine =
  Text.takeWhile (/= '\n') . toText . displayException

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
  ((.kind) <$> incoming) @?= Just ChatGroup
  ((.chatAliases) <$> incoming) @?= Just ["!room:example.org"]
  ((.senderUsername) <$> incoming) @?= Just (Just "@alice:example.org")
  ((.text) <$> incoming) @?= Just "hello"

testMatrixDirectRoomConvertsToPrivateMessage :: IO ()
testMatrixDirectRoomConvertsToPrivateMessage = do
  let incoming = Matrix.eventToIncomingMessage matrixDirectRoomEvent
  ((.platform) <$> incoming) @?= Just PlatformMatrix
  ((.kind) <$> incoming) @?= Just ChatPrivate
  ((.chatAliases) <$> incoming) @?= Just ["!room:example.org"]
  ((.senderUsername) <$> incoming) @?= Just (Just "@alice:example.org")
  ((.text) <$> incoming) @?= Just "hello"

testMatrixImageMessageIncludesMediaUrl :: IO ()
testMatrixImageMessageIncludesMediaUrl = do
  let incoming = Matrix.eventToIncomingMessage matrixImageRoomEvent
  ((.text) <$> incoming) @?= Just "image.png"
  ((.imageUrls) <$> incoming) @?= Just ["mxc://example.org/plain-image"]

testMatrixEncryptedImageMessageIncludesMediaUrl :: IO ()
testMatrixEncryptedImageMessageIncludesMediaUrl = do
  let incoming = Matrix.eventToIncomingMessage matrixEncryptedImageRoomEvent
  ((.text) <$> incoming) @?= Just "image.png"
  ((.imageUrls) <$> incoming) @?= Just ["mxc://example.org/encrypted-image"]

testMatrixMarkdownRendersCustomHtml :: IO ()
testMatrixMarkdownRendersCustomHtml =
  Matrix.formatMatrixMarkdown markdown @?= Just expected
  where
    markdown =
      Text.unlines
        [ "**hi** and `code`"
        , ""
        , "~~old~~"
        , ""
        , "- [x] done"
        , ""
        , "| a | b |"
        , "| - | - |"
        , "| 1 | 2 |"
        ]
    expected =
      Text.intercalate "\n"
        [ "<p><strong>hi</strong> and <code>code</code></p>"
        , "<p><del>old</del></p>"
        , "<ul class=\"task-list\">"
        , "<li><input type=\"checkbox\" disabled=\"\" checked=\"\" />done"
        , "</li>"
        , "</ul>"
        , "<table>"
        , "<thead>"
        , "<tr>"
        , "<th>a</th>"
        , "<th>b</th>"
        , "</tr>"
        , "</thead>"
        , "<tbody>"
        , "<tr>"
        , "<td>1</td>"
        , "<td>2</td>"
        , "</tr>"
        , "</tbody>"
        , "</table>"
        ]

testMatrixMarkdownRendersUserIdsAsMentionLinks :: IO ()
testMatrixMarkdownRendersUserIdsAsMentionLinks =
  Matrix.formatMatrixMarkdownWithMentionNames mentionNames "@foo:matrix.org @bar:matrix.org." @?= Just expected
  where
    mentionNames =
      Map.fromList
        [ ("@foo:matrix.org", "Foo")
        , ("@bar:matrix.org", "Bar")
        ]
    expected =
      Text.intercalate "\n"
        [ "<p><a href=\"https://matrix.to/#/@foo:matrix.org\">@Foo</a> <a href=\"https://matrix.to/#/@bar:matrix.org\">@Bar</a>.</p>"
        ]

testMatrixReplyRelationConvertsToReplyMessageId :: IO ()
testMatrixReplyRelationConvertsToReplyMessageId = do
  let incoming = Matrix.eventToIncomingMessage matrixReplyRoomEvent
  ((.replyToMessageId) <$> incoming) @?= Just (Just (textMessageId "$parent:example.org"))

testMatrixEditEventIsIgnored :: IO ()
testMatrixEditEventIsIgnored =
  assertBool
    "Matrix edit events should not trigger handlers"
    (isNothing (Matrix.eventToIncomingMessage matrixEditRoomEvent))

testMatrixSuperuserIsMarkedInDigest :: IO ()
testMatrixSuperuserIsMarkedInDigest = do
  let incoming = fromMaybe (error "expected incoming Matrix message") $
        Matrix.eventToIncomingMessageWith matrixMentionConfig matrixMentionRoomEvent
  incoming.digest.chatIsAllowed @?= True
  incoming.digest.senderIsAllowed @?= True
  incoming.digest.senderIsSuperuser @?= True
  incoming.digest.mentionsBot @?= True

testMatrixBotMentionUsesMentionsFieldOnly :: IO ()
testMatrixBotMentionUsesMentionsFieldOnly = do
  let incoming = fromMaybe (error "expected incoming Matrix message") $
        Matrix.eventToIncomingMessageWith matrixMentionConfig matrixTextOnlyMentionRoomEvent
  incoming.digest.mentionsBot @?= False

testDiscordMessageConvertsToIncomingMessage :: IO ()
testDiscordMessageConvertsToIncomingMessage = do
  let incoming = Discord.eventToIncomingMessage discordMessage
  ((.platform) <$> incoming) @?= Just PlatformDiscord
  ((.kind) <$> incoming) @?= Just ChatGroup
  ((.chatAliases) <$> incoming) @?= Just ["90001", "80001"]
  ((.senderId) <$> incoming) @?= Just (Just "10001")
  ((.senderUsername) <$> incoming) @?= Just (Just "alice")
  ((.messageId) <$> incoming) @?= Just (Just (textMessageId "70001"))
  ((.replyToMessageId) <$> incoming) @?= Just (Just (textMessageId "60001"))
  ((.mentions) <$> incoming) @?= Just ["424242"]
  ((.imageUrls) <$> incoming) @?= Just ["https://cdn.discordapp.com/image.png"]
  ((.text) <$> incoming) @?= Just "hello <@424242>"

testDiscordSelfMessageIsIgnored :: IO ()
testDiscordSelfMessageIsIgnored =
  assertBool
    "Discord messages sent by the bot itself are ignored"
    (isNothing (Discord.eventToIncomingMessageWith discordConfig discordMessage{Discord.author = discordUser "424242" "krkr" True}))

testDiscordSuperuserAndBotMentionAreMarked :: IO ()
testDiscordSuperuserAndBotMentionAreMarked = do
  let incoming = fromMaybe (error "expected incoming Discord message") $
        Discord.eventToIncomingMessageWith discordConfig discordMessage
  incoming.digest.chatIsAllowed @?= True
  incoming.digest.senderIsAllowed @?= True
  incoming.digest.senderIsSuperuser @?= True
  incoming.digest.mentionsBot @?= True

testDiscordCommonMarkExtensionsRenderDiscordMarkdown :: IO ()
testDiscordCommonMarkExtensionsRenderDiscordMarkdown = do
  Discord.formatDiscordMarkdown "**hi** and `code`" @?= "**hi** and `code`"
  Discord.formatDiscordMarkdown "~~old~~ and [site](https://example.test/a)" @?= "~~old~~ and [site](https://example.test/a)"
  Discord.formatDiscordMarkdown "- [ ] todo\n- [x] **done**" @?= "- [ ] todo\n- [x] **done**"
  Discord.formatDiscordMarkdown "Use $x^2$ and $$y$$" @?= "Use `x^2` and ```\ny\n```"
  Discord.formatDiscordMarkdown "| a | b |\n| - | - |\n| 1 | 2 |" @?= "```\na | b\n1 | 2\n```"

testDiscordAvatarValueIncludesAvatarUrl :: IO ()
testDiscordAvatarValueIncludesAvatarUrl = do
  let customAvatar = (discordUser "10001" "alice" False){Discord.avatar = Just "hash"}
  avatarUrl (fromMaybe (error "expected custom avatar") (Discord.discordUserAvatarValue customAvatar))
    @?= Just "https://cdn.discordapp.com/avatars/10001/hash.png?size=512"

  let defaultAvatar = (discordUser "10001" "alice" False){Discord.avatar = Nothing}
  avatarUrl (fromMaybe (error "expected default avatar") (Discord.discordUserAvatarValue defaultAvatar))
    @?= Just "https://cdn.discordapp.com/embed/avatars/0.png"

testDiscordImageContextIncludesEmbedsAndImageLinks :: IO ()
testDiscordImageContextIncludesEmbedsAndImageLinks = do
  let message = (discordMessageNoReference "70002")
        { Discord.content = "look https://example.test/generated.webp?size=512"
        , Discord.attachments =
            [ Discord.Attachment
                { Discord.id = "2"
                , Discord.filename = "photo.png"
                , Discord.url = "https://cdn.discordapp.com/attachment-without-content-type"
                , Discord.contentType = Nothing
                }
            ]
        , Discord.embeds =
            [ Discord.Embed
                { Discord.image = Just (Discord.EmbedImage "https://example.test/embed.png")
                , Discord.thumbnail = Just (Discord.EmbedImage "https://example.test/thumb.jpg")
                }
            ]
        }
      incoming = fromMaybe (error "expected incoming Discord message") (Discord.eventToIncomingMessage message)
  incoming.imageUrls @?=
    [ "https://cdn.discordapp.com/attachment-without-content-type"
    , "https://example.test/embed.png"
    , "https://example.test/thumb.jpg"
    , "https://example.test/generated.webp?size=512"
    ]

avatarUrl :: Aeson.Value -> Maybe Text
avatarUrl =
  AesonTypes.parseMaybe $
    Aeson.withObject "avatar value" (Aeson..: "avatar_url")

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
    , Matrix.roomIsDirect = False
    , Matrix.event = Matrix.Event
        { Matrix.type_ = "m.room.message"
        , Matrix.sender = "@alice:example.org"
        , Matrix.eventId = Just "$event:example.org"
        , Matrix.content = Matrix.EventContent
            { Matrix.msgtype = Just "m.text"
            , Matrix.body = Just "hello"
            , Matrix.mentions = []
            , Matrix.replyToEventId = Nothing
            }
        , Matrix.raw = Aeson.Null
        }
    }

matrixDirectRoomEvent :: Matrix.RoomEvent
matrixDirectRoomEvent =
  matrixRoomEvent{Matrix.roomIsDirect = True}

matrixImageRoomEvent :: Matrix.RoomEvent
matrixImageRoomEvent =
  matrixRoomEvent
    { Matrix.event = matrixRoomEvent.event
        { Matrix.content = matrixRoomEvent.event.content
            { Matrix.msgtype = Just "m.image"
            , Matrix.body = Just "image.png"
            }
        , Matrix.raw = matrixImageRawContent
            [ "msgtype" Aeson..= ("m.image" :: Text)
            , "body" Aeson..= ("image.png" :: Text)
            , "url" Aeson..= ("mxc://example.org/plain-image" :: Text)
            , "info" Aeson..= Aeson.object
                [ "mimetype" Aeson..= ("image/png" :: Text)
                ]
            ]
        }
    }

matrixEncryptedImageRoomEvent :: Matrix.RoomEvent
matrixEncryptedImageRoomEvent =
  matrixRoomEvent
    { Matrix.event = matrixRoomEvent.event
        { Matrix.content = matrixRoomEvent.event.content
            { Matrix.msgtype = Just "m.image"
            , Matrix.body = Just "image.png"
            }
        , Matrix.raw = matrixImageRawContent
            [ "msgtype" Aeson..= ("m.image" :: Text)
            , "body" Aeson..= ("image.png" :: Text)
            , "file" Aeson..= Aeson.object
                [ "url" Aeson..= ("mxc://example.org/encrypted-image" :: Text)
                ]
            , "info" Aeson..= Aeson.object
                [ "mimetype" Aeson..= ("image/png" :: Text)
                ]
            ]
        }
    }

matrixImageRawContent :: [AesonTypes.Pair] -> Aeson.Value
matrixImageRawContent content =
  Aeson.object ["content" Aeson..= Aeson.object content]

matrixReplyRoomEvent :: Matrix.RoomEvent
matrixReplyRoomEvent =
  matrixRoomEvent
    { Matrix.event = matrixRoomEvent.event
        { Matrix.content = matrixRoomEvent.event.content
            { Matrix.replyToEventId = Just "$parent:example.org"
            }
        }
    }

matrixEditRoomEvent :: Matrix.RoomEvent
matrixEditRoomEvent =
  matrixRoomEvent
    { Matrix.event = matrixRoomEvent.event
        { Matrix.raw = Aeson.object
            [ "content" Aeson..= Aeson.object
                [ "m.relates_to" Aeson..= Aeson.object
                    [ "rel_type" Aeson..= ("m.replace" :: Text)
                    , "event_id" Aeson..= ("$event:example.org" :: Text)
                    ]
                ]
            ]
        }
    }

matrixMentionRoomEvent :: Matrix.RoomEvent
matrixMentionRoomEvent =
  Matrix.RoomEvent
    { Matrix.roomId = "!room:example.org"
    , Matrix.roomIsDirect = False
    , Matrix.event = Matrix.Event
        { Matrix.content = Matrix.EventContent
            { Matrix.msgtype = Just "m.text"
            , Matrix.body = Just "hello @bot:example.org"
            , Matrix.mentions = ["@bot:example.org"]
            , Matrix.replyToEventId = Nothing
            }
        , Matrix.type_ = "m.room.message"
        , Matrix.sender = "@alice:example.org"
        , Matrix.eventId = Just "$event:example.org"
        , Matrix.raw = Aeson.Null
        }
    }

matrixTextOnlyMentionRoomEvent :: Matrix.RoomEvent
matrixTextOnlyMentionRoomEvent =
  matrixMentionRoomEvent
    { Matrix.event = matrixMentionRoomEvent.event
        { Matrix.content = matrixMentionRoomEvent.event.content
            { Matrix.mentions = []
            }
        }
    }

matrixMentionConfig :: Matrix.Config
matrixMentionConfig =
  Matrix.Config
    { Matrix.homeserver = "https://matrix.example.org"
    , Matrix.loginUser = Nothing
    , Matrix.loginPassword = Nothing
    , Matrix.deviceId = Nothing
    , Matrix.directRooms = []
    , Matrix.userId = Just "@bot:example.org"
    , Matrix.allowedRooms = ["!room:example.org"]
    , Matrix.superusers = ["@alice:example.org"]
    }

discordConfig :: Discord.Config
discordConfig =
  Discord.Config
    { Discord.botToken = ""
    , Discord.botId = Just "424242"
    , Discord.applicationId = Nothing
    , Discord.allowedGuilds = [80001]
    , Discord.allowedChannels = []
    , Discord.allowedUsers = []
    , Discord.superusers = ["10001"]
    , Discord.gatewayHost = "gateway.discord.gg"
    , Discord.gatewayPath = "/?v=10&encoding=json"
    }

discordMessage :: Discord.Message
discordMessage =
  Discord.Message
    { Discord.id = "70001"
    , Discord.channelId = "90001"
    , Discord.guildId = Just "80001"
    , Discord.author = discordUser "10001" "alice" False
    , Discord.member = Nothing
    , Discord.content = "hello <@424242>"
    , Discord.attachments =
        [ Discord.Attachment
            { Discord.id = "1"
            , Discord.filename = "image.png"
            , Discord.url = "https://cdn.discordapp.com/image.png"
            , Discord.contentType = Just "image/png"
            }
        ]
    , Discord.embeds = []
    , Discord.mentions = [discordUser "424242" "krkr" True]
    , Discord.referencedMessage = Just (discordReferencedMessage "60001")
    , Discord.messageReference = Nothing
    , Discord.raw = Aeson.object ["guild_id" Aeson..= ("80001" :: Text)]
    }

discordReferencedMessage :: Text -> Discord.Message
discordReferencedMessage messageId =
  (discordMessageNoReference messageId)
    { Discord.content = "quoted"
    , Discord.attachments = []
    , Discord.embeds = []
    , Discord.mentions = []
    }

discordMessageNoReference :: Text -> Discord.Message
discordMessageNoReference messageId =
  Discord.Message
    { Discord.id = messageId
    , Discord.channelId = "90001"
    , Discord.guildId = Just "80001"
    , Discord.author = discordUser "20001" "bob" False
    , Discord.member = Nothing
    , Discord.content = ""
    , Discord.attachments = []
    , Discord.embeds = []
    , Discord.mentions = []
    , Discord.referencedMessage = Nothing
    , Discord.messageReference = Nothing
    , Discord.raw = Aeson.object ["guild_id" Aeson..= ("80001" :: Text)]
    }

discordUser :: Text -> Text -> Bool -> Discord.User
discordUser userId username fromBot =
  Discord.User
    { Discord.id = userId
    , Discord.username = Just username
    , Discord.globalName = Nothing
    , Discord.bot = fromBot
    , Discord.avatar = Nothing
    }
