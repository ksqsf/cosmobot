{-|
Module      : Bot.Agent.Tools.Chat
Description : Agent tools for chat IO and chat metadata
Stability   : experimental
-}

module Bot.Agent.Tools.Chat
  ( queryChatLogTool
  , queryCurrentSenderChatLogTool
  , sendReplyTool
  , sendFileTool
  , mentionUserTool
  , senderMemberInfoTool
  , memberInfoTool
  , userAvatarTool
  , listGroupMembersTool
  , currentMessageInfoTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

queryChatLogTool :: ChatLog.ChatLog :> es => Tool es
queryChatLogTool = Tool
  { name = "chat_log"
  , description = "Return recent messages recorded in the current chat. Results are in chronological order and include sender ids, message ids, mentions, image urls, and text."
  , parameters = objectSchema
      [ fieldInteger "limit" "Maximum number of recent messages to return."
      , fieldBoolean "include_bot_messages" "Whether to include bot messages. Defaults to false."
      ]
      ["limit"]
  , noisy = False
  , allowed = everyone
  , start = \context -> pure \args ->
      withParsedToolArgs queryChatLogArgs args \(limit, includeBotMessages) -> do
        entries <- ChatLog.queryChat context.message (fromInteger (max 0 limit)) includeBotMessages
        pure (toolText (jsonText entries))
  }

queryCurrentSenderChatLogTool :: ChatLog.ChatLog :> es => Tool es
queryCurrentSenderChatLogTool = Tool
  { name = "sender_chat_log"
  , description = "Return messages from the current sender in the current chat whose text matches any keyword group. Each keyword group is matched as a SQL LIKE pattern with '%' between its terms. Results are newest first and limited to at most 100."
  , parameters = objectSchema
      [ fieldTextArrayArray "keywords" "Keyword groups. Each inner array is joined with '%' and wrapped with '%' for ordered fuzzy matching."
      , fieldIntegerMax "limit" 100 "Maximum number of matching messages to return. Must be <= 100."
      ]
      ["keywords", "limit"]
  , noisy = False
  , allowed = everyone
  , start = \context -> pure \args ->
      withParsedToolArgs queryCurrentSenderChatLogArgs args \(keywords, limit) ->
        case currentSenderChatLogScopeError context.message of
          Just message ->
            pure (toolFailure (permanentArgumentFailure message message).failure)
          Nothing -> do
            entries <- ChatLog.queryCurrentSenderChatLog context.message keywords limit
            pure (toolText (jsonText entries))
  }

sendReplyTool :: Chat.Chat :> es => Tool es
sendReplyTool = Tool
  { name = "send_reply"
  , description = "Send a reply message to the same chat as the current user message. Supports text and image URLs. Use image_urls when the user asks you to send an image found or generated elsewhere. Use only when the user asks you to send an additional message before the final answer."
  , parameters = objectSchema
      [ fieldText "text" "Message text to send. May be omitted when image_urls is non-empty."
      , fieldTextArray "image_urls" "Image URLs to send as images in the same reply. The platform must be able to fetch these URLs."
      ]
      []
  , noisy = False
  , allowed = everyone
  , start = \context -> pure \args ->
      withParsedToolArgs sendReplyArgs args \body -> do
        sent <- Chat.replyTo context.message body
        let sentText = show sent :: String
        pure (toolText [i|Sent message id: #{sentText}|])
  }

sendFileTool :: Chat.Chat :> es => Tool es
sendFileTool = Tool
  { name = "send_file"
  , description = "Send a local file to the same chat as the current user message. The path must be readable by the bot for Telegram and Matrix. For QQ/NapCat, the path is passed to NapCat and must be accessible from the NapCat container. Use only when the user explicitly asks you to send a file."
  , parameters = objectSchema
      [ fieldText "path" "Local file path to send. A file:// prefix is accepted and stripped before upload."
      ]
      ["path"]
  , noisy = True
  , allowed = superuserOnly
  , start = \context -> pure \args ->
      withParsedToolArgs sendFileArgs args \path -> do
        result <- Chat.uploadFile context.message path
        case result of
          Right sent -> do
            let sentText = show sent :: String
            pure (toolText [i|Sent file #{Text.pack path}; message id: #{sentText}|])
          Left err -> do
            let failureText = "发送文件失败：" <> err
            void $ Chat.replyTo context.message failureText
            pure (toolFailure AgentFailure
              { category = ExternalServiceUnavailable
              , userMessage = failureText
              , detail = err
              })
  }

mentionUserTool :: Chat.Chat :> es => Tool es
mentionUserTool = Tool
  { name = "mention_user"
  , description = "Send a reply in the current chat that mentions the given platform user id. Matrix user ids are textual, for example @user:server."
  , parameters = objectSchema
      [ fieldText "user_id" "Platform user id to mention."
      , fieldText "text" "Message text to send after the mention."
      ]
      ["user_id", "text"]
  , noisy = False
  , allowed = everyone
  , start = \context -> pure \args ->
      withParsedToolArgs mentionUserArgs args \(userId, text) -> do
        sent <- Chat.mentionUser context.message userId text
        let sentText = show sent :: String
        pure (toolText [i|Sent mention message id: #{sentText}|])
  }

senderMemberInfoTool :: Chat.Chat :> es => Tool es
senderMemberInfoTool = Tool
  { name = "sender_info"
  , description = "Get platform-provided member information for the sender of the current message in the current group chat."
  , parameters = objectSchema [] []
  , noisy = False
  , allowed = everyone
  , start = \context -> pure \_ -> do
      info <- Chat.getSenderMemberInfo context.message
      pure (toolText (maybe "No member information is available for this message." jsonText info))
  }

memberInfoTool :: Chat.Chat :> es => Tool es
memberInfoTool = Tool
  { name = "member_info"
  , description = "Get platform-provided member information for any user id in the current group chat."
  , parameters = objectSchema
      [ fieldText "user_id" "Platform user id to query in the current group."
      ]
      ["user_id"]
  , noisy = False
  , allowed = everyone
  , start = \context -> pure \args -> withParsedToolArgs memberInfoArgs args \userId -> do
      info <- Chat.getMemberInfo context.message userId
      pure (toolText (maybe "No member information is available for this user in the current chat." jsonText info))
  }

userAvatarTool :: (Chat.Chat :> es, KatipE :> es) => Tool es
userAvatarTool = Tool
  { name = "user_avatar"
  , description = "Get avatar information for a platform user id and send the avatar image to the current chat."
  , parameters = objectSchema
      [ fieldText "user_id" "Platform user id to query. Use message_info first when the target is the current sender or a mentioned user. 0 is invalid."
      ]
      ["user_id"]
  , noisy = False
  , allowed = everyone
  , start = \context -> pure \args ->
      withParsedToolArgs userAvatarArgs args \userId -> do
        avatar <- Chat.getUserAvatar context.message userId
        case avatar of
          Nothing ->
            pure (toolText "No avatar is available for this user on this platform.")
          Just value ->
            userAvatarResult context value
  }

listGroupMembersTool :: Chat.Chat :> es => Tool es
listGroupMembersTool = Tool
  { name = "group_members"
  , description = "List members in the current group chat, including platform user ids and nicknames when available. QQ groups are supported. Telegram Bot API does not expose full member lists, so Telegram may return unavailable."
  , parameters = objectSchema [] []
  , noisy = False
  , allowed = everyone
  , start = \context -> pure \_ -> do
      members <- Chat.listGroupMembers context.message
      pure (toolText (maybe "Group member listing is not available for this platform or chat." jsonText members))
  }

currentMessageInfoTool :: Tool es
currentMessageInfoTool = Tool
  { name = "message_info"
  , description = "Return structured metadata for the current message, including platform, chat, sender, message ids, mentions, image URLs, and text."
  , parameters = objectSchema [] []
  , noisy = False
  , allowed = everyone
  , start = \context -> pure \_ ->
      pure (toolText (jsonText (currentMessageInfoValue context.message)))
  }

currentMessageInfoValue :: IncomingMessage -> Aeson.Value
currentMessageInfoValue message =
  Aeson.object
    [ "platform" Aeson..= chatPlatformKey message.platform
    , "chat_kind" Aeson..= (show message.kind :: Text)
    , "chat_id" Aeson..= message.chatId
    , "chat_aliases" Aeson..= message.chatAliases
    , "message_id" Aeson..= message.messageId
    , "reply_to_message_id" Aeson..= message.replyToMessageId
    , "sender_id" Aeson..= message.senderId
    , "sender_username" Aeson..= message.senderUsername
    , "mentions" Aeson..= Aeson.object
        [ "user_ids" Aeson..= message.mentions
        , "text_user_ids" Aeson..= message.mentionUsernames
        ]
    , "image_urls" Aeson..= message.imageUrls
    , "text" Aeson..= message.text
    ]

queryChatLogArgs :: Aeson.Value -> AesonTypes.Parser (Integer, Bool)
queryChatLogArgs =
  Aeson.withObject "query chat log arguments" $ \o -> do
    limit <- o Aeson..: Key.fromText "limit"
    includeBotMessages <- fromMaybe False <$> o Aeson..:? Key.fromText "include_bot_messages"
    pure (limit, includeBotMessages)

queryCurrentSenderChatLogArgs :: Aeson.Value -> AesonTypes.Parser ([[Text]], Int)
queryCurrentSenderChatLogArgs =
  Aeson.withObject "query current sender chat log arguments" $ \o -> do
    keywords <- o Aeson..: Key.fromText "keywords"
    limit <- o Aeson..: Key.fromText "limit"
    when (limit < 0) do
      fail "limit must be >= 0."
    when (limit > 100) do
      fail "limit must be <= 100."
    pure (keywords, limit)

currentSenderChatLogScopeError :: IncomingMessage -> Maybe Text
currentSenderChatLogScopeError message
  | isNothing message.senderId =
      Just "Current message has no sender_id; cannot query sender-scoped chat log."
  | isNothing message.chatId =
      Just "Current message has no chat_id; cannot query chat-scoped chat log."
  | otherwise =
      Nothing

mentionUserArgs :: Aeson.Value -> AesonTypes.Parser (Text, Text)
mentionUserArgs =
  Aeson.withObject "mention user arguments" $ \o -> do
    userId <- validateUserId =<< parseUserIdValue =<< o Aeson..: Key.fromText "user_id"
    text <- o Aeson..: Key.fromText "text"
    pure (userId, text)

memberInfoArgs :: Aeson.Value -> AesonTypes.Parser Text
memberInfoArgs =
  Aeson.withObject "member info arguments" $ \o ->
    validateUserId =<< parseUserIdValue =<< o Aeson..: Key.fromText "user_id"

userAvatarArgs :: Aeson.Value -> AesonTypes.Parser Text
userAvatarArgs =
  Aeson.withObject "user avatar arguments" $ \o -> do
    userId <- o Aeson..: Key.fromText "user_id"
    validateUserId =<< parseUserIdValue userId

parseUserIdValue :: Aeson.Value -> AesonTypes.Parser Text
parseUserIdValue value =
  (Text.strip <$> Aeson.parseJSON value)
    <|> (Text.pack . show <$> (Aeson.parseJSON value :: AesonTypes.Parser Integer))

validateUserId :: Text -> AesonTypes.Parser Text
validateUserId userId
  | Text.null (Text.strip userId) =
      fail "user_id must not be empty."
  | Text.strip userId == "0" =
      fail "user_id must not be 0."
  | otherwise =
      pure (Text.strip userId)

avatarUrl :: Aeson.Value -> Maybe Text
avatarUrl =
  AesonTypes.parseMaybe $
    Aeson.withObject "user avatar" (Aeson..: Key.fromText "avatar_url")

userAvatarResult :: (Chat.Chat :> es, KatipE :> es) => AgentContext es -> Aeson.Value -> Eff es ToolResult
userAvatarResult context value =
  case avatarUrl value of
    Nothing ->
      pure (toolText (jsonText value))
    Just url -> do
      let body = ReplyBody.imageDirective url
      sent <- Chat.replyTo context.message body
      logInfo [i|user_avatar sent avatar image: url=#{url} message_id=#{show sent :: Text}|]
      pure (toolTextWithImages (jsonText value) [url])

sendReplyArgs :: Aeson.Value -> AesonTypes.Parser Text
sendReplyArgs =
  Aeson.withObject "send reply arguments" $ \o -> do
    text <- Text.strip . fromMaybe "" <$> o Aeson..:? Key.fromText "text"
    imageUrls <- map Text.strip . fromMaybe [] <$> o Aeson..:? Key.fromText "image_urls"
    let body = replyBodyWithImages text (filter (not . Text.null) imageUrls)
    when (Text.null body) do
      fail "Either text or image_urls must be provided."
    pure body

sendFileArgs :: Aeson.Value -> AesonTypes.Parser FilePath
sendFileArgs =
  Aeson.withObject "send file arguments" $ \o -> do
    rawPath <- Text.strip <$> o Aeson..: Key.fromText "path"
    let path = fromMaybe rawPath (Text.stripPrefix "file://" rawPath)
    when (Text.null (Text.strip path)) do
      fail "path must not be empty."
    pure (Text.unpack path)

replyBodyWithImages :: Text -> [Text] -> Text
replyBodyWithImages text imageUrls =
  Text.strip $ Text.unlines $
    [ text | not (Text.null text) ]
      <> map ReplyBody.imageDirective imageUrls

fieldTextArrayArray :: Text -> Text -> (Text, Aeson.Value)
fieldTextArrayArray name description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "array"
      , "items" Aeson..= Aeson.object
          [ "type" Aeson..= Aeson.String "array"
          , "items" Aeson..= Aeson.object
              [ "type" Aeson..= Aeson.String "string"
              ]
          ]
      , "description" Aeson..= description
      ]
  )

fieldIntegerMax :: Text -> Int -> Text -> (Text, Aeson.Value)
fieldIntegerMax name maximum description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "integer"
      , "minimum" Aeson..= (0 :: Int)
      , "maximum" Aeson..= maximum
      , "description" Aeson..= description
      ]
  )
