{-|
Module      : Bot.Agent.Tools.Chat
Description : Agent tools for chat IO and chat metadata
Stability   : experimental
-}

module Bot.Agent.Tools.Chat
  ( queryChatLogTool
  , sendReplyTool
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
  { name = "query_current_chat_log"
  , description = "Return recent messages recorded in the current chat. Results are in chronological order and include sender ids, message ids, mentions, image urls, and text."
  , parameters = objectSchema
      [ fieldInteger "limit" "Maximum number of recent messages to return."
      , fieldBoolean "include_bot_messages" "Whether to include bot messages. Defaults to false."
      ]
      ["limit"]
  , allowed = everyone
  , start = \context -> pure \args ->
      withParsedToolArgs queryChatLogArgs args \(limit, includeBotMessages) -> do
        entries <- ChatLog.queryChat context.message (fromInteger (max 0 limit)) includeBotMessages
        pure (toolText (jsonText entries))
  }

sendReplyTool :: Chat.Chat :> es => Tool es
sendReplyTool = Tool
  { name = "send_reply_to_current_chat"
  , description = "Send a reply message to the same chat as the current user message. Supports text and image URLs. Use image_urls when the user asks you to send an image found or generated elsewhere. Use only when the user asks you to send an additional message before the final answer."
  , parameters = objectSchema
      [ fieldText "text" "Message text to send. May be omitted when image_urls is non-empty."
      , fieldTextArray "image_urls" "Image URLs to send as images in the same reply. The platform must be able to fetch these URLs."
      ]
      []
  , allowed = everyone
  , start = \context -> pure \args ->
      withParsedToolArgs sendReplyArgs args \body -> do
        sent <- Chat.replyTo context.message body
        context.recordBotMessage sent body
        let sentText = show sent :: String
        pure (toolMessage sent [i|Sent message id: #{sentText}|])
  }

mentionUserTool :: Chat.Chat :> es => Tool es
mentionUserTool = Tool
  { name = "mention_user"
  , description = "Send a reply in the current chat that mentions the given user id. On QQ this sends an actual at segment."
  , parameters = objectSchema
      [ fieldInteger "user_id" "Platform user id to mention."
      , fieldText "text" "Message text to send after the mention."
      ]
      ["user_id", "text"]
  , allowed = everyone
  , start = \context -> pure \args ->
      withParsedToolArgs mentionUserArgs args \(userId, text) -> do
        sent <- Chat.mentionUser context.message userId text
        context.recordBotMessage sent text
        let sentText = show sent :: String
        pure (toolMessage sent [i|Sent mention message id: #{sentText}|])
  }

senderMemberInfoTool :: Chat.Chat :> es => Tool es
senderMemberInfoTool = Tool
  { name = "get_current_sender_member_info"
  , description = "Get platform-provided member information for the sender of the current message in the current group chat."
  , parameters = objectSchema [] []
  , allowed = everyone
  , start = \context -> pure \_ -> do
      info <- Chat.getSenderMemberInfo context.message
      pure (toolText (maybe "No member information is available for this message." jsonText info))
  }

memberInfoTool :: Chat.Chat :> es => Tool es
memberInfoTool = Tool
  { name = "get_group_member_info"
  , description = "Get platform-provided member information for any user id in the current group chat."
  , parameters = objectSchema
      [ fieldInteger "user_id" "Platform user id to query in the current group."
      ]
      ["user_id"]
  , allowed = everyone
  , start = \context -> pure \args -> withIntegerArg "user_id" (\userId -> do
      info <- Chat.getMemberInfo context.message userId
      pure (toolText (maybe "No member information is available for this user in the current chat." jsonText info))
      ) args
  }

userAvatarTool :: (Chat.Chat :> es, Log :> es) => Tool es
userAvatarTool = Tool
  { name = "get_user_avatar"
  , description = "Get avatar information for a platform user id and send the avatar image to the current chat."
  , parameters = objectSchema
      [ fieldText "user_id" "Platform user id to query. Use get_current_message_info first when the target is the current sender or a mentioned user. 0 is invalid."
      ]
      ["user_id"]
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
  { name = "list_group_members"
  , description = "List members in the current group chat, including platform user ids and nicknames when available. QQ groups are supported. Telegram Bot API does not expose full member lists, so Telegram may return unavailable."
  , parameters = objectSchema [] []
  , allowed = everyone
  , start = \context -> pure \_ -> do
      members <- Chat.listGroupMembers context.message
      pure (toolText (maybe "Group member listing is not available for this platform or chat." jsonText members))
  }

currentMessageInfoTool :: Tool es
currentMessageInfoTool = Tool
  { name = "get_current_message_info"
  , description = "Return structured metadata for the current message, including platform, chat, sender, message ids, mentions, image URLs, and text."
  , parameters = objectSchema [] []
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
        [ "user_ids" Aeson..= map (Text.pack . show) message.mentions
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

mentionUserArgs :: Aeson.Value -> AesonTypes.Parser (Integer, Text)
mentionUserArgs =
  Aeson.withObject "mention user arguments" $ \o -> do
    userId <- o Aeson..: Key.fromText "user_id"
    text <- o Aeson..: Key.fromText "text"
    pure (userId, text)

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

userAvatarResult :: (Chat.Chat :> es, Log :> es) => AgentContext es -> Aeson.Value -> Eff es ToolResult
userAvatarResult context value =
  case avatarUrl value of
    Nothing ->
      pure (toolText (jsonText value))
    Just url -> do
      let body = ReplyBody.imageDirective url
      sent <- Chat.replyTo context.message body
      logInfo_ [i|get_user_avatar sent avatar image: url=#{url} message_id=#{show sent :: Text}|]
      context.recordBotMessage sent body
      pure (toolMessageWithImages sent (jsonText value) [url])

sendReplyArgs :: Aeson.Value -> AesonTypes.Parser Text
sendReplyArgs =
  Aeson.withObject "send reply arguments" $ \o -> do
    text <- Text.strip . fromMaybe "" <$> o Aeson..:? Key.fromText "text"
    imageUrls <- map Text.strip . fromMaybe [] <$> o Aeson..:? Key.fromText "image_urls"
    let body = replyBodyWithImages text (filter (not . Text.null) imageUrls)
    when (Text.null body) do
      fail "Either text or image_urls must be provided."
    pure body

replyBodyWithImages :: Text -> [Text] -> Text
replyBodyWithImages text imageUrls =
  Text.strip $ Text.unlines $
    [ text | not (Text.null text) ]
      <> map ReplyBody.imageDirective imageUrls
