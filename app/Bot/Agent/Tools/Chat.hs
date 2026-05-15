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
  , currentMentionsTool
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

userAvatarTool :: Chat.Chat :> es => Tool es
userAvatarTool = Tool
  { name = "get_user_avatar"
  , description = "Get avatar information for a platform user id. If user_id is omitted, this queries the sender of the current message."
  , parameters = objectSchema
      [ fieldInteger "user_id" "Platform user id to query. Defaults to the current message sender."
      ]
      []
  , allowed = everyone
  , start = \context -> pure \args ->
      withParsedToolArgs (userAvatarArgs context.message.senderId) args \userId -> do
        avatar <- Chat.getUserAvatar context.message userId
        pure (toolText (maybe "No avatar is available for this user on this platform." jsonText avatar))
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

currentMentionsTool :: Tool es
currentMentionsTool = Tool
  { name = "get_current_message_mentions"
  , description = "Return platform user ids mentioned in the current message, in message order. On QQ these are QQ numbers from at segments."
  , parameters = objectSchema [] []
  , allowed = everyone
  , start = \context -> pure \_ ->
      pure (toolText (jsonText context.message.mentions))
  }

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

userAvatarArgs :: Maybe Integer -> Aeson.Value -> AesonTypes.Parser Integer
userAvatarArgs senderId =
  Aeson.withObject "user avatar arguments" $ \o ->
    o Aeson..:? Key.fromText "user_id" >>= \case
      Just userId ->
        pure userId
      Nothing ->
        maybe (fail "user_id is required when the current message has no sender id.") pure senderId

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
