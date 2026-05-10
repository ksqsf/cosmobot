{-|
Module      : Bot.Effect.Chat
Description : Unified chat platform effect
Stability   : experimental
-}

module Bot.Effect.Chat
  ( -- * Effect
    Chat
  , replyTo
  , getMessageContent
  , getSenderMemberInfo
  , getMemberInfo
  , listGroupMembers
  , mentionUser
  , runChatWith

    -- * Reply rendering
  , renderReplyBody
  , replyImageUrls
  )
where

import Bot.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

-- | Platform-independent chat operations used by handlers and tools.
data Chat :: Effect where
  ReplyTo
    :: IncomingMessage
    -> Text
    -> Chat m (Maybe Integer)
  GetMessageContent
    :: IncomingMessage
    -> Integer
    -> Chat m (Maybe ReferencedMessage)
  GetSenderMemberInfo
    :: IncomingMessage
    -> Chat m (Maybe Aeson.Value)
  GetMemberInfo
    :: IncomingMessage
    -> Integer
    -> Chat m (Maybe Aeson.Value)
  ListGroupMembers
    :: IncomingMessage
    -> Chat m (Maybe Aeson.Value)
  MentionUser
    :: IncomingMessage
    -> Integer
    -> Text
    -> Chat m (Maybe Integer)

type instance DispatchOf Chat = Dynamic

-- | Reply to the chat containing the incoming message.
replyTo :: Chat :> es => IncomingMessage -> Text -> Eff es (Maybe Integer)
replyTo message body =
  send (ReplyTo message body)

-- | Fetch content for a referenced platform message id.
getMessageContent :: Chat :> es => IncomingMessage -> Integer -> Eff es (Maybe ReferencedMessage)
getMessageContent message messageId =
  send (GetMessageContent message messageId)

-- | Fetch member info for the sender of the current message.
getSenderMemberInfo :: Chat :> es => IncomingMessage -> Eff es (Maybe Aeson.Value)
getSenderMemberInfo message =
  send (GetSenderMemberInfo message)

-- | Fetch member info for a user id in the current chat.
getMemberInfo :: Chat :> es => IncomingMessage -> Integer -> Eff es (Maybe Aeson.Value)
getMemberInfo message userId =
  send (GetMemberInfo message userId)

-- | List group members when the platform exposes such an API.
listGroupMembers :: Chat :> es => IncomingMessage -> Eff es (Maybe Aeson.Value)
listGroupMembers message =
  send (ListGroupMembers message)

-- | Send a reply that mentions a platform user id.
mentionUser :: Chat :> es => IncomingMessage -> Integer -> Text -> Eff es (Maybe Integer)
mentionUser message userId body =
  send (MentionUser message userId body)

-- | Interpret chat operations by delegating each operation to platform code.
runChatWith
  :: (IncomingMessage -> Text -> Eff es (Maybe Integer))
  -> (IncomingMessage -> Integer -> Eff es (Maybe ReferencedMessage))
  -> (IncomingMessage -> Eff es (Maybe Aeson.Value))
  -> (IncomingMessage -> Integer -> Eff es (Maybe Aeson.Value))
  -> (IncomingMessage -> Eff es (Maybe Aeson.Value))
  -> (IncomingMessage -> Integer -> Text -> Eff es (Maybe Integer))
  -> Eff (Chat : es) a
  -> Eff es a
runChatWith reply fetch fetchSenderMember fetchMember listMembers mention = interpret $ \_ -> \case
  ReplyTo message body ->
    reply message body
  GetMessageContent message messageId ->
    fetch message messageId
  GetSenderMemberInfo message ->
    fetchSenderMember message
  GetMemberInfo message userId ->
    fetchMember message userId
  ListGroupMembers message ->
    listMembers message
  MentionUser message userId body ->
    mention message userId body

-- | Remove image directives from a reply body before storing it as text.
renderReplyBody :: Text -> Text
renderReplyBody body =
  Text.strip (Text.unlines (filter (not . isImageLine) (Text.lines body)))

-- | Extract image URLs from @\[image\] ...@ reply directives.
replyImageUrls :: Text -> [Text]
replyImageUrls body =
  mapMaybe imageLineUrl (Text.lines body)

isImageLine :: Text -> Bool
isImageLine =
  isJust . imageLineUrl

imageLineUrl :: Text -> Maybe Text
imageLineUrl line =
  let marker = "[image] "
      stripped = Text.strip line
  in Text.strip <$> Text.stripPrefix marker stripped
