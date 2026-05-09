{-
Module      : Bot.Effect.Chat
Description : Unified chat platform effect
Stability   : experimental
-}

module Bot.Effect.Chat where

import Bot.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

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

replyTo :: Chat :> es => IncomingMessage -> Text -> Eff es (Maybe Integer)
replyTo message body =
  send (ReplyTo message body)

getMessageContent :: Chat :> es => IncomingMessage -> Integer -> Eff es (Maybe ReferencedMessage)
getMessageContent message messageId =
  send (GetMessageContent message messageId)

getSenderMemberInfo :: Chat :> es => IncomingMessage -> Eff es (Maybe Aeson.Value)
getSenderMemberInfo message =
  send (GetSenderMemberInfo message)

getMemberInfo :: Chat :> es => IncomingMessage -> Integer -> Eff es (Maybe Aeson.Value)
getMemberInfo message userId =
  send (GetMemberInfo message userId)

listGroupMembers :: Chat :> es => IncomingMessage -> Eff es (Maybe Aeson.Value)
listGroupMembers message =
  send (ListGroupMembers message)

mentionUser :: Chat :> es => IncomingMessage -> Integer -> Text -> Eff es (Maybe Integer)
mentionUser message userId body =
  send (MentionUser message userId body)

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

renderReplyBody :: Text -> Text
renderReplyBody body =
  Text.strip (Text.unlines (filter (not . isImageLine) (Text.lines body)))

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
