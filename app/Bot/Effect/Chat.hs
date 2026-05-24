{-|
Module      : Bot.Effect.Chat
Description : Unified chat capability facade
Stability   : experimental
-}

module Bot.Effect.Chat
  ( -- * Effect
    Chat
  , replyTo
  , replyAudio
  , uploadFile
  , editMessage
  , deleteMessage
  , ReplyStreamStyle (..)
  , ReplyStreamUpdate (..)
  , streamReplyTo
  , streamReplySegmentsTo
  , getMessageContent
  , getSenderMemberInfo
  , getMemberInfo
  , getUserAvatar
  , listGroupMembers
  , mentionUser
  , setMemberTitle
  , ChatHandlers (..)
  , runChatWith
  , runChatRecordingSelfMessages
  , runChatRecordingExtraMessages

    -- * Reply rendering
  , imageDirective
  , renderReplyBody
  , replyImageUrls
  , isBase64ImageRef
  )
where

import Bot.Core.Message
import Bot.Prelude
import Bot.Chat.Types
import Bot.Core.ReplyBody
import qualified Bot.Chat.ReplyStream as ReplyStream
import qualified Data.Aeson as Aeson

-- | Platform-independent chat operations used by handlers and tools.
data Chat :: Effect where
  ReplyTo
    :: IncomingMessage
    -> Text
    -> Chat m (Maybe MessageId)
  ReplyAudio
    :: IncomingMessage
    -> Text
    -> Maybe Text
    -> Chat m (Either Text MessageId)
  UploadFile
    :: IncomingMessage
    -> FilePath
    -> Chat m (Either Text MessageId)
  EditMessage
    :: IncomingMessage
    -> MessageId
    -> Text
    -> Chat m Bool
  DeleteMessage
    :: IncomingMessage
    -> MessageId
    -> Chat m Bool
  ReplyStreamStyle
    :: IncomingMessage
    -> Chat m ReplyStreamStyle
  GetMessageContent
    :: IncomingMessage
    -> MessageId
    -> Chat m (Maybe ReferencedMessage)
  GetSenderMemberInfo
    :: IncomingMessage
    -> Chat m (Maybe Aeson.Value)
  GetMemberInfo
    :: IncomingMessage
    -> Text
    -> Chat m (Maybe Aeson.Value)
  GetUserAvatar
    :: IncomingMessage
    -> Text
    -> Chat m (Maybe Aeson.Value)
  ListGroupMembers
    :: IncomingMessage
    -> Chat m (Maybe Aeson.Value)
  MentionUser
    :: IncomingMessage
    -> Text
    -> Text
    -> Chat m (Maybe MessageId)
  SetMemberTitle
    :: IncomingMessage
    -> Text
    -> Text
    -> Chat m Bool

type instance DispatchOf Chat = Dynamic

-- | Reply to the chat containing the incoming message.
replyTo :: Chat :> es => IncomingMessage -> Text -> Eff es (Maybe MessageId)
replyTo message body =
  send (ReplyTo message body)

-- | Send an audio reference to the chat containing the incoming message.
replyAudio :: Chat :> es => IncomingMessage -> Text -> Maybe Text -> Eff es (Either Text MessageId)
replyAudio message audioRef caption =
  send (ReplyAudio message audioRef caption)

-- | Upload a local file to the chat containing the incoming message.
uploadFile :: Chat :> es => IncomingMessage -> FilePath -> Eff es (Either Text MessageId)
uploadFile message path =
  send (UploadFile message path)

-- | Edit a previously sent message when the platform supports it.
editMessage :: Chat :> es => IncomingMessage -> MessageId -> Text -> Eff es Bool
editMessage message messageId body =
  send (EditMessage message messageId body)

-- | Delete or recall a previously sent message when the platform supports it.
deleteMessage :: Chat :> es => IncomingMessage -> MessageId -> Eff es Bool
deleteMessage message messageId =
  send (DeleteMessage message messageId)

streamReplyTo
  :: (Chat :> es, Prim :> es)
  => IncomingMessage
  -> (r -> Text)
  -> Stream (Of Text) (Eff es) r
  -> Stream (Of ReplyStreamUpdate) (Eff es) (Maybe MessageId, r)
streamReplyTo =
  ReplyStream.streamReplyTo chatReplyStreamCallbacks

streamReplySegmentsTo
  :: (Chat :> es, Prim :> es)
  => IncomingMessage
  -> (r -> Text)
  -> Stream (Of ReplySegmentEvent) (Eff es) r
  -> Stream (Of ReplyStreamUpdate) (Eff es) (Maybe MessageId, r)
streamReplySegmentsTo =
  ReplyStream.streamReplySegmentsTo chatReplyStreamCallbacks

chatReplyStreamCallbacks :: Chat :> es => ReplyStream.ReplyStreamCallbacks es
chatReplyStreamCallbacks = ReplyStream.ReplyStreamCallbacks
  { ReplyStream.replyStreamStyleFor = \message -> send (ReplyStreamStyle message)
  , ReplyStream.sendReplyTo = replyTo
  , ReplyStream.editReplyMessage = editMessage
  }

-- | Fetch content for a referenced platform message id.
getMessageContent :: Chat :> es => IncomingMessage -> MessageId -> Eff es (Maybe ReferencedMessage)
getMessageContent message messageId =
  send (GetMessageContent message messageId)

-- | Fetch member info for the sender of the current message.
getSenderMemberInfo :: Chat :> es => IncomingMessage -> Eff es (Maybe Aeson.Value)
getSenderMemberInfo message =
  send (GetSenderMemberInfo message)

-- | Fetch member info for a user id in the current chat.
getMemberInfo :: Chat :> es => IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
getMemberInfo message userId =
  send (GetMemberInfo message userId)

-- | Fetch avatar information for a platform user id.
getUserAvatar :: Chat :> es => IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
getUserAvatar message userId =
  send (GetUserAvatar message userId)

-- | List group members when the platform exposes such an API.
listGroupMembers :: Chat :> es => IncomingMessage -> Eff es (Maybe Aeson.Value)
listGroupMembers message =
  send (ListGroupMembers message)

-- | Send a reply that mentions a platform user id.
mentionUser :: Chat :> es => IncomingMessage -> Text -> Text -> Eff es (Maybe MessageId)
mentionUser message userId body =
  send (MentionUser message userId body)

-- | Set a platform-specific title for a group member when supported.
setMemberTitle :: Chat :> es => IncomingMessage -> Text -> Text -> Eff es Bool
setMemberTitle message userId title =
  send (SetMemberTitle message userId title)

-- | Interpret chat operations by delegating each operation to platform code.
data ChatHandlers es = ChatHandlers
  { handleReplyTo :: IncomingMessage -> Text -> Eff es (Maybe MessageId)
  , handleReplyAudio :: IncomingMessage -> Text -> Maybe Text -> Eff es (Either Text MessageId)
  , handleUploadFile :: IncomingMessage -> FilePath -> Eff es (Either Text MessageId)
  , handleEditMessage :: IncomingMessage -> MessageId -> Text -> Eff es Bool
  , handleDeleteMessage :: IncomingMessage -> MessageId -> Eff es Bool
  , handleReplyStreamStyle :: IncomingMessage -> Eff es ReplyStreamStyle
  , handleGetMessageContent :: IncomingMessage -> MessageId -> Eff es (Maybe ReferencedMessage)
  , handleGetSenderMemberInfo :: IncomingMessage -> Eff es (Maybe Aeson.Value)
  , handleGetMemberInfo :: IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
  , handleGetUserAvatar :: IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
  , handleListGroupMembers :: IncomingMessage -> Eff es (Maybe Aeson.Value)
  , handleMentionUser :: IncomingMessage -> Text -> Text -> Eff es (Maybe MessageId)
  , handleSetMemberTitle :: IncomingMessage -> Text -> Text -> Eff es Bool
  }

runChatWith
  :: ChatHandlers es
  -> Eff (Chat : es) a
  -> Eff es a
runChatWith handlers = interpret $ \_ -> \case
  ReplyTo message body ->
    handlers.handleReplyTo message body
  ReplyAudio message audioRef caption ->
    handlers.handleReplyAudio message audioRef caption
  UploadFile message path ->
    handlers.handleUploadFile message path
  EditMessage message messageId body ->
    handlers.handleEditMessage message messageId body
  DeleteMessage message messageId ->
    handlers.handleDeleteMessage message messageId
  ReplyStreamStyle message ->
    handlers.handleReplyStreamStyle message
  GetMessageContent message messageId ->
    handlers.handleGetMessageContent message messageId
  GetSenderMemberInfo message ->
    handlers.handleGetSenderMemberInfo message
  GetMemberInfo message userId ->
    handlers.handleGetMemberInfo message userId
  GetUserAvatar message userId ->
    handlers.handleGetUserAvatar message userId
  ListGroupMembers message ->
    handlers.handleListGroupMembers message
  MentionUser message userId body ->
    handlers.handleMentionUser message userId body
  SetMemberTitle message userId title ->
    handlers.handleSetMemberTitle message userId title

-- | Locally override chat sending so self messages can be recorded while all
-- platform operations still delegate to the outer 'Chat' interpreter.
runChatRecordingSelfMessages
  :: Chat :> es
  => (Text -> Eff es ())
  -> Eff es a
  -> Eff es a
runChatRecordingSelfMessages recordSelf =
  interpose $ \localEnv -> \case
    operation@(ReplyTo _ body) -> do
      sent <- passthrough localEnv operation
      recordSelf body
      pure sent
    operation@(MentionUser _ _ body) -> do
      sent <- passthrough localEnv operation
      recordSelf body
      pure sent
    operation ->
      passthrough localEnv operation

runChatRecordingExtraMessages
  :: Chat :> es
  => (Maybe MessageId -> Eff es ())
  -> Eff es a
  -> Eff es a
runChatRecordingExtraMessages record =
  interpose $ \localEnv -> \case
    operation@ReplyTo{} -> do
      sent <- passthrough localEnv operation
      record sent
      pure sent
    operation@MentionUser{} -> do
      sent <- passthrough localEnv operation
      record sent
      pure sent
    operation@ReplyAudio{} -> do
      result <- passthrough localEnv operation
      record (rightToMaybe result)
      pure result
    operation@UploadFile{} -> do
      result <- passthrough localEnv operation
      record (rightToMaybe result)
      pure result
    operation ->
      passthrough localEnv operation
