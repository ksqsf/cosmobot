{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Effect.Chat
Description : Unified chat capability facade
Stability   : experimental
-}

module Bot.Effect.Chat
  ( -- * Effect
    Chat (..)
  , ChatHandler
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
  , setTyping
  , incomingMessages
  , runChatWithHandler
  , runChatWith
  , chatDriverHandler
  , runChatMappingReplies
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
import qualified Bot.Chat.Driver.Types as Driver
import Bot.Chat.Types
import Bot.Core.ReplyBody
import qualified Bot.Chat.ReplyStream as ReplyStream
import qualified Data.Aeson as Aeson
import qualified Streaming as S

-- | Platform-independent chat operations used by handlers and tools.
data Chat :: Effect where
  ReplyTo
    :: IncomingMessage
    -> Text
    -> Chat m (Either Text MessageId)
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
    -> Chat m (Either Text MessageId)
  SetMemberTitle
    :: IncomingMessage
    -> Text
    -> Text
    -> Chat m Bool
  SetTyping
    :: IncomingMessage
    -> Int
    -> Chat m ()
  IncomingMessages
    :: Chat m (Stream (Of IncomingMessage) m ())

type instance DispatchOf Chat = Dynamic

type ChatHandler es =
  EffectHandler Chat es

-- | Reply to the chat containing the incoming message.
replyTo :: Chat :> es => IncomingMessage -> Text -> Eff es (Either Text MessageId)
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
  -> Stream (Stream (Of Text) (Eff es)) (Eff es) r
  -> Stream (Of ReplyStreamUpdate) (Eff es) (Maybe MessageId, r)
streamReplySegmentsTo =
  ReplyStream.streamReplySegmentsTo chatReplyStreamCallbacks

chatReplyStreamCallbacks :: Chat :> es => ReplyStream.ReplyStreamCallbacks es
chatReplyStreamCallbacks = ReplyStream.ReplyStreamCallbacks
  { ReplyStream.replyStreamStyleFor = \message -> send (ReplyStreamStyle message)
  , ReplyStream.sendReplyTo = \message body -> rightToMaybe <$> replyTo message body
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
mentionUser :: Chat :> es => IncomingMessage -> Text -> Text -> Eff es (Either Text MessageId)
mentionUser message userId body =
  send (MentionUser message userId body)

-- | Set a platform-specific title for a group member when supported.
setMemberTitle :: Chat :> es => IncomingMessage -> Text -> Text -> Eff es Bool
setMemberTitle message userId title =
  send (SetMemberTitle message userId title)

setTyping :: Chat :> es => IncomingMessage -> Int -> Eff es ()
setTyping message timeout =
  send (SetTyping message timeout)

incomingMessages :: Chat :> es => Stream (Of IncomingMessage) (Eff es) ()
incomingMessages = do
  stream <- S.lift (send IncomingMessages)
  stream

runChatWithHandler
  :: ChatHandler es
  -> Eff (Chat : es) a
  -> Eff es a
runChatWithHandler =
  interpret

runChatWith
  :: forall driver es a.
     (Driver.ChatDriver driver, Driver.ChatDriverEffects driver es)
  => driver
  -> Eff (Chat : es) a
  -> Eff es a
runChatWith driver =
  runChatWithHandler (chatDriverHandler driver)

chatDriverHandler
  :: forall driver es.
     (Driver.ChatDriver driver, Driver.ChatDriverEffects driver es)
  => driver
  -> ChatHandler es
chatDriverHandler driver _ = \case
  ReplyTo message body ->
    Driver.replyTo driver message body
  ReplyAudio message audioRef caption ->
    Driver.replyAudio driver message audioRef caption
  UploadFile message path ->
    Driver.uploadFile driver message path
  EditMessage message messageId body ->
    Driver.editMessage driver message messageId body
  DeleteMessage message messageId ->
    Driver.deleteMessage driver message messageId
  ReplyStreamStyle message ->
    Driver.replyStreamStyle driver message
  GetMessageContent message messageId ->
    Driver.getMessageContent driver message messageId
  GetSenderMemberInfo message ->
    Driver.getSenderMemberInfo driver message
  GetMemberInfo message userId ->
    Driver.getMemberInfo driver message userId
  GetUserAvatar message userId ->
    Driver.getUserAvatar driver message userId
  ListGroupMembers message ->
    Driver.listGroupMembers driver message
  MentionUser message userId body ->
    Driver.mentionUser driver message userId body
  SetMemberTitle message userId title ->
    Driver.setMemberTitle driver message userId title
  SetTyping message timeout ->
    Driver.setTyping driver message timeout
  IncomingMessages ->
    pure (pure ())

-- | Locally rewrite ordinary reply bodies while all platform operations still
-- delegate to the outer 'Chat' interpreter.
runChatMappingReplies
  :: Chat :> es
  => (Text -> Eff es (Either Text Text))
  -> Eff es a
  -> Eff es a
runChatMappingReplies rewrite =
  interpose $ \localEnv -> \case
    ReplyTo message body -> do
      rewrite body >>= \case
        Left err ->
          pure (Left err)
        Right rewritten ->
          passthrough localEnv (ReplyTo message rewritten)
    operation ->
      passthrough localEnv operation

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
      result <- passthrough localEnv operation
      record (rightToMaybe result)
      pure result
    operation@MentionUser{} -> do
      result <- passthrough localEnv operation
      record (rightToMaybe result)
      pure result
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
