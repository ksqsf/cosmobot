{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.ACP
Description : ACP chat driver implementation
Stability   : experimental
-}

module Bot.Chat.Driver.ACP
  ( AcpChatDriver
  , acpChatDriver
  )
where

import qualified Bot.ACP.Content as ACPContent
import qualified Bot.ACP.State as ACP
import qualified Bot.Chat.Types as Chat
import Bot.Chat.Driver.Types
import Bot.Core.Message
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Effect.Media as Media
import Bot.Effect.Media (MediaObject (..))
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import qualified Bot.Session as Session
import qualified Bot.Storage.Session as SessionStorage
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified JSONRPC
import qualified Streaming.ByteString as Q
import System.FilePath (takeExtension, takeFileName)

newtype AcpChatDriver = AcpChatDriver
  { acpState :: ACP.AcpState
  }

acpChatDriver :: ACP.AcpState -> AcpChatDriver
acpChatDriver acpState =
  AcpChatDriver{acpState}

instance ChatDriver AcpChatDriver where
  type ChatDriverEffects AcpChatDriver es = (Concurrent :> es, IOE :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)

  driverPlatform _ =
    PlatformACP

  sendReplyMessage driver message body =
    storeReply driver message body >>= \case
      Left err ->
        pure (Left err)
      Right messageId -> do
        ACP.notifyPromptComplete driver.acpState (ACP.sessionIdFromMessage message) messageId
        pure (Right messageId)

  sendStreamingReplyMessage driver message body =
    storeReply driver message body

  editMessage driver message messageId body = do
    let sessionId = ACP.sessionIdFromMessage message
        text = ReplyBody.renderReplyBody body
    updated <- SessionStorage.updateMessageText (ACP.acpSessionIdText sessionId) messageId text
    when updated $
      ACP.broadcast driver.acpState $
        Aeson.toJSON $
          sessionUpdateNotification sessionId $
            agentMessageChunkUpdate messageId (ACPContent.textContentBlock text)
    pure updated

  completeMessageEdit driver message messageId = do
    ACP.notifyPromptComplete driver.acpState (ACP.sessionIdFromMessage message) messageId
    pure True

  messageOutPolicy _ _ =
    pure (Chat.EditableMessage 1200 4000)

storeReply
  :: (Concurrent :> es, IOE :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => AcpChatDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Either Text MessageId)
storeReply driver message body = do
  let sessionId = ACP.sessionIdFromMessage message
      parentMessageId = message.messageId
  reply <- acpReplyContent body
  stored <- SessionStorage.appendMessage
    (ACP.acpSessionIdText sessionId)
    "assistant"
    reply.text
    reply.imageUrls
    reply.attachments
    parentMessageId
    parentMessageId
  case stored of
    Left err ->
      pure (Left err)
    Right Nothing ->
      pure (Left "ACP reply did not produce a message id.")
    Right (Just storedReply) -> do
      let messageId = storedReply.messageId
      traverse_ (broadcastReplyContent driver sessionId messageId) $
        ACPContent.messageContentBlocks reply.text reply.imageUrls (map Session.storedAttachmentToSession reply.attachments)
      pure (Right messageId)

data AcpReplyContent = AcpReplyContent
  { text :: !Text
  , imageUrls :: ![Text]
  , attachments :: ![SessionStorage.StoredMediaRef]
  }

acpReplyContent
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => Text
  -> Eff es AcpReplyContent
acpReplyContent body = do
  converted <- traverse acpReplyImage (ReplyBody.replyImageUrls body)
  pure AcpReplyContent
    { text = ReplyBody.renderReplyBody body
    , imageUrls = [url | Left url <- converted]
    , attachments = [attachment | Right attachment <- converted]
    }

acpReplyImage
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => Text
  -> Eff es (Either Text SessionStorage.StoredMediaRef)
acpReplyImage ref =
  case Text.stripPrefix "file://" (Text.strip ref) of
    Just pathText -> do
      let path = Text.unpack pathText
      exists <- FileSystem.doesFileExist path
      if exists
        then do
          bytes <- FileSystemByteString.readFile path
          mediaRef <- Media.storeMediaObject $
            MediaObject
              { bytes = Q.fromStrict bytes
              , mimeType = imageMediaType path
              , sourceName = Just (Text.pack (takeFileName path))
              }
          case mediaRef >>= Session.parseMediaId of
            Nothing ->
              pure (Left ref)
            Just fileId ->
              Media.mediaFileInfo fileId >>= \case
                Nothing ->
                  pure (Left ref)
                Just info -> do
                  url <- Media.publicMediaRef info.ref
                  pure (Right (Session.storedMediaRef info url))
        else
          pure (Left ref)
    Nothing ->
      case Session.parseMediaId ref of
        Nothing ->
          pure (Left ref)
        Just fileId ->
          Media.mediaFileInfo fileId >>= \case
            Nothing ->
              pure (Left ref)
            Just info -> do
              url <- Media.publicMediaRef info.ref
              pure (Right (Session.storedMediaRef info url))

broadcastReplyContent
  :: Concurrent :> es
  => AcpChatDriver
  -> ACP.AcpSessionId
  -> MessageId
  -> ACPContent.AcpContentBlock
  -> Eff es ()
broadcastReplyContent driver sessionId messageId content =
  ACP.broadcast driver.acpState $
    Aeson.toJSON $
      sessionUpdateNotification sessionId $
        agentMessageChunkUpdate messageId content

sessionUpdateNotification :: ACP.AcpSessionId -> Aeson.Value -> JSONRPC.JSONRPCMessage
sessionUpdateNotification sessionId update =
  JSONRPC.NotificationMessage $
    JSONRPC.JSONRPCNotification
      JSONRPC.rPC_VERSION
      "session/update"
      ( Aeson.object
          [ "sessionId" Aeson..= ACP.acpSessionIdText sessionId
          , "update" Aeson..= update
          ]
      )

agentMessageChunkUpdate :: MessageId -> ACPContent.AcpContentBlock -> Aeson.Value
agentMessageChunkUpdate messageId content =
  Aeson.object
    [ "sessionUpdate" Aeson..= ("agent_message_chunk" :: Text)
    , "messageId" Aeson..= messageId
    , "content" Aeson..= ACPContent.contentBlockValue content
    ]

imageMediaType :: FilePath -> Text
imageMediaType path =
  case Text.toLower (Text.pack (takeExtension path)) of
    ".avif" -> "image/avif"
    ".gif" -> "image/gif"
    ".jpeg" -> "image/jpeg"
    ".jpg" -> "image/jpeg"
    ".png" -> "image/png"
    ".webp" -> "image/webp"
    _ -> "application/octet-stream"
