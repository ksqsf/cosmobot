{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.RPC
Description : RPC chat driver implementation
Stability   : experimental
-}

module Bot.Chat.Driver.RPC
  ( RpcChatDriver
  , rpcChatDriver
  )
where

import qualified Bot.Chat.Types as Chat
import Bot.Chat.Driver.Types
import Bot.Core.Message
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Effect.Media as Media
import Bot.Effect.Media (MediaObject (..))
import qualified Bot.Effect.Storage as StorageEffect
import qualified Bot.Media.Object as MediaObject
import Bot.Prelude
import qualified Bot.JSONRPC as JSONRPC
import qualified Bot.RPC.State as RPC
import qualified Bot.Storage.Session as SessionStorage
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Effectful.FileSystem as FileSystem
import System.FilePath (takeFileName)

data RpcChatDriver = RpcChatDriver
  { rpcState :: !RPC.RpcState }

rpcChatDriver :: RPC.RpcState -> RpcChatDriver
rpcChatDriver rpcState =
  RpcChatDriver{rpcState}

instance ChatDriver RpcChatDriver where
  type ChatDriverEffects RpcChatDriver es = (Concurrent :> es, IOE :> es, StorageEffect.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)

  driverPlatform _ =
    PlatformRPC

  sendReplyMessage driver message body = do
    reply <- rpcReplyContent body
    storeReplyMessage driver message reply

  replyAudio driver message audioRef caption = do
    let body = maybe audioRef (\c -> c <> "\n" <> audioRef) caption
    sendReplyMessage driver message body

  uploadFile driver message path fileName =
    rpcStoredFile path (uploadFileName path fileName) >>= \case
      Nothing -> pure (Left [i|RPC could not read or store file: #{path}|])
      Just attachment ->
        storeReplyMessage driver message RpcReplyContent
          { text = ""
          , imageUrls = []
          , attachments = [attachment]
          }

  editMessage driver message messageId body = do
    let sessionId = RPC.sessionIdFromMessage message
    reply <- rpcReplyContent body
    updated <- SessionStorage.replaceMessageContent
      (RPC.unRpcSessionId sessionId)
      messageId
      reply.text
      reply.imageUrls
      reply.attachments
    for_ updated \stored ->
      RPC.publish driver.rpcState (RPC.ChatEvents sessionId) (Aeson.toJSON (JSONRPC.notification "chat.message_update" (RPC.storedMessageToRpc stored)))
    pure (isJust updated)

  completeMessageEdit driver message messageId = do
    let payload = Aeson.object
          [ "sessionId" Aeson..= RPC.sessionIdFromMessage message
          , "messageId" Aeson..= messageId
          ]
    RPC.publish driver.rpcState (RPC.ChatEvents (RPC.sessionIdFromMessage message)) (Aeson.toJSON (JSONRPC.notification "chat.message_done" payload))
    pure True

  publishActivity driver message activity = do
    let sessionId = RPC.sessionIdFromMessage message
    RPC.publish driver.rpcState (RPC.ChatEvents sessionId) (activityNotification sessionId activity)

  messageOutPolicy _ _ =
    pure (Chat.EditableMessage 1200 4000)

activityNotification :: RPC.RpcSessionId -> Chat.Activity -> Aeson.Value
activityNotification sessionId = \case
  Chat.ReasoningStarted runId turn ->
    notification "chat.reasoning_start" runId turn []
  Chat.ReasoningFinished runId turn answerKind ->
    notification "chat.reasoning_end" runId turn ["answerKind" Aeson..= answerKind]
  Chat.ToolCallStarted runId turn toolCallId toolName ->
    notification "chat.tool_call_start" runId turn
      ["toolCallId" Aeson..= toolCallId, "toolName" Aeson..= toolName]
  Chat.ToolCallFinished runId turn toolCallId toolName status ->
    notification "chat.tool_call_end" runId turn
      [ "toolCallId" Aeson..= toolCallId
      , "toolName" Aeson..= toolName
      , "status" Aeson..= status
      ]
  where
    notification method runId turn fields =
      Aeson.toJSON $ JSONRPC.notification method $ Aeson.object $
        [ "sessionId" Aeson..= sessionId
        , "runId" Aeson..= runId
        , "turn" Aeson..= turn
        ] <> fields

data RpcReplyContent = RpcReplyContent
  { text :: !Text
  , imageUrls :: ![Text]
  , attachments :: ![SessionStorage.StoredMediaRef]
  }

storeReplyMessage
  :: (Concurrent :> es, StorageEffect.Storage :> es)
  => RpcChatDriver
  -> IncomingMessage
  -> RpcReplyContent
  -> Eff es (Either Text MessageId)
storeReplyMessage driver message reply = do
  let sessionId = RPC.sessionIdFromMessage message
      parentMessageId = message.messageId
  stored <- SessionStorage.appendMessage
    (RPC.unRpcSessionId sessionId)
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
      pure (Left "RPC reply did not produce a message id.")
    Right (Just storedReply) -> do
      RPC.publish driver.rpcState (RPC.ChatEvents sessionId) (Aeson.toJSON (JSONRPC.notification "chat.message" (RPC.storedMessageToRpc storedReply)))
      pure (Right storedReply.messageId)

rpcReplyContent
  :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => Text
  -> Eff es RpcReplyContent
rpcReplyContent body = do
  converted <- traverse rpcReplyImage (ReplyBody.replyImageUrls body)
  pure RpcReplyContent
    { text = ReplyBody.renderReplyBody body
    , imageUrls = [url | Left url <- converted]
    , attachments = [attachment | Right attachment <- converted]
    }

rpcReplyImage
  :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => Text
  -> Eff es (Either Text SessionStorage.StoredMediaRef)
rpcReplyImage ref =
  case Text.stripPrefix "file://" (Text.strip ref) of
    Nothing ->
      case RPC.parseMediaId ref of
        Nothing -> pure (Left ref)
        Just fileId -> maybe (Left ref) Right <$> storedMediaAttachment fileId
    Just pathText -> do
      let path = Text.unpack pathText
      exists <- FileSystem.doesFileExist path
      if exists
        then do
          attachment <- rpcStoredFile path (Text.pack (takeFileName path))
          pure (maybe (Left ref) Right attachment)
        else
          pure (Left ref)

rpcStoredFile
  :: (FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => FilePath
  -> Text
  -> Eff es (Maybe SessionStorage.StoredMediaRef)
rpcStoredFile path name = do
  exists <- FileSystem.doesFileExist path
  if not exists
    then pure Nothing
    else do
      mediaObject <- MediaObject.fileObject ("file://" <> Text.pack path)
      mediaRef <- Media.storeMediaObject mediaObject{sourceName = Just name}
      maybe (pure Nothing) storedMediaAttachment (mediaRef >>= RPC.parseMediaId)

storedMediaAttachment :: Media.Media :> es => Text -> Eff es (Maybe SessionStorage.StoredMediaRef)
storedMediaAttachment fileId =
  Media.mediaFileInfo fileId >>= \case
    Nothing -> pure Nothing
    Just info -> do
      url <- Media.publicMediaRef info.ref
      pure (Just (RPC.storedMediaRef info url))
