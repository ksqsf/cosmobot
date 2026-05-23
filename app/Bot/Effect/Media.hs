{-|
Module      : Bot.Effect.Media
Description : Media normalization and object storage capability
Stability   : experimental
-}

module Bot.Effect.Media
  ( Media (..)
  , MediaObject (..)
  , storeMediaObject
  , normalizeMediaRef
  , normalizeMediaRefs
  , normalizeIncomingMessage
  , normalizeIncomingMessages
  , normalizeReferencedMessage
  , normalizeReplyBody
  , runMediaPassthrough
  )
where

import qualified Bot.Core.ReplyBody as ReplyBody
import Bot.Core.Message
import Bot.Prelude
import qualified Data.ByteString as StrictByteString
import qualified Streaming.Prelude as S

data MediaObject = MediaObject
  { bytes :: !StrictByteString.ByteString
  , mimeType :: !Text
  , sourceName :: !(Maybe Text)
  }
  deriving (Show, Eq)

data Media :: Effect where
  StoreMediaObject :: MediaObject -> Media m (Maybe Text)
  NormalizeMediaRef :: Text -> Media m Text

type instance DispatchOf Media = Dynamic

storeMediaObject :: Media :> es => MediaObject -> Eff es (Maybe Text)
storeMediaObject =
  send . StoreMediaObject

normalizeMediaRef :: Media :> es => Text -> Eff es Text
normalizeMediaRef =
  send . NormalizeMediaRef

normalizeMediaRefs :: Media :> es => [Text] -> Eff es [Text]
normalizeMediaRefs =
  traverse normalizeMediaRef

normalizeIncomingMessage :: Media :> es => IncomingMessage -> Eff es IncomingMessage
normalizeIncomingMessage message = do
  imageUrls <- normalizeMediaRefs message.imageUrls
  pure IncomingMessage
    { platform = message.platform
    , kind = message.kind
    , chatId = message.chatId
    , chatAliases = message.chatAliases
    , digest = message.digest
    , senderId = message.senderId
    , senderUsername = message.senderUsername
    , messageId = message.messageId
    , replyToMessageId = message.replyToMessageId
    , mentions = message.mentions
    , mentionUsernames = message.mentionUsernames
    , imageUrls
    , text = message.text
    , raw = message.raw
    }

normalizeIncomingMessages
  :: Media :> es
  => Stream (Of IncomingMessage) (Eff es) ()
  -> Stream (Of IncomingMessage) (Eff es) ()
normalizeIncomingMessages =
  S.mapM normalizeIncomingMessage

normalizeReferencedMessage :: Media :> es => ReferencedMessage -> Eff es ReferencedMessage
normalizeReferencedMessage message = do
  imageUrls <- normalizeMediaRefs message.imageUrls
  pure ReferencedMessage
    { messageId = message.messageId
    , senderDisplayName = message.senderDisplayName
    , senderIdentifier = message.senderIdentifier
    , text = message.text
    , imageUrls
    }

normalizeReplyBody :: Media :> es => Text -> Eff es Text
normalizeReplyBody =
  ReplyBody.traverseReplyImageUrls normalizeMediaRef

runMediaPassthrough :: Eff (Media : es) a -> Eff es a
runMediaPassthrough =
  interpret \_ -> \case
    StoreMediaObject mediaObject ->
      pure (Just ("data:" <> mediaObject.mimeType <> ";base64,"))
    NormalizeMediaRef ref ->
      pure ref
