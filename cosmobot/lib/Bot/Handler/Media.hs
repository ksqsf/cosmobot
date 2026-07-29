{-|
Module      : Bot.Handler.Media
Description : Commands for inspecting cached media
Stability   : experimental
-}
module Bot.Handler.Media
  ( mediaHandlers
  )
where

import Bot.Core.Message
import Bot.Core.Route
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Media as Media
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

mediaHandlers :: (Chat.Chat :> es, Media.Media :> es) => [RouteHandler es]
mediaHandlers =
  [ withHelp (RouteHelp "!media/info <media_id>..." "Show cached media information.") $
      stopOn (command "!media/info") handleInfo
  , withHelp (RouteHelp "!media/get <media_id>..." "Get public URLs for cached media.") $
      stopOn (command "!media/get") handleGet
  ]

handleInfo :: (Chat.Chat :> es, Media.Media :> es) => IncomingMessage -> Text -> Eff es ()
handleInfo message input =
  resolveInfoMediaIds message input >>= either (reply message) (replyWithMediaInfo message)

resolveInfoMediaIds
  :: Chat.Chat :> es
  => IncomingMessage
  -> Text
  -> Eff es (Either Text [Text])
resolveInfoMediaIds message input
  | mediaIds@(_ : _) <- ordNub (Text.words input) =
      pure (Right mediaIds)
  | Just messageId <- message.replyToMessageId =
      maybe (Left "Could not read the replied message.") referencedMediaIdsResult
        <$> Chat.getMessageContent message messageId
  | otherwise =
      pure (Left "Usage: !media/info <media_id>..., or reply to a message containing media.")

referencedMediaIdsResult :: ReferencedMessage -> Either Text [Text]
referencedMediaIdsResult referenced =
  maybe (Left "No media ids found in the replied message.") (Right . toList)
    (viaNonEmpty id (referencedMediaIds referenced))

handleGet :: (Chat.Chat :> es, Media.Media :> es) => IncomingMessage -> Text -> Eff es ()
handleGet message input =
  case ordNub (Text.words input) of
    [] ->
      reply message "Usage: !media/get <media_id>..."
    mediaIds ->
      traverse mediaPublicUrl mediaIds >>= reply message . jsonText

replyWithMediaInfo :: (Chat.Chat :> es, Media.Media :> es) => IncomingMessage -> [Text] -> Eff es ()
replyWithMediaInfo message mediaIds =
  traverse mediaInfo mediaIds >>= reply message . jsonText

referencedMediaIds :: ReferencedMessage -> [Text]
referencedMediaIds referenced =
  ordNub $
    mapMaybe mediaIdFromRef
      (referenced.imageUrls <> map (.ref) referenced.files)
      <> mediaIdsInText referenced.text

mediaIdFromRef :: Text -> Maybe Text
mediaIdFromRef ref = do
  fileId <- Text.stripPrefix "media:" (Text.strip ref)
  guard (validMediaFileId fileId)
  pure ("media:" <> fileId)

mediaIdsInText :: Text -> [Text]
mediaIdsInText =
  mapMaybe (mediaIdFromRef . ("media:" <>))
    . drop 1
    . map (Text.takeWhile mediaIdChar)
    . Text.splitOn "media:"

validMediaFileId :: Text -> Bool
validMediaFileId fileId =
  "mf_" `Text.isPrefixOf` fileId && Text.all mediaIdChar fileId

mediaIdChar :: Char -> Bool
mediaIdChar char =
  (Char.isAscii char && Char.isAlphaNum char) || char == '-' || char == '_'

mediaInfo :: Media.Media :> es => Text -> Eff es Aeson.Value
mediaInfo rawMediaId = do
  let (mediaId, fileId) = normalizeMediaId rawMediaId
  Media.mediaCacheEntry fileId <&> \case
    Nothing -> notFound mediaId
    Just info -> Aeson.object
      [ "media_id" Aeson..= mediaId
      , "info" Aeson..= info
      ]

mediaPublicUrl :: Media.Media :> es => Text -> Eff es Aeson.Value
mediaPublicUrl rawMediaId = do
  let (mediaId, fileId) = normalizeMediaId rawMediaId
  Media.mediaCacheEntry fileId >>= \case
    Nothing ->
      pure (notFound mediaId)
    Just info -> do
      publicUrl <- Media.publicMediaRef info.file.ref
      pure $ Aeson.object
        [ "media_id" Aeson..= mediaId
        , "public_url" Aeson..= publicUrl
        ]

normalizeMediaId :: Text -> (Text, Text)
normalizeMediaId rawMediaId =
  let stripped = Text.strip rawMediaId
      fileId = fromMaybe stripped (Text.stripPrefix "media:" stripped)
  in ("media:" <> fileId, fileId)

notFound :: Text -> Aeson.Value
notFound mediaId =
  Aeson.object
    [ "media_id" Aeson..= mediaId
    , "error" Aeson..= Aeson.String "not found"
    ]

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . AesonPretty.encodePretty

reply :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
reply message =
  void . Chat.replyTo message
