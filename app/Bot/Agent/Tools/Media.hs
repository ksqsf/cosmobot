{-|
Module      : Bot.Agent.Tools.Media
Description : Agent tools for cached media objects
Stability   : experimental
-}

module Bot.Agent.Tools.Media
  ( readMediaTextTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import qualified Bot.Effect.Media as Media
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as StrictByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import Effectful.FileSystem (FileSystem)

defaultReadSize :: Int
defaultReadSize =
  4096

maxReadSize :: Int
maxReadSize =
  16384

readMediaTextTool :: (Media.Media :> es, FileSystem :> es) => Tool es
readMediaTextTool = Tool
  { name = "read_media_text"
  , description = "Read a UTF-8 text slice from a cached media object. Use this with media ids returned in omitted tool results, such as mf_xxx or media:mf_xxx. offset and size are character counts."
  , parameters = objectSchema
      [ fieldText "media_id" "Media id to read, either mf_xxx or media:mf_xxx."
      , fieldInteger "offset" "Optional zero-based character offset. Defaults to 0."
      , fieldInteger "size" "Optional maximum number of characters to return. Defaults to 4096 and is capped at 16384."
      ]
      ["media_id"]
  , noisy = False
  , allowed = everyone
  , start = \_ -> pure \args ->
      withParsedToolArgs readMediaTextArgs args readMediaText
  }

data ReadMediaTextArgs = ReadMediaTextArgs
  { mediaId :: !Text
  , offset :: !Int
  , size :: !Int
  }

readMediaTextArgs :: Aeson.Value -> AesonTypes.Parser ReadMediaTextArgs
readMediaTextArgs =
  Aeson.withObject "read media text arguments" \o -> do
    mediaId <- Text.strip <$> o Aeson..: Key.fromText "media_id"
    offset <- nonNegativeInt "offset" . fromMaybe 0 =<< o Aeson..:? Key.fromText "offset"
    requestedSize <- nonNegativeInt "size" . fromMaybe (fromIntegral defaultReadSize) =<< o Aeson..:? Key.fromText "size"
    when (Text.null mediaId) do
      fail "media_id must not be empty."
    pure ReadMediaTextArgs
      { mediaId
      , offset
      , size = min maxReadSize requestedSize
      }

nonNegativeInt :: Text -> Integer -> AesonTypes.Parser Int
nonNegativeInt name value
  | value < 0 =
      fail [i|#{name} must be >= 0.|]
  | value > fromIntegral (maxBound :: Int) =
      fail [i|#{name} is too large.|]
  | otherwise =
      pure (fromInteger value)

readMediaText :: (Media.Media :> es, FileSystem :> es) => ReadMediaTextArgs -> Eff es ToolResult
readMediaText ReadMediaTextArgs{mediaId, offset, size} = do
  info <- Media.mediaFileInfoByRef ref
  path <- Media.localMediaPath ref
  case (info, path) of
    (_, Nothing) ->
      pure (toolText [i|Media object not found: #{mediaId}|])
    (Just mediaInfo, Just filePath) -> do
      bytes <- FileSystemByteString.readFile filePath
      let text = TextEncoding.decodeUtf8With TextEncoding.lenientDecode bytes
          chunk = Text.take size (Text.drop offset text)
      pure (toolText (jsonText (readMediaTextResult mediaInfo bytes text chunk offset size)))
    (Nothing, Just filePath) -> do
      bytes <- FileSystemByteString.readFile filePath
      let text = TextEncoding.decodeUtf8With TextEncoding.lenientDecode bytes
          chunk = Text.take size (Text.drop offset text)
      pure (toolText (jsonText (readMediaTextResultWithoutInfo mediaId bytes text chunk offset size)))
  where
    ref = mediaRef mediaId

mediaRef :: Text -> Text
mediaRef mediaId
  | "media:" `Text.isPrefixOf` mediaId = mediaId
  | otherwise = "media:" <> mediaId

readMediaTextResult :: Media.MediaFileInfo -> StrictByteString.ByteString -> Text -> Text -> Int -> Int -> Aeson.Value
readMediaTextResult info bytes text chunk offset size =
  Aeson.object
    [ "media_id" Aeson..= info.fileId
    , "mime" Aeson..= info.mimeType
    , "byte_size" Aeson..= StrictByteString.length bytes
    , "total_chars" Aeson..= Text.length text
    , "offset" Aeson..= offset
    , "requested_size" Aeson..= size
    , "returned_chars" Aeson..= Text.length chunk
    , "content" Aeson..= chunk
    ]

readMediaTextResultWithoutInfo :: Text -> StrictByteString.ByteString -> Text -> Text -> Int -> Int -> Aeson.Value
readMediaTextResultWithoutInfo mediaId bytes text chunk offset size =
  Aeson.object
    [ "media_id" Aeson..= mediaId
    , "byte_size" Aeson..= StrictByteString.length bytes
    , "total_chars" Aeson..= Text.length text
    , "offset" Aeson..= offset
    , "requested_size" Aeson..= size
    , "returned_chars" Aeson..= Text.length chunk
    , "content" Aeson..= chunk
    ]
