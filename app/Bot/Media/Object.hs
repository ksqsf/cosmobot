{-|
Module      : Bot.Media.Object
Description : Media object construction from local and remote references
Stability   : experimental
-}

module Bot.Media.Object
  ( decodeDataMediaObject
  , downloadObject
  , fileObject
  , sniffImageMime
  )
where

import Bot.Effect.Media (MediaObject (..))
import Bot.Prelude
import qualified Bot.Media.Cache as Cache
import qualified Bot.Util.Image as Image
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTPHeader
import qualified Network.HTTP.Types.Status as HTTPStatus
import System.FilePath (takeFileName)
import System.IO.Error (ioError, userError)

decodeDataMediaObject :: Text -> Maybe MediaObject
decodeDataMediaObject ref = do
  bytes <- Image.decodeDataImageReference ref
  let mime = fromMaybe "image/png" (dataImageMime ref)
  pure MediaObject
    { bytes
    , mimeType = mime
    , sourceName = Nothing
    }

dataImageMime :: Text -> Maybe Text
dataImageMime ref =
  Text.stripPrefix "data:" (Text.strip ref)
    <&> Text.takeWhile (/= ';')
    >>= nonEmptyText

downloadObject :: IOE :> es => HTTP.Manager -> Text -> Eff es MediaObject
downloadObject manager ref = do
  request <- liftIO (HTTP.parseRequest (Text.unpack ref))
  response <- liftIO (HTTP.httpLbs request manager)
  let bytes = LazyByteString.toStrict (HTTP.responseBody response)
      mime = fromMaybe (responseMime response) (sniffImageMime bytes)
      status = HTTP.responseStatus response
  unless (HTTPStatus.statusIsSuccessful status) $
    liftIO (ioError (userError [i|Remote media download failed: #{ref} returned HTTP #{HTTPStatus.statusCode status}|]))
  unless ("image/" `Text.isPrefixOf` Text.toLower mime) $
    liftIO (ioError (userError [i|Remote media download returned non-image content-type #{mime}: #{ref}|]))
  pure MediaObject
    { bytes
    , mimeType = mime
    , sourceName = Just (TextEncoding.decodeUtf8 (StrictByteString.takeWhile (/= 63) (HTTP.path request)))
    }

responseMime :: HTTP.Response body -> Text
responseMime response =
  fromMaybe "application/octet-stream" do
    raw <- List.lookup HTTPHeader.hContentType (HTTP.responseHeaders response)
    nonEmptyText (Text.takeWhile (/= ';') (TextEncoding.decodeUtf8 raw))

sniffImageMime :: StrictByteString.ByteString -> Maybe Text
sniffImageMime bytes
  | "\x89PNG\r\n\x1a\n" `StrictByteString.isPrefixOf` bytes = Just "image/png"
  | "\xff\xd8\xff" `StrictByteString.isPrefixOf` bytes = Just "image/jpeg"
  | "GIF87a" `StrictByteString.isPrefixOf` bytes || "GIF89a" `StrictByteString.isPrefixOf` bytes = Just "image/gif"
  | "RIFF" `StrictByteString.isPrefixOf` bytes && "WEBP" `StrictByteString.isPrefixOf` StrictByteString.drop 8 bytes = Just "image/webp"
  | otherwise = Nothing

fileObject :: (IOE :> es, FileSystem :> es) => Text -> Eff es MediaObject
fileObject ref = do
  path <- case Text.stripPrefix "file://" ref of
    Just path -> pure (Text.unpack path)
    Nothing -> liftIO (ioError (userError [i|Invalid file media reference: #{ref}|]))
  bytes <- FileSystemByteString.readFile path
  pure MediaObject
    { bytes
    , mimeType = Cache.mimeFromName (Text.pack path)
    , sourceName = Just (Text.pack (takeFileName path))
    }

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped
