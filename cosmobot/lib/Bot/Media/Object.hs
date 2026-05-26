{-|
Module      : Bot.Media.Object
Description : Media object construction from local and remote references
Stability   : experimental
-}

module Bot.Media.Object
  ( decodeDataMediaObject
  , downloadObject
  , fileObject
  )
where

import Bot.Effect.Media (MediaObject (..))
import Bot.Prelude
import qualified Bot.Media.Mime as Mime
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
  request <- mediaDownloadRequest <$> liftIO (HTTP.parseRequest (Text.unpack ref))
  response <- liftIO (HTTP.httpLbs request manager)
  let sourceName = TextEncoding.decodeUtf8 (StrictByteString.takeWhile (/= 63) (HTTP.path request))
      bytes = LazyByteString.toStrict (HTTP.responseBody response)
      headerMime = responseMime response
      nameMime = Mime.mimeFromName sourceName
      mime = fromMaybe (fallbackMime headerMime nameMime) (Mime.sniffMime bytes)
      status = HTTP.responseStatus response
  unless (HTTPStatus.statusIsSuccessful status) $
    liftIO (ioError (userError [i|Remote media download failed: #{ref} returned HTTP #{HTTPStatus.statusCode status}|]))
  unless (Mime.isProbablyMediaMime mime) $
    liftIO (ioError (userError [i|Remote media download returned non-media content-type #{mime}: #{ref}|]))
  pure MediaObject
    { bytes
    , mimeType = mime
    , sourceName = Just sourceName
    }

mediaDownloadRequest :: HTTP.Request -> HTTP.Request
mediaDownloadRequest request =
  request
    { HTTP.requestHeaders =
        (HTTPHeader.hUserAgent, mediaDownloadUserAgent) : filter ((/= HTTPHeader.hUserAgent) . fst) request.requestHeaders
    }

mediaDownloadUserAgent :: StrictByteString.ByteString
mediaDownloadUserAgent =
  "cosmobot/0.1 (+https://github.com/ksqsf/cosmobot)"

responseMime :: HTTP.Response body -> Text
responseMime response =
  fromMaybe "application/octet-stream" do
    raw <- List.lookup HTTPHeader.hContentType (HTTP.responseHeaders response)
    nonEmptyText (Text.takeWhile (/= ';') (TextEncoding.decodeUtf8 raw))

fallbackMime :: Text -> Text -> Text
fallbackMime headerMime nameMime
  | Mime.isGenericMime headerMime = nameMime
  | otherwise = headerMime

fileObject :: (IOE :> es, FileSystem :> es) => Text -> Eff es MediaObject
fileObject ref = do
  path <- case Text.stripPrefix "file://" ref of
    Just path -> pure (Text.unpack path)
    Nothing -> liftIO (ioError (userError [i|Invalid file media reference: #{ref}|]))
  bytes <- FileSystemByteString.readFile path
  pure MediaObject
    { bytes
    , mimeType = Mime.mimeFromName (Text.pack path)
    , sourceName = Just (Text.pack (takeFileName path))
    }

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped
