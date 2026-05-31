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
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Control.Monad.Trans.Resource (ResourceT)
import Effectful.FileSystem (FileSystem)
import qualified Data.ByteString.Streaming.HTTP as StreamingHTTP
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTPHeader
import qualified Network.HTTP.Types.Status as HTTPStatus
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as S
import System.FilePath (takeFileName)
import System.IO.Error (ioError, userError)

decodeDataMediaObject :: Text -> Maybe MediaObject
decodeDataMediaObject ref = do
  bytes <- dataImageByteStream ref
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

dataImageByteStream :: Text -> Maybe (Q.ByteStream (ResourceT IO) ())
dataImageByteStream ref = do
  let (_, encodedWithMarker) = Text.breakOn ";base64," ref
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  pure (base64DecodedTextByteStream encoded)

base64DecodedTextByteStream :: Text -> Q.ByteStream (ResourceT IO) ()
base64DecodedTextByteStream encoded =
  Q.fromChunks (go StrictByteString.empty encoded)
  where
    go pending text
      | Text.null text =
          unless (StrictByteString.null pending) (decodeAndYield pending)
      | otherwise = do
          let (piece, rest) = Text.splitAt 32768 text
              clean = TextEncoding.encodeUtf8 (Text.filter (not . isBase64TextWhitespace) piece)
              joined = pending <> clean
              decodeLength = (StrictByteString.length joined `div` 4) * 4
              (ready, nextPending) = StrictByteString.splitAt decodeLength joined
          decodeAndYield ready
          go nextPending rest

decodeAndYield :: StrictByteString.ByteString -> Stream (Of StrictByteString.ByteString) (ResourceT IO) ()
decodeAndYield bytes
  | StrictByteString.null bytes =
      pure ()
  | otherwise =
      case Base64.decode bytes of
        Left err ->
          liftIO (ioError (userError [i|Invalid data:image base64 data: #{Text.pack err}|]))
        Right decoded ->
          unless (StrictByteString.null decoded) (S.yield decoded)

isBase64TextWhitespace :: Char -> Bool
isBase64TextWhitespace char =
  char == ' ' || char == '\n' || char == '\r' || char == '\t'

downloadObject :: IOE :> es => HTTP.Manager -> Text -> Eff es MediaObject
downloadObject manager ref = do
  request <- mediaDownloadRequest <$> liftIO (HTTP.parseRequest (Text.unpack ref))
  let sourceName = TextEncoding.decodeUtf8 (StrictByteString.takeWhile (/= 63) (HTTP.path request))
      nameMime = Mime.mimeFromName sourceName
  mime <- probeRemoteMime manager request nameMime
  pure MediaObject
    { bytes = downloadByteStream manager ref request mime
    , mimeType = mime
    , sourceName = Just sourceName
    }

probeRemoteMime :: IOE :> es => HTTP.Manager -> HTTP.Request -> Text -> Eff es Text
probeRemoteMime manager request nameMime = do
  result <- trySync (probeRemoteMimeWithRangeGet manager request nameMime)
  pure case result of
    Right mime -> mime
    Left _ -> fallbackMime "application/octet-stream" nameMime

probeRemoteMimeWithRangeGet :: IOE :> es => HTTP.Manager -> HTTP.Request -> Text -> Eff es Text
probeRemoteMimeWithRangeGet manager request nameMime =
  bracket
    (liftIO $ HTTP.responseOpen (mediaProbeRequest request) manager)
    (liftIO . HTTP.responseClose)
    \response -> do
      let status = HTTP.responseStatus response
      unless (HTTPStatus.statusIsSuccessful status) $
        liftIO (ioError (userError [i|Remote media probe failed with HTTP #{HTTPStatus.statusCode status}|]))
      chunk <- liftIO (HTTP.brRead (HTTP.responseBody response))
      pure (resolvedRemoteMime (responseMime response) nameMime chunk)

downloadByteStream :: HTTP.Manager -> Text -> HTTP.Request -> Text -> Q.ByteStream (ResourceT IO) ()
downloadByteStream manager ref request expectedMime = do
  response <- lift (StreamingHTTP.http request manager)
  let status = HTTP.responseStatus response
      headerMime = responseMime response
  unless (HTTPStatus.statusIsSuccessful status) $
    liftIO (ioError (userError [i|Remote media download failed: #{ref} returned HTTP #{HTTPStatus.statusCode status}|]))
  unless (Mime.isProbablyMediaMime headerMime || Mime.isProbablyMediaMime expectedMime) $
    liftIO (ioError (userError [i|Remote media download returned non-media content-type #{headerMime}: #{ref}|]))
  HTTP.responseBody response

mediaDownloadRequest :: HTTP.Request -> HTTP.Request
mediaDownloadRequest request =
  request
    { HTTP.requestHeaders =
        (HTTPHeader.hUserAgent, mediaDownloadUserAgent) : filter ((/= HTTPHeader.hUserAgent) . fst) request.requestHeaders
    }

mediaProbeRequest :: HTTP.Request -> HTTP.Request
mediaProbeRequest request =
  request
    { HTTP.requestHeaders =
        ("Range", "bytes=0-0") : filter ((/= "Range") . fst) request.requestHeaders
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

resolvedRemoteMime :: Text -> Text -> StrictByteString.ByteString -> Text
resolvedRemoteMime headerMime nameMime chunk =
  case Mime.sniffMime chunk of
    Just sniffedMime
      | not (Mime.isProbablyMediaMime headerMime) || Mime.isGenericMime headerMime ->
          sniffedMime
    _ ->
      fallbackMime headerMime nameMime

fileObject :: (IOE :> es, FileSystem :> es) => Text -> Eff es MediaObject
fileObject ref = do
  path <- case Text.stripPrefix "file://" ref of
    Just path -> pure (Text.unpack path)
    Nothing -> liftIO (ioError (userError [i|Invalid file media reference: #{ref}|]))
  pure MediaObject
    { bytes = Q.readFile path
    , mimeType = Mime.mimeFromName (Text.pack path)
    , sourceName = Just (Text.pack (takeFileName path))
    }

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped
