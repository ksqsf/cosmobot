{-|
Module      : Bot.Media.Mime
Description : MIME type lookup and lightweight content sniffing
Stability   : experimental
-}

module Bot.Media.Mime
  ( mimeFromName
  , extensionFromMime
  , sniffMime
  , isProbablyMediaMime
  , isGenericMime
  )
where

import Bot.Prelude
import Data.Bits ((.&.))
import qualified Data.ByteString as StrictByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Network.Mime as Mime

mimeFromName :: Text -> Text
mimeFromName name =
  TextEncoding.decodeUtf8 (Mime.defaultMimeLookup name)

extensionFromMime :: Text -> Text
extensionFromMime mime =
  case cleanMime mime of
    "image/apng" -> ".apng"
    "image/avif" -> ".avif"
    "image/bmp" -> ".bmp"
    "image/gif" -> ".gif"
    "image/heic" -> ".heic"
    "image/heif" -> ".heif"
    "image/jpeg" -> ".jpg"
    "image/png" -> ".png"
    "image/svg+xml" -> ".svg"
    "image/tiff" -> ".tiff"
    "image/webp" -> ".webp"
    "audio/aac" -> ".aac"
    "audio/flac" -> ".flac"
    "audio/midi" -> ".mid"
    "audio/mp4" -> ".m4a"
    "audio/mpeg" -> ".mp3"
    "audio/ogg" -> ".ogg"
    "audio/opus" -> ".opus"
    "audio/wav" -> ".wav"
    "audio/webm" -> ".webm"
    "video/avi" -> ".avi"
    "video/mp4" -> ".mp4"
    "video/mpeg" -> ".mpeg"
    "video/ogg" -> ".ogv"
    "video/quicktime" -> ".mov"
    "video/webm" -> ".webm"
    "application/pdf" -> ".pdf"
    "application/gzip" -> ".gz"
    "application/zip" -> ".zip"
    "application/x-7z-compressed" -> ".7z"
    "application/x-rar-compressed" -> ".rar"
    "application/x-tar" -> ".tar"
    _ -> ".bin"

sniffMime :: StrictByteString.ByteString -> Maybe Text
sniffMime bytes
  | "\x89PNG\r\n\x1a\n" `StrictByteString.isPrefixOf` bytes = Just "image/png"
  | "\xff\xd8\xff" `StrictByteString.isPrefixOf` bytes = Just "image/jpeg"
  | "GIF87a" `StrictByteString.isPrefixOf` bytes || "GIF89a" `StrictByteString.isPrefixOf` bytes = Just "image/gif"
  | "RIFF" `StrictByteString.isPrefixOf` bytes && "WEBP" `StrictByteString.isPrefixOf` StrictByteString.drop 8 bytes = Just "image/webp"
  | "RIFF" `StrictByteString.isPrefixOf` bytes && "WAVE" `StrictByteString.isPrefixOf` StrictByteString.drop 8 bytes = Just "audio/wav"
  | "ID3" `StrictByteString.isPrefixOf` bytes || mp3FrameSync bytes = Just "audio/mpeg"
  | "fLaC" `StrictByteString.isPrefixOf` bytes = Just "audio/flac"
  | "OggS" `StrictByteString.isPrefixOf` bytes = Just "audio/ogg"
  | "\x1a\x45\xdf\xa3" `StrictByteString.isPrefixOf` bytes = Just "video/webm"
  | "ftyp" `StrictByteString.isPrefixOf` StrictByteString.drop 4 bytes = Just "video/mp4"
  | "%PDF-" `StrictByteString.isPrefixOf` bytes = Just "application/pdf"
  | "\x50\x4b\x03\x04" `StrictByteString.isPrefixOf` bytes = Just "application/zip"
  | "\x1f\x8b" `StrictByteString.isPrefixOf` bytes = Just "application/gzip"
  | "7z\xbc\xaf\x27\x1c" `StrictByteString.isPrefixOf` bytes = Just "application/x-7z-compressed"
  | "Rar!\x1a\x07" `StrictByteString.isPrefixOf` bytes = Just "application/x-rar-compressed"
  | otherwise = Nothing

isProbablyMediaMime :: Text -> Bool
isProbablyMediaMime mime =
  let mime' = cleanMime mime
  in any (`Text.isPrefixOf` mime')
      [ "image/"
      , "audio/"
      , "video/"
      ]
      || mime' `elem`
        [ "application/pdf"
        , "application/gzip"
        , "application/octet-stream"
        , "application/zip"
        , "application/x-7z-compressed"
        , "application/x-rar-compressed"
        , "application/x-tar"
        ]

isGenericMime :: Text -> Bool
isGenericMime mime =
  cleanMime mime `elem`
    [ "application/octet-stream"
    , "binary/octet-stream"
    ]

cleanMime :: Text -> Text
cleanMime =
  Text.toLower . Text.strip . Text.takeWhile (/= ';')

mp3FrameSync :: StrictByteString.ByteString -> Bool
mp3FrameSync bytes =
  case StrictByteString.unpack (StrictByteString.take 2 bytes) of
    byte1 : byte2 : _ ->
      byte1 == 0xff && (byte2 .&. 0xe0) == 0xe0
    _ ->
      False
