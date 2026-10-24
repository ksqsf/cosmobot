module Bot.Util.Mime (sniffMime) where

import Bot.Prelude
import Data.Bits ((.&.))
import qualified Data.ByteString as StrictByteString

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

mp3FrameSync :: StrictByteString.ByteString -> Bool
mp3FrameSync bytes =
  case StrictByteString.unpack (StrictByteString.take 2 bytes) of
    byte1 : byte2 : _ ->
      byte1 == 0xff && (byte2 .&. 0xe0) == 0xe0
    _ ->
      False
