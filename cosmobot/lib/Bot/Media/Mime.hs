{-|
Module      : Bot.Media.Mime
Description : MIME type lookup and lightweight content sniffing
Stability   : experimental
-}

module Bot.Media.Mime
  ( mimeFromName
  , extensionFromMime
  , sniffMime
  , sniffTextMime
  , isProbablyMediaMime
  , isGenericMime
  )
where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import Bot.Util.Mime (sniffMime)
import qualified Data.ByteString as StrictByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Network.Mime as Mime

mimeFromName :: Text -> Text
mimeFromName name =
  TextEncoding.decodeUtf8 (Mime.defaultMimeLookup name)

extensionFromMime :: Text -> Text
extensionFromMime mime
  | cleanMime mime == "image/jpeg" =
      ".jpg"
  | otherwise =
      maybe ".bin" ("." <>) (viaNonEmpty head =<< Map.lookup (TextEncoding.encodeUtf8 (cleanMime mime)) Mime.defaultExtensionMap)

sniffTextMime :: StrictByteString.ByteString -> Text -> Text
sniffTextMime bytes text =
  case Aeson.eitherDecodeStrict' bytes :: Either String Aeson.Value of
    Right _ ->
      "application/json"
    Left _ ->
      sniffStructuredTextMime text

sniffStructuredTextMime :: Text -> Text
sniffStructuredTextMime text
  | "<!doctype html" `Text.isPrefixOf` lowerStripped || "<html" `Text.isPrefixOf` lowerStripped =
      "text/html; charset=utf-8"
  | "<?xml" `Text.isPrefixOf` lowerStripped =
      "application/xml"
  | otherwise =
      "text/plain; charset=utf-8"
  where
    lowerStripped = Text.toLower (Text.stripStart text)

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
