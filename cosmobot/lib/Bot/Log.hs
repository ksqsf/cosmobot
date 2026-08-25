module Bot.Log
  ( module Effectful.Katip
  , logDebug
  , logInfo
  , logNotice
  , logWarning
  , logError
  , logCritical
  , logExceptionAt
  , logJsonText
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful
import Effectful.Katip
import Language.Haskell.TH (ExpQ)
import Relude

logDebug :: ExpQ
logDebug = [| \message -> $(logTM) DebugS (logStr (message :: Text)) |]

logInfo :: ExpQ
logInfo = [| \message -> $(logTM) InfoS (logStr (message :: Text)) |]

logNotice :: ExpQ
logNotice = [| \message -> $(logTM) NoticeS (logStr (message :: Text)) |]

logWarning :: ExpQ
logWarning = [| \message -> $(logTM) WarningS (logStr (message :: Text)) |]

logError :: ExpQ
logError = [| \message -> $(logTM) ErrorS (logStr (message :: Text)) |]

logCritical :: ExpQ
logCritical = [| \message -> $(logTM) CriticalS (logStr (message :: Text)) |]

logExceptionAt :: KatipE :> es => Severity -> Eff es a -> Eff es a
logExceptionAt severity action =
  action `logExceptionM` severity

logJsonText :: Aeson.ToJSON a => a -> Text
logJsonText =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode . sanitizeLogValue . Aeson.toJSON

sanitizeLogValue :: Aeson.Value -> Aeson.Value
sanitizeLogValue = \case
  Aeson.Object obj ->
    Aeson.Object (sanitizeLogValue <$> obj)
  Aeson.Array values ->
    Aeson.Array (sanitizeLogValue <$> values)
  Aeson.String text ->
    Aeson.String (sanitizeBase64DataUrls text)
  value ->
    value

sanitizeBase64DataUrls :: Text -> Text
sanitizeBase64DataUrls text =
  case Text.breakOn base64Marker text of
    (_, "") ->
      text
    (before, markerAndRest) ->
      let afterMarker = Text.drop (Text.length base64Marker) markerAndRest
          (payload, rest) = Text.span isBase64UrlChar afterMarker
          shortened = shortenBase64Payload payload
      in before <> base64Marker <> shortened <> sanitizeBase64DataUrls rest

base64Marker :: Text
base64Marker =
  ";base64,"

base64LogPrefixChars :: Int
base64LogPrefixChars =
  96

shortenBase64Payload :: Text -> Text
shortenBase64Payload payload
  | Text.length payload > base64LogPrefixChars =
      Text.take base64LogPrefixChars payload <> "..."
  | otherwise =
      payload

isBase64UrlChar :: Char -> Bool
isBase64UrlChar char =
  Char.isAlphaNum char || char `elem` ("+/=-_" :: String)
