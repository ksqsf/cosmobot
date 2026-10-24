module Bot.LLM.ImageInput (parseDataImage) where

import Bot.Prelude
import Data.Char (isAscii, isAlphaNum)
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- Validate the envelope without changing the provider's image bytes.
parseDataImage :: Text -> A.Parser (Text, Text)
parseDataImage url = do
  rest <- maybe invalid pure (T.stripPrefix "data:image/" url)
  let (subtype, marker) = T.breakOn ";base64," rest
  unless (not (T.null subtype) && T.all validSubtype subtype) invalid
  encoded <- maybe invalid pure (T.stripPrefix ";base64," marker)
  when (T.null encoded) invalid
  either (const invalid) (const (pure ())) (Base64.decode (TE.encodeUtf8 encoded))
  pure ("image/" <> subtype, encoded)
  where
    invalid = fail "Invalid image data URI: expected image MIME type and nonempty base64 data"
    validSubtype c = isAscii c && (isAlphaNum c || c `elem` ['-', '+', '.'])
