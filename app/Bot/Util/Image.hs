{-|
Module      : Bot.Util.Image
Description : Shared image helpers
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Util.Image
  ( ImageCompressionConfig (..)
  , removeFilesIfExists
  , decodeDataImageReference
  )
where

import Bot.Prelude
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful.FileSystem (FileSystem, removeFile)

data ImageCompressionConfig = ImageCompressionConfig
  { compressionFormat :: !(Maybe Text)
  , compressionLevel :: !(Maybe Int)
  }
  deriving (Show, Eq)

removeFilesIfExists :: (IOE :> es, Fail :> es, FileSystem :> es) => [FilePath] -> Eff es ()
removeFilesIfExists =
  traverse_ removeIfExists

decodeDataImageReference :: Text -> Maybe StrictByteString.ByteString
decodeDataImageReference imageRef = do
  let (_, encodedWithMarker) = Text.breakOn ";base64," imageRef
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encoded))

removeIfExists :: (Fail :> es, FileSystem :> es) => FilePath -> Eff es ()
removeIfExists path =
  removeFile path `catchSync` \_ -> pure ()
