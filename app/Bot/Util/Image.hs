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
  , compressImageBytes
  )
where

import Bot.Prelude
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful.FileSystem (getTemporaryDirectory, removeFile)
import Effectful.FileSystem.IO
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import System.Exit (ExitCode(..))
import System.FilePath ((<.>))
import System.IO (openBinaryTempFile)
import Effectful.Process (Process, readProcessWithExitCode)

data ImageCompressionConfig = ImageCompressionConfig
  { compressionFormat :: !(Maybe Text)
  , compressionLevel :: !(Maybe Int)
  }
  deriving (Show, Eq)

removeFilesIfExists :: (IOE :> es, Fail :> es, FileSystem :> es) => [FilePath] -> Eff es ()
removeFilesIfExists =
  traverse_ removeIfExists

targetImageFormat :: ImageCompressionConfig -> Maybe Text
targetImageFormat cfg =
  case Text.toLower . Text.strip <$> cfg.compressionFormat of
    Just "jpeg" -> Just "jpg"
    Just "jpg"  -> Just "jpg"
    Just "webp" -> Just "webp"
    _           -> Nothing

compressImageBytes
  :: (IOE :> es, FileSystem :> es, Process :> es, Fail :> es)
  => ImageCompressionConfig
  -> StrictByteString.ByteString
  -> Eff es (Maybe (Text, StrictByteString.ByteString))
compressImageBytes cfg bytes =
  case targetImageFormat cfg of
    Nothing ->
      pure Nothing
    Just format ->
      convertImageBytes format cfg.compressionLevel bytes

decodeDataImageReference :: Text -> Maybe StrictByteString.ByteString
decodeDataImageReference imageRef = do
  let (_, encodedWithMarker) = Text.breakOn ";base64," imageRef
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encoded))

convertImageBytes :: (IOE :> es, FileSystem :> es, Process :> es, Fail :> es) => Text -> Maybe Int -> StrictByteString.ByteString -> Eff es (Maybe (Text, StrictByteString.ByteString))
convertImageBytes format quality bytes = do
  output <- convertImageBytesToFile format quality bytes
  forM output \outputPath -> do
    outputBytes <- FileSystemByteString.readFile outputPath
    removeIfExists outputPath
    pure (mimeForImageFormat format, outputBytes)

convertImageBytesToFile :: (IOE :> es, FileSystem :> es, Process :> es, Fail :> es) => Text -> Maybe Int -> StrictByteString.ByteString -> Eff es (Maybe FilePath)
convertImageBytesToFile format quality bytes = do
  dir <- getTemporaryDirectory
  (inputPath, inputHandle) <- liftIO $ openBinaryTempFile dir ("cosmobot-image-input" <.> "png")
  liftIO $ StrictByteString.hPut inputHandle bytes
  hClose inputHandle
  (outputPath, outputHandle) <- liftIO $ openBinaryTempFile dir ("cosmobot-image-output" <.> Text.unpack format)
  hClose outputHandle
  let args =
        [ inputPath
        , "-auto-orient"
        , "-strip"
        ]
          <> maybe [] (\value -> ["-quality", show (clampImageQuality value)]) quality
          <> [outputPath]
  (code, _out, _err) <- readProcessWithExitCode "magick" args ""
  removeIfExists inputPath
  case code of
    ExitSuccess ->
      pure (Just outputPath)
    ExitFailure _ -> do
      removeIfExists outputPath
      pure Nothing

mimeForImageFormat :: Text -> Text
mimeForImageFormat format =
  case Text.toLower (Text.strip format) of
    "jpg" -> "image/jpeg"
    "jpeg" -> "image/jpeg"
    "webp" -> "image/webp"
    "png" -> "image/png"
    other -> "image/" <> other

clampImageQuality :: Int -> Int
clampImageQuality =
  min 100 . max 1

removeIfExists :: (Fail :> es, FileSystem :> es) => FilePath -> Eff es ()
removeIfExists path =
  removeFile path `catchSync` \_ -> pure ()
