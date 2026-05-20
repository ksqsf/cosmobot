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
  , compressDataImageReference
  )
where

import Bot.Prelude
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful.FileSystem (getTemporaryDirectory, removeFile)
import Effectful.FileSystem.IO
import System.Exit (ExitCode(..))
import System.FilePath ((<.>))
import System.IO (openBinaryTempFile)
import Effectful.Process (Process, readProcessWithExitCode)

data ImageCompressionConfig = ImageCompressionConfig
  { outputFormat :: !(Maybe Text)
  , outputCompression :: !(Maybe Int)
  }
  deriving (Show)

removeFilesIfExists :: (IOE :> es, Fail :> es, FileSystem :> es) => [FilePath] -> Eff es ()
removeFilesIfExists =
  traverse_ removeIfExists

compressDataImageReference :: (IOE :> es, Log :> es, FileSystem :> es, Process :> es, Fail :> es) => ImageCompressionConfig -> Text -> Eff es (Maybe Text)
compressDataImageReference cfg imageRef =
  case targetImageFormat cfg of
    Nothing ->
      pure Nothing
    Just format ->
      compressDataImage format cfg.outputCompression imageRef

targetImageFormat :: ImageCompressionConfig -> Maybe Text
targetImageFormat cfg =
  case Text.toLower . Text.strip <$> cfg.outputFormat of
    Just "jpeg" -> Just "jpg"
    Just "jpg"  -> Just "jpg"
    Just "webp" -> Just "webp"
    _           -> Nothing

compressDataImage :: (IOE :> es, Log :> es, FileSystem :> es, Process :> es, Fail :> es) => Text -> Maybe Int -> Text -> Eff es (Maybe Text)
compressDataImage format quality imageRef =
  case decodeDataImageReference imageRef of
    Nothing ->
      pure Nothing
    Just bytes -> do
      result <- (convertDataImage format quality bytes) `catchSync` \err -> do
        logAttention_ [i|Image compression failed: #{show err :: String}|]
        pure Nothing
      traverse_ (\url -> logInfo_ [i|Compressed image response URL: #{url}|]) result
      pure result

decodeDataImageReference :: Text -> Maybe StrictByteString.ByteString
decodeDataImageReference imageRef = do
  let (_, encodedWithMarker) = Text.breakOn ";base64," imageRef
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encoded))

convertDataImage :: (IOE :> es, FileSystem :> es, Process :> es, Fail :> es) => Text -> Maybe Int -> StrictByteString.ByteString -> Eff es (Maybe Text)
convertDataImage format quality bytes = do
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
      pure (Just ("file://" <> Text.pack outputPath))
    ExitFailure _ -> do
      removeIfExists outputPath
      pure Nothing

clampImageQuality :: Int -> Int
clampImageQuality =
  min 100 . max 1

removeIfExists :: (Fail :> es, FileSystem :> es) => FilePath -> Eff es ()
removeIfExists path =
  removeFile path `catchSync` \_ -> pure ()
