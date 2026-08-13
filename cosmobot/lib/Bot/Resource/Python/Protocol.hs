module Bot.Resource.Python.Protocol
  ( FrameError (..)
  , maxRpcBytes
  , encodeFrame
  , decodeFrame
  )
where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text

data FrameError
  = FrameTooLarge !Int
  | FrameMissingNewline
  | FrameContainsNewline
  | FrameInvalidJSON !Text
  deriving stock (Eq, Show)

maxRpcBytes :: Int
maxRpcBytes = 4 * 1024 * 1024

encodeFrame :: Aeson.ToJSON a => a -> Either FrameError ByteString
encodeFrame value
  | payloadLength > maxRpcBytes = Left (FrameTooLarge payloadLength)
  | otherwise = Right (ByteString.snoc payload 10)
  where
    bounded = LazyByteString.take (fromIntegral maxRpcBytes + 1) (Aeson.encode value)
    payloadLength = fromIntegral (LazyByteString.length bounded)
    payload = LazyByteString.toStrict bounded

decodeFrame :: Aeson.FromJSON a => ByteString -> Either FrameError a
decodeFrame frame = do
  payload <- case ByteString.unsnoc frame of
    Just (payload, 10) -> Right payload
    _ -> Left FrameMissingNewline
  when (ByteString.length payload > maxRpcBytes) $ Left (FrameTooLarge (ByteString.length payload))
  when (ByteString.elem 10 payload) $ Left FrameContainsNewline
  first (FrameInvalidJSON . Text.pack) (Aeson.eitherDecodeStrict' payload)
