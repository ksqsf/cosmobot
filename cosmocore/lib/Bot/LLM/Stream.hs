module Bot.LLM.Stream
  ( streamHttpResponseBody
  , streamSsePayloads
  ) where

import Bot.Prelude
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Util.StreamLifetime as StreamUtil
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Effectful.Resource as Resource
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types.Status as HTTPStatus
import qualified Streaming.Prelude as S

streamHttpResponseBody :: (HTTP.HTTP :> es, IOE :> es, Resource.Resource :> es) => Client.Request -> Stream (Of StrictByteString.ByteString) (Eff es) ()
streamHttpResponseBody httpRequest = do
  StreamUtil.bracketStream
    (HTTP.openResponse httpRequest)
    (liftIO . Client.responseClose)
    \response -> do
      ensureSuccessfulStreamingResponse httpRequest response
      streamBody (Client.responseBody response)
  where
    streamBody bodyReader = do
      chunk <- liftIO (Client.brRead bodyReader)
      unless (StrictByteString.null chunk) do
        S.yield chunk
        streamBody bodyReader

streamSsePayloads
  :: Monad m
  => Stream (Of StrictByteString.ByteString) m r
  -> Stream (Of StrictByteString.ByteString) m r
streamSsePayloads =
  events [] . linesFromChunks [] False True
  where
    -- Keep fragments separately so one long line split into small HTTP chunks
    -- is copied only once. A CR ends a line immediately; swallow its optional LF.
    linesFromChunks fragments skipLF firstLine input =
      lift (S.next input) >>= \case
        Left result -> pure result -- An unterminated event is not dispatched.
        Right (chunk, rest)
          | StrictByteString.null chunk -> linesFromChunks fragments skipLF firstLine rest
          | otherwise -> consume fragments firstLine
              (if skipLF then fromMaybe chunk (ByteString.stripPrefix "\n" chunk) else chunk) rest

    consume fragments firstLine bytes rest =
      let (prefix, suffix) = ByteString.break (\c -> c == '\r' || c == '\n') bytes
      in case ByteString.uncons suffix of
        Nothing -> linesFromChunks (prefix : fragments) False firstLine rest
        Just (ending, remaining) -> do
          let line = StrictByteString.concat (reverse (prefix : fragments))
              withoutBom = if firstLine then fromMaybe line (StrictByteString.stripPrefix "\xef\xbb\xbf" line) else line
          S.yield withoutBom
          if StrictByteString.null remaining
            then linesFromChunks [] (ending == '\r') False rest
            else consume [] False
              (if ending == '\r' then fromMaybe remaining (ByteString.stripPrefix "\n" remaining) else remaining) rest

    events fields input =
      lift (S.next input) >>= \case
        Left result -> pure result
        Right (line, rest)
          | StrictByteString.null line -> do
              let payload = ByteString.intercalate "\n" (reverse fields)
              unless (null fields || payload == "[DONE]") (S.yield payload)
              events [] rest
          | line == "data" -> events ("" : fields) rest
          | Just value <- ByteString.stripPrefix "data:" line ->
              events (fromMaybe value (ByteString.stripPrefix " " value) : fields) rest
          | otherwise -> events fields rest

ensureSuccessfulStreamingResponse :: IOE :> es => Client.Request -> Client.Response Client.BodyReader -> Stream (Of a) (Eff es) ()
ensureSuccessfulStreamingResponse request response = do
  let status = Client.responseStatus response
      code = HTTPStatus.statusCode status
  unless (200 <= code && code < 300) do
    chunks <- liftIO (Client.brConsume (Client.responseBody response))
    let preview = LazyByteString.toStrict (LazyByteString.fromChunks chunks)
    lift (throwIO (Client.HttpExceptionRequest request (Client.StatusCodeException (void response) preview)))
