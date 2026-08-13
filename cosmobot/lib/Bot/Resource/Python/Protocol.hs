module Bot.Resource.Python.Protocol
  ( FrameError (..)
  , ProtocolState (..)
  , WorkerMessage (..)
  , maxRpcBytes
  , maxControlBytes
  , encodeFrame
  , decodeFrame
  , pythonRunRequest
  , toolsRunResponse
  , parseWorkerMessage
  )
where

import Bot.Agent.Program.Python (PythonToolCall (..))
import Bot.Agent.Failure (Failure (..))
import Bot.Agent.Types (ToolResult, failureStatus, toolResultContent, toolResultFailure)
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

data FrameError
  = FrameTooLarge !Int
  | FrameMissingNewline
  | FrameContainsNewline
  | FrameInvalidJSON !Text
  deriving stock (Eq, Show)

data ProtocolState
  = Created
  | RunSent
  | Waiting !Int
  | Completed !Text
  | Failed !Text
  deriving stock (Eq, Show)

data WorkerMessage
  = RunTools !Int !(NonEmpty PythonToolCall)
  | RunCompleted !Text
  | RunFailed !Text
  deriving stock (Eq, Show)

maxRpcBytes :: Int
maxRpcBytes = 4 * 1024 * 1024

maxControlBytes :: Int
maxControlBytes = 8 * 1024

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

pythonRunRequest :: Text -> Aeson.Value
pythonRunRequest code =
  Aeson.object
    [ "jsonrpc" Aeson..= ("2.0" :: Text)
    , "id" Aeson..= ("host:run" :: Text)
    , "method" Aeson..= ("python.run" :: Text)
    , "params" Aeson..= Aeson.object ["code" Aeson..= code]
    ]

toolsRunResponse :: Int -> NonEmpty ToolResult -> Aeson.Value
toolsRunResponse rpcId results =
  Aeson.object
    [ "jsonrpc" Aeson..= ("2.0" :: Text)
    , "id" Aeson..= rpcId
    , "result" Aeson..= fmap resultEnvelope results
    ]
  where
    resultEnvelope result =
      case toolResultFailure result of
        Nothing -> Aeson.object
          [ "ok" Aeson..= True
          , "content" Aeson..= toolResultContent result
          ]
        Just failure@Failure{userMessage, detail} -> Aeson.object
          [ "ok" Aeson..= False
          , "failure" Aeson..= Aeson.object
              [ "category" Aeson..= failureStatus failure
              , "message" Aeson..= userMessage
              , "detail" Aeson..= detail
              ]
          ]

parseWorkerMessage :: Aeson.Value -> Either Text WorkerMessage
parseWorkerMessage = first Text.pack . AesonTypes.parseEither parser
  where
    parser = Aeson.withObject "Python worker JSON-RPC message" \object -> do
      version <- object Aeson..: Key.fromText "jsonrpc"
      unless (version == ("2.0" :: Text)) (fail "jsonrpc must be 2.0")
      method <- object Aeson..:? Key.fromText "method"
      case method of
        Just ("tools.run" :: Text) -> parseRunTools object
        Just unknown -> fail ("unknown worker method: " <> Text.unpack unknown)
        Nothing -> parseRunResult object

    parseRunTools object = do
      rpcId <- object Aeson..: Key.fromText "id"
      when (rpcId <= (0 :: Int)) (fail "tools.run id must be positive")
      params <- object Aeson..: Key.fromText "params"
      calls <- Aeson.withObject "tools.run params" (Aeson..: Key.fromText "calls") params
      traverse parseCall calls >>= \case
        [] -> fail "tools.run calls must be non-empty"
        firstCall : rest -> pure (RunTools rpcId (firstCall :| rest))

    parseCall = Aeson.withObject "tools.run call" \object -> do
      name <- object Aeson..: Key.fromText "name"
      when (Text.null name) (fail "tool name must be non-empty")
      arguments <- object Aeson..: Key.fromText "args" :: AesonTypes.Parser Aeson.Value
      pure PythonToolCall
        { name
        , arguments = TextEncoding.decodeUtf8 (LazyByteString.toStrict (Aeson.encode arguments))
        }

    parseRunResult object = do
      responseId <- object Aeson..: Key.fromText "id"
      unless (responseId == ("host:run" :: Text)) (fail "unexpected response id")
      result <- object Aeson..: Key.fromText "result"
      Aeson.withObject "python.run result" parseResult result

    parseResult object = do
      kind <- object Aeson..: Key.fromText "kind" :: AesonTypes.Parser Text
      case kind of
        "completed" -> do
          content <- object Aeson..: Key.fromText "content"
          validateBound "completion content" content
          pure (RunCompleted content)
        "failed" -> do
          message <- object Aeson..: Key.fromText "message"
          when (Text.null message) (fail "failure message must be non-empty")
          validateBound "failure message" message
          pure (RunFailed message)
        _ -> fail "unknown python.run result kind"

    validateBound label value =
      when (ByteString.length (TextEncoding.encodeUtf8 value) > maxControlBytes) (fail (label <> " exceeds limit"))
