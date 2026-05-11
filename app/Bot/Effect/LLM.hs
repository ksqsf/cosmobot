{-|
Module      : Bot.Effect.LLM
Description : Query LLM
Stability   : experimental
-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bot.Effect.LLM
  ( -- * Effect
    LLM
  , ask
  , askWithHistory
  , askStreamingWithHistory
  , askImageWithHistory
  , askWithTools
  , askWithToolsStreaming
  , runLLM
  , runLLMWith

    -- * Configuration
  , Config (..)
  , defaultConfig

    -- * Conversation values
  , ChatMessage (..)
  , MessageContent (..)
  , ContentPart (..)
  , ChatAnswer (..)
  , FunctionTool (..)
  , ToolCall (..)
  , userText
  , userWithImages
  , systemText
  , assistantText
  , assistantAnswer
  , toolResult
  )
where

import Bot.Prelude hiding (ask)
import qualified Bot.Util.Image as Image
import qualified Bot.Core.ReplyBody as ReplyBody
import Control.Concurrent (forkIO, killThread)
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Req
import Optics ((%~))
import qualified Streaming.Prelude as S
import System.IO.Error (ioError, userError)
import qualified Text.URI as URI

-- | Runtime configuration for the OpenAI-compatible chat endpoint.
data Config = Config
  { endpoint :: !Text
  , apiKey   :: !(Maybe Text)
  , model    :: !Text
  , reasoningEffort :: !Text
  , imageGeneration :: !Bool
  , imageGenerationEndpoint :: !(Maybe Text)
  , imageGenerationApiKey :: !(Maybe Text)
  , imageGenerationModel :: !(Maybe Text)
  , imageGenerationQuality :: !(Maybe Text)
  , imageGenerationSize :: !(Maybe Text)
  , imageGenerationAspectRatio :: !(Maybe Text)
  , imageGenerationBackground :: !(Maybe Text)
  , imageGenerationOutputFormat :: !(Maybe Text)
  , imageGenerationOutputCompression :: !(Maybe Int)
  , imageGenerationModeration :: !(Maybe Text)
  }
  deriving (Show)

-- | Defaults for optional LLM features.
defaultConfig :: Config
defaultConfig = Config
  { endpoint = "https://openrouter.ai/api/v1"
  , apiKey   = Nothing
  , model    = "openai/gpt-4o-mini"
  , reasoningEffort = "low"
  , imageGeneration = False
  , imageGenerationEndpoint = Nothing
  , imageGenerationApiKey = Nothing
  , imageGenerationModel = Nothing
  , imageGenerationQuality = Nothing
  , imageGenerationSize = Nothing
  , imageGenerationAspectRatio = Nothing
  , imageGenerationBackground = Nothing
  , imageGenerationOutputFormat = Nothing
  , imageGenerationOutputCompression = Nothing
  , imageGenerationModeration = Nothing
  }

-- | Effect for text, image, and tool-calling LLM requests.
data LLM :: Effect where
  Ask :: [ChatMessage] -> LLM m Text
  AskStream :: [ChatMessage] -> (Text -> IO ()) -> LLM m Text
  AskImage :: [ChatMessage] -> LLM m Text
  AskTools :: [FunctionTool] -> [ChatMessage] -> LLM m ChatAnswer
  AskToolsStream :: [FunctionTool] -> [ChatMessage] -> (Text -> IO ()) -> LLM m ChatAnswer

type instance DispatchOf LLM = Dynamic

-- | Ask a one-shot text question without preserving history.
ask :: LLM :> es => Text -> Eff es Text
ask prompt = askWithHistory [userText prompt]

-- | Ask for a text answer using an explicit chat history.
askWithHistory :: LLM :> es => [ChatMessage] -> Eff es Text
askWithHistory = send . Ask

-- | Ask for a text answer while receiving response chunks.
askStreamingWithHistory :: (LLM :> es, IOE :> es) => [ChatMessage] -> Stream (Of Text) (Eff es) Text
askStreamingWithHistory messages =
  streamFromCallback \emit -> send (AskStream messages emit)

-- | Ask the configured image model to generate an image response.
askImageWithHistory :: LLM :> es => [ChatMessage] -> Eff es Text
askImageWithHistory = send . AskImage

-- | Ask with function tools and return both text and tool calls.
askWithTools :: LLM :> es => [FunctionTool] -> [ChatMessage] -> Eff es ChatAnswer
askWithTools tools messages =
  send (AskTools tools messages)

-- | Ask with function tools while receiving assistant text chunks.
askWithToolsStreaming :: (LLM :> es, IOE :> es) => [FunctionTool] -> [ChatMessage] -> Stream (Of Text) (Eff es) ChatAnswer
askWithToolsStreaming tools messages =
  streamFromCallback \emit -> send (AskToolsStream tools messages emit)

data StreamEvent r
  = StreamChunk !Text
  | StreamDone !r
  | StreamFailed !SomeException

streamFromCallback
  :: IOE :> es
  => ((Text -> IO ()) -> Eff es r)
  -> Stream (Of Text) (Eff es) r
streamFromCallback action = do
  queue <- lift (liftIO Chan.newChan)
  worker <- lift $ withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
    liftIO $ forkIO do
      result <- Exception.try $ runInIO do
        action \chunk -> Chan.writeChan queue (StreamChunk chunk)
      liftIO $ Chan.writeChan queue case result of
        Right value -> StreamDone value
        Left err -> StreamFailed err
  readQueue queue `streamFinally` liftIO (killThread worker)
  where
    readQueue queue = do
      event <- lift (liftIO (Chan.readChan queue))
      case event of
        StreamChunk chunk -> do
          S.yield chunk
          readQueue queue
        StreamDone value ->
          pure value
        StreamFailed err ->
          lift (throwIO err)

streamFinally
  :: Stream (Of a) (Eff es) r
  -> Eff es ()
  -> Stream (Of a) (Eff es) r
streamFinally stream cleanup =
  go stream
  where
    go current = do
      next <- lift (S.next current `onException` cleanup)
      case next of
        Left result -> do
          lift cleanup
          pure result
        Right (item, rest) -> do
          S.yield item
          go rest

-- | Interpret LLM requests through an OpenAI-compatible HTTP endpoint.
runLLM
  :: IOE :> es
  => Log :> es
  => Config
  -> Eff (LLM : es) a
  -> Eff es a
runLLM cfg = interpret $ \_ -> \case
  Ask messages -> askOpenAI False cfg messages
  AskStream messages emit -> askOpenAIStreaming cfg messages emit
  AskImage messages -> askOpenAI True cfg messages
  AskTools tools messages -> askOpenAIWithTools cfg tools messages
  AskToolsStream tools messages emit -> askOpenAIWithToolsStreaming cfg tools messages emit

runLLMWith
  :: ([ChatMessage] -> Eff es Text)
  -> ([ChatMessage] -> (Text -> IO ()) -> Eff es Text)
  -> ([ChatMessage] -> Eff es Text)
  -> ([FunctionTool] -> [ChatMessage] -> Eff es ChatAnswer)
  -> ([FunctionTool] -> [ChatMessage] -> (Text -> IO ()) -> Eff es ChatAnswer)
  -> Eff (LLM : es) a
  -> Eff es a
runLLMWith askText askTextStream askImage askTools askToolsStream = interpret $ \_ -> \case
  Ask messages -> askText messages
  AskStream messages emit -> askTextStream messages emit
  AskImage messages -> askImage messages
  AskTools tools messages -> askTools tools messages
  AskToolsStream tools messages emit -> askToolsStream tools messages emit

askOpenAI :: (IOE :> es, Log :> es) => Bool -> Config -> [ChatMessage] -> Eff es Text
askOpenAI _ Config{apiKey = Nothing} _ =
  pure "LLM is not configured: set llm.api_key."
askOpenAI forceImage cfg@Config{endpoint, apiKey = Just key, model} messages
  | forceImage && not cfg.imageGeneration =
      pure "Image generation is not configured: set llm.image_generation = true."
  | otherwise = do
  let imageRequest = forceImage
      requestEndpoint = if imageRequest then fromMaybe endpoint cfg.imageGenerationEndpoint else endpoint
      requestApiKey = if imageRequest then fromMaybe key cfg.imageGenerationApiKey else key
      requestModel = if imageRequest then fromMaybe model cfg.imageGenerationModel else model
      request = ChatCompletionRequest
        { model = requestModel
        , reasoningEffort = if imageRequest then Nothing else Just cfg.reasoningEffort
        , messages = imagePromptMessages imageRequest messages
        , tools = Nothing
        , modalities = if imageRequest then Just ["image", "text"] else Nothing
        , imageConfig = if imageRequest then imageGenerationConfig cfg else Nothing
        , stream = Nothing
        }
  logInfo "LLM request" (llmRequestLogLine requestEndpoint request)
  (url, options) <- liftIO (chatCompletionsUrl requestEndpoint)
  response <- liftIO $ runReq defaultHttpConfig $
    req POST
      url
      (ReqBodyJson request)
      jsonResponse
      (options <> header "Authorization" (ByteString.pack [i|Bearer #{requestApiKey}|]))
  let body = responseBody response
  logInfo "LLM response" (llmResponseLogLine requestEndpoint requestModel body)
  case chatCompletionText body of
    Just answer
      | imageRequest -> compressImageAnswer (imageCompressionConfig cfg) answer
      | otherwise -> pure answer
    Nothing     -> throwIO (LLMException [i|OpenAI response had no text choices: #{show body :: String}|])

askOpenAIWithTools :: (IOE :> es, Log :> es) => Config -> [FunctionTool] -> [ChatMessage] -> Eff es ChatAnswer
askOpenAIWithTools Config{apiKey = Nothing} _ _ =
  pure (ChatAnswer "LLM is not configured: set llm.api_key." [])
askOpenAIWithTools Config{endpoint, apiKey = Just key, model, reasoningEffort} functionTools messages = do
  let request = ChatCompletionRequest
        { model = model
        , reasoningEffort = Just reasoningEffort
        , messages = messages
        , tools = toolSpecs functionTools
        , modalities = Nothing
        , imageConfig = Nothing
        , stream = Nothing
        }
  logInfo "LLM request" (llmRequestLogLine endpoint request)
  (url, options) <- liftIO (chatCompletionsUrl endpoint)
  response <- liftIO $ runReq defaultHttpConfig $
    req POST
      url
      (ReqBodyJson request)
      jsonResponse
      (options <> header "Authorization" (ByteString.pack [i|Bearer #{key}|]))
  let body = responseBody response
  logInfo "LLM response" (llmResponseLogLine endpoint model body)
  pure (chatCompletionAnswer body)

askOpenAIStreaming :: (IOE :> es, Log :> es) => Config -> [ChatMessage] -> (Text -> IO ()) -> Eff es Text
askOpenAIStreaming Config{apiKey = Nothing} _ emit = do
  liftIO $ emit "LLM is not configured: set llm.api_key."
  pure "LLM is not configured: set llm.api_key."
askOpenAIStreaming Config{endpoint, apiKey = Just key, model, reasoningEffort} messages emit = do
  let request = ChatCompletionRequest
        { model = model
        , reasoningEffort = Just reasoningEffort
        , messages = messages
        , tools = Nothing
        , modalities = Nothing
        , imageConfig = Nothing
        , stream = Just True
        }
  logInfo "LLM streaming request" (llmRequestLogLine endpoint request)
  answer <- streamChatCompletion endpoint key request emit
  logInfo "LLM streaming response" (llmStreamResponseLogLine endpoint model answer)
  pure answer.content

askOpenAIWithToolsStreaming
  :: (IOE :> es, Log :> es)
  => Config
  -> [FunctionTool]
  -> [ChatMessage]
  -> (Text -> IO ())
  -> Eff es ChatAnswer
askOpenAIWithToolsStreaming Config{apiKey = Nothing} _ _ emit = do
  liftIO $ emit "LLM is not configured: set llm.api_key."
  pure (ChatAnswer "LLM is not configured: set llm.api_key." [])
askOpenAIWithToolsStreaming Config{endpoint, apiKey = Just key, model, reasoningEffort} functionTools messages emit = do
  let request = ChatCompletionRequest
        { model = model
        , reasoningEffort = Just reasoningEffort
        , messages = messages
        , tools = toolSpecs functionTools
        , modalities = Nothing
        , imageConfig = Nothing
        , stream = Just True
        }
  logInfo "LLM streaming request" (llmRequestLogLine endpoint request)
  answer <- streamChatCompletion endpoint key request emit
  logInfo "LLM streaming response" (llmStreamResponseLogLine endpoint model answer)
  pure answer

chatCompletionsUrl :: Text -> IO (Url 'Https, Option 'Https)
chatCompletionsUrl endpoint = do
  uri <- URI.mkURI endpoint
  case useHttpsURI uri of
    Nothing ->
      ioError (userError [i|Unsupported LLM endpoint URL: #{endpoint}. Use a full HTTPS base URL such as https://api.openai.com/v1.|])
    Just (url, options) ->
      pure (url /: "chat" /: "completions", options)

newtype LLMException = LLMException Text
  deriving (Show)
instance Exception LLMException

data ChatCompletionRequest = ChatCompletionRequest
  { model       :: !Text
  , reasoningEffort :: !(Maybe Text)
  , messages    :: ![ChatMessage]
  , tools       :: !(Maybe [ToolSpec])
  , modalities  :: !(Maybe [Text])
  , imageConfig :: !(Maybe Aeson.Value)
  , stream      :: !(Maybe Bool)
  }
  deriving (Show, Generic)

instance Aeson.ToJSON ChatCompletionRequest where
  toJSON ChatCompletionRequest{model, reasoningEffort, messages, tools, modalities, imageConfig, stream} =
    Aeson.object $
      [ "model" Aeson..= model
      , "messages" Aeson..= messages
      ]
      <> maybe [] (\value -> ["reasoning_effort" Aeson..= value]) reasoningEffort
      <> maybe [] (\value -> ["tools" Aeson..= value]) tools
      <> maybe [] (\value -> ["modalities" Aeson..= value]) modalities
      <> maybe [] (\value -> ["image_config" Aeson..= value]) imageConfig
      <> maybe [] (\value -> ["stream" Aeson..= value]) stream

newtype ToolSpec
  = FunctionToolSpec FunctionTool
  deriving (Show)

instance Aeson.ToJSON ToolSpec where
  toJSON = \case
    FunctionToolSpec tool -> Aeson.toJSON tool

-- | OpenAI-compatible function tool schema exposed to the model.
data FunctionTool = FunctionTool
  { name        :: !Text
  , description :: !Text
  , parameters  :: !Aeson.Value
  }
  deriving (Show)

instance Aeson.ToJSON FunctionTool where
  toJSON FunctionTool{name, description, parameters} =
    Aeson.object
      [ "type" Aeson..= Aeson.String "function"
      , "function" Aeson..= Aeson.object
          [ "name" Aeson..= name
          , "description" Aeson..= description
          , "parameters" Aeson..= parameters
          ]
      ]

toolSpecs :: [FunctionTool] -> Maybe [ToolSpec]
toolSpecs function =
  case map FunctionToolSpec function of
    [] -> Nothing
    specs -> Just specs

imageGenerationConfig :: Config -> Maybe Aeson.Value
imageGenerationConfig Config{imageGenerationQuality, imageGenerationSize, imageGenerationAspectRatio, imageGenerationBackground, imageGenerationOutputFormat, imageGenerationOutputCompression, imageGenerationModeration} =
  case fields of
    [] -> Nothing
    _  -> Just (Aeson.object fields)
  where
    fields =
      maybe [] (\value -> ["quality" Aeson..= value]) imageGenerationQuality
        <> maybe [] (\value -> ["size" Aeson..= value]) imageGenerationSize
        <> maybe [] (\value -> ["aspect_ratio" Aeson..= value]) imageGenerationAspectRatio
        <> maybe [] (\value -> ["background" Aeson..= value]) imageGenerationBackground
        <> maybe [] (\value -> ["output_format" Aeson..= value]) imageGenerationOutputFormat
        <> maybe [] (\value -> ["output_compression" Aeson..= value]) imageGenerationOutputCompression
        <> maybe [] (\value -> ["moderation" Aeson..= value]) imageGenerationModeration

imageCompressionConfig :: Config -> Image.ImageCompressionConfig
imageCompressionConfig Config{imageGenerationOutputFormat, imageGenerationOutputCompression} =
  Image.ImageCompressionConfig
    { outputFormat = imageGenerationOutputFormat
    , outputCompression = imageGenerationOutputCompression
    }

compressImageAnswer :: (IOE :> es, Log :> es) => Image.ImageCompressionConfig -> Text -> Eff es Text
compressImageAnswer cfg =
  ReplyBody.traverseReplyImageUrls \ref -> do
    compressed <- Image.compressDataImageReference cfg ref
    pure (fromMaybe ref compressed)

llmRequestLogLine :: Text -> ChatCompletionRequest -> Text
llmRequestLogLine endpoint request =
  Text.unwords
    [ "endpoint=" <> endpoint
    , "model=" <> request.model
    , "messages=" <> show (length request.messages)
    , "tools=" <> maybe "0" (show . length) request.tools
    , "modalities=" <> maybe "-" (Text.intercalate ",") request.modalities
    , "image_config=" <> show (isJust request.imageConfig)
    , "stream=" <> show (fromMaybe False request.stream)
    ]

llmResponseLogLine :: Text -> Text -> ChatCompletionResponse -> Text
llmResponseLogLine endpoint model response =
  Text.unwords
    [ "endpoint=" <> endpoint
    , "model=" <> model
    , "choices=" <> show (length response.choices)
    , "usage=" <> show (isJust response.usage)
    , "annotations=" <> show (length annotations)
    , "images=" <> show (length images)
    , "tool_calls=" <> show (length toolCalls)
    ]
  where
    annotations = foldMap (fromMaybe [] . (.message.annotations)) response.choices
    images = foldMap (fromMaybe [] . (.message.images)) response.choices
    toolCalls = foldMap (.message.toolCalls) response.choices

llmStreamResponseLogLine :: Text -> Text -> ChatAnswer -> Text
llmStreamResponseLogLine endpoint model answer =
  Text.unwords
    [ "endpoint=" <> endpoint
    , "model=" <> model
    , "content_chars=" <> show (Text.length answer.content)
    , "tool_calls=" <> show (length answer.toolCalls)
    ]

streamChatCompletion
  :: (IOE :> es, Log :> es)
  => Text
  -> Text
  -> ChatCompletionRequest
  -> (Text -> IO ())
  -> Eff es ChatAnswer
streamChatCompletion endpoint apiKey request emit = do
  httpRequest <- liftIO (streamingHttpRequest endpoint apiKey request)
  manager <- liftIO HTTP.newTlsManager
  response <- liftIO (HTTP.responseOpen httpRequest manager)
  let bodyReader = HTTP.responseBody response
  let initial = StreamState "" "" Map.empty
  streamStateAnswer <$> (processBody bodyReader initial `finally` liftIO (HTTP.responseClose response))
  where
    processBody bodyReader streamState = do
      chunk <- liftIO (HTTP.brRead bodyReader)
      if StrictByteString.null chunk
        then processSseText True "" streamState
        else do
          let text = TextEncoding.decodeUtf8With TextEncoding.lenientDecode chunk
          next <- processSseText False text streamState
          processBody bodyReader next

    processSseText flush text streamState = do
      let buffered = streamState.pendingLine <> text
          lines_ = Text.splitOn "\n" buffered
          completeLines =
            if flush then lines_ else dropLast lines_
          pendingLine =
            if flush then "" else lastOrEmpty lines_
      next <- foldlM processSseLine streamState{pendingLine = pendingLine} completeLines
      pure next

    processSseLine streamState rawLine =
      case Text.strip <$> Text.stripPrefix "data:" (Text.stripStart rawLine) of
        Nothing ->
          pure streamState
        Just "[DONE]" ->
          pure streamState
        Just payload ->
          case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 payload) of
            Left err -> do
              logAttention "Ignoring malformed LLM stream chunk" (Text.pack err)
              pure streamState
            Right chunk ->
              applyStreamChunk emit streamState chunk

streamingHttpRequest :: Text -> Text -> ChatCompletionRequest -> IO HTTP.Request
streamingHttpRequest endpoint apiKey request = do
  base <- HTTP.parseRequest (Text.unpack (Text.dropWhileEnd (== '/') endpoint <> "/chat/completions"))
  pure base
    { HTTP.method = "POST"
    , HTTP.requestHeaders =
        [ ("Authorization", ByteString.pack [i|Bearer #{apiKey}|])
        , ("Content-Type", "application/json")
        ]
    , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode request)
    , HTTP.responseTimeout = HTTP.responseTimeoutNone
    }

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x : xs) = x : dropLast xs

lastOrEmpty :: [Text] -> Text
lastOrEmpty [] = ""
lastOrEmpty xs = fromMaybe "" (viaNonEmpty last xs)

data StreamState = StreamState
  { pendingLine :: !Text
  , contentAccumulator :: !Text
  , toolAccumulator :: !(Map Int PartialToolCall)
  }
  deriving (Show, Generic)

data PartialToolCall = PartialToolCall
  { partialId :: !(Maybe Text)
  , partialName :: !(Maybe Text)
  , partialArguments :: !Text
  }
  deriving (Show, Generic)

emptyPartialToolCall :: PartialToolCall
emptyPartialToolCall = PartialToolCall Nothing Nothing ""

streamStateAnswer :: StreamState -> ChatAnswer
streamStateAnswer streamState =
  ChatAnswer
    { content = Text.strip streamState.contentAccumulator
    , toolCalls = mapMaybe completePartialToolCall (Map.elems streamState.toolAccumulator)
    }

completePartialToolCall :: PartialToolCall -> Maybe ToolCall
completePartialToolCall PartialToolCall{partialId, partialName, partialArguments} = do
  callId <- partialId
  functionName <- partialName
  pure ToolCall
    { id = callId
    , name = functionName
    , arguments = partialArguments
    }

applyStreamChunk :: IOE :> es => (Text -> IO ()) -> StreamState -> ChatCompletionStreamChunk -> Eff es StreamState
applyStreamChunk emit streamState chunk =
  foldlM applyStreamChoice streamState chunk.choices
  where
    applyStreamChoice acc StreamChoice{delta} = do
      let contentDelta = fromMaybe "" delta.content
      unless (Text.null contentDelta) (liftIO (emit contentDelta))
      pure $ acc
        & #contentAccumulator %~ (<> contentDelta)
        & #toolAccumulator %~ \toolAccumulator ->
            foldl' applyToolCallDelta toolAccumulator delta.toolCalls

applyToolCallDelta :: Map Int PartialToolCall -> ToolCallDelta -> Map Int PartialToolCall
applyToolCallDelta acc delta =
  Map.alter (Just . applyToPartial . fromMaybe emptyPartialToolCall) delta.index acc
  where
    applyToPartial partial =
      partial
        & #partialId %~ (delta.id <|>)
        & #partialName %~ ((delta.function >>= (.name)) <|>)
        & #partialArguments %~ (<> fromMaybe "" (delta.function >>= (.arguments)))

data ChatCompletionStreamChunk = ChatCompletionStreamChunk
  { choices :: ![StreamChoice]
  }
  deriving (Show)

instance Aeson.FromJSON ChatCompletionStreamChunk where
  parseJSON = Aeson.withObject "ChatCompletionStreamChunk" $ \o ->
    ChatCompletionStreamChunk <$> o Aeson..: "choices"

data StreamChoice = StreamChoice
  { delta :: !StreamDelta
  }
  deriving (Show)

instance Aeson.FromJSON StreamChoice where
  parseJSON = Aeson.withObject "StreamChoice" $ \o ->
    StreamChoice <$> o Aeson..: "delta"

data StreamDelta = StreamDelta
  { content :: !(Maybe Text)
  , toolCalls :: ![ToolCallDelta]
  }
  deriving (Show)

instance Aeson.FromJSON StreamDelta where
  parseJSON = Aeson.withObject "StreamDelta" $ \o -> do
    content <- o Aeson..:? "content"
    toolCalls <- fromMaybe [] <$> o Aeson..:? "tool_calls"
    pure StreamDelta{content, toolCalls}

data ToolCallDelta = ToolCallDelta
  { index :: !Int
  , id :: !(Maybe Text)
  , function :: !(Maybe FunctionDelta)
  }
  deriving (Show)

instance Aeson.FromJSON ToolCallDelta where
  parseJSON = Aeson.withObject "ToolCallDelta" $ \o -> do
    index <- o Aeson..: "index"
    callId <- o Aeson..:? "id"
    function <- o Aeson..:? "function"
    pure ToolCallDelta{index, id = callId, function}

data FunctionDelta = FunctionDelta
  { name :: !(Maybe Text)
  , arguments :: !(Maybe Text)
  }
  deriving (Show, Generic, Aeson.FromJSON)

-- | One message in an OpenAI-compatible chat transcript.
data ChatMessage = ChatMessage
  { role    :: !Text
  , content :: !(Maybe MessageContent)
  , toolCalls :: ![ToolCall]
  , toolCallId :: !(Maybe Text)
  }
  deriving (Show)

instance Aeson.ToJSON ChatMessage where
  toJSON ChatMessage{role, content, toolCalls, toolCallId} =
    Aeson.object $
      [ "role" Aeson..= role
      ]
      <> maybe [] (\value -> ["content" Aeson..= value]) content
      <> [ "tool_calls" Aeson..= toolCalls | not (null toolCalls) ]
      <> maybe [] (\value -> ["tool_call_id" Aeson..= value]) toolCallId

instance Aeson.FromJSON ChatMessage where
  parseJSON = Aeson.withObject "ChatMessage" $ \o -> do
    role <- o Aeson..: "role"
    content <- o Aeson..:? "content"
    toolCalls <- fromMaybe [] <$> o Aeson..:? "tool_calls"
    toolCallId <- o Aeson..:? "tool_call_id"
    pure ChatMessage
      { role = role
      , content = content
      , toolCalls = toolCalls
      , toolCallId = toolCallId
      }

-- | Chat message content as plain text or OpenAI-compatible content parts.
data MessageContent
  = TextContent !Text
  | PartsContent ![ContentPart]
  deriving (Show)

instance Aeson.ToJSON MessageContent where
  toJSON = \case
    TextContent text -> Aeson.String text
    PartsContent parts -> Aeson.toJSON parts

instance Aeson.FromJSON MessageContent where
  parseJSON value =
    (TextContent <$> Aeson.parseJSON value) <|>
      (PartsContent <$> Aeson.parseJSON value)

-- | One OpenAI-compatible multimodal content part.
data ContentPart
  = TextPart !Text
  | ImageUrlPart !Text
  deriving (Show)

instance Aeson.ToJSON ContentPart where
  toJSON = \case
    TextPart text ->
      Aeson.object
        [ "type" Aeson..= Aeson.String "text"
        , "text" Aeson..= text
        ]
    ImageUrlPart url ->
      Aeson.object
        [ "type" Aeson..= Aeson.String "image_url"
        , "image_url" Aeson..= Aeson.object
            [ "url" Aeson..= url
            ]
        ]

instance Aeson.FromJSON ContentPart where
  parseJSON = Aeson.withObject "ContentPart" $ \o -> do
    type_ <- o Aeson..: "type" :: AesonTypes.Parser Text
    case type_ of
      "text" ->
        TextPart <$> o Aeson..: "text"
      "image_url" -> do
        imageUrl <- o Aeson..: "image_url"
        url <- parseImageUrl imageUrl
        pure (ImageUrlPart url)
      other ->
        fail [i|Unknown content part type: #{other}|]
    where
      parseImageUrl = \case
        Aeson.String url -> pure url
        Aeson.Object obj -> obj Aeson..: "url"
        _ -> fail "Invalid image_url content part"

-- | Construct a text-only user message.
userText :: Text -> ChatMessage
userText prompt =
  ChatMessage "user" (Just (TextContent prompt)) [] Nothing

-- | Construct a system prompt message.
systemText :: Text -> ChatMessage
systemText prompt =
  ChatMessage "system" (Just (TextContent prompt)) [] Nothing

-- | Construct a user message with optional image URL parts.
userWithImages :: Text -> [Text] -> ChatMessage
userWithImages prompt [] =
  userText prompt
userWithImages prompt urls =
  ChatMessage "user" (Just (PartsContent (TextPart prompt : map ImageUrlPart urls))) [] Nothing

-- | Construct a text-only assistant message.
assistantText :: Text -> ChatMessage
assistantText answer =
  ChatMessage "assistant" (Just (TextContent answer)) [] Nothing

-- | Convert a normalized answer back into transcript form.
assistantAnswer :: ChatAnswer -> ChatMessage
assistantAnswer ChatAnswer{content, toolCalls} =
  ChatMessage "assistant" messageContent toolCalls Nothing
  where
    messageContent
      | Text.null content = Nothing
      | otherwise         = Just (TextContent content)

-- | Construct a tool result message for a previously requested call.
toolResult :: ToolCall -> Text -> ChatMessage
toolResult call result =
  ChatMessage "tool" (Just (TextContent result)) [] (Just call.id)

imagePromptMessages :: Bool -> [ChatMessage] -> [ChatMessage]
imagePromptMessages False messages = messages
imagePromptMessages True messages =
  systemText "The user is asking for an actual generated image. Generate image output; do not answer with ASCII art, SVG, markdown art, or only a textual description." : messages

data ChatCompletionResponse = ChatCompletionResponse
  { choices :: [Choice]
  , usage   :: Maybe Aeson.Value
  }
  deriving (Show, Generic, Aeson.FromJSON)

data Choice = Choice
  { message :: ChatMessageResponse
  }
  deriving (Show, Generic, Aeson.FromJSON)

data ChatMessageResponse = ChatMessageResponse
  { content     :: Maybe Text
  , annotations :: Maybe [Aeson.Value]
  , images      :: Maybe [Aeson.Value]
  , toolCalls   :: [ToolCall]
  }
  deriving (Show, Generic)

instance Aeson.FromJSON ChatMessageResponse where
  parseJSON = Aeson.withObject "ChatMessageResponse" $ \o -> do
    content <- o Aeson..:? "content"
    annotations <- o Aeson..:? "annotations"
    images <- o Aeson..:? "images"
    toolCalls <- fromMaybe [] <$> o Aeson..:? "tool_calls"
    pure ChatMessageResponse
      { content = content
      , annotations = annotations
      , images = images
      , toolCalls = toolCalls
      }

-- | Assistant response after normalizing provider-specific response shapes.
data ChatAnswer = ChatAnswer
  { content   :: !Text
  , toolCalls :: ![ToolCall]
  }
  deriving (Show, Generic, Aeson.ToJSON)

-- | A single function call requested by the model.
data ToolCall = ToolCall
  { id        :: !Text
  , name      :: !Text
  , arguments :: !Text
  }
  deriving (Eq, Show, Generic)

instance Aeson.FromJSON ToolCall where
  parseJSON = Aeson.withObject "ToolCall" $ \o -> do
    callId <- o Aeson..: "id"
    function <- o Aeson..: "function"
    functionName <- function Aeson..: "name"
    functionArguments <- parseArguments function
    pure ToolCall
      { id = callId
      , name = functionName
      , arguments = functionArguments
      }
    where
      parseArguments functionObject =
        (functionObject Aeson..:? "arguments" :: AesonTypes.Parser (Maybe Text)) >>= \case
          Just text -> pure text
          Nothing -> do
            value <- functionObject Aeson..: "arguments"
            pure (TextEncoding.decodeUtf8 (LazyByteString.toStrict (Aeson.encode (value :: Aeson.Value))))

instance Aeson.ToJSON ToolCall where
  toJSON ToolCall{id = callId, name = functionName, arguments = functionArguments} =
    Aeson.object
      [ "id" Aeson..= callId
      , "type" Aeson..= Aeson.String "function"
      , "function" Aeson..= Aeson.object
          [ "name" Aeson..= functionName
          , "arguments" Aeson..= functionArguments
          ]
      ]

chatCompletionText :: ChatCompletionResponse -> Maybe Text
chatCompletionText response =
  viaNonEmpty head response.choices >>= \choice ->
    let imageText = imageUrlsText choice.message.images
    in case Text.strip <$> choice.message.content of
      Just "" -> imageText
      Just answer ->
        Just (Text.strip (answer <> maybe "" ("\n" <>) imageText))
      Nothing -> imageText

chatCompletionAnswer :: ChatCompletionResponse -> ChatAnswer
chatCompletionAnswer response =
  case viaNonEmpty head response.choices of
    Nothing ->
      ChatAnswer "" []
    Just choice ->
      ChatAnswer
        { content = fromMaybe "" (chatMessageText choice.message)
        , toolCalls = choice.message.toolCalls
        }

chatMessageText :: ChatMessageResponse -> Maybe Text
chatMessageText message =
  let imageText = imageUrlsText message.images
  in case Text.strip <$> message.content of
    Just "" -> imageText
    Just answer ->
      Just (Text.strip (answer <> maybe "" ("\n" <>) imageText))
    Nothing -> imageText

imageUrlsText :: Maybe [Aeson.Value] -> Maybe Text
imageUrlsText images =
  case mapMaybe imageValueUrl (fromMaybe [] images) of
    []   -> Nothing
    urls -> Just (Text.unlines (map ReplyBody.imageDirective urls))

imageValueUrl :: Aeson.Value -> Maybe Text
imageValueUrl = \case
  Aeson.Object obj -> do
    imageUrl <- KeyMap.lookup "image_url" obj <|> KeyMap.lookup "imageUrl" obj
    case imageUrl of
      Aeson.Object imageObject -> do
        Aeson.String url <- KeyMap.lookup "url" imageObject
        pure url
      Aeson.String url -> pure url
      _ -> Nothing
  _ -> Nothing
