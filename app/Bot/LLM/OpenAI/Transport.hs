{-|
Module      : Bot.LLM.OpenAI.Transport
Description : OpenAI-compatible LLM transport
Stability   : experimental
-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bot.LLM.OpenAI.Transport
  ( -- * Streaming transport requests
    askOpenAIStreaming
  , askImageOpenAIStreaming
  , askImageEditOpenAIStreaming
  , askAudioOpenAIStreaming
  , askOpenAIWithToolsStreaming

    -- * Streaming parser
  , chatStreamTextFromPayloads
  , imageGenerationStreamingRequestPayload
  , imageGenerationStreamTextFromPayloads
  , audioSpeechRequestPayload
  )
where

import Bot.Prelude
import qualified Bot.Effect.LLM as LLM
import Bot.LLM.OpenAI.Config
import Bot.LLM.Types
import qualified Bot.Util.Image as Image
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Util.HTTP as Http
import qualified Bot.Util.Stream as StreamUtil
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
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import qualified Network.HTTP.Types.Status as HTTPStatus
import Network.HTTP.Req
import Optics ((%~))
import qualified Streaming.Prelude as S
import System.IO.Error (ioError, userError)
import qualified Effectful.Timeout as Timeout
import qualified Text.URI as URI
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import Effectful.Process (Process)
import GHC.Clock (getMonotonicTimeNSec)
import System.FilePath ((</>), (<.>))

chatCompletionsPath :: [Text]
chatCompletionsPath =
  ["chat", "completions"]

imageGenerationsPath :: [Text]
imageGenerationsPath =
  ["images", "generations"]

imageEditsPath :: [Text]
imageEditsPath =
  ["images", "edits"]

audioSpeechPath :: [Text]
audioSpeechPath =
  ["audio", "speech"]

chatCompletionsEndpoint :: ChatProviderConfig -> Text
chatCompletionsEndpoint ChatProviderConfig{baseUrl} =
  endpointText baseUrl chatCompletionsPath

endpointText :: Text -> [Text] -> Text
endpointText url path =
  case path of
    [] -> Text.dropWhileEnd (== '/') url
    _  -> Text.dropWhileEnd (== '/') url <> "/" <> Text.intercalate "/" path

secondsToMicros :: Int -> Int
secondsToMicros seconds =
  seconds * 1000000

chatNotConfiguredMessage :: Text
chatNotConfiguredMessage =
  "LLM chat is not configured: set llm.chat and llm.chat_provider.<name>.api_key."

chatApiKeyNotConfiguredMessage :: Text
chatApiKeyNotConfiguredMessage =
  "LLM chat is not configured: set llm.chat_provider.<name>.api_key."

imageNotConfiguredMessage :: Text
imageNotConfiguredMessage =
  "Image generation is not configured: set llm.image and llm.image_provider.<name>.api_key."

imageEditNotConfiguredMessage :: Text
imageEditNotConfiguredMessage =
  "Image editing is not configured: set llm.image and llm.image_provider.<name>.api_key."

imageApiKeyNotConfiguredMessage :: Text
imageApiKeyNotConfiguredMessage =
  "Image generation is not configured: set llm.image_provider.<name>.api_key."

audioNotConfiguredMessage :: Text
audioNotConfiguredMessage =
  "Audio generation is not configured: set llm.audio and llm.audio_provider.<name>.api_key."

audioApiKeyNotConfiguredMessage :: Text
audioApiKeyNotConfiguredMessage =
  "Audio generation is not configured: set llm.audio_provider.<name>.api_key."

llmHttpConfig :: HTTP.Manager -> HttpConfig
llmHttpConfig manager =
  (Http.httpConfig manager)
    { httpConfigRetryJudge = \_ _ -> False
    , httpConfigRetryJudgeException = \_ _ -> False
    }

runTimedLLMReq :: (Fail :> es, Timeout.Timeout :> es, IOE :> es) => Text -> Int -> Req a -> Eff es a
runTimedLLMReq label timeoutSeconds action = do
  manager <- liftIO $ Http.newTlsManager
  result <- Timeout.timeout (secondsToMicros timeoutSeconds) $
    liftIO $ Http.runReqWithConfig (llmHttpConfig manager) action
  case result of
    Just value ->
      pure value
    Nothing ->
      throwIO (LLMException [i|#{label} timed out after #{timeoutSeconds} seconds.|])

runTimedEff :: (Timeout.Timeout :> es, IOE :> es) => Text -> Int -> Eff es a -> Eff es a
runTimedEff label timeoutSeconds action = do
  result <- Timeout.timeout (secondsToMicros timeoutSeconds) action
  case result of
    Just value ->
      pure value
    Nothing ->
      throwIO (LLMException [i|#{label} timed out after #{timeoutSeconds} seconds.|])

askImageOpenAIStreaming :: (IOE :> es, KatipE :> es, Timeout.Timeout :> es, Fail :> es, FileSystem :> es, Process :> es) => Config -> LLM.ImageRequestOptions -> [ChatMessage] -> Stream (Of Text) (Eff es) Text
askImageOpenAIStreaming Config{imageProvider = Nothing} _ _ =
  yieldTextResult (pure imageNotConfiguredMessage)
askImageOpenAIStreaming Config{imageProvider = Just provider} options messages
  | provider.canGenerate =
      askImageGenerationsOpenAIStreaming provider options messages
  | otherwise =
      askImageChatCompletionsOpenAIStreaming provider options messages

askImageEditOpenAIStreaming :: (IOE :> es, KatipE :> es, Timeout.Timeout :> es, Fail :> es, FileSystem :> es, Process :> es, Fail :> es) => Config -> LLM.ImageRequestOptions -> Text -> [Text] -> Maybe Text -> Stream (Of Text) (Eff es) Text
askImageEditOpenAIStreaming Config{imageProvider = Nothing} _ _ _ _ =
  yieldTextResult (pure imageEditNotConfiguredMessage)
askImageEditOpenAIStreaming Config{imageProvider = Just cfg@ImageProviderConfig{apiKey, model, canEdit, requestTimeout}} options prompt imageRefs maskRef
  | not canEdit =
      yieldTextResult (pure "Image editing is not configured for this image provider: set llm.image_provider.<name>.can_edit = true only when the configured image model and endpoint support /images/edits.")
  | null imageRefs =
      yieldTextResult (pure "Image editing requires at least one input image.")
  | otherwise =
      case apiKey of
        Nothing ->
          yieldTextResult (pure imageApiKeyNotConfiguredMessage)
        Just key -> do
          let requestBaseUrl = cfg.baseUrl
              requestPath = imageEditsPath
              requestEndpoint = endpointText requestBaseUrl requestPath
              requestModel = model
          imageUploads <- lift $ traverse (uncurry (imageUploadFromReference requestTimeout)) (zip [1 :: Int ..] imageRefs)
          maskUpload <- lift $ traverse (imageUploadFromReference requestTimeout 0) maskRef
          let parts = imageEditMultipartParts cfg options requestModel prompt imageUploads maskUpload
          lift $ logInfo ("LLM image edit streaming request: " <> imageEditRequestLogLine requestEndpoint requestTimeout requestModel prompt imageUploads maskUpload)
          body <- lift $
            runTimedEff "LLM image edit streaming request" requestTimeout $
              foldImageGenerationStreamWith "Image edit" $
                streamSseMultipartPost requestBaseUrl requestPath key (secondsToMicros requestTimeout) parts
          lift $ logInfo ("LLM image edit response: " <> imageResponseLogLine requestEndpoint requestModel body)
          case imageGenerationResponseText cfg body of
            Just answer -> do
              compressed <- lift (compressImageAnswer (imageCompressionConfig cfg) answer)
              S.yield compressed
              pure compressed
            Nothing ->
              lift (throwIO (LLMException "Image edit response was empty: no image output."))

askAudioOpenAIStreaming :: (IOE :> es, KatipE :> es, Timeout.Timeout :> es, Fail :> es, FileSystem :> es) => Config -> LLM.AudioRequestOptions -> [ChatMessage] -> Stream (Of Text) (Eff es) Text
askAudioOpenAIStreaming Config{audioProvider = Nothing} _ _ =
  yieldTextResult (pure audioNotConfiguredMessage)
askAudioOpenAIStreaming Config{audioProvider = Just AudioProviderConfig{apiKey = Nothing}} _ _ =
  yieldTextResult (pure audioApiKeyNotConfiguredMessage)
askAudioOpenAIStreaming Config{audioProvider = Just cfg@AudioProviderConfig{apiKey = Just key, model, requestTimeout}} options messages = do
  let requestBaseUrl = cfg.baseUrl
      requestPath = audioSpeechPath
      requestEndpoint = endpointText requestBaseUrl requestPath
      request = audioSpeechRequest cfg options model (audioPromptFromMessages messages)
  lift $ logInfo ("LLM audio request: " <> audioRequestLogLine requestEndpoint requestTimeout request)
  bytes <- lift $
    runTimedEff "LLM audio streaming request" requestTimeout $
      collectByteStream $
        streamRawJsonPost requestBaseUrl requestPath key (secondsToMicros requestTimeout) "application/octet-stream" request
  ref <- lift (writeGeneratedAudio request.responseFormat bytes)
  lift $ logInfo ("LLM audio response: " <> audioResponseLogLine requestEndpoint request.model request.responseFormat bytes)
  S.yield ref
  pure ref

yieldTextResult :: Monad m => m Text -> Stream (Of Text) m Text
yieldTextResult action = do
  answer <- lift action
  unless (Text.null (Text.strip answer)) (S.yield answer)
  pure answer

askImageChatCompletionsOpenAIStreaming :: (IOE :> es, KatipE :> es, Timeout.Timeout :> es, Fail :> es, FileSystem :> es, Process :> es, Fail :> es) => ImageProviderConfig -> LLM.ImageRequestOptions -> [ChatMessage] -> Stream (Of Text) (Eff es) Text
askImageChatCompletionsOpenAIStreaming cfg@ImageProviderConfig{apiKey, model, requestTimeout} options messages =
  case apiKey of
    Nothing ->
      yieldTextResult (pure imageApiKeyNotConfiguredMessage)
    Just key -> do
      let requestBaseUrl = cfg.baseUrl
          requestPath = chatCompletionsPath
          requestEndpoint = endpointText requestBaseUrl requestPath
          requestModel = model
          request = ChatCompletionRequest
            { model = requestModel
            , reasoningEffort = Nothing
            , messages = imagePromptMessages True messages
            , tools = Nothing
            , modalities = Just ["image", "text"]
            , imageConfig = imageGenerationConfig cfg options
            , stream = Just True
            }
      do
        lift $ logInfo ("LLM image chat streaming request: " <> llmRequestLogLine requestEndpoint request)
        lift $ logLLMRequestMessages request
        answer <- streamChatCompletion False requestBaseUrl requestPath key (secondsToMicros requestTimeout) request
        lift $ logInfo ("LLM image chat streaming response: " <> llmStreamResponseLogLine requestEndpoint requestModel answer)
        let text = chatAnswerContent answer
        if Text.null (Text.strip text)
          then lift (throwIO (LLMException "OpenAI image chat streaming response was empty: no text or image output."))
          else do
            compressed <- lift (compressImageAnswer (imageCompressionConfig cfg) text)
            S.yield compressed
            pure compressed

askImageGenerationsOpenAIStreaming :: (IOE :> es, KatipE :> es, Timeout.Timeout :> es, Fail :> es, FileSystem :> es, Process :> es, Fail :> es) => ImageProviderConfig -> LLM.ImageRequestOptions -> [ChatMessage] -> Stream (Of Text) (Eff es) Text
askImageGenerationsOpenAIStreaming cfg@ImageProviderConfig{apiKey, model, requestTimeout} options messages =
  case apiKey of
    Nothing ->
      yieldTextResult (pure imageApiKeyNotConfiguredMessage)
    Just key -> do
      let requestBaseUrl = cfg.baseUrl
          requestPath = imageGenerationsPath
          requestEndpoint = endpointText requestBaseUrl requestPath
          requestModel = model
          request = imageGenerationRequest cfg options requestModel (imagePromptFromMessages messages) (Just True)
      do
        lift (logInfo ("LLM image streaming request: " <> imageRequestLogLine requestEndpoint requestTimeout request))
        body <- lift $
          runTimedEff "LLM image streaming request" requestTimeout $
            foldImageGenerationStreamWith "Image generation" $
              streamSseJsonPost requestBaseUrl requestPath key (secondsToMicros requestTimeout) request
        lift (logInfo ("LLM image response: " <> imageResponseLogLine requestEndpoint request.model body))
        case imageGenerationResponseText cfg body of
          Just answer -> do
            compressed <- lift (compressImageAnswer (imageCompressionConfig cfg) answer)
            S.yield compressed
            pure compressed
          Nothing ->
            lift (throwIO (LLMException "Image generation response was empty: no image output."))

askOpenAIStreaming
  :: (IOE :> es, KatipE :> es, Timeout.Timeout :> es, Fail :> es)
  => Config
  -> [ChatMessage]
  -> Stream (Of Text) (Eff es) Text
askOpenAIStreaming Config{chatProvider = Nothing} _ = do
  S.yield chatNotConfiguredMessage
  pure chatNotConfiguredMessage
askOpenAIStreaming Config{chatProvider = Just ChatProviderConfig{apiKey = Nothing}} _ = do
  S.yield chatApiKeyNotConfiguredMessage
  pure chatApiKeyNotConfiguredMessage
askOpenAIStreaming Config{chatProvider = Just cfg@ChatProviderConfig{apiKey = Just key, model, reasoningEffort, requestTimeout}} messages = do
  let requestEndpoint = chatCompletionsEndpoint cfg
      request = ChatCompletionRequest
        { model = model
        , reasoningEffort = Just reasoningEffort
        , messages = messages
        , tools = Nothing
        , modalities = Nothing
        , imageConfig = Nothing
        , stream = Just True
        }
  lift $ logInfo ("LLM streaming request: " <> llmRequestLogLine requestEndpoint request)
  lift $ logLLMRequestMessages request
  answer <- streamChatCompletion True cfg.baseUrl chatCompletionsPath key (secondsToMicros requestTimeout) request
  lift $ logInfo ("LLM streaming response: " <> llmStreamResponseLogLine requestEndpoint model answer)
  pure (chatAnswerContent answer)

askOpenAIWithToolsStreaming
  :: (IOE :> es, KatipE :> es, Timeout.Timeout :> es, Fail :> es)
  => Config
  -> [FunctionTool]
  -> [ChatMessage]
  -> Stream (Of Text) (Eff es) ChatAnswer
askOpenAIWithToolsStreaming Config{chatProvider = Nothing} _ _ = do
  S.yield chatNotConfiguredMessage
  pure (ChatFinalAnswer chatNotConfiguredMessage)
askOpenAIWithToolsStreaming Config{chatProvider = Just ChatProviderConfig{apiKey = Nothing}} _ _ = do
  S.yield chatApiKeyNotConfiguredMessage
  pure (ChatFinalAnswer chatApiKeyNotConfiguredMessage)
askOpenAIWithToolsStreaming Config{chatProvider = Just cfg@ChatProviderConfig{apiKey = Just key, model, reasoningEffort, requestTimeout}} functionTools messages = do
  let requestEndpoint = chatCompletionsEndpoint cfg
      request = ChatCompletionRequest
        { model = model
        , reasoningEffort = Just reasoningEffort
        , messages = messages
        , tools = toolSpecs functionTools
        , modalities = Nothing
        , imageConfig = Nothing
        , stream = Just True
        }
  lift $ logInfo ("LLM streaming request: " <> llmRequestLogLine requestEndpoint request)
  lift $ logLLMRequestMessages request
  answer <- streamChatCompletion True cfg.baseUrl chatCompletionsPath key (secondsToMicros requestTimeout) request
  lift $ logInfo ("LLM streaming response: " <> llmStreamResponseLogLine requestEndpoint model answer)
  pure answer

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

data ImageGenerationRequest = ImageGenerationRequest
  { model :: !Text
  , prompt :: !Text
  , quality :: !(Maybe Text)
  , size :: !(Maybe Text)
  , background :: !(Maybe Text)
  , outputFormat :: !(Maybe Text)
  , outputCompression :: !(Maybe Int)
  , moderation :: !(Maybe Text)
  , stream :: !(Maybe Bool)
  , partialImages :: !(Maybe Int)
  }
  deriving (Show)

instance Aeson.ToJSON ImageGenerationRequest where
  toJSON ImageGenerationRequest{model, prompt, quality, size, background, outputFormat, outputCompression, moderation, stream, partialImages} =
    Aeson.object $
      [ "model" Aeson..= model
      , "prompt" Aeson..= prompt
      ]
      <> maybe [] (\value -> ["quality" Aeson..= value]) quality
      <> maybe [] (\value -> ["size" Aeson..= value]) size
      <> maybe [] (\value -> ["background" Aeson..= value]) background
      <> maybe [] (\value -> ["output_format" Aeson..= value]) outputFormat
      <> maybe [] (\value -> ["output_compression" Aeson..= value]) outputCompression
      <> maybe [] (\value -> ["moderation" Aeson..= value]) moderation
      <> maybe [] (\value -> ["stream" Aeson..= value]) stream
      <> maybe [] (\value -> ["partial_images" Aeson..= value]) partialImages

data AudioSpeechRequest = AudioSpeechRequest
  { model :: !Text
  , input :: !Text
  , voice :: !Text
  , responseFormat :: !Text
  , speed :: !(Maybe Double)
  , instructions :: !(Maybe Text)
  }
  deriving (Show)

instance Aeson.ToJSON AudioSpeechRequest where
  toJSON AudioSpeechRequest{model, input, voice, responseFormat, speed, instructions} =
    Aeson.object $
      [ "model" Aeson..= model
      , "input" Aeson..= input
      , "voice" Aeson..= voice
      , "response_format" Aeson..= responseFormat
      ]
      <> maybe [] (\value -> ["speed" Aeson..= value]) speed
      <> maybe [] (\value -> ["instructions" Aeson..= value]) instructions

data ImageGenerationResponse = ImageGenerationResponse
  { data_ :: ![ImageGenerationData]
  }
  deriving (Show)

instance Aeson.FromJSON ImageGenerationResponse where
  parseJSON = Aeson.withObject "ImageGenerationResponse" \o ->
    ImageGenerationResponse <$> o Aeson..: "data"

data ImageGenerationData = ImageGenerationData
  { url :: !(Maybe Text)
  , b64Json :: !(Maybe Text)
  }
  deriving (Show)

instance Aeson.FromJSON ImageGenerationData where
  parseJSON = Aeson.withObject "ImageGenerationData" \o ->
    ImageGenerationData
      <$> o Aeson..:? "url"
      <*> o Aeson..:? "b64_json"

data ImageGenerationStreamEvent = ImageGenerationStreamEvent
  { type_ :: !Text
  , b64Json :: !(Maybe Text)
  }
  deriving (Show)

instance Aeson.FromJSON ImageGenerationStreamEvent where
  parseJSON = Aeson.withObject "ImageGenerationStreamEvent" \o ->
    ImageGenerationStreamEvent
      <$> o Aeson..: "type"
      <*> o Aeson..:? "b64_json"

newtype ToolSpec
  = FunctionToolSpec FunctionTool
  deriving (Show)

instance Aeson.ToJSON ToolSpec where
  toJSON = \case
    FunctionToolSpec tool -> Aeson.toJSON tool

toolSpecs :: [FunctionTool] -> Maybe [ToolSpec]
toolSpecs function =
  case map FunctionToolSpec function of
    [] -> Nothing
    specs -> Just specs

imageGenerationConfig :: ImageProviderConfig -> LLM.ImageRequestOptions -> Maybe Aeson.Value
imageGenerationConfig provider options =
  case fields of
    [] -> Nothing
    _  -> Just (Aeson.object fields)
  where
    resolved = resolvedImageRequestOptions provider options
    fields =
      maybe [] (\value -> ["quality" Aeson..= value]) resolved.quality
        <> maybe [] (\value -> ["size" Aeson..= value]) resolved.size
        <> maybe [] (\value -> ["aspect_ratio" Aeson..= value]) provider.aspectRatio
        <> maybe [] (\value -> ["background" Aeson..= value]) resolved.background
        <> maybe [] (\value -> ["output_format" Aeson..= value]) provider.outputFormat
        <> maybe [] (\value -> ["output_compression" Aeson..= value]) provider.outputCompression
        <> maybe [] (\value -> ["moderation" Aeson..= value]) resolved.moderation

imageGenerationRequest :: ImageProviderConfig -> LLM.ImageRequestOptions -> Text -> Text -> Maybe Bool -> ImageGenerationRequest
imageGenerationRequest provider options model prompt stream =
  ImageGenerationRequest
    { model = model
    , prompt = prompt
    , quality = resolved.quality
    , size = resolved.size
    , background = resolved.background
    , outputFormat = provider.outputFormat
    , outputCompression = provider.outputCompression
    , moderation = resolved.moderation
    , stream = stream
    , partialImages =
        if stream == Just True
          then Just 0
          else Nothing
    }
  where
    resolved = resolvedImageRequestOptions provider options

resolvedImageRequestOptions :: ImageProviderConfig -> LLM.ImageRequestOptions -> LLM.ImageRequestOptions
resolvedImageRequestOptions provider options =
  LLM.ImageRequestOptions
    { quality = options.quality <|> provider.quality
    , size = options.size <|> provider.size
    , background = options.background <|> provider.background
    , moderation = options.moderation <|> provider.moderation
    }

audioSpeechRequest :: AudioProviderConfig -> LLM.AudioRequestOptions -> Text -> Text -> AudioSpeechRequest
audioSpeechRequest provider options model input =
  AudioSpeechRequest
    { model = model
    , input = input
    , voice = fromMaybe provider.voice options.voice
    , responseFormat = fromMaybe provider.responseFormat options.responseFormat
    , speed = options.speed <|> provider.speed
    , instructions = options.instructions <|> provider.instructions
    }

audioSpeechRequestPayload :: AudioProviderConfig -> LLM.AudioRequestOptions -> Text -> Text -> Aeson.Value
audioSpeechRequestPayload cfg options model input =
  Aeson.toJSON (audioSpeechRequest cfg options model input)

audioPromptFromMessages :: [ChatMessage] -> Text
audioPromptFromMessages messages =
  case Text.strip (Text.intercalate "\n\n" (mapMaybe chatMessagePromptText messages)) of
    "" -> "Generate speech for this message."
    prompt -> prompt

writeGeneratedAudio :: (IOE :> es, FileSystem :> es) => Text -> StrictByteString.ByteString -> Eff es Text
writeGeneratedAudio format bytes = do
  dir <- FileSystem.getTemporaryDirectory
  nonce <- liftIO getMonotonicTimeNSec
  let path = dir </> ("cosmobot-audio-" <> show nonce <.> Text.unpack (safeAudioExtension format))
  FileSystemByteString.writeFile path bytes
  pure ("file://" <> Text.pack path)

safeAudioExtension :: Text -> Text
safeAudioExtension format =
  case Text.toLower (Text.strip format) of
    "aac" -> "aac"
    "flac" -> "flac"
    "mp3" -> "mp3"
    "opus" -> "opus"
    "pcm" -> "pcm"
    "wav" -> "wav"
    other | Text.all validExtensionChar other && not (Text.null other) -> other
    _ -> "mp3"
  where
    validExtensionChar char =
      (char >= 'a' && char <= 'z') || (char >= '0' && char <= '9')

data ImageUpload = ImageUpload
  { filename :: !FilePath
  , bytes :: !StrictByteString.ByteString
  }

imageUploadFromReference :: (Timeout.Timeout :> es, IOE :> es, Fail :> es) => Int -> Int -> Text -> Eff es ImageUpload
imageUploadFromReference timeoutSeconds index imageRef = do
  bytes <- imageBytesFromReference timeoutSeconds imageRef
  pure ImageUpload
    { filename = "image-" <> show index <> "." <> imageExtension bytes
    , bytes = bytes
    }

imageBytesFromReference :: (Timeout.Timeout :> es, IOE :> es, Fail :> es) => Int -> Text -> Eff es StrictByteString.ByteString
imageBytesFromReference timeoutSeconds imageRef
  | Just bytes <- Image.decodeDataImageReference stripped =
      pure bytes
  | Just path <- Text.stripPrefix "file://" stripped =
      liftIO $ StrictByteString.readFile (Text.unpack path)
  | otherwise =
      downloadImageReference timeoutSeconds stripped
  where
    stripped = Text.strip imageRef

downloadImageReference :: (Timeout.Timeout :> es, IOE :> es, Fail :> es) => Int -> Text -> Eff es StrictByteString.ByteString
downloadImageReference timeoutSeconds imageRef = do
  uri <- URI.mkURI imageRef
  case useHttpsURI uri of
    Nothing ->
      liftIO $ ioError (userError [i|Unsupported image reference URL for image edit: #{imageRef}. Use HTTPS, file://, or data:image/...;base64.|])
    Just (url, options) ->
      runTimedLLMReq "LLM image edit input download" timeoutSeconds $
        responseBody <$> req GET url NoReqBody bsResponse options

imageEditMultipartParts :: ImageProviderConfig -> LLM.ImageRequestOptions -> Text -> Text -> [ImageUpload] -> Maybe ImageUpload -> [Multipart.Part]
imageEditMultipartParts provider options model prompt imageUploads maskUpload =
  textFields
    <> map (imageUploadPart "image[]") imageUploads
    <> maybe [] (\upload -> [imageUploadPart "mask" upload]) maskUpload
  where
    resolved = resolvedImageRequestOptions provider options
    textFields =
      [ multipartTextPart "model" model
      , multipartTextPart "prompt" prompt
      , multipartTextPart "stream" "true"
      , multipartTextPart "partial_images" "0"
      ]
        <> maybeTextPart "quality" resolved.quality
        <> maybeTextPart "size" resolved.size
        <> maybeTextPart "background" (imageEditBackground model resolved.background)
        <> maybeTextPart "output_format" provider.outputFormat
        <> maybeIntPart "output_compression" provider.outputCompression
        <> maybeTextPart "moderation" resolved.moderation

imageUploadPart :: Text -> ImageUpload -> Multipart.Part
imageUploadPart fieldName upload =
  Multipart.partFileRequestBody fieldName upload.filename (HTTP.RequestBodyBS upload.bytes)

multipartTextPart :: Text -> Text -> Multipart.Part
multipartTextPart name value =
  Multipart.partBS name (TextEncoding.encodeUtf8 value)

maybeTextPart :: Text -> Maybe Text -> [Multipart.Part]
maybeTextPart name =
  maybe [] \value -> [multipartTextPart name value]

maybeIntPart :: Text -> Maybe Int -> [Multipart.Part]
maybeIntPart name =
  maybe [] \value -> [multipartTextPart name (show value)]

-- gpt-image-2 rejects transparent backgrounds; omit that unsupported option
-- while preserving other model/config combinations.
imageEditBackground :: Text -> Maybe Text -> Maybe Text
imageEditBackground model background =
  case Text.toLower . Text.strip <$> background of
    Just "transparent" | isGptImage2 model -> Nothing
    _ -> background

isGptImage2 :: Text -> Bool
isGptImage2 model =
  "gpt-image-2" `Text.isSuffixOf` Text.toLower (Text.strip model)

imageExtension :: StrictByteString.ByteString -> FilePath
imageExtension bytes
  | StrictByteString.pack [0x89, 0x50, 0x4e, 0x47] `StrictByteString.isPrefixOf` bytes = "png"
  | StrictByteString.pack [0xff, 0xd8, 0xff] `StrictByteString.isPrefixOf` bytes = "jpg"
  | isWebP bytes = "webp"
  | otherwise = "png"

isWebP :: StrictByteString.ByteString -> Bool
isWebP bytes =
  StrictByteString.pack [0x52, 0x49, 0x46, 0x46] `StrictByteString.isPrefixOf` bytes &&
    StrictByteString.pack [0x57, 0x45, 0x42, 0x50] == StrictByteString.take 4 (StrictByteString.drop 8 bytes)

imageGenerationStreamingRequestPayload :: ImageProviderConfig -> LLM.ImageRequestOptions -> Text -> Text -> Aeson.Value
imageGenerationStreamingRequestPayload cfg options model prompt =
  Aeson.toJSON (imageGenerationRequest cfg options model prompt (Just True))

imagePromptFromMessages :: [ChatMessage] -> Text
imagePromptFromMessages messages =
  case Text.strip (Text.intercalate "\n\n" (mapMaybe chatMessagePromptText messages)) of
    "" -> "Generate an image."
    prompt -> prompt

chatMessagePromptText :: ChatMessage -> Maybe Text
chatMessagePromptText message =
  case message.content of
    Just (TextContent text) ->
      nonEmptyText text
    Just (PartsContent parts) ->
      nonEmptyText (Text.unlines (map partPromptText parts))
    Nothing ->
      Nothing

partPromptText :: ContentPart -> Text
partPromptText = \case
  TextPart text ->
    text
  ImageUrlPart url ->
    "Input image: " <> url

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped

imageGenerationResponseText :: ImageProviderConfig -> ImageGenerationResponse -> Maybe Text
imageGenerationResponseText cfg response =
  case mapMaybe (imageGenerationDataRef cfg) response.data_ of
    [] -> Nothing
    refs -> Just (Text.unlines (map ReplyBody.imageDirective refs))

imageGenerationDataRef :: ImageProviderConfig -> ImageGenerationData -> Maybe Text
imageGenerationDataRef cfg image =
  image.url <|> (dataImageRef cfg <$> image.b64Json)

dataImageRef :: ImageProviderConfig -> Text -> Text
dataImageRef ImageProviderConfig{outputFormat} b64 =
  "data:image/" <> fromMaybe "png" outputFormat <> ";base64," <> b64

imageCompressionConfig :: ImageProviderConfig -> Image.ImageCompressionConfig
imageCompressionConfig ImageProviderConfig{outputFormat, outputCompression} =
  Image.ImageCompressionConfig
    { outputFormat = outputFormat
    , outputCompression = outputCompression
    }

compressImageAnswer :: (IOE :> es, KatipE :> es, FileSystem :> es, Process :> es, Fail :> es) => Image.ImageCompressionConfig -> Text -> Eff es Text
compressImageAnswer cfg =
  ReplyBody.traverseReplyImageUrls \ref -> do
    compressed <- Image.compressDataImageReference cfg ref
    pure (fromMaybe ref compressed)

imageRequestLogLine :: Text -> Int -> ImageGenerationRequest -> Text
imageRequestLogLine endpoint timeoutSeconds request =
  Text.unwords
    [ "endpoint=" <> endpoint
    , "model=" <> request.model
    , "prompt_chars=" <> show (Text.length request.prompt)
    , "timeout_seconds=" <> show timeoutSeconds
    ]

imageEditRequestLogLine :: Text -> Int -> Text -> Text -> [ImageUpload] -> Maybe ImageUpload -> Text
imageEditRequestLogLine endpoint timeoutSeconds model prompt imageUploads maskUpload =
  Text.unwords
    [ "endpoint=" <> endpoint
    , "model=" <> model
    , "prompt_chars=" <> show (Text.length prompt)
    , "images=" <> show (length imageUploads)
    , "mask=" <> show (isJust maskUpload)
    , "timeout_seconds=" <> show timeoutSeconds
    ]

imageResponseLogLine :: Text -> Text -> ImageGenerationResponse -> Text
imageResponseLogLine endpoint model response =
  Text.unwords
    [ "endpoint=" <> endpoint
    , "model=" <> model
    , "images=" <> show (length response.data_)
    ]

audioRequestLogLine :: Text -> Int -> AudioSpeechRequest -> Text
audioRequestLogLine endpoint timeoutSeconds request =
  Text.unwords
    [ "endpoint=" <> endpoint
    , "model=" <> request.model
    , "voice=" <> request.voice
    , "response_format=" <> request.responseFormat
    , "input_chars=" <> show (Text.length request.input)
    , "instructions=" <> show (isJust request.instructions)
    , "timeout_seconds=" <> show timeoutSeconds
    ]

audioResponseLogLine :: Text -> Text -> Text -> StrictByteString.ByteString -> Text
audioResponseLogLine endpoint model responseFormat bytes =
  Text.unwords
    [ "endpoint=" <> endpoint
    , "model=" <> model
    , "response_format=" <> responseFormat
    , "bytes=" <> show (StrictByteString.length bytes)
    ]

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

logLLMRequestMessages :: KatipE :> es => ChatCompletionRequest -> Eff es ()
logLLMRequestMessages request = do
  logDebug ("LLM request first message: " <> firstMessagePreview request.messages)
  logDebug ("LLM request messages: " <> jsonText request.messages)

firstMessagePreview :: [ChatMessage] -> Text
firstMessagePreview [] =
  "<none>"
firstMessagePreview (message : _) =
  Text.unwords
    [ "role=" <> message.role
    , "content=" <> previewMessageContent message.content
    ]

previewMessageContent :: Maybe MessageContent -> Text
previewMessageContent = \case
  Nothing ->
    "<none>"
  Just (TextContent text) ->
    previewText 500 text
  Just (PartsContent parts) ->
    previewText 500 (jsonText parts)

previewText :: Int -> Text -> Text
previewText maxChars text =
  let oneLine = Text.unwords (Text.words text)
  in if Text.length oneLine > maxChars
    then Text.take maxChars oneLine <> "..."
    else oneLine

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

llmStreamResponseLogLine :: Text -> Text -> ChatAnswer -> Text
llmStreamResponseLogLine endpoint model answer =
  Text.unwords
    [ "endpoint=" <> endpoint
    , "model=" <> model
    , "content_chars=" <> show (Text.length (chatAnswerContent answer))
    , "tool_calls=" <> show (length (chatAnswerToolCalls answer))
    ]

streamSseJsonPost
  :: (Aeson.ToJSON body, IOE :> es)
  => Text
  -> [Text]
  -> Text
  -> Int
  -> body
  -> Stream (Of StrictByteString.ByteString) (Eff es) ()
streamSseJsonPost baseUrl path apiKey timeoutMicros request = do
  httpRequest <- liftIO (sseJsonPostRequest baseUrl path apiKey timeoutMicros request)
  streamSsePayloads (streamHttpResponseBody httpRequest)

streamRawJsonPost
  :: (Aeson.ToJSON body, IOE :> es)
  => Text
  -> [Text]
  -> Text
  -> Int
  -> ByteString.ByteString
  -> body
  -> Stream (Of StrictByteString.ByteString) (Eff es) ()
streamRawJsonPost baseUrl path apiKey timeoutMicros accept request = do
  httpRequest <- liftIO (rawJsonPostRequest baseUrl path apiKey timeoutMicros accept request)
  streamHttpResponseBody httpRequest

streamSseMultipartPost
  :: IOE :> es
  => Text
  -> [Text]
  -> Text
  -> Int
  -> [Multipart.Part]
  -> Stream (Of StrictByteString.ByteString) (Eff es) ()
streamSseMultipartPost baseUrl path apiKey timeoutMicros parts = do
  httpRequest <- liftIO (sseMultipartPostRequest baseUrl path apiKey timeoutMicros parts)
  streamSsePayloads (streamHttpResponseBody httpRequest)

sseJsonPostRequest :: Aeson.ToJSON body => Text -> [Text] -> Text -> Int -> body -> IO HTTP.Request
sseJsonPostRequest baseUrl path apiKey timeoutMicros request = do
  httpRequest <- Http.streamingJsonPostRequest baseUrl path apiKey timeoutMicros request
  pure httpRequest
    { HTTP.requestHeaders = ("Accept", "text/event-stream") : HTTP.requestHeaders httpRequest
    }

rawJsonPostRequest :: Aeson.ToJSON body => Text -> [Text] -> Text -> Int -> ByteString.ByteString -> body -> IO HTTP.Request
rawJsonPostRequest baseUrl path apiKey timeoutMicros accept request = do
  httpRequest <- Http.streamingJsonPostRequest baseUrl path apiKey timeoutMicros request
  pure httpRequest
    { HTTP.requestHeaders = ("Accept", accept) : HTTP.requestHeaders httpRequest
    }

sseMultipartPostRequest :: Text -> [Text] -> Text -> Int -> [Multipart.Part] -> IO HTTP.Request
sseMultipartPostRequest baseUrl path apiKey timeoutMicros parts = do
  base <- HTTP.parseRequest (Text.unpack (endpointText baseUrl path))
  Multipart.formDataBody parts base
    { HTTP.method = "POST"
    , HTTP.requestHeaders =
        [ ("Authorization", ByteString.pack [i|Bearer #{apiKey}|])
        , ("Accept", "text/event-stream")
        ]
    , HTTP.responseTimeout = HTTP.responseTimeoutMicro timeoutMicros
    }

streamHttpResponseBody :: IOE :> es => HTTP.Request -> Stream (Of StrictByteString.ByteString) (Eff es) ()
streamHttpResponseBody httpRequest = do
  manager <- liftIO Http.newTlsManager
  StreamUtil.bracketStream
    (liftIO (HTTP.responseOpen httpRequest manager))
    (liftIO . HTTP.responseClose)
    \response -> do
      ensureSuccessfulStreamingResponse httpRequest response
      streamBody (HTTP.responseBody response)
  where
    streamBody bodyReader = do
      chunk <- liftIO (HTTP.brRead bodyReader)
      unless (StrictByteString.null chunk) do
        S.yield chunk
        streamBody bodyReader

streamSsePayloads
  :: Monad m
  => Stream (Of StrictByteString.ByteString) m r
  -> Stream (Of StrictByteString.ByteString) m r
streamSsePayloads =
  go ""
  where
    go pending input = do
      lift (S.next input) >>= \case
        Left result -> do
          traverse_ S.yield (snd (ssePayloadsFromText True pending ""))
          pure result
        Right (chunk, rest) -> do
          let (nextPending, payloads) =
                ssePayloadsFromText False pending (TextEncoding.decodeUtf8With TextEncoding.lenientDecode chunk)
          traverse_ S.yield payloads
          go nextPending rest

ssePayloadsFromText :: Bool -> Text -> Text -> (Text, [StrictByteString.ByteString])
ssePayloadsFromText flush pending text =
  let buffered = pending <> text
      lines_ = Text.splitOn "\n" buffered
      completeLines =
        if flush then lines_ else dropLast lines_
      pendingLine =
        if flush then "" else lastOrEmpty lines_
  in (pendingLine, mapMaybe sseDataPayload completeLines)

sseDataPayload :: Text -> Maybe StrictByteString.ByteString
sseDataPayload rawLine = do
  payload <- Text.strip <$> Text.stripPrefix "data:" (Text.stripStart rawLine)
  guard (not (Text.null payload) && payload /= "[DONE]")
  pure (TextEncoding.encodeUtf8 payload)

ensureSuccessfulStreamingResponse :: IOE :> es => HTTP.Request -> HTTP.Response HTTP.BodyReader -> Stream (Of a) (Eff es) ()
ensureSuccessfulStreamingResponse request response = do
  let status = HTTP.responseStatus response
      code = HTTPStatus.statusCode status
  unless (200 <= code && code < 300) do
    chunks <- liftIO (HTTP.brConsume (HTTP.responseBody response))
    let preview = LazyByteString.toStrict (LazyByteString.fromChunks chunks)
    lift (throwIO (HTTP.HttpExceptionRequest request (HTTP.StatusCodeException (void response) preview)))

collectByteStream :: Monad m => Stream (Of StrictByteString.ByteString) m r -> m StrictByteString.ByteString
collectByteStream =
  go []
  where
    go chunks stream =
      S.next stream >>= \case
        Left _ ->
          pure (StrictByteString.concat (reverse chunks))
        Right (chunk, rest) ->
          go (chunk : chunks) rest

foldImageGenerationStreamWith
  :: IOE :> es
  => Text
  -> Stream (Of StrictByteString.ByteString) (Eff es) ()
  -> Eff es ImageGenerationResponse
foldImageGenerationStreamWith label =
  go Nothing
  where
    go latest stream =
      S.next stream >>= \case
        Left () ->
          case latest of
            Just response -> pure response
            Nothing -> throwIO (LLMException [i|#{label} streaming response was empty: no image output.|])
        Right (payload, rest) ->
          case imageGenerationResponseFromPayload payload of
            Left err ->
              throwIO (LLMException err)
            Right Nothing ->
              go latest rest
            Right (Just response) ->
              go (Just response) rest

imageGenerationResponseFromPayload :: StrictByteString.ByteString -> Either Text (Maybe ImageGenerationResponse)
imageGenerationResponseFromPayload payload =
  case Aeson.eitherDecodeStrict' payload of
    Left err ->
      Left [i|Malformed image stream chunk: #{Text.pack err}|]
    Right value ->
      imageGenerationResponseFromValue value

imageGenerationResponseFromValue :: Aeson.Value -> Either Text (Maybe ImageGenerationResponse)
imageGenerationResponseFromValue value
  | Just err <- streamPayloadError value =
      Left [i|OpenAI image streaming response error: #{err}|]
  | Just response <- AesonTypes.parseMaybe Aeson.parseJSON value
  , not (null response.data_) =
      Right (Just response)
  | Just event <- AesonTypes.parseMaybe Aeson.parseJSON value =
      imageGenerationResponseFromStreamEvent event
  | otherwise =
      Right Nothing

imageGenerationResponseFromStreamEvent :: ImageGenerationStreamEvent -> Either Text (Maybe ImageGenerationResponse)
imageGenerationResponseFromStreamEvent event
  | event.type_ == "image_generation.completed" || event.type_ == "image_edit.completed" =
      case event.b64Json of
        Just b64 ->
          Right (Just (ImageGenerationResponse [ImageGenerationData Nothing (Just b64)]))
        Nothing ->
          Left [i|#{eventType} event did not include b64_json.|]
  | otherwise =
      Right Nothing
  where
    eventType = event.type_

imageGenerationStreamTextFromPayloads :: ImageProviderConfig -> [Aeson.Value] -> Either Text Text
imageGenerationStreamTextFromPayloads cfg payloads =
  case foldlM collectResponse Nothing payloads of
    Left err ->
      Left err
    Right Nothing ->
      Left "Image generation streaming response was empty: no image output."
    Right (Just response) ->
      maybe (Left "Image generation response was empty: no image output.") Right (imageGenerationResponseText cfg response)
  where
    collectResponse latest value =
      case imageGenerationResponseFromValue value of
        Left err ->
          Left err
        Right Nothing ->
          Right latest
        Right (Just response) ->
          Right (Just response)

streamChatCompletion
  :: (IOE :> es, KatipE :> es)
  => Bool
  -> Text
  -> [Text]
  -> Text
  -> Int
  -> ChatCompletionRequest
  -> Stream (Of Text) (Eff es) ChatAnswer
streamChatCompletion emitContentDeltas baseUrl path apiKey timeoutMicros request =
  processPayloads (streamSseJsonPost baseUrl path apiKey timeoutMicros request) emptyStreamState
  where
    processPayloads payloads streamState = do
      lift (S.next payloads) >>= \case
        Left () -> do
          traverse_ S.yield (finishStreamOutputs emitContentDeltas streamState)
          pure (streamStateAnswer streamState)
        Right (payload, rest) -> do
          (next, outputs) <- lift (processPayload payload streamState)
          traverse_ S.yield outputs
          processPayloads rest next

    processPayload payload streamState =
      case Aeson.eitherDecodeStrict' payload of
        Left err -> do
          logWarning [i|Ignoring malformed LLM stream chunk: #{Text.pack err}|]
          pure (streamState, [])
        Right value ->
          case streamPayloadError value of
            Just err ->
              throwIO (LLMException [i|OpenAI streaming response error: #{err}|])
            Nothing ->
              case AesonTypes.parseEither Aeson.parseJSON value of
                Left err -> do
                  logWarning [i|Ignoring malformed LLM stream chunk: #{Text.pack err}|]
                  pure (streamState, [])
                Right chunk ->
                  pure (applyStreamChunk emitContentDeltas streamState chunk)

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x : xs) = x : dropLast xs

lastOrEmpty :: [Text] -> Text
lastOrEmpty [] = ""
lastOrEmpty xs = fromMaybe "" (viaNonEmpty last xs)

data StreamState = StreamState
  { contentAccumulator :: !TextBuilder.Builder
  , toolAccumulator :: !(Map Int PartialToolCall)
  , pendingContentOutputs :: ![Text]
  }
  deriving (Generic)

emptyStreamState :: StreamState
emptyStreamState =
  StreamState mempty Map.empty []

data PartialToolCall = PartialToolCall
  { partialId :: !(Maybe Text)
  , partialName :: !(Maybe Text)
  , partialArguments :: !TextBuilder.Builder
  }
  deriving (Generic)

emptyPartialToolCall :: PartialToolCall
emptyPartialToolCall = PartialToolCall Nothing Nothing mempty

streamStateAnswer :: StreamState -> ChatAnswer
streamStateAnswer streamState =
  chatAnswer
    (Text.strip (builderToStrictText streamState.contentAccumulator))
    (mapMaybe completePartialToolCall (Map.elems streamState.toolAccumulator))

completePartialToolCall :: PartialToolCall -> Maybe ToolCall
completePartialToolCall PartialToolCall{partialId, partialName, partialArguments} = do
  callId <- partialId
  functionName <- partialName
  pure ToolCall
    { id = callId
    , name = functionName
    , arguments = builderToStrictText partialArguments
    }

builderToStrictText :: TextBuilder.Builder -> Text
builderToStrictText =
  LazyText.toStrict . TextBuilder.toLazyText

applyStreamChunk :: Bool -> StreamState -> ChatCompletionStreamChunk -> (StreamState, [Text])
applyStreamChunk emitContentDeltas streamState chunk =
  foldl' applyStreamChoice (streamState, []) chunk.choices
  where
    applyStreamChoice (acc, outputs) StreamChoice{delta} =
      let contentDelta = fromMaybe "" delta.content
      in
      ( acc
          & #contentAccumulator %~ (<> TextBuilder.fromText contentDelta)
          & #toolAccumulator %~ (\toolAccumulator ->
              foldl' applyToolCallDelta toolAccumulator delta.toolCalls)
          & #pendingContentOutputs %~ appendPendingContent contentDelta
      , if emitContentDeltas && not (Text.null contentDelta)
          then outputs <> [contentDelta]
          else outputs
      )

appendPendingContent :: Text -> [Text] -> [Text]
appendPendingContent content chunks
  | Text.null content = chunks
  | otherwise = chunks <> [content]

finishStreamOutputs :: Bool -> StreamState -> [Text]
finishStreamOutputs emitContentDeltas streamState
  | emitContentDeltas = []
  | otherwise =
      case streamStateAnswer streamState of
        ChatFinalAnswer{} ->
          streamState.pendingContentOutputs
        ChatToolRequest{} ->
          []

applyToolCallDelta :: Map Int PartialToolCall -> ToolCallDelta -> Map Int PartialToolCall
applyToolCallDelta acc delta =
  Map.alter (Just . applyToPartial . fromMaybe emptyPartialToolCall) delta.index acc
  where
    applyToPartial partial =
      partial
        & #partialId %~ (delta.id <|>)
        & #partialName %~ ((delta.function >>= (.name)) <|>)
        & #partialArguments %~ (<> TextBuilder.fromText (fromMaybe "" (delta.function >>= (.arguments))))

data ChatCompletionStreamChunk = ChatCompletionStreamChunk
  { choices :: ![StreamChoice]
  }
  deriving (Show)

instance Aeson.FromJSON ChatCompletionStreamChunk where
  parseJSON = Aeson.withObject "ChatCompletionStreamChunk" $ \o ->
    ChatCompletionStreamChunk . fromMaybe [] <$> o Aeson..:? "choices"

streamPayloadError :: Aeson.Value -> Maybe Text
streamPayloadError = \case
  Aeson.Object obj ->
    KeyMap.lookup "error" obj >>= errorValueText
  _ ->
    Nothing
  where
    errorValueText = \case
      Aeson.String message ->
        Just message
      Aeson.Object obj ->
        let message = stringField "message" obj
            type_ = stringField "type" obj
            code = stringField "code" obj
        in case catMaybes [message, type_, code] of
          []    -> Just (TextEncoding.decodeUtf8 (LazyByteString.toStrict (Aeson.encode (Aeson.Object obj))))
          parts -> Just (Text.intercalate " " parts)
      value ->
        Just (TextEncoding.decodeUtf8 (LazyByteString.toStrict (Aeson.encode value)))

    stringField key obj =
      case KeyMap.lookup key obj of
        Just (Aeson.String text) -> Just text
        _                        -> Nothing

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

imagePromptMessages :: Bool -> [ChatMessage] -> [ChatMessage]
imagePromptMessages False messages = messages
imagePromptMessages True messages =
  systemText "The user is asking for an actual generated image. Generate image output; do not answer with ASCII art, SVG, markdown art, or only a textual description." : messages

-- | Convert decoded OpenAI-compatible SSE payloads into streamed assistant
-- content chunks and the completed assistant turn.
--
-- When 'emitContentDeltas' is false, text is yielded only after the full turn
-- is known to be a final answer. Tool-request content remains in the returned
-- 'ChatToolRequest' and is not yielded.
chatStreamTextFromPayloads :: Bool -> [Aeson.Value] -> Either Text ([Text], ChatAnswer)
chatStreamTextFromPayloads emitContentDeltas payloads = do
  chunks <- traverse parseChunk payloads
  let (finalState, outputs) =
        foldl'
          (\(streamState, collected) chunk ->
              let (nextState, chunkOutputs) = applyStreamChunk emitContentDeltas streamState chunk
              in (nextState, collected <> chunkOutputs)
          )
          (emptyStreamState, [])
          chunks
  pure (outputs <> finishStreamOutputs emitContentDeltas finalState, streamStateAnswer finalState)
  where
    parseChunk value =
      case AesonTypes.parseEither Aeson.parseJSON value of
        Left err -> Left (Text.pack err)
        Right chunk -> Right chunk
