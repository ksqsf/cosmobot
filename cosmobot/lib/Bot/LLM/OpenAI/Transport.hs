{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.LLM.OpenAI.Transport
Description : OpenAI-compatible LLM transport
Stability   : experimental
-}
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
  , streamImageGenerationImageBytes
  , audioSpeechRequestPayload
  )
where

import Bot.Prelude
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.HTTP as HTTP
import qualified Bot.Effect.LLM as LLM
import Bot.LLM.OpenAI.Config
import Bot.LLM.Types
import Bot.Util.Aeson
import qualified Bot.Util.Image as Image
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Util.Stream as StreamUtil
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import qualified Network.HTTP.Types.Status as HTTPStatus
import Network.HTTP.Req
import Optics ((%~))
import qualified Streaming as Streaming
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as S
import System.IO.Error (ioError, userError)
import qualified Effectful.Timeout as Timeout
import qualified Text.URI as URI
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO as FileSystemIO
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

llmHttpConfig :: HttpConfig
llmHttpConfig =
  defaultHttpConfig
    { httpConfigRetryJudge = \_ _ -> False
    , httpConfigRetryJudgeException = \_ _ -> False
    }

runTimedLLMReq :: (Fail :> es, Timeout.Timeout :> es, HTTP.HTTP :> es) => Text -> Int -> Req a -> Eff es a
runTimedLLMReq label timeoutSeconds action = do
  result <- Timeout.timeout (secondsToMicros timeoutSeconds) $
    HTTP.runReqWithConfig llmHttpConfig action
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

askImageOpenAIStreaming :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Timeout.Timeout :> es, Fail :> es, FileSystem :> es, Process :> es) => Config -> LLM.ImageRequestOptions -> [ChatMessage] -> Stream (Of Text) (Eff es) Text
askImageOpenAIStreaming Config{imageProvider = Nothing} _ _ =
  yieldTextResult (pure imageNotConfiguredMessage)
askImageOpenAIStreaming Config{imageProvider = Just provider} options messages
  | provider.canGenerate =
      askImageGenerationsOpenAIStreaming provider options messages
  | otherwise =
      askImageChatCompletionsOpenAIStreaming provider options messages

askImageEditOpenAIStreaming :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Timeout.Timeout :> es, Fail :> es, FileSystem :> es, Process :> es, Fail :> es) => Config -> LLM.ImageRequestOptions -> Text -> [Text] -> Maybe Text -> Stream (Of Text) (Eff es) Text
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
              S.yield answer
              pure answer
            Nothing ->
              lift (throwIO (LLMException "Image edit response was empty: no image output."))

askAudioOpenAIStreaming :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Timeout.Timeout :> es, Fail :> es, FileSystem :> es) => Config -> LLM.AudioRequestOptions -> [ChatMessage] -> Stream (Of Text) (Eff es) Text
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
  ref <- lift $
    runTimedEff "LLM audio streaming request" requestTimeout $
      writeGeneratedAudioStream request.responseFormat $
        Q.fromChunks $
          streamRawJsonPost requestBaseUrl requestPath key (secondsToMicros requestTimeout) "application/octet-stream" request
  lift $ logInfo ("LLM audio response: " <> audioResponseLogLine requestEndpoint request.model request.responseFormat ref)
  S.yield ref
  pure ref

yieldTextResult :: Monad m => m Text -> Stream (Of Text) m Text
yieldTextResult action = do
  answer <- lift action
  unless (Text.null (Text.strip answer)) (S.yield answer)
  pure answer

askImageChatCompletionsOpenAIStreaming :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Timeout.Timeout :> es, Fail :> es, FileSystem :> es, Process :> es, Fail :> es) => ImageProviderConfig -> LLM.ImageRequestOptions -> [ChatMessage] -> Stream (Of Text) (Eff es) Text
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
            S.yield text
            pure text

askImageGenerationsOpenAIStreaming :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Timeout.Timeout :> es, Fail :> es, FileSystem :> es, Process :> es, Fail :> es) => ImageProviderConfig -> LLM.ImageRequestOptions -> [ChatMessage] -> Stream (Of Text) (Eff es) Text
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
            S.yield answer
            pure answer
          Nothing ->
            lift (throwIO (LLMException "Image generation response was empty: no image output."))

askOpenAIStreaming
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Timeout.Timeout :> es, Fail :> es)
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
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Timeout.Timeout :> es, Fail :> es)
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
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing ChatCompletionRequest)

data ImageGenerationRequest = ImageGenerationRequest
  { model :: !Text
  , prompt :: !Text
  , quality :: !(Maybe Text)
  , size :: !(Maybe Text)
  , background :: !(Maybe Text)
  , moderation :: !(Maybe Text)
  , stream :: !(Maybe Bool)
  , partialImages :: !(Maybe Int)
  }
  deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing ImageGenerationRequest)

data AudioSpeechRequest = AudioSpeechRequest
  { model :: !Text
  , input :: !Text
  , voice :: !Text
  , responseFormat :: !Text
  , speed :: !(Maybe Double)
  , instructions :: !(Maybe Text)
  }
  deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing AudioSpeechRequest)

data ImageGenerationResponse = ImageGenerationResponse
  { data_ :: ![ImageGenerationData]
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON ImageGenerationResponse)

data ImageGenerationData = ImageGenerationData
  { url :: !(Maybe Text)
  , b64Json :: !(Maybe Text)
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON ImageGenerationData)

data ImageGenerationStreamEvent = ImageGenerationStreamEvent
  { type_ :: !Text
  , b64Json :: !(Maybe Text)
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON ImageGenerationStreamEvent)

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
        <> maybe [] (\value -> ["moderation" Aeson..= value]) resolved.moderation

imageGenerationRequest :: ImageProviderConfig -> LLM.ImageRequestOptions -> Text -> Text -> Maybe Bool -> ImageGenerationRequest
imageGenerationRequest provider options model prompt stream =
  ImageGenerationRequest
    { model = model
    , prompt = prompt
    , quality = resolved.quality
    , size = resolved.size
    , background = resolved.background
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

writeGeneratedAudioStream :: (IOE :> es, FileSystem :> es) => Text -> Q.ByteStream (Eff es) () -> Eff es Text
writeGeneratedAudioStream format bytes = do
  dir <- FileSystem.getTemporaryDirectory
  nonce <- liftIO getMonotonicTimeNSec
  let path = dir </> ("cosmobot-audio-" <> show nonce <.> Text.unpack (safeAudioExtension format))
  FileSystemIO.withBinaryFile path FileSystemIO.WriteMode \audioHandle ->
    S.mapM_ (FileSystemByteString.hPut audioHandle) (Q.toChunks bytes)
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

imageUploadFromReference :: (Timeout.Timeout :> es, HTTP.HTTP :> es, IOE :> es, Fail :> es) => Int -> Int -> Text -> Eff es ImageUpload
imageUploadFromReference timeoutSeconds index imageRef = do
  bytes <- imageBytesFromReference timeoutSeconds imageRef
  pure ImageUpload
    { filename = "image-" <> show index <> "." <> imageExtension bytes
    , bytes = bytes
    }

imageBytesFromReference :: (Timeout.Timeout :> es, HTTP.HTTP :> es, IOE :> es, Fail :> es) => Int -> Text -> Eff es StrictByteString.ByteString
imageBytesFromReference timeoutSeconds imageRef
  | Just bytes <- Image.decodeDataImageReference stripped =
      pure bytes
  | Just path <- Text.stripPrefix "file://" stripped =
      liftIO $ StrictByteString.readFile (Text.unpack path)
  | otherwise =
      downloadImageReference timeoutSeconds stripped
  where
    stripped = Text.strip imageRef

downloadImageReference :: (Timeout.Timeout :> es, HTTP.HTTP :> es, IOE :> es, Fail :> es) => Int -> Text -> Eff es StrictByteString.ByteString
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
        <> maybeTextPart "moderation" resolved.moderation

imageUploadPart :: Text -> ImageUpload -> Multipart.Part
imageUploadPart fieldName upload =
  Multipart.partFileRequestBody fieldName upload.filename (Client.RequestBodyBS upload.bytes)

multipartTextPart :: Text -> Text -> Multipart.Part
multipartTextPart name value =
  Multipart.partBS name (TextEncoding.encodeUtf8 value)

maybeTextPart :: Text -> Maybe Text -> [Multipart.Part]
maybeTextPart name =
  maybe [] \value -> [multipartTextPart name value]

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
imageGenerationDataRef _cfg image =
  image.url <|> (dataImageRef <$> image.b64Json)

dataImageRef :: Text -> Text
dataImageRef b64 =
  "data:image/png;base64," <> b64

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

audioResponseLogLine :: Text -> Text -> Text -> Text -> Text
audioResponseLogLine endpoint model responseFormat ref =
  Text.unwords
    [ "endpoint=" <> endpoint
    , "model=" <> model
    , "response_format=" <> responseFormat
    , "ref=" <> ref
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
  logDebug ("LLM request messages: " <> logJsonText request.messages)

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
    previewText 500 (logJsonText parts)

previewText :: Int -> Text -> Text
previewText maxChars text =
  let oneLine = Text.unwords (Text.words text)
  in if Text.length oneLine > maxChars
    then Text.take maxChars oneLine <> "..."
    else oneLine

llmStreamResponseLogLine :: Text -> Text -> ChatAnswer -> Text
llmStreamResponseLogLine endpoint model answer =
  Text.unwords
    [ "endpoint=" <> endpoint
    , "model=" <> model
    , "content_chars=" <> show (Text.length (chatAnswerContent answer))
    , "tool_calls=" <> show (length (chatAnswerToolCalls answer))
    ]

streamSseJsonPost
  :: (Aeson.ToJSON body, HTTP.HTTP :> es, IOE :> es)
  => Text
  -> [Text]
  -> Text
  -> Int
  -> body
  -> Stream (Of StrictByteString.ByteString) (Eff es) ()
streamSseJsonPost baseUrl path apiKey timeoutMicros request = do
  httpRequest <- liftIO (sseJsonPostRequest baseUrl path apiKey timeoutMicros request)
  streamSsePayloads (streamHttpResponseBody httpRequest)

streamImageGenerationImageBytes
  :: (Aeson.ToJSON body, HTTP.HTTP :> es, IOE :> es)
  => Text
  -> [Text]
  -> Text
  -> Int
  -> body
  -> Q.ByteStream (Eff es) ()
streamImageGenerationImageBytes baseUrl path apiKey timeoutMicros request =
  Q.fromChunks do
    httpRequest <- liftIO (sseJsonPostRequest baseUrl path apiKey timeoutMicros request)
    base64DecodedChunks (b64JsonBase64Chunks (streamHttpResponseBody httpRequest))

streamRawJsonPost
  :: (Aeson.ToJSON body, HTTP.HTTP :> es, IOE :> es)
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
  :: (HTTP.HTTP :> es, IOE :> es)
  => Text
  -> [Text]
  -> Text
  -> Int
  -> [Multipart.Part]
  -> Stream (Of StrictByteString.ByteString) (Eff es) ()
streamSseMultipartPost baseUrl path apiKey timeoutMicros parts = do
  httpRequest <- liftIO (sseMultipartPostRequest baseUrl path apiKey timeoutMicros parts)
  streamSsePayloads (streamHttpResponseBody httpRequest)

sseJsonPostRequest :: Aeson.ToJSON body => Text -> [Text] -> Text -> Int -> body -> IO Client.Request
sseJsonPostRequest baseUrl path apiKey timeoutMicros request = do
  httpRequest <- HTTP.streamingJsonPostRequest baseUrl path apiKey timeoutMicros request
  pure httpRequest
    { Client.requestHeaders = ("Accept", "text/event-stream") : Client.requestHeaders httpRequest
    }

rawJsonPostRequest :: Aeson.ToJSON body => Text -> [Text] -> Text -> Int -> ByteString.ByteString -> body -> IO Client.Request
rawJsonPostRequest baseUrl path apiKey timeoutMicros accept request = do
  httpRequest <- HTTP.streamingJsonPostRequest baseUrl path apiKey timeoutMicros request
  pure httpRequest
    { Client.requestHeaders = ("Accept", accept) : Client.requestHeaders httpRequest
    }

sseMultipartPostRequest :: Text -> [Text] -> Text -> Int -> [Multipart.Part] -> IO Client.Request
sseMultipartPostRequest baseUrl path apiKey timeoutMicros parts = do
  base <- Client.parseRequest (Text.unpack (endpointText baseUrl path))
  Multipart.formDataBody parts base
    { Client.method = "POST"
    , Client.requestHeaders =
        [ ("Authorization", ByteString.pack [i|Bearer #{apiKey}|])
        , ("Accept", "text/event-stream")
        ]
    , Client.responseTimeout = Client.responseTimeoutMicro timeoutMicros
    }

streamHttpResponseBody :: (HTTP.HTTP :> es, IOE :> es) => Client.Request -> Stream (Of StrictByteString.ByteString) (Eff es) ()
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

ensureSuccessfulStreamingResponse :: IOE :> es => Client.Request -> Client.Response Client.BodyReader -> Stream (Of a) (Eff es) ()
ensureSuccessfulStreamingResponse request response = do
  let status = Client.responseStatus response
      code = HTTPStatus.statusCode status
  unless (200 <= code && code < 300) do
    chunks <- liftIO (Client.brConsume (Client.responseBody response))
    let preview = LazyByteString.toStrict (LazyByteString.fromChunks chunks)
    lift (throwIO (Client.HttpExceptionRequest request (Client.StatusCodeException (void response) preview)))

-- Raw Image Byte Streaming

b64JsonBase64Chunks
  :: IOE :> es
  => Stream (Of StrictByteString.ByteString) (Eff es) r
  -> Stream (Of StrictByteString.ByteString) (Eff es) ()
b64JsonBase64Chunks input =
  findB64JsonKey (Q.split quote (Q.fromChunks input))

findB64JsonKey
  :: IOE :> es
  => Stream (Q.ByteStream (Eff es)) (Eff es) r
  -> Stream (Of StrictByteString.ByteString) (Eff es) ()
findB64JsonKey segments =
  lift (Streaming.inspect segments) >>= \case
    Left _ ->
      lift (throwIO (LLMException "Image generation response did not include b64_json."))
    Right segment -> do
      keyCandidate S.:> rest <- lift (Q.toStrict segment)
      if keyCandidate == "b64_json"
        then b64JsonValueChunks rest
        else findB64JsonKey rest

b64JsonValueChunks
  :: IOE :> es
  => Stream (Q.ByteStream (Eff es)) (Eff es) r
  -> Stream (Of StrictByteString.ByteString) (Eff es) ()
b64JsonValueChunks segments =
  lift (Streaming.inspect segments) >>= \case
    Left _ ->
      lift (throwIO (LLMException "Image generation b64_json value was missing."))
    Right afterKeySegment -> do
      _afterKey S.:> afterKey <- lift (Q.toStrict afterKeySegment)
      lift (Streaming.inspect afterKey) >>= \case
        Left _ ->
          lift (throwIO (LLMException "Image generation b64_json value was missing."))
        Right valueSegment -> do
          _rest <- Q.toChunks valueSegment
          pure ()

base64DecodedChunks
  :: IOE :> es
  => Stream (Of StrictByteString.ByteString) (Eff es) r
  -> Stream (Of StrictByteString.ByteString) (Eff es) ()
base64DecodedChunks =
  go StrictByteString.empty
  where
    go pending input =
      lift (S.next input) >>= \case
        Left _ ->
          unless (StrictByteString.null pending) (decodeAndYield pending)
        Right (chunk, rest) -> do
          let clean = StrictByteString.filter (not . isJsonWhitespace) chunk
              joined = pending <> clean
              decodeLength = (StrictByteString.length joined `div` 4) * 4
              (ready, nextPending) = StrictByteString.splitAt decodeLength joined
          decodeAndYield ready
          go nextPending rest

decodeAndYield :: IOE :> es => StrictByteString.ByteString -> Stream (Of StrictByteString.ByteString) (Eff es) ()
decodeAndYield bytes
  | StrictByteString.null bytes =
      pure ()
  | otherwise =
      case Base64.decode bytes of
        Left err ->
          lift (throwIO (LLMException [i|Invalid b64_json image data: #{Text.pack err}|]))
        Right decoded ->
          yieldNonEmptyBytes decoded

yieldNonEmptyBytes :: Monad m => StrictByteString.ByteString -> Stream (Of StrictByteString.ByteString) m ()
yieldNonEmptyBytes bytes =
  unless (StrictByteString.null bytes) (S.yield bytes)

quote :: Word8
quote = 34

isJsonWhitespace :: Word8 -> Bool
isJsonWhitespace byte =
  byte == 32 || byte == 10 || byte == 13 || byte == 9

-- Image Stream Response Parsing

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
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
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
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON StreamChoice)

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
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON ToolCallDelta)

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
