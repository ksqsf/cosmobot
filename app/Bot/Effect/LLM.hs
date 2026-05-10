{-
Module      : Bot.Effect.LLM
Description : Query LLM
Stability   : experimental
-}
module Bot.Effect.LLM where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Network.HTTP.Req

data Config = Config
  { endpoint :: !Text
  , apiKey   :: !(Maybe Text)
  , model    :: !Text
  , webSearch :: !Bool
  , webSearchMaxResults :: !(Maybe Int)
  , webFetch :: !Bool
  , webFetchMaxUses :: !(Maybe Int)
  , webFetchMaxContentTokens :: !(Maybe Int)
  , datetime :: !Bool
  , imageGeneration :: !Bool
  , imageGenerationModel :: !(Maybe Text)
  , imageGenerationQuality :: !(Maybe Text)
  , imageGenerationSize :: !(Maybe Text)
  , imageGenerationAspectRatio :: !(Maybe Text)
  , imageGenerationBackground :: !(Maybe Text)
  , imageGenerationOutputFormat :: !(Maybe Text)
  , imageGenerationModeration :: !(Maybe Text)
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig = Config
  { endpoint = "openrouter.ai"
  , apiKey   = Nothing
  , model    = "openai/gpt-4o-mini"
  , webSearch = False
  , webSearchMaxResults = Nothing
  , webFetch = False
  , webFetchMaxUses = Nothing
  , webFetchMaxContentTokens = Nothing
  , datetime = False
  , imageGeneration = False
  , imageGenerationModel = Nothing
  , imageGenerationQuality = Nothing
  , imageGenerationSize = Nothing
  , imageGenerationAspectRatio = Nothing
  , imageGenerationBackground = Nothing
  , imageGenerationOutputFormat = Nothing
  , imageGenerationModeration = Nothing
  }

data LLM :: Effect where
  Ask :: [ChatMessage] -> LLM m Text
  AskImage :: [ChatMessage] -> LLM m Text
  AskTools :: [FunctionTool] -> [ChatMessage] -> LLM m ChatAnswer

type instance DispatchOf LLM = Dynamic

ask :: LLM :> es => Text -> Eff es Text
ask prompt = askWithHistory [userText prompt]

askWithHistory :: LLM :> es => [ChatMessage] -> Eff es Text
askWithHistory = send . Ask

askImageWithHistory :: LLM :> es => [ChatMessage] -> Eff es Text
askImageWithHistory = send . AskImage

askWithTools :: LLM :> es => [FunctionTool] -> [ChatMessage] -> Eff es ChatAnswer
askWithTools tools messages =
  send (AskTools tools messages)

runLLM
  :: IOE :> es
  => Log :> es
  => Config
  -> Eff (LLM : es) a
  -> Eff es a
runLLM cfg = interpret $ \_ -> \case
  Ask messages -> askOpenAI False cfg messages
  AskImage messages -> askOpenAI True cfg messages
  AskTools tools messages -> askOpenAIWithTools cfg tools messages

askOpenAI :: (IOE :> es, Log :> es) => Bool -> Config -> [ChatMessage] -> Eff es Text
askOpenAI _ Config{apiKey = Nothing} _ =
  pure "LLM is not configured: set llm.api_key."
askOpenAI forceImage cfg@Config{endpoint, apiKey = Just key, model} messages
  | forceImage && not cfg.imageGeneration =
      pure "Image generation is not configured: set llm.image_generation = true."
  | otherwise = do
  let imageRequest = forceImage
      requestModel = if imageRequest then fromMaybe model cfg.imageGenerationModel else model
      request = ChatCompletionRequest
        { model = requestModel
        , messages = imagePromptMessages imageRequest messages
        , tools = if imageRequest then Nothing else toolSpecs (serverTools cfg) []
        , modalities = if imageRequest then Just ["image", "text"] else Nothing
        , imageConfig = if imageRequest then imageGenerationConfig cfg else Nothing
        }
  logInfo "LLM request" (llmRequestLogLine endpoint request)
  response <- liftIO $ runReq defaultHttpConfig $
    req POST
      (chatCompletionsUrl endpoint)
      (ReqBodyJson request)
      jsonResponse
      (header "Authorization" (ByteString.pack [i|Bearer #{key}|]))
  let body = responseBody response
  logInfo "LLM response" (llmResponseLogLine endpoint requestModel body)
  case chatCompletionText body of
    Just answer -> pure answer
    Nothing     -> throwIO (LLMException [i|OpenAI response had no text choices: #{show body :: String}|])

askOpenAIWithTools :: (IOE :> es, Log :> es) => Config -> [FunctionTool] -> [ChatMessage] -> Eff es ChatAnswer
askOpenAIWithTools Config{apiKey = Nothing} _ _ =
  pure (ChatAnswer "LLM is not configured: set llm.api_key." [])
askOpenAIWithTools cfg@Config{endpoint, apiKey = Just key, model} functionTools messages = do
  let request = ChatCompletionRequest
        { model = model
        , messages = messages
        , tools = toolSpecs (serverTools cfg) functionTools
        , modalities = Nothing
        , imageConfig = Nothing
        }
  logInfo "LLM request" (llmRequestLogLine endpoint request)
  response <- liftIO $ runReq defaultHttpConfig $
    req POST
      (chatCompletionsUrl endpoint)
      (ReqBodyJson request)
      jsonResponse
      (header "Authorization" (ByteString.pack [i|Bearer #{key}|]))
  let body = responseBody response
  logInfo "LLM response" (llmResponseLogLine endpoint model body)
  pure (chatCompletionAnswer body)

chatCompletionsUrl :: Text -> Url 'Https
chatCompletionsUrl endpoint =
  case endpoint of
    "openrouter"       -> openRouterUrl
    "openrouter.ai"    -> openRouterUrl
    "openai"           -> openAIUrl
    "api.openai.com"   -> openAIUrl
    host               -> https host /: "v1" /: "chat" /: "completions"
  where
    openRouterUrl =
      https "openrouter.ai" /: "api" /: "v1" /: "chat" /: "completions"
    openAIUrl =
      https "api.openai.com" /: "v1" /: "chat" /: "completions"

newtype LLMException = LLMException Text
  deriving (Show)
instance Exception LLMException

data ChatCompletionRequest = ChatCompletionRequest
  { model       :: !Text
  , messages    :: ![ChatMessage]
  , tools       :: !(Maybe [ToolSpec])
  , modalities  :: !(Maybe [Text])
  , imageConfig :: !(Maybe Aeson.Value)
  }
  deriving (Show, Generic)

instance Aeson.ToJSON ChatCompletionRequest where
  toJSON ChatCompletionRequest{model, messages, tools, modalities, imageConfig} =
    Aeson.object $
      [ "model" Aeson..= model
      , "messages" Aeson..= messages
      ]
      <> maybe [] (\value -> ["tools" Aeson..= value]) tools
      <> maybe [] (\value -> ["modalities" Aeson..= value]) modalities
      <> maybe [] (\value -> ["image_config" Aeson..= value]) imageConfig

data ToolSpec
  = ServerToolSpec !ServerTool
  | FunctionToolSpec !FunctionTool
  deriving (Show)

instance Aeson.ToJSON ToolSpec where
  toJSON = \case
    ServerToolSpec tool -> Aeson.toJSON tool
    FunctionToolSpec tool -> Aeson.toJSON tool

data ServerTool
  = WebSearchTool
      { maxResults :: !(Maybe Int)
      }
  | WebFetchTool
      { maxUses :: !(Maybe Int)
      , maxContentTokens :: !(Maybe Int)
      }
  | DatetimeTool
  deriving (Show)

instance Aeson.ToJSON ServerTool where
  toJSON WebSearchTool{maxResults} =
    Aeson.object $
      [ "type" Aeson..= Aeson.String "openrouter:web_search"
      ]
      <> maybe [] (\value ->
        [ "parameters" Aeson..= Aeson.object
            [ "max_results" Aeson..= value
            ]
        ]) maxResults
  toJSON WebFetchTool{maxUses, maxContentTokens} =
    let parameters =
          maybe [] (\value -> ["max_uses" Aeson..= value]) maxUses
            <> maybe [] (\value -> ["max_content_tokens" Aeson..= value]) maxContentTokens
    in Aeson.object $
        [ "type" Aeson..= Aeson.String "openrouter:web_fetch"
        ]
        <> if null parameters
          then []
          else ["parameters" Aeson..= Aeson.object parameters]
  toJSON DatetimeTool =
    Aeson.object
      [ "type" Aeson..= Aeson.String "openrouter:datetime"
      ]

serverTools :: Config -> [ServerTool]
serverTools Config{webSearch, webSearchMaxResults, webFetch, webFetchMaxUses, webFetchMaxContentTokens, datetime} =
  catMaybes
    [ if webSearch then Just (WebSearchTool webSearchMaxResults) else Nothing
    , if webFetch then Just (WebFetchTool webFetchMaxUses webFetchMaxContentTokens) else Nothing
    , if datetime then Just DatetimeTool else Nothing
    ]

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

toolSpecs :: [ServerTool] -> [FunctionTool] -> Maybe [ToolSpec]
toolSpecs server function =
  case map ServerToolSpec server <> map FunctionToolSpec function of
    [] -> Nothing
    specs -> Just specs

imageGenerationConfig :: Config -> Maybe Aeson.Value
imageGenerationConfig Config{imageGenerationQuality, imageGenerationSize, imageGenerationAspectRatio, imageGenerationBackground, imageGenerationOutputFormat, imageGenerationModeration} =
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
        <> maybe [] (\value -> ["moderation" Aeson..= value]) imageGenerationModeration

data LLMRequestLog = LLMRequestLog
  { endpoint :: !Text
  , request  :: !ChatCompletionRequest
  }
  deriving (Show, Generic, Aeson.ToJSON)

data LLMResponseLog = LLMResponseLog
  { endpoint    :: !Text
  , model       :: !Text
  , usage       :: !(Maybe Aeson.Value)
  , annotations :: ![Aeson.Value]
  , images      :: ![Aeson.Value]
  , toolCalls   :: ![ToolCall]
  }
  deriving (Show, Generic, Aeson.ToJSON)

llmResponseLog :: Text -> Text -> ChatCompletionResponse -> LLMResponseLog
llmResponseLog endpoint model response =
  LLMResponseLog
    { endpoint = endpoint
    , model = model
    , usage = response.usage
    , annotations = foldMap (fromMaybe [] . (.message.annotations)) response.choices
    , images = foldMap (fromMaybe [] . (.message.images)) response.choices
    , toolCalls = foldMap (.message.toolCalls) response.choices
    }

llmRequestLogLine :: Text -> ChatCompletionRequest -> Text
llmRequestLogLine endpoint request =
  Text.unwords
    [ "endpoint=" <> endpoint
    , "model=" <> request.model
    , "messages=" <> show (length request.messages)
    , "tools=" <> maybe "0" (show . length) request.tools
    , "modalities=" <> maybe "-" (Text.intercalate ",") request.modalities
    , "image_config=" <> show (isJust request.imageConfig)
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

userText :: Text -> ChatMessage
userText prompt =
  ChatMessage "user" (Just (TextContent prompt)) [] Nothing

systemText :: Text -> ChatMessage
systemText prompt =
  ChatMessage "system" (Just (TextContent prompt)) [] Nothing

userWithImages :: Text -> [Text] -> ChatMessage
userWithImages prompt [] =
  userText prompt
userWithImages prompt urls =
  ChatMessage "user" (Just (PartsContent (TextPart prompt : map ImageUrlPart urls))) [] Nothing

assistantText :: Text -> ChatMessage
assistantText answer =
  ChatMessage "assistant" (Just (TextContent answer)) [] Nothing

assistantAnswer :: ChatAnswer -> ChatMessage
assistantAnswer ChatAnswer{content, toolCalls} =
  ChatMessage "assistant" messageContent toolCalls Nothing
  where
    messageContent
      | Text.null content = Nothing
      | otherwise         = Just (TextContent content)

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

data ChatAnswer = ChatAnswer
  { content   :: !Text
  , toolCalls :: ![ToolCall]
  }
  deriving (Show, Generic, Aeson.ToJSON)

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
    urls -> Just (Text.unlines (map ("[image] " <>) urls))

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
