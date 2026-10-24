module Bot.LLM.Responses
  ( askStreaming
  , requestPayload
  , httpRequest
  , streamFromPayloads
  ) where

import Bot.Prelude hiding (state)
import Control.Monad (foldM)
import qualified Bot.Effect.HTTP as HTTP
import Bot.LLM.Provider (ChatProviderConfig (..))
import Bot.LLM.ImageInput (parseDataImage)
import Bot.LLM.Stream
import Bot.LLM.Types
import Bot.Util.Time (secondsToMicroseconds)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Effectful.Resource as Resource
import qualified Network.HTTP.Client as Client
import qualified Streaming.Prelude as S

requestPayload :: ChatProviderConfig -> [FunctionTool] -> [ChatMessage] -> Either Text A.Value
requestPayload cfg tools messages = parse $ do
  input <- concat <$> traverse message (orderedResults messages)
  pure $ A.object $
    [ "model" A..= cfg.model
    , "input" A..= input
    , "stream" A..= True
    , "store" A..= False
    , "include" A..= [A.String "reasoning.encrypted_content"]
    , "max_output_tokens" A..= cfg.maxTokens
    ] <> ["tools" A..= map tool tools | not (null tools)]
      <> ["reasoning" A..= A.object ["effort" A..= cfg.reasoningEffort] | not (T.null cfg.reasoningEffort)]
  where
    tool FunctionTool{name, description, parameters} = A.object
      [ "type" A..= A.String "function", "name" A..= name
      , "description" A..= description, "parameters" A..= parameters
      , "strict" A..= False -- Preserve optional arguments in existing tool schemas.
      ]
    message msg = case msg.role of
      "assistant" -> case msg.providerState of
        Just opaque | opaque.provider == "responses" -> pure opaque.blocks
        _ -> do
          content <- textOnly msg.content
          calls <- traverse functionCall msg.toolCalls
          pure ([A.object ["role" A..= A.String "assistant", "content" A..= content] | not (T.null content)] <> calls)
      "tool" -> do
        callId <- maybe (fail "Responses tool result is missing tool_call_id") pure msg.toolCallId
        when (T.null callId) (fail "Empty Responses call_id")
        output <- textOnly msg.content
        pure [A.object ["type" A..= A.String "function_call_output", "call_id" A..= callId, "output" A..= output]]
      role | role == "system" || role == "developer" -> do
        content <- textOnly msg.content
        pure [A.object ["role" A..= role, "content" A..= content]]
      role | role == "user" || role == "synthetic" -> do
        content <- traverse inputPart (parts msg.content)
        pure [A.object ["role" A..= A.String "user", "content" A..= content]]
      _ -> fail "Unsupported Responses transcript role"
    functionCall call = do
      validateCall call
      pure $ A.object
        [ "type" A..= A.String "function_call", "call_id" A..= call.id
        , "name" A..= call.name, "arguments" A..= call.arguments
        ]

-- Keep parallel outputs in call order for gateways that pair same-name calls positionally.
orderedResults :: [ChatMessage] -> [ChatMessage]
orderedResults [] = []
orderedResults (msg : rest)
  | msg.role == "assistant" && not (null msg.toolCalls) =
      let (results, remaining) = span ((== "tool") . (.role)) rest
          positions = Map.fromList (zip (map (Just . (.id)) msg.toolCalls) [0 :: Int ..])
          position result = Map.findWithDefault maxBound result.toolCallId positions
      in msg : (sortOn position results <> orderedResults remaining)
  | otherwise = msg : orderedResults rest

parts :: Maybe MessageContent -> [ContentPart]
parts = \case
  Nothing -> []
  Just (TextContent t) -> [TextPart t | not (T.null t)]
  Just (PartsContent ps) -> ps

textOnly :: Maybe MessageContent -> A.Parser Text
textOnly content = T.intercalate "\n" <$> traverse
  (\case TextPart t -> pure t; _ -> fail "Responses system, assistant and tool messages must be text")
  (parts content)

inputPart :: ContentPart -> A.Parser A.Value
inputPart = \case
  TextPart t -> pure $ A.object ["type" A..= A.String "input_text", "text" A..= t]
  ImageUrlPart url -> do
    when ("data:" `T.isPrefixOf` url) (void (parseDataImage url))
    unless (any (`T.isPrefixOf` url) ["http://", "https://", "data:image/"])
      (fail "Responses images require an HTTP URL or image data URI")
    pure $ A.object ["type" A..= A.String "input_image", "image_url" A..= url, "detail" A..= A.String "auto"]

httpRequest :: ChatProviderConfig -> Text -> A.Value -> IO Client.Request
httpRequest cfg key payload = do
  base <- Client.parseRequest (T.unpack (T.dropWhileEnd (== '/') cfg.baseUrl <> "/responses"))
  pure base
    { Client.method = "POST"
    , Client.requestHeaders =
        [("Authorization", "Bearer " <> TE.encodeUtf8 key), ("Content-Type", "application/json"), ("Accept", "text/event-stream")]
    , Client.requestBody = Client.RequestBodyLBS (A.encode payload)
    , Client.responseTimeout = Client.responseTimeoutMicro (secondsToMicroseconds (toInteger cfg.requestTimeout))
    , Client.redirectCount = 0
    }

askStreaming :: (HTTP.HTTP :> es, IOE :> es, Resource.Resource :> es)
  => ChatProviderConfig -> [FunctionTool] -> [ChatMessage] -> Stream (Of Text) (Eff es) ChatAnswer
askStreaming cfg tools messages = do
  key <- lift $ maybe (throwIO (LLMException "Responses API key is not configured")) pure cfg.apiKey
  payload <- lift $ either (throwIO . LLMException) pure (requestPayload cfg tools messages)
  request <- liftIO (httpRequest cfg key payload)
  go emptyState (streamSsePayloads (streamHttpResponseBody request))
  where
    go state source = lift (S.next source) >>= \case
      Left () -> lift $ either (throwIO . LLMException) pure (finish state)
      Right (bytes, rest) -> do
        value <- lift $ either (throwIO . LLMException . T.pack) pure (A.eitherDecodeStrict' bytes)
        (next, output) <- lift $ either (throwIO . LLMException) pure (step state value)
        traverse_ S.yield output
        go next rest

data StreamState = StreamState
  { responseId :: !(Maybe Text)
  , emitted :: ![Text]
  , answer :: !(Maybe ChatAnswer)
  }

emptyState :: StreamState
emptyState = StreamState Nothing [] Nothing

parse :: A.Parser a -> Either Text a
parse parser = first T.pack (A.parseEither (const parser) ())

step :: StreamState -> A.Value -> Either Text (StreamState, [Text])
step state value = parse $ A.withObject "Responses stream event" (\o -> do
  when (isJust state.answer) (fail "Responses event after completion")
  kind <- o A..: "type" :: A.Parser Text
  case kind of
    "error" -> fail "Responses streaming error"
    "response.failed" -> fail "Responses generation failed"
    "response.incomplete" -> fail "Responses generation incomplete"
    "response.created" -> do
      when (isJust state.responseId) (fail "Duplicate Responses creation event")
      response <- o A..: "response"
      ident <- response A..: "id"
      when (T.null ident) (fail "Empty Responses response id")
      pure (state{responseId = Just ident}, [])
    "response.output_text.delta" -> delta o
    "response.refusal.delta" -> delta o
    "response.completed" -> do
      requireCreated
      response <- o A..: "response"
      ident <- response A..: "id"
      unless (Just ident == state.responseId) (fail "Responses response id mismatch")
      status <- response A..: "status" :: A.Parser Text
      unless (status == "completed") (fail "Responses response is not completed")
      err <- response A..:? "error" :: A.Parser (Maybe A.Value)
      incomplete <- response A..:? "incomplete_details" :: A.Parser (Maybe A.Value)
      unless (isNothing err && isNothing incomplete) (fail "Responses completed snapshot contains failure details")
      output <- response A..: "output"
      usage <- traverse parseUsage =<< response A..:? "usage"
      final <- completedAnswer output usage
      -- The completed snapshot is authoritative for tools and encrypted reasoning.
      let visible = chatAnswerContent final
          streamed = T.concat (reverse state.emitted)
      unless (streamed `T.isPrefixOf` visible) (fail "Responses text differs from streamed deltas")
      let remaining = T.drop (T.length streamed) visible
      pure (state{answer = Just final}, [remaining | not (T.null remaining)])
    _ -> pure (state, [])) value
  where
    requireCreated = when (isNothing state.responseId) (fail "Responses stream missing response.created")
    delta o = do
      requireCreated
      t <- o A..: "delta"
      pure (state{emitted = t : state.emitted}, [t | not (T.null t)])

validateCall :: ToolCall -> A.Parser ()
validateCall call = do
  when (T.null call.id || T.null call.name) (fail "Empty Responses function identity")
  args <- either fail pure (A.eitherDecodeStrict' (TE.encodeUtf8 call.arguments))
  void (A.withObject "Responses function arguments" pure args)

completedAnswer :: [A.Value] -> Maybe TokenUsage -> A.Parser ChatAnswer
completedAnswer output usage = do
  entries <- traverse (A.withObject "Responses output item" item) output
  let ids = map (\(ident, _, _) -> ident) entries
      texts = map (\(_, txt, _) -> txt) entries
      calls = mapMaybe (\(_, _, call) -> call) entries
  unless (length (ordNub ids) == length ids) (fail "Duplicate Responses output item ids")
  unless (length (ordNub (map (.id) calls)) == length calls) (fail "Duplicate Responses call_ids")
  pure $ withChatAnswerProviderState (ProviderState "responses" output) $
    withChatAnswerTokenUsage usage (chatAnswer (T.concat texts) calls)
  where
    item o = do
      ident <- o A..: "id"
      when (T.null ident) (fail "Empty Responses output item id")
      status <- o A..:? "status" :: A.Parser (Maybe Text)
      unless (isNothing status || status == Just "completed") (fail "Incomplete Responses output item")
      kind <- o A..: "type" :: A.Parser Text
      case kind of
        "reasoning" -> pure (ident, "", Nothing)
        "message" -> do
          role <- o A..: "role" :: A.Parser Text
          unless (role == "assistant") (fail "Unexpected Responses output role")
          content <- o A..: "content"
          text <- T.concat <$> traverse (A.withObject "Responses output part" $ \p -> do
            partType <- p A..: "type" :: A.Parser Text
            case partType of
              "output_text" -> p A..: "text"
              "refusal" -> p A..: "refusal"
              _ -> fail "Unsupported Responses output part") content
          pure (ident, text, Nothing)
        "function_call" -> do
          call <- ToolCall <$> o A..: "call_id" <*> o A..: "name" <*> o A..: "arguments"
          validateCall call
          pure (ident, "", Just call)
        _ -> fail "Unsupported Responses output item"

parseUsage :: A.Value -> A.Parser TokenUsage
parseUsage = A.withObject "Responses usage" $ \o -> do
  input <- o A..: "input_tokens"
  output <- o A..: "output_tokens"
  total <- o A..: "total_tokens"
  details <- o A..:? "input_tokens_details"
  cached <- maybe (pure Nothing) (A..:? "cached_tokens") details
  pure (TokenUsage input output total cached)

finish :: StreamState -> Either Text ChatAnswer
finish state = maybe (Left "Incomplete Responses stream: missing response.completed") Right state.answer

streamFromPayloads :: [A.Value] -> Either Text ([Text], ChatAnswer)
streamFromPayloads values = do
  (state, outputs) <- foldM (\(state, outputs) value -> do
    (next, output) <- step state value
    pure (next, outputs <> output)) (emptyState, []) values
  final <- finish state
  pure (outputs, final)
