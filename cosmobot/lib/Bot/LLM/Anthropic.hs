module Bot.LLM.Anthropic
  ( askStreaming
  , requestPayload
  , httpRequest
  , streamFromPayloads
  ) where

import Bot.Prelude hiding (state)
import Control.Monad (foldM)
import qualified Bot.Effect.HTTP as HTTP
import Bot.LLM.Config (ChatProviderConfig (..))
import Bot.LLM.ImageInput (parseDataImage)
import Bot.LLM.Stream
import Bot.LLM.Types
import Bot.Util.Time (secondsToMicroseconds)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Effectful.Resource as Resource
import qualified Network.HTTP.Client as Client
import qualified Streaming.Prelude as S

requestPayload :: ChatProviderConfig -> [FunctionTool] -> [ChatMessage] -> Either Text A.Value
requestPayload cfg tools messages = parse $ do
  system <- concat <$> traverse systemParts (filter ((`elem` ["system", "developer"]) . (.role)) messages)
  turns <- traverse message (filter ((`notElem` ["system", "developer"]) . (.role)) messages)
  pure $ A.object $
    [ "model" A..= cfg.model
    , "max_tokens" A..= cfg.maxTokens
    , "stream" A..= True
    , "messages" A..= mergeTurns turns
    ] <> ["system" A..= system | not (null system)]
      <> ["tools" A..= map tool tools | not (null tools)]
      <> ["thinking" A..= A.object ["type" A..= mode] | mode <- maybeToList cfg.thinking]
      <> ["output_config" A..= A.object ["effort" A..= cfg.reasoningEffort] | not (T.null cfg.reasoningEffort)]
  where
    systemParts msg = traverse (\case TextPart t -> pure (textBlock t); _ -> fail "Anthropic system prompts must be text") (parts msg.content)
    message msg = case msg.role of
      "assistant" -> do
        blocks <- case msg.providerState of
          Just state | state.provider == "anthropic" -> pure state.blocks
          _ -> (<>) <$> traverse contentBlock (parts msg.content) <*> traverse toolCall msg.toolCalls
        pure ("assistant", blocks)
      "user" -> ("user",) <$> traverse contentBlock (parts msg.content)
      "synthetic" -> ("user",) <$> traverse contentBlock (parts msg.content)
      "tool" -> do
        callId <- maybe (fail "Anthropic tool result is missing tool_call_id") pure msg.toolCallId
        content <- traverse contentBlock (parts msg.content)
        pure ("user", [A.object ["type" A..= A.String "tool_result", "tool_use_id" A..= callId, "content" A..= content]])
      _ -> fail "Unsupported Anthropic transcript role"
    tool FunctionTool{name, description, parameters} = A.object
      ["name" A..= name, "description" A..= description, "input_schema" A..= parameters]
    toolCall call = do
      input <- either fail pure (A.eitherDecodeStrict' (TE.encodeUtf8 call.arguments))
      _ <- A.withObject "tool arguments" pure input
      pure $ A.object ["type" A..= A.String "tool_use", "id" A..= call.id, "name" A..= call.name, "input" A..= input]

parts :: Maybe MessageContent -> [ContentPart]
parts = \case
  Nothing -> []
  Just (TextContent t) -> [TextPart t | not (T.null t)]
  Just (PartsContent ps) -> ps

textBlock :: Text -> A.Value
textBlock t = A.object ["type" A..= A.String "text", "text" A..= t]

contentBlock :: ContentPart -> A.Parser A.Value
contentBlock = \case
  TextPart t -> pure (textBlock t)
  ImageUrlPart url -> do
    source <- case T.stripPrefix "data:" url of
      Just _ -> do
        (mime, encoded) <- parseDataImage url
        pure $ A.object ["type" A..= A.String "base64", "media_type" A..= mime, "data" A..= encoded]
      Nothing | "https://" `T.isPrefixOf` url || "http://" `T.isPrefixOf` url ->
        pure $ A.object ["type" A..= A.String "url", "url" A..= url]
      _ -> fail "Anthropic images require an HTTP URL or base64 data URI"
    pure $ A.object ["type" A..= A.String "image", "source" A..= source]

-- Adjacent tool results must share a user turn, before any following user text.
mergeTurns :: [(Text, [A.Value])] -> [A.Value]
mergeTurns [] = []
mergeTurns ((role, content) : rest) = go content rest
  where
    go acc ((nextRole, nextContent) : more) | nextRole == role = go (acc <> nextContent) more
    go acc more = A.object ["role" A..= role, "content" A..= acc] : mergeTurns more

httpRequest :: ChatProviderConfig -> Text -> A.Value -> IO Client.Request
httpRequest cfg key payload = do
  base <- Client.parseRequest (T.unpack (T.dropWhileEnd (== '/') cfg.baseUrl <> "/messages"))
  let authHeader = if cfg.anthropicAuth == "bearer"
        then ("Authorization", "Bearer " <> TE.encodeUtf8 key)
        else ("x-api-key", TE.encodeUtf8 key)
  pure base
    { Client.method = "POST"
    , Client.requestHeaders =
        [authHeader, ("anthropic-version", "2023-06-01"), ("Content-Type", "application/json"), ("Accept", "text/event-stream")]
    , Client.requestBody = Client.RequestBodyLBS (A.encode payload)
    , Client.responseTimeout = Client.responseTimeoutMicro (secondsToMicroseconds (toInteger cfg.requestTimeout))
    , Client.redirectCount = 0 -- Custom authentication headers must not cross redirects.
    }

askStreaming :: (HTTP.HTTP :> es, IOE :> es, Resource.Resource :> es)
  => ChatProviderConfig -> [FunctionTool] -> [ChatMessage] -> Stream (Of Text) (Eff es) ChatAnswer
askStreaming cfg tools messages = do
  key <- lift $ maybe (throwIO (LLMException "Anthropic API key is not configured")) pure cfg.apiKey
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

data Block = Block !Int !A.Object ![Text] ![Text] ![Text]
data StreamState = StreamState
  { started :: !Bool
  , stopped :: !Bool
  , stopReason :: !(Maybe Text)
  , current :: !(Maybe Block)
  , completed :: ![A.Value]
  , usage :: !(Maybe TokenUsage)
  }

emptyState :: StreamState
emptyState = StreamState False False Nothing Nothing [] Nothing

parse :: A.Parser a -> Either Text a
parse parser = first T.pack (A.parseEither (const parser) ())

step :: StreamState -> A.Value -> Either Text (StreamState, [Text])
step state value = parse $ A.withObject "Anthropic stream event" event value
  where
    event o = do
      kind <- o A..: "type" :: A.Parser Text
      when state.stopped (fail "Anthropic event after message_stop")
      case kind of
        "error" -> fail "Anthropic streaming error"
        "ping" -> pure (state, [])
        "message_start" -> do
          when state.started (fail "Duplicate Anthropic message_start")
          msg <- o A..: "message"
          usage <- msg A..: "usage" >>= parseUsage
          pure (state{started = True, usage = Just usage}, [])
        _ | not state.started -> fail "Anthropic stream missing message_start"
        "content_block_start" -> do
          unless (isNothing state.current) (fail "Overlapping Anthropic blocks")
          index <- o A..: "index"
          when (index /= length state.completed) (fail "Unexpected Anthropic block index")
          block <- o A..: "content_block"
          blockType <- block A..: "type" :: A.Parser Text
          unless (blockType `elem` ["text", "thinking", "redacted_thinking", "tool_use"]) (fail "Unsupported Anthropic block type")
          initial <- if blockType == "text" then block A..:? "text" A..!= "" else pure ""
          pure (state{current = Just (Block index block [] [] [])}, [initial | not (T.null initial)])
        "content_block_delta" -> do
          index <- o A..: "index"
          delta <- o A..: "delta"
          Block currentIndex block texts signatures inputs <- maybe (fail "Anthropic delta without block") pure state.current
          unless (index == currentIndex) (fail "Anthropic delta index mismatch")
          kind' <- delta A..: "type" :: A.Parser Text
          blockType <- block A..: "type" :: A.Parser Text
          let update ts ss ins out = pure (state{current = Just (Block index block ts ss ins)}, out)
          case (blockType, kind') of
            ("text", "text_delta") -> do
              t <- delta A..: "text"
              update (t:texts) signatures inputs [t | not (T.null t)]
            ("thinking", "thinking_delta") -> do
              t <- delta A..: "thinking"
              update (t:texts) signatures inputs []
            ("thinking", "signature_delta") -> do
              t <- delta A..: "signature"
              update texts (t:signatures) inputs []
            ("tool_use", "input_json_delta") -> do
              t <- delta A..: "partial_json"
              update texts signatures (t:inputs) []
            _ -> fail "Unsupported Anthropic block delta"
        "content_block_stop" -> do
          index <- o A..: "index"
          block@(Block currentIndex _ _ _ _) <- maybe (fail "Anthropic stop without block") pure state.current
          unless (index == currentIndex) (fail "Anthropic stop index mismatch")
          completed <- finishBlock block
          pure (state{current = Nothing, completed = completed : state.completed}, [])
        "message_delta" -> do
          delta <- o A..: "delta"
          reason <- delta A..:? "stop_reason"
          u <- o A..: "usage"
          output <- u A..: "output_tokens"
          let usage = fmap (\old -> old{completionTokens = output, totalTokens = old.promptTokens + output}) state.usage
          pure (state{stopReason = reason <|> state.stopReason, usage}, [])
        "message_stop" -> do
          unless (isNothing state.current) (fail "Anthropic stopped inside a block")
          pure (state{stopped = True}, [])
        _ -> pure (state, [])

parseUsage :: A.Value -> A.Parser TokenUsage
parseUsage = A.withObject "Anthropic usage" $ \o -> do
  input <- o A..: "input_tokens"
  output <- o A..: "output_tokens"
  cached <- o A..:? "cache_read_input_tokens"
  created <- o A..:? "cache_creation_input_tokens" A..!= 0
  let prompt = input + fromMaybe 0 cached + created
  pure (TokenUsage prompt output (prompt + output) cached)

finishBlock :: Block -> A.Parser A.Value
finishBlock (Block _ block texts signatures inputs) = do
  kind <- block A..: "type" :: A.Parser Text
  let append key fragments obj = do
        initial <- obj A..:? key A..!= ""
        pure (KM.insert key (A.String (initial <> T.concat (reverse fragments))) obj)
  result <- case kind of
    "text" -> append "text" texts block
    "thinking" -> append "thinking" texts block >>= append "signature" signatures
    "tool_use" | not (null inputs) -> do
      input <- either fail pure (A.eitherDecodeStrict' (TE.encodeUtf8 (T.concat (reverse inputs))))
      _ <- A.withObject "Anthropic tool input" pure input
      pure (KM.insert "input" input block)
    _ -> pure block
  pure (A.Object result)

finish :: StreamState -> Either Text ChatAnswer
finish state = parse $ do
  unless (state.started && state.stopped && isNothing state.current) (fail "Incomplete Anthropic stream")
  unless (state.stopReason `elem` map Just ["end_turn", "stop_sequence", "tool_use"]) (fail "Anthropic response stopped without a complete turn")
  let blocks = reverse state.completed
  texts <- traverse (A.withObject "block" $ \o -> do
    kind <- o A..: "type" :: A.Parser Text
    if kind == "text" then o A..: "text" else pure "") blocks
  calls <- catMaybes <$> traverse (A.withObject "block" $ \o -> do
    kind <- o A..: "type" :: A.Parser Text
    if kind /= "tool_use" then pure Nothing else do
      callId <- o A..: "id"
      name <- o A..: "name"
      input <- o A..: "input"
      _ <- A.withObject "Anthropic tool input" pure input
      when (T.null callId || T.null name) (fail "Empty Anthropic tool identity")
      pure $ Just (ToolCall callId name (TE.decodeUtf8 (BL.toStrict (A.encode input))))) blocks
  unless ((state.stopReason == Just "tool_use") == not (null calls)) (fail "Anthropic stop reason/tool mismatch")
  unless (length (ordNub (map (.id) calls)) == length calls) (fail "Duplicate Anthropic tool ids")
  pure $ withChatAnswerProviderState (ProviderState "anthropic" blocks) $
    withChatAnswerTokenUsage state.usage (chatAnswer (T.concat texts) calls)

streamFromPayloads :: [A.Value] -> Either Text ([Text], ChatAnswer)
streamFromPayloads values = do
  (state, outputs) <- foldM (\(state, outputs) value -> do
    (next, output) <- step state value
    pure (next, outputs <> output)) (emptyState, []) values
  answer <- finish state
  pure (outputs, answer)
