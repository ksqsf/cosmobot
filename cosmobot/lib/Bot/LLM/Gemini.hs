module Bot.LLM.Gemini
  ( askStreaming
  , requestPayload
  , httpRequest
  , streamFromPayloads
  ) where

import Bot.Prelude hiding (state)
import Control.Monad (foldM)
import qualified Control.Exception as Exception
import qualified Bot.Effect.HTTP as HTTP
import Bot.LLM.Config (ChatProviderConfig (..))
import Bot.LLM.Stream
import Bot.LLM.Types
import Bot.Media.Mime (sniffMime)
import Bot.Util.Time (secondsToMicroseconds)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Effectful.Resource as Resource
import qualified Network.HTTP.Client as Client
import qualified Streaming.Prelude as S

requestPayload :: ChatProviderConfig -> [FunctionTool] -> [ChatMessage] -> Either Text A.Value
requestPayload cfg tools messages = parse $ do
  system <- concat <$> traverse systemParts (filter ((`elem` ["system", "developer"]) . (.role)) messages)
  (_, turns) <- foldM message (Map.empty, []) (filter ((`notElem` ["system", "developer"]) . (.role)) messages)
  pure $ A.object $
    [ "contents" A..= mergeTurns (reverse turns)
    , "generationConfig" A..= A.object
        (["maxOutputTokens" A..= cfg.maxTokens, "candidateCount" A..= (1 :: Int)] <>
          ["thinkingConfig" A..= A.object ["thinkingLevel" A..= cfg.reasoningEffort] | not (T.null cfg.reasoningEffort)])
    ] <> ["systemInstruction" A..= A.object ["parts" A..= system] | not (null system)]
      <> ["tools" A..= [A.object ["functionDeclarations" A..= map tool tools]] | not (null tools)]
  where
    systemParts msg = traverse (\case TextPart t -> pure (textPart t); _ -> fail "Gemini system instructions must be text") (parts msg.content)
    tool FunctionTool{name, description, parameters} = A.object
      ["name" A..= name, "description" A..= description, "parametersJsonSchema" A..= parameters]
    message (pending, turns) msg = case msg.role of
      "assistant" -> do
        content <- case msg.providerState of
          Just opaque | opaque.provider == "gemini" -> pure opaque.blocks
          _ -> (<>) <$> traverse contentPart (parts msg.content) <*> traverse historicalCall msg.toolCalls
        calls <- catMaybes <$> traverse nativeCall content
        unless (length calls == length msg.toolCalls) (fail "Gemini transcript call/state mismatch")
        bindings <- forM (zip msg.toolCalls calls) $ \(call, (name, wireId, _)) -> do
          unless (call.name == name) (fail "Gemini transcript function name mismatch")
          pure (call.id, (name, wireId))
        pure (Map.union (Map.fromList bindings) pending, ("model", content) : turns)
      "user" -> user pending turns msg
      "synthetic" -> user pending turns msg
      "tool" -> do
        callId <- maybe (fail "Gemini tool result is missing tool_call_id") pure msg.toolCallId
        (name, wireId) <- maybe (fail "Gemini tool result has no matching function call") pure (Map.lookup callId pending)
        result <- traverse (\case TextPart t -> pure t; _ -> fail "Gemini tool results must be text") (parts msg.content)
        let response = A.object ["functionResponse" A..= A.object
              (["name" A..= name, "response" A..= A.object ["result" A..= T.intercalate "\n" result]] <>
                ["id" A..= ident | ident <- maybeToList wireId])]
        pure (Map.delete callId pending, ("user", [response]) : turns)
      _ -> fail "Unsupported Gemini transcript role"
    user pending turns msg = do
      content <- traverse contentPart (parts msg.content)
      pure (pending, ("user", content) : turns)
    historicalCall call = do
      args <- either fail pure (A.eitherDecodeStrict' (TE.encodeUtf8 call.arguments))
      _ <- A.withObject "Gemini function arguments" pure args
      -- Google's documented sentinel permits imported/compacted historical calls.
      pure $ A.object
        [ "functionCall" A..= A.object ["id" A..= call.id, "name" A..= call.name, "args" A..= args]
        , "thoughtSignature" A..= A.String "skip_thought_signature_validator"
        ]

parts :: Maybe MessageContent -> [ContentPart]
parts = \case
  Nothing -> []
  Just (TextContent t) -> [TextPart t | not (T.null t)]
  Just (PartsContent ps) -> ps

textPart :: Text -> A.Value
textPart t = A.object ["text" A..= t]

contentPart :: ContentPart -> A.Parser A.Value
contentPart = \case
  TextPart t -> pure (textPart t)
  ImageUrlPart url -> case T.stripPrefix "data:" url of
    Just rest -> case T.breakOn ";base64," rest of
      (mime, encoded) | "image/" `T.isPrefixOf` mime && not (T.null encoded) ->
        pure $ A.object ["inlineData" A..= A.object ["mimeType" A..= mime, "data" A..= T.drop 8 encoded]]
      _ -> fail "Invalid Gemini image data URI"
    Nothing | isGeminiFile url -> pure $ A.object ["fileData" A..= A.object ["fileUri" A..= url]]
    _ -> fail "Gemini image URL must be resolved to inline data or a Gemini file URI"

isGeminiFile :: Text -> Bool
isGeminiFile = T.isPrefixOf "https://generativelanguage.googleapis.com/v1beta/files/"

mergeTurns :: [(Text, [A.Value])] -> [A.Value]
mergeTurns [] = []
mergeTurns turns@((role, _) : _)
  | role == "model" = A.object ["role" A..= A.String "user", "parts" A..= [textPart "Continue from the supplied conversation history."]] : merge turns
  | otherwise = merge turns
  where
    merge [] = []
    merge ((r, ps) : rest) = go ps rest
      where
        go acc ((nextRole, nextParts) : more) | nextRole == r = go (acc <> nextParts) more
        go acc more = A.object ["role" A..= r, "parts" A..= acc] : merge more

httpRequest :: ChatProviderConfig -> Text -> A.Value -> IO Client.Request
httpRequest cfg key payload = do
  let model = fromMaybe cfg.model (T.stripPrefix "models/" cfg.model)
  when (T.null model || not (T.all validModelChar model)) (Exception.throwIO (LLMException "Invalid Gemini model identifier"))
  base <- Client.parseRequest (T.unpack (T.dropWhileEnd (== '/') cfg.baseUrl <> "/models/" <> model <> ":streamGenerateContent?alt=sse"))
  pure base
    { Client.method = "POST"
    , Client.requestHeaders = [("x-goog-api-key", TE.encodeUtf8 key), ("Content-Type", "application/json"), ("Accept", "text/event-stream")]
    , Client.requestBody = Client.RequestBodyLBS (A.encode payload)
    , Client.responseTimeout = Client.responseTimeoutMicro (secondsToMicroseconds (toInteger cfg.requestTimeout))
    , Client.redirectCount = 0
    }
  where
    validModelChar c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || c `elem` ['-', '_', '.']

askStreaming :: (HTTP.HTTP :> es, IOE :> es, Resource.Resource :> es)
  => ChatProviderConfig -> [FunctionTool] -> [ChatMessage] -> Stream (Of Text) (Eff es) ChatAnswer
askStreaming cfg tools messages = do
  key <- lift $ maybe (throwIO (LLMException "Gemini API key is not configured")) pure cfg.apiKey
  resolved <- lift (resolveImages cfg messages)
  payload <- lift $ either (throwIO . LLMException) pure (requestPayload cfg tools resolved)
  when (BL.length (A.encode payload) > fromIntegral maxRequestBytes) $
    lift (throwIO (LLMException "Gemini inline request exceeds 20 MB; use Gemini file URIs"))
  request <- liftIO (httpRequest cfg key payload)
  go emptyState (streamSsePayloads (streamHttpResponseBody request))
  where
    usedIds = concatMap (map (.id) . (.toolCalls)) messages
    go state source = lift (S.next source) >>= \case
      Left () -> lift $ either (throwIO . LLMException) pure (finish usedIds state)
      Right (bytes, rest) -> do
        value <- lift $ either (throwIO . LLMException . T.pack) pure (A.eitherDecodeStrict' bytes)
        (next, output) <- lift $ either (throwIO . LLMException) pure (step state value)
        traverse_ S.yield output
        go next rest

-- Gemini inline requests have a 20 MB limit; bound downloads before encoding.
maxRequestBytes :: Int
maxRequestBytes = 20 * 1000 * 1000

resolveImages :: (HTTP.HTTP :> es, IOE :> es, Resource.Resource :> es)
  => ChatProviderConfig -> [ChatMessage] -> Eff es [ChatMessage]
resolveImages cfg messages = reverse . snd <$> foldM message (maxRequestBytes, []) messages
  where
    message (remaining, acc) ChatMessage{role, content = Just (PartsContent ps), toolCalls, toolCallId, providerState} = do
      (left, reversed) <- foldM part (remaining, []) ps
      pure (left, ChatMessage{role, content = Just (PartsContent (reverse reversed)), toolCalls, toolCallId, providerState} : acc)
    message (remaining, acc) msg = pure (remaining, msg:acc)
    part (remaining, acc) (ImageUrlPart url) = do
      resolved <- if "data:" `T.isPrefixOf` url || isGeminiFile url then pure url else do
        unless ("https://" `T.isPrefixOf` url || "http://" `T.isPrefixOf` url) $
          throwIO (LLMException "Gemini image requires an HTTP URL, base64 data URI, or Gemini file URI")
        request <- liftIO (Client.parseRequest (T.unpack url))
        let bounded = request{Client.responseTimeout = Client.responseTimeoutMicro (secondsToMicroseconds (toInteger cfg.requestTimeout))}
        (_, chunks) <- S.foldM_ (collect (remaining * 3 `div` 4)) (pure (0, [])) pure (streamHttpResponseBody bounded)
        let bytes = BS.concat (reverse chunks)
        mime <- case sniffMime bytes of
          Just mime | "image/" `T.isPrefixOf` mime -> pure mime
          _ -> throwIO (LLMException "Gemini image download is not a supported image; supply a typed data URI")
        pure ("data:" <> mime <> ";base64," <> TE.decodeUtf8 (Base64.encode bytes))
      let left = remaining - T.length resolved
      when (left < 0) (throwIO (LLMException "Gemini inline images exceed 20 MB; use Gemini file URIs"))
      pure (left, ImageUrlPart resolved : acc)
    part (remaining, acc) p = pure (remaining, p:acc)
    collect limit (size, chunks) chunk = do
      let next = size + BS.length chunk
      when (next > limit) (throwIO (LLMException "Gemini image download exceeds inline request limit"))
      pure (next, chunk:chunks)

data StreamState = StreamState
  { reversedParts :: ![A.Value]
  , finished :: !Bool
  , usage :: !(Maybe TokenUsage)
  }

emptyState :: StreamState
emptyState = StreamState [] False Nothing

parse :: A.Parser a -> Either Text a
parse parser = first T.pack (A.parseEither (const parser) ())

step :: StreamState -> A.Value -> Either Text (StreamState, [Text])
step state value = parse $ A.withObject "Gemini stream response" (\o -> do
  when (KM.member "error" o) (fail "Gemini streaming error")
  feedback <- o A..:? "promptFeedback"
  blocked <- maybe (pure Nothing) (A..:? "blockReason") feedback :: A.Parser (Maybe Text)
  when (isJust blocked) (fail "Gemini blocked the prompt")
  usage <- traverse parseUsage =<< o A..:? "usageMetadata"
  candidates <- o A..:? "candidates" A..!= []
  let next = state{usage = usage <|> state.usage}
  case candidates of
    [] -> pure (next, [])
    [candidate] -> do
      when state.finished (fail "Gemini candidate after finishReason")
      index <- candidate A..:? "index" A..!= (0 :: Int)
      unless (index == 0) (fail "Unexpected Gemini candidate index")
      reason <- candidate A..:? "finishReason" :: A.Parser (Maybe Text)
      for_ reason $ \r -> unless (r == "STOP") (fail ("Gemini response stopped: " <> T.unpack r))
      content <- candidate A..:? "content"
      ps <- maybe (pure []) (\c -> do
        role <- c A..:? "role" A..!= ("model" :: Text)
        unless (role == "model") (fail "Unexpected Gemini candidate role")
        c A..:? "parts" A..!= []) content
      texts <- traverse visibleText ps
      pure (next{reversedParts = reverse ps <> state.reversedParts, finished = isJust reason}, filter (not . T.null) texts)
    _ -> fail "Gemini returned multiple candidates") value

visibleText :: A.Value -> A.Parser Text
visibleText = A.withObject "Gemini part" $ \p -> do
  thought <- p A..:? "thought" A..!= False
  text <- p A..:? "text"
  call <- p A..:? "functionCall" :: A.Parser (Maybe A.Value)
  unless (isJust text || isJust call || KM.member "thoughtSignature" p) (fail "Unsupported Gemini response part")
  pure $ if thought then "" else fromMaybe "" text

nativeCall :: A.Value -> A.Parser (Maybe (Text, Maybe Text, A.Value))
nativeCall = A.withObject "Gemini part" $ \p -> p A..:? "functionCall" >>= traverse (\c -> do
  name <- c A..: "name"
  ident <- c A..:? "id"
  args <- c A..:? "args" A..!= A.object []
  _ <- A.withObject "Gemini function arguments" pure args
  when (T.null name || maybe False T.null ident) (fail "Empty Gemini function identity")
  pure (name, ident, args))

parseUsage :: A.Value -> A.Parser TokenUsage
parseUsage = A.withObject "Gemini usage" $ \o -> do
  prompt <- o A..:? "promptTokenCount" A..!= 0
  output <- o A..:? "candidatesTokenCount" A..!= 0
  thoughts <- o A..:? "thoughtsTokenCount" A..!= 0
  toolPrompt <- o A..:? "toolUsePromptTokenCount" A..!= 0
  total <- o A..:? "totalTokenCount" A..!= (prompt + toolPrompt + output + thoughts)
  cached <- o A..:? "cachedContentTokenCount"
  pure (TokenUsage (prompt + toolPrompt) (output + thoughts) total cached)

finish :: [Text] -> StreamState -> Either Text ChatAnswer
finish usedIds state = parse $ do
  unless state.finished (fail "Incomplete Gemini stream: missing finishReason")
  let ps = reverse state.reversedParts
  texts <- traverse visibleText ps
  calls <- catMaybes <$> traverse nativeCall ps
  let explicitIds = mapMaybe (\(_, ident, _) -> ident) calls
  unless (length (ordNub explicitIds) == length explicitIds) (fail "Duplicate Gemini function call ids")
  (_, reversed) <- foldM assign (Set.fromList (usedIds <> explicitIds), []) calls
  pure $ withChatAnswerProviderState (ProviderState "gemini" ps) $
    withChatAnswerTokenUsage state.usage (chatAnswer (T.concat texts) (reverse reversed))
  where
    assign (reserved, acc) (name, ident, args) = do
      let callId = fromMaybe (freshId reserved 0) ident
      pure (Set.insert callId reserved, ToolCall callId name (TE.decodeUtf8 (BL.toStrict (A.encode args))) : acc)
    freshId :: Set.Set Text -> Int -> Text
    freshId reserved n =
      let ident = "gemini-call-" <> show n
      in if Set.member ident reserved then freshId reserved (n + 1) else ident

streamFromPayloads :: [Text] -> [A.Value] -> Either Text ([Text], ChatAnswer)
streamFromPayloads usedIds values = do
  (state, outputs) <- foldM (\(state, outputs) value -> do
    (next, output) <- step state value
    pure (next, outputs <> output)) (emptyState, []) values
  answer <- finish usedIds state
  pure (outputs, answer)
