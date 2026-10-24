module Main (main) where

import Bot.Prelude
import qualified Bot.HTTP as HTTPInterpreter
import qualified Data.IORef as IORef
import qualified Effectful.Resource as Resource
import qualified Network.HTTP.Types as Status
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Streaming.Prelude as S
import qualified Prelude
import qualified Bot.LLM.Anthropic as Anthropic
import qualified Bot.LLM.Gemini as Gemini
import qualified Bot.LLM.Responses as Responses
import qualified Bot.LLM.Config as Config
import qualified Bot.LLM.Config as ChatConfig (ChatProviderConfig (..))
import Bot.LLM.Types
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "native LLM"
  [ testCase "Responses request format and authentication" testResponsesRequest
  , testCase "Responses reasoning, phase and parallel call ids survive persistence" testResponsesTools
  , testCase "Responses text and refusal deltas are not duplicated" testResponsesText
  , testCase "Responses rejects incomplete and malformed output" testResponsesErrors
  , testCase "Responses HTTP/SSE integration" testResponsesHTTP
  , testCase "Gemini request and HTTP/SSE image integration" testGeminiHTTP
  , testCase "Gemini signatures, parallel same-name calls and missing ids" testGeminiTools
  , testCase "Gemini text, hidden thoughts and cumulative usage" testGeminiText
  , testCase "Gemini rejects incomplete, blocked and malformed responses" testGeminiErrors
  , testCase "Anthropic HTTP/SSE integration" testHTTP
  , testCase "Anthropic request uses native fields and auth" testRequest
  , testCase "Anthropic signed thinking and parallel tools survive transcript persistence" testToolRoundTrip
  , testCase "Anthropic rejects truncated and invalid tool streams" testIncomplete
  , testCase "Anthropic handles arbitrary JSON delta splits" testSplits
  , testCase "Anthropic text streams exclude thinking and normalize usage" testText
  , testCase "legacy transcripts decode without provider state" testLegacy
  ]

provider :: Config.ChatProviderConfig
provider = Config.defaultChatProviderConfig
  { ChatConfig.api = "anthropic"
  , ChatConfig.baseUrl = "https://api.anthropic.com/v1/"
  , ChatConfig.model = "claude-opus-5-5"
  , ChatConfig.thinking = Just "adaptive"
  , ChatConfig.reasoningEffort = "high"
  }

json :: Text -> A.Value
json = either (error . toText) id . A.eitherDecodeStrict' . encodeUtf8

field :: A.Key -> A.Value -> A.Value
field key (A.Object o) = fromMaybe A.Null (KM.lookup key o)
field _ _ = A.Null

array :: A.Value -> [A.Value]
array (A.Array a) = toList a
array _ = []

right :: Show e => Either e a -> IO a
right = either (assertFailure . show) pure

testRequest :: Assertion
testRequest = do
  let schema = json "{\"type\":\"object\",\"properties\":{}}"
  body <- right $ Anthropic.requestPayload provider [FunctionTool "weather" "Weather" schema]
    [systemText "system", userWithImages "hi" ["data:image/png;base64,YQ==", "https://example.org/a.png"]]
  field "thinking" body @?= json "{\"type\":\"adaptive\"}"
  field "output_config" body @?= json "{\"effort\":\"high\"}"
  field "reasoning_effort" body @?= A.Null
  field "max_tokens" body @?= A.Number 16384
  field "tools" body @?= A.toJSON [A.object ["name" A..= A.String "weather", "description" A..= A.String "Weather", "input_schema" A..= schema]]
  field "system" body @?= json "[{\"type\":\"text\",\"text\":\"system\"}]"
  let content = field "messages" body & array & concatMap (array . field "content")
  map (field "type") content @?= map A.String ["text", "image", "image"]
  field "source" (content Prelude.!! 1) @?= json "{\"type\":\"base64\",\"media_type\":\"image/png\",\"data\":\"YQ==\"}"
  request <- Anthropic.httpRequest provider "test-secret" body
  HTTP.path request @?= "/v1/messages"
  Prelude.lookup "x-api-key" (HTTP.requestHeaders request) @?= Just "test-secret"
  Prelude.lookup "anthropic-version" (HTTP.requestHeaders request) @?= Just "2023-06-01"
  Prelude.lookup "Authorization" (HTTP.requestHeaders request) @?= Nothing
  HTTP.redirectCount request @?= 0
  proxyRequest <- Anthropic.httpRequest provider{ChatConfig.anthropicAuth = "bearer", ChatConfig.baseUrl = "https://openrouter.ai/api/v1"} "router-key" body
  Prelude.lookup "Authorization" (HTTP.requestHeaders proxyRequest) @?= Just "Bearer router-key"
  Prelude.lookup "x-api-key" (HTTP.requestHeaders proxyRequest) @?= Nothing
  HTTP.path proxyRequest @?= "/api/v1/messages"
  omitted <- right $ Anthropic.requestPayload provider{ChatConfig.thinking = Nothing, ChatConfig.reasoningEffort = ""} [] [userText "hi"]
  field "thinking" omitted @?= A.Null
  field "output_config" omitted @?= A.Null

start :: A.Value
start = json "{\"type\":\"message_start\",\"message\":{\"usage\":{\"input_tokens\":7,\"output_tokens\":0,\"cache_read_input_tokens\":11,\"cache_creation_input_tokens\":3}}}"

blockStart :: Int -> A.Value -> A.Value
blockStart index block = A.object ["type" A..= A.String "content_block_start", "index" A..= index, "content_block" A..= block]

blockDelta :: Int -> A.Value -> A.Value
blockDelta index delta = A.object ["type" A..= A.String "content_block_delta", "index" A..= index, "delta" A..= delta]

blockStop :: Int -> A.Value
blockStop index = A.object ["type" A..= A.String "content_block_stop", "index" A..= index]

end :: Text -> [A.Value]
end reason =
  [ A.object ["type" A..= A.String "message_delta", "delta" A..= A.object ["stop_reason" A..= reason], "usage" A..= A.object ["output_tokens" A..= (5 :: Int)]]
  , json "{\"type\":\"message_stop\"}"
  ]

thinking :: [A.Value]
thinking =
  [ blockStart 0 (json "{\"type\":\"thinking\",\"thinking\":\"\",\"signature\":\"\"}")
  , blockDelta 0 (json "{\"type\":\"thinking_delta\",\"thinking\":\"private\"}")
  , blockDelta 0 (json "{\"type\":\"signature_delta\",\"signature\":\"signed\"}")
  , blockStop 0
  ]

tool :: Int -> Text -> [Text] -> [A.Value]
tool index callId fragments =
  [blockStart index (A.object ["type" A..= A.String "tool_use", "id" A..= callId, "name" A..= A.String "weather", "input" A..= A.object []])]
  <> map (\t -> blockDelta index (A.object ["type" A..= A.String "input_json_delta", "partial_json" A..= t])) fragments
  <> [blockStop index]

testToolRoundTrip :: Assertion
testToolRoundTrip = do
  (output, answer) <- right $ Anthropic.streamFromPayloads ([start] <> thinking <> tool 1 "a" ["{\"city\":", "\"東京\"}"] <> tool 2 "b" ["{}"] <> end "tool_use")
  output @?= []
  chatAnswerToolCalls answer @?= [ToolCall "a" "weather" "{\"city\":\"東京\"}", ToolCall "b" "weather" "{}"]
  persisted <- right (A.eitherDecode (A.encode (assistantAnswer answer)) :: Either String ChatMessage)
  persisted.providerState @?= answer.providerState
  body <- right $ Anthropic.requestPayload provider []
    ([userText "weather", persisted] <> map (\c -> toolResult c "sunny") (chatAnswerToolCalls answer) <> [userText "thanks"])
  let turns = array (field "messages" body)
  length turns @?= 3
  let blocks = array (field "content" (turns Prelude.!! 1))
  field "thinking" (blocks Prelude.!! 0) @?= A.String "private"
  field "signature" (blocks Prelude.!! 0) @?= A.String "signed"
  let results = array (field "content" (turns Prelude.!! 2))
  map (field "tool_use_id") results @?= [A.String "a", A.String "b", A.Null]
  assertBool "opaque state Show is redacted" (not ("private" `T.isInfixOf` show persisted))

testIncomplete :: Assertion
testIncomplete = do
  let valid = [start] <> tool 0 "a" ["{}"] <> end "tool_use"
  for_ [0 .. length valid - 1] $ \n -> assertBool "every incomplete prefix fails" (isLeft (Anthropic.streamFromPayloads (take n valid)))
  for_ [ [start] <> tool 0 "a" ["{\"x\":"] <> end "tool_use"
       , [start] <> tool 0 "a" ["[]"] <> end "tool_use"
       , [start] <> tool 0 "a" ["{}"] <> tool 1 "a" ["{}"] <> end "tool_use"
       , [start] <> tool 0 "a" ["{}"] <> end "max_tokens"
       , [start] <> tool 0 "a" ["{}"] <> end "end_turn"
       , [start, json "{\"type\":\"error\",\"error\":{\"type\":\"overloaded_error\"}}"]
       ] $ \events -> assertBool "malformed/incomplete tool turn fails" (isLeft (Anthropic.streamFromPayloads events))

testSplits :: Assertion
testSplits = do
  let input = "{\"city\":\"東京\",\"days\":2}"
  for_ [0 .. T.length input] $ \n -> do
    (_, answer) <- right $ Anthropic.streamFromPayloads ([start] <> tool 0 "call" [T.take n input, T.drop n input] <> end "tool_use")
    case chatAnswerToolCalls answer of
      [call] -> A.eitherDecodeStrict' (encodeUtf8 call.arguments) @?= Right (json "{\"city\":\"東京\",\"days\":2}")
      _ -> assertFailure "expected one call"

testText :: Assertion
testText = do
  (output, answer) <- right $ Anthropic.streamFromPayloads $ [start] <> thinking <>
    [ blockStart 1 (json "{\"type\":\"text\",\"text\":\"Hi\"}")
    , blockDelta 1 (json "{\"type\":\"text_delta\",\"text\":\" there\"}")
    , blockStop 1
    ] <> end "end_turn"
  output @?= ["Hi", " there"]
  chatAnswerContent answer @?= "Hi there"
  chatAnswerTokenUsage answer @?= Just (TokenUsage 21 5 26 (Just 11))

testLegacy :: Assertion
testLegacy = do
  msg <- right (A.eitherDecode "{\"role\":\"assistant\",\"content\":\"old\"}" :: Either String ChatMessage)
  msg.providerState @?= Nothing

testHTTP :: Assertion
testHTTP = do
  captured <- IORef.newIORef Nothing
  let events = [start] <> tool 0 "call" ["{}"] <> end "tool_use"
      wire = foldMap (\v -> "event: ignored\r\ndata: " <> A.encode v <> "\r\n\r\n") events
      app req respond = do
        body <- Wai.strictRequestBody req
        IORef.writeIORef captured (Just (Wai.pathInfo req, Wai.requestHeaders req, body))
        respond (Wai.responseLBS Status.status200 [("Content-Type", "text/event-stream")] wire)
  Warp.testWithApplication (pure app) $ \port -> do
    let cfg = provider{ChatConfig.baseUrl = "http://127.0.0.1:" <> show port <> "/v1", ChatConfig.apiKey = Just "test-key"}
    (outputs S.:> answer) <- runEff $ Resource.runResource $ HTTPInterpreter.runHTTP $
      S.toList (Anthropic.askStreaming cfg [] [userText "hello"])
    outputs @?= []
    chatAnswerToolCalls answer @?= [ToolCall "call" "weather" "{}"]
  IORef.readIORef captured >>= \case
    Just (path, headers, body) -> do
      path @?= ["v1", "messages"]
      Prelude.lookup "x-api-key" headers @?= Just "test-key"
      decoded <- right (A.eitherDecode body)
      field "model" decoded @?= A.String "claude-opus-5-5"
    Nothing -> assertFailure "request was not received"
  let incomplete _ respond = respond (Wai.responseLBS Status.status200 [("Content-Type", "text/event-stream")] ("data: " <> A.encode start <> "\n\n"))
  Warp.testWithApplication (pure incomplete) $ \port -> do
    result <- runEff $ Resource.runResource $ HTTPInterpreter.runHTTP $ trySync $
      S.toList (Anthropic.askStreaming provider{ChatConfig.baseUrl = "http://127.0.0.1:" <> show port, ChatConfig.apiKey = Just "key"} [] [userText "hi"])
    assertBool "HTTP EOF cannot complete a tool turn" (isLeft result)

gemini :: Config.ChatProviderConfig
gemini = Config.defaultChatProviderConfig
  { ChatConfig.api = "gemini"
  , ChatConfig.baseUrl = "https://generativelanguage.googleapis.com/v1beta"
  , ChatConfig.model = "gemini-2.5-flash"
  , ChatConfig.reasoningEffort = ""
  }

geminiFrame :: [A.Value] -> Maybe Text -> A.Value
geminiFrame ps reason = A.object ["candidates" A..= [A.object $
  ["index" A..= (0 :: Int), "content" A..= A.object ["role" A..= A.String "model", "parts" A..= ps]]
  <> ["finishReason" A..= r | r <- maybeToList reason]]]

testGeminiTools :: Assertion
testGeminiTools = do
  -- Enumerate missing/present ids on both sides of a collision with prior history.
  for_ [False, True] $ \firstHasId -> for_ [False, True] $ \secondHasId -> do
    let call hasId ident signed = A.object $
          ["functionCall" A..= A.object
            (["name" A..= A.String "weather", "args" A..= json "{\"city\":\"東京\"}"] <> ["id" A..= ident | hasId])]
          <> ["thoughtSignature" A..= A.String "signed" | signed]
        raw = [call firstHasId ("gemini-call-0" :: Text) True, call secondHasId ("native-second" :: Text) False]
    (_, answer) <- right $ Gemini.streamFromPayloads ["gemini-call-1"] [geminiFrame raw (Just "STOP")]
    let calls = chatAnswerToolCalls answer
        ids = map (.id) calls
    length calls @?= 2
    length (ordNub ids) @?= 2
    assertBool "generated ids avoid history" ("gemini-call-1" `notElem` ids)
    persisted <- right (A.eitherDecode (A.encode (assistantAnswer answer)) :: Either String ChatMessage)
    body <- right $ Gemini.requestPayload gemini [] ([userText "weather", persisted] <> map (\c -> toolResult c "sunny") calls)
    let turns = array (field "contents" body)
        results = map (field "functionResponse") (array (field "parts" (turns Prelude.!! 2)))
    field "parts" (turns Prelude.!! 1) @?= A.toJSON raw
    map (field "name") results @?= replicate 2 (A.String "weather")
    map (field "id") results @?=
      [if firstHasId then A.String "gemini-call-0" else A.Null, if secondHasId then A.String "native-second" else A.Null]
    -- A second model/tool turn must retain the earlier signed part unchanged.
    (_, next) <- right $ Gemini.streamFromPayloads ids [geminiFrame [call False ("unused" :: Text) True] (Just "STOP")]
    body2 <- right $ Gemini.requestPayload gemini [] $
      [userText "weather", persisted] <> map (\c -> toolResult c "sunny") calls <>
      [assistantAnswer next] <> map (\c -> toolResult c "rain") (chatAnswerToolCalls next)
    field "parts" (array (field "contents" body2) Prelude.!! 1) @?= A.toJSON raw

testGeminiText :: Assertion
testGeminiText = do
  let frames =
        [ geminiFrame [json "{\"text\":\"private\",\"thought\":true}"] Nothing
        , json "{\"usageMetadata\":{\"promptTokenCount\":2,\"candidatesTokenCount\":1}}"
        , geminiFrame [json "{\"text\":\"Hi\"}"] Nothing
        , geminiFrame [json "{\"text\":\" there\"}", json "{\"text\":\"\",\"thoughtSignature\":\"signed\"}"] (Just "STOP")
        , json "{\"usageMetadata\":{\"promptTokenCount\":7,\"cachedContentTokenCount\":3,\"candidatesTokenCount\":2,\"thoughtsTokenCount\":5,\"toolUsePromptTokenCount\":1,\"totalTokenCount\":15}}"
        ]
  (output, answer) <- right $ Gemini.streamFromPayloads [] frames
  output @?= ["Hi", " there"]
  chatAnswerContent answer @?= "Hi there"
  chatAnswerTokenUsage answer @?= Just (TokenUsage 8 7 15 (Just 3))
  (answer.providerState >>= listToMaybe . reverse . (.blocks)) @?= Just (json "{\"text\":\"\",\"thoughtSignature\":\"signed\"}")

testGeminiErrors :: Assertion
testGeminiErrors = do
  let call = json "{\"functionCall\":{\"id\":\"a\",\"name\":\"f\",\"args\":{}}}"
      valid = [geminiFrame [call] Nothing, geminiFrame [] (Just "STOP")]
  for_ [0 .. length valid - 1] $ \n ->
    assertBool "incomplete prefixes fail" (isLeft (Gemini.streamFromPayloads [] (take n valid)))
  for_ [ [geminiFrame [call] (Just "MAX_TOKENS")]
       , [geminiFrame [] (Just "SAFETY")]
       , [json "{\"promptFeedback\":{\"blockReason\":\"SAFETY\"}}"]
       , [json "{\"error\":{\"code\":429}}"]
       , [geminiFrame [call, call] (Just "STOP")]
       , [geminiFrame [json "{\"functionCall\":{\"name\":\"f\",\"args\":[]}}"] (Just "STOP")]
       , [geminiFrame [json "{\"inlineData\":{\"data\":\"a\"}}"] (Just "STOP")]
       , [json "{\"candidates\":[{},{}]}"]
       , valid <> valid
       ] $ \frames -> assertBool "invalid stream fails" (isLeft (Gemini.streamFromPayloads [] frames))
  assertBool "orphan tool result fails" (isLeft (Gemini.requestPayload gemini [] [toolResult (ToolCall "orphan" "f" "{}") "x"]))
  badModel <- runEff $ trySync $ liftIO (Gemini.httpRequest gemini{ChatConfig.model = "../secret?key=x"} "key" A.Null)
  assertBool "model cannot inject URL" (isLeft badModel)

testGeminiHTTP :: Assertion
testGeminiHTTP = do
  captured <- IORef.newIORef Nothing
  imageHeaders <- IORef.newIORef []
  let wire = "data: " <> A.encode (geminiFrame [json "{\"text\":\"hello\"}"] (Just "STOP")) <> "\n\n"
      app req respond
        | Wai.pathInfo req == ["image"] = do
            IORef.writeIORef imageHeaders (Wai.requestHeaders req)
            respond (Wai.responseLBS Status.status200 [] "\137PNG\r\n\26\nimage")
        | otherwise = do
            body <- Wai.strictRequestBody req
            IORef.writeIORef captured (Just (Wai.pathInfo req, Wai.queryString req, Wai.requestHeaders req, body))
            respond (Wai.responseLBS Status.status200 [("Content-Type", "text/event-stream")] wire)
  Warp.testWithApplication (pure app) $ \port -> do
    let root = "http://127.0.0.1:" <> show port
        cfg = gemini{ChatConfig.baseUrl = root <> "/v1beta", ChatConfig.apiKey = Just "test-key", ChatConfig.reasoningEffort = "high"}
        schema = json "{\"type\":\"object\",\"additionalProperties\":false}"
    (output S.:> answer) <- runEff $ Resource.runResource $ HTTPInterpreter.runHTTP $
      S.toList (Gemini.askStreaming cfg [FunctionTool "weather" "Weather" schema]
        [systemText "system", userWithImages "hi" [root <> "/image"]])
    output @?= ["hello"]
    chatAnswerContent answer @?= "hello"
    IORef.readIORef captured >>= \case
      Just (path, query, headers, body) -> do
        path @?= ["v1beta", "models", "gemini-2.5-flash:streamGenerateContent"]
        query @?= [("alt", Just "sse")]
        Prelude.lookup "x-goog-api-key" headers @?= Just "test-key"
        decoded <- right (A.eitherDecode body)
        field "systemInstruction" decoded @?= json "{\"parts\":[{\"text\":\"system\"}]}"
        field "thinkingConfig" (field "generationConfig" decoded) @?= json "{\"thinkingLevel\":\"high\"}"
        let ps = concatMap (array . field "parts") (array (field "contents" decoded))
        field "mimeType" (field "inlineData" (ps Prelude.!! 1)) @?= A.String "image/png"
        field "parametersJsonSchema" (array (field "functionDeclarations" (array (field "tools" decoded) Prelude.!! 0)) Prelude.!! 0) @?= schema
      Nothing -> assertFailure "request was not received"
  headers <- IORef.readIORef imageHeaders
  Prelude.lookup "x-goog-api-key" headers @?= Nothing
  omitted <- right (Gemini.requestPayload gemini [] [userText "hello"])
  field "thinkingConfig" (field "generationConfig" omitted) @?= A.Null
  req <- Gemini.httpRequest gemini "key" omitted
  HTTP.redirectCount req @?= 0

responses :: Config.ChatProviderConfig
responses = Config.defaultChatProviderConfig
  { ChatConfig.api = "responses"
  , ChatConfig.baseUrl = "https://api.openai.com/v1/"
  , ChatConfig.model = "gpt-5.4-mini"
  , ChatConfig.reasoningEffort = "low"
  }

responseStart :: A.Value
responseStart = json "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-1\",\"status\":\"in_progress\"}}"

responseEnd :: [A.Value] -> A.Value
responseEnd output = A.object
  [ "type" A..= A.String "response.completed"
  , "response" A..= A.object
      [ "id" A..= A.String "resp-1", "status" A..= A.String "completed", "output" A..= output
      , "usage" A..= json "{\"input_tokens\":11,\"output_tokens\":7,\"total_tokens\":18,\"input_tokens_details\":{\"cached_tokens\":5},\"output_tokens_details\":{\"reasoning_tokens\":3}}"
      ]
  ]

responseMessage :: Text -> A.Value
responseMessage txt = A.object
  [ "type" A..= A.String "message", "id" A..= A.String "msg-1"
  , "role" A..= A.String "assistant", "status" A..= A.String "completed"
  , "phase" A..= A.String "final_answer"
  , "content" A..= [A.object ["type" A..= A.String "output_text", "text" A..= txt, "annotations" A..= ([] :: [A.Value])]]
  ]

responseCall :: Text -> Text -> Text -> A.Value
responseCall itemId callId args = A.object
  [ "type" A..= A.String "function_call", "id" A..= itemId, "call_id" A..= callId
  , "name" A..= A.String "weather", "arguments" A..= args, "status" A..= A.String "completed"
  ]

testResponsesRequest :: Assertion
testResponsesRequest = do
  let schema = json "{\"type\":\"object\",\"properties\":{\"optional\":{\"type\":\"string\"}}}"
      history = [systemText "system", ChatMessage "developer" (Just (TextContent "developer")) [] Nothing Nothing,
        userWithImages "hi" ["https://example.org/a.png", "data:image/png;base64,YQ=="], assistantText "earlier"]
  body <- right $ Responses.requestPayload responses [FunctionTool "weather" "Weather" schema] history
  field "store" body @?= A.Bool False
  field "stream" body @?= A.Bool True
  field "include" body @?= A.toJSON [A.String "reasoning.encrypted_content"]
  field "max_output_tokens" body @?= A.Number 16384
  field "reasoning" body @?= json "{\"effort\":\"low\"}"
  field "messages" body @?= A.Null
  let input = array (field "input" body)
  map (field "role") input @?= map A.String ["system","developer","user","assistant"]
  map (field "type") (array (field "content" (input Prelude.!! 2))) @?= map A.String ["input_text","input_image","input_image"]
  let function = array (field "tools" body) Prelude.!! 0
  field "parameters" function @?= schema
  field "strict" function @?= A.Bool False
  field "function" function @?= A.Null
  request <- Responses.httpRequest responses "test-secret" body
  HTTP.path request @?= "/v1/responses"
  Prelude.lookup "Authorization" (HTTP.requestHeaders request) @?= Just "Bearer test-secret"
  HTTP.redirectCount request @?= 0
  omitted <- right (Responses.requestPayload responses{ChatConfig.reasoningEffort = ""} [] [userText "hi"])
  field "reasoning" omitted @?= A.Null
  assertBool "missing tool call id fails" (isLeft (Responses.requestPayload responses [] [ChatMessage "tool" Nothing [] Nothing Nothing]))

testResponsesTools :: Assertion
testResponsesTools = do
  let reasoning = json "{\"type\":\"reasoning\",\"id\":\"rs-1\",\"summary\":[],\"encrypted_content\":\"opaque-reasoning\"}"
      output = [reasoning, responseMessage "Checking.", responseCall "fc-1" "call-a" "{\"city\":\"東京\"}", responseCall "fc-2" "call-b" "{}"]
      partial = json "{\"type\":\"response.function_call_arguments.delta\",\"output_index\":2,\"item_id\":\"fc-1\",\"delta\":\"{\\\"city\\\":\"}"
  (_, answer) <- right $ Responses.streamFromPayloads [responseStart, partial, responseEnd output]
  chatAnswerToolCalls answer @?= [ToolCall "call-a" "weather" "{\"city\":\"東京\"}", ToolCall "call-b" "weather" "{}"]
  chatAnswerTokenUsage answer @?= Just (TokenUsage 11 7 18 (Just 5))
  saved <- right (A.eitherDecode (A.encode (assistantAnswer answer)) :: Either String ChatMessage)
  saved.toolCalls @?= chatAnswerToolCalls answer
  saved.role @?= "assistant"
  body <- right $ Responses.requestPayload responses [] $
    [userText "weather", saved] <> map (\c -> toolResult c (if c.id == "call-a" then "11" else "29")) (reverse (chatAnswerToolCalls answer))
  let input = array (field "input" body)
  take 4 (drop 1 input) @?= output
  map (field "call_id") (drop 5 input) @?= [A.String "call-a", A.String "call-b"]
  map (field "output") (drop 5 input) @?= [A.String "11", A.String "29"]
  field "previous_response_id" body @?= A.Null
  -- Imported history must construct function_call items without borrowing another API's metadata.
  let imported = ChatMessage "assistant" Nothing [ToolCall "old-call" "weather" "{}"] Nothing
        (Just (ProviderState "anthropic" [A.String "must-not-leak"]))
  importedBody <- right (Responses.requestPayload responses [] [imported, toolResult (ToolCall "old-call" "weather" "{}") "old"])
  map (field "type") (array (field "input" importedBody)) @?= map A.String ["function_call","function_call_output"]
  assertBool "opaque state Show is redacted" (not ("opaque-reasoning" `T.isInfixOf` show saved))

testResponsesText :: Assertion
testResponsesText = do
  let txt = "Hello 東京!"
  for_ [0 .. T.length txt] $ \n -> do
    let delta t = A.object ["type" A..= A.String "response.output_text.delta", "delta" A..= t]
    (chunks, answer) <- right $ Responses.streamFromPayloads
      [responseStart, delta (T.take n txt), delta (T.drop n txt), responseEnd [responseMessage txt]]
    T.concat chunks @?= txt
    chatAnswerContent answer @?= txt
  let refusal = json "{\"type\":\"message\",\"id\":\"msg-1\",\"role\":\"assistant\",\"status\":\"completed\",\"content\":[{\"type\":\"refusal\",\"refusal\":\"Cannot help\"}]}"
  (chunks, answer) <- right $ Responses.streamFromPayloads
    [responseStart, json "{\"type\":\"response.refusal.delta\",\"delta\":\"Cannot\"}", responseEnd [refusal]]
  chunks @?= ["Cannot"," help"]
  chatAnswerContent answer @?= "Cannot help"
  -- A gateway may send only a completed snapshot; it must still produce visible text.
  (snapshot, _) <- right $ Responses.streamFromPayloads [json "{\"type\":\"keepalive\"}", responseStart, responseEnd [responseMessage "snapshot"]]
  snapshot @?= ["snapshot"]

testResponsesErrors :: Assertion
testResponsesErrors = do
  let call = responseCall "fc-1" "call-a" "{}"
      valid = [responseStart, json "{\"type\":\"response.output_item.done\"}", responseEnd [call]]
  for_ [0 .. length valid - 1] $ \n ->
    assertBool "every incomplete prefix fails" (isLeft (Responses.streamFromPayloads (take n valid)))
  for_ [ [responseStart, json "{\"type\":\"response.incomplete\"}"]
       , [responseStart, json "{\"type\":\"response.failed\"}"]
       , [responseStart, json "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-1\",\"status\":\"completed\",\"error\":{\"code\":\"server_error\"},\"output\":[]}}"]
       , [json "{\"type\":\"error\"}"]
       , [responseEnd [call]]
       , valid <> [responseEnd [call]]
       , [responseStart, responseStart, responseEnd []]
       , [responseStart, responseEnd [call, responseCall "fc-2" "call-a" "{}"]]
       , [responseStart, responseEnd [call, responseCall "fc-1" "call-b" "{}"]]
       , [responseStart, responseEnd [responseCall "fc-1" "call-a" "[]"]]
       , [responseStart, responseEnd [responseCall "fc-1" "" "{}"]]
       , [responseStart, responseEnd [json "{\"type\":\"message\",\"id\":\"x\",\"status\":\"in_progress\",\"role\":\"assistant\",\"content\":[]}"]]
       , [responseStart, responseEnd [json "{\"type\":\"web_search_call\",\"id\":\"x\"}"]]
       , [responseStart, json "{\"type\":\"response.output_text.delta\",\"delta\":\"wrong\"}", responseEnd [responseMessage "right"]]
       , [responseStart, json "{\"type\":\"response.completed\",\"response\":{\"id\":\"different\",\"status\":\"completed\",\"output\":[]}}"]
       , [responseStart, json "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-1\",\"status\":\"incomplete\",\"output\":[]}}"]
       ] $ \events -> assertBool "malformed output fails before tools execute" (isLeft (Responses.streamFromPayloads events))

testResponsesHTTP :: Assertion
testResponsesHTTP = do
  captured <- IORef.newIORef Nothing
  let events = [responseStart, responseEnd [responseCall "fc-1" "call-a" "{}"]]
      wire = foldMap (\v -> "event: ignored\r\ndata: " <> A.encode v <> "\r\n\r\n") events
      app req respond = do
        body <- Wai.strictRequestBody req
        IORef.writeIORef captured (Just (Wai.pathInfo req, Wai.requestHeaders req, body))
        respond (Wai.responseLBS Status.status200 [("Content-Type","text/event-stream")] wire)
  Warp.testWithApplication (pure app) $ \port -> do
    let cfg = responses{ChatConfig.baseUrl = "http://127.0.0.1:" <> show port <> "/v1", ChatConfig.apiKey = Just "key"}
    (_ S.:> answer) <- runEff $ Resource.runResource $ HTTPInterpreter.runHTTP $
      S.toList (Responses.askStreaming cfg [] [userText "hello"])
    chatAnswerToolCalls answer @?= [ToolCall "call-a" "weather" "{}"]
  IORef.readIORef captured >>= \case
    Just (path, headers, body) -> do
      path @?= ["v1","responses"]
      Prelude.lookup "Authorization" headers @?= Just "Bearer key"
      decoded <- right (A.eitherDecode body)
      field "store" decoded @?= A.Bool False
    Nothing -> assertFailure "request was not received"

  let incomplete _ respond = respond (Wai.responseLBS Status.status200 [("Content-Type","text/event-stream")]
        ("data: " <> A.encode responseStart <> "\n\n"))
  Warp.testWithApplication (pure incomplete) $ \port -> do
    result <- runEff $ Resource.runResource $ HTTPInterpreter.runHTTP $ trySync $
      S.toList (Responses.askStreaming responses{ChatConfig.baseUrl = "http://127.0.0.1:" <> show port, ChatConfig.apiKey = Just "key"} [] [userText "hi"])
    assertBool "Responses HTTP EOF cannot release a tool turn" (isLeft result)
