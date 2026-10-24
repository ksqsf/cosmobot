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
  [ testCase "Anthropic HTTP/SSE integration" testHTTP
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
