{-# LANGUAGE DataKinds #-}
module Main (main) where

import Bot.Agent.Core hiding (runModel)
import Bot.Agent.Program (agentProgram, limitProgram, runProgramWith)
import Bot.Agent.Result
import Bot.Agent.Runtime
import Bot.Agent.Tool
import Bot.Agent.Transcript (closeInterruptedToolCalls)
import Bot.Core.Transcript
import qualified Bot.Effect.LLM as LLM
import qualified Bot.LLM.Test as LLMTest
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import qualified Streaming.Prelude as S
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "standalone core"
  [ testCase "program needs only identity and an event interpreter" testProgramWithoutRuntime
  , testCase "runtime executes tools with non-bot context and metadata" testRuntime
  , testCase "model transcript projection does not replace canonical history" testProjection
  , testCase "assistant text is not interpreted as a bot directive" testRawTranscript
  , testCase "tool limits stop at the boundary and allow explicit transfers" testLimit
  , testProperty "partial tool exchanges repair in call order and are idempotent" propRepair
  ]

type TestEffects = '[LLM.LLM, KatipE, Concurrent, IOE]

runModel
  :: ([LLM.FunctionTool] -> [LLM.ChatMessage] -> Stream (Of Text) (Eff '[KatipE, Concurrent, IOE]) LLM.ChatAnswer)
  -> Eff TestEffects a
  -> IO a
runModel model = runEff . runConcurrent . startKatipE "core-runtime-spec" "test"
  . LLMTest.runLLMWith (const (pure "")) (\_ _ -> pure "")
      (\_ _ _ _ -> pure "") (\_ _ -> pure "") model

testRuntime :: Assertion
testRuntime = do
  result <- runModel model do
    runtime <- startRuntime "standalone" 4 True (37 :: Int) [definition]
    S.effects $ runProgram (\_ -> Foldable.toList . (.messages)) runtime
      (agentProgram runtime.runId (initialState (startWithUser "hello")))
  result.status @?= "answered"
  result.turnsUsed @?= 2
  let messages = Foldable.toList result.transcript.messages
  map (.role) messages @?= ["user", "assistant", "tool", "assistant"]
  mapMaybe (.toolCallId) messages @?= ["call-1"]
  mapMaybe (.content) (filter ((== "tool") . (.role)) messages)
    & Aeson.toJSON & (@?= Aeson.toJSON [LLM.TextContent "True:37:1"])
  where
    definition :: Tool Bool Int (Eff TestEffects)
    definition = tool "probe" noArguments do
      host <- askToolContext @Bool @Int
      metadata <- askToolCallMetadata @Bool @Int
      turn <- askToolTurn @Bool @Int
      pure (toolText [i|#{show host :: String}:#{metadata}:#{turn}|])
    model schemas messages = do
      liftIO $ map (.name) schemas @?= ["probe"]
      pure $ if any ((== "tool") . (.role)) messages
        then LLM.ChatFinalAnswer "done" Nothing Nothing
        else LLM.ChatToolRequest "" (LLM.ToolCall "call-1" "probe" "{}" :| []) Nothing Nothing

testProjection :: Assertion
testProjection = do
  result <- runModel model do
    runtime <- startRuntime "projection" 2 () () []
    let projected = runtime{modelInputTranscript = \_ _ -> pure (startWithUser "projected")}
    S.effects $ runProgram (\_ -> Foldable.toList . (.messages)) projected
      (agentProgram projected.runId (initialState (startWithUser "canonical")))
  Aeson.toJSON (Foldable.toList result.transcript.messages)
    @?= Aeson.toJSON [LLM.userText "canonical", LLM.assistantText "done"]
  where
    model _ messages = do
      liftIO $ Aeson.toJSON messages @?= Aeson.toJSON [LLM.userText "projected"]
      pure (LLM.ChatFinalAnswer "done" Nothing Nothing)

testRawTranscript :: Assertion
testRawTranscript = do
  let raw = "[image] https://example.invalid/unchanged.png"
  Aeson.toJSON (Foldable.toList (appendAssistant raw (startWithUser "hello")).messages)
    @?= Aeson.toJSON [LLM.userText "hello", LLM.assistantText raw]

testLimit :: Assertion
testLimit = for_ [(1, False, False), (2, False, True), (3, False, True), (2, True, False), (3, True, False)] $
  \(turn, transfer, shouldStop) -> do
    let agentState = (initialState (startWithUser "hello")){turn}
        request = ToolRequest agentState agentState.transcript "" (LLM.ToolCall "c" "probe" "{}" :| [])
        result = Result "limit-test" agentState.transcript "stopped" "" turn Nothing
        program = trigger (RunTools request) *> pure result
        limited = limitProgram 2 (const transfer) (const (pure (Finished result))) program
        step = runPureEff (S.effects limited.observe)
    case step of
      Finished _ -> assertBool "stopped below limit or during transfer" shouldStop
      Visible (RunTools _) _ -> assertBool "executed at or above limit" (not shouldStop)
      _ -> assertFailure "unexpected program step"

propRepair :: NonEmptyList Bool -> Property
propRepair (NonEmpty present) =
  let calls = zipWith (\n _ -> LLM.ToolCall (show n) "probe" "{}") [(1 :: Int)..] present
      answers = [LLM.toolResult call "existing" | (call, True) <- reverse (zip calls present)]
      transcript = Transcript (Seq.fromList
        (LLM.assistantAnswer (LLM.ChatToolRequest "" (NonEmpty.fromList calls) Nothing Nothing) : answers))
      repaired = closeInterruptedToolCalls transcript
      messages = Foldable.toList repaired.messages
      expectedResults = map (\exists ->
        if exists then Just (LLM.TextContent "existing") else Nothing) present
  in conjoin
      [ mapMaybe (.toolCallId) messages === map (.id) calls
      , Aeson.toJSON (closeInterruptedToolCalls repaired) === Aeson.toJSON repaired
      , conjoin [Aeson.toJSON actual.content === Aeson.toJSON (Just expected)
                | (actual, Just expected) <- zip (drop 1 messages) expectedResults]
      ]

-- The calculus can finish or reject a response without constructing a runtime.
testProgramWithoutRuntime :: Assertion
testProgramWithoutRuntime =
  for_ [ (LLM.ChatFinalAnswer "done" Nothing Nothing, "answered", 2)
       , (LLM.ChatToolRequest "" (LLM.ToolCall " " "probe" "{}" :| []) Nothing Nothing, "model_protocol_error", 1)
       , (LLM.ChatToolRequest "" (LLM.ToolCall "same" "a" "{}" :| [LLM.ToolCall "same" "b" "{}"]) Nothing Nothing, "model_protocol_error", 1)
       ] $ \(answer, status, messageCount) -> do
    let interpretEvent :: AgentEvent response -> Stream (Of Output) (Eff '[]) response
        interpretEvent (RunModel agentState) = pure (agentState, answer)
        interpretEvent (RunTools _) = error "invalid calls must not reach tool execution"
        result = runPureEff . S.effects $
          runProgramWith (const (pure ())) (pure ()) interpretEvent
            (agentProgram "pure-program" (initialState (startWithUser "hello")))
    result.runId @?= "pure-program"
    result.status @?= status
    length result.transcript.messages @?= messageCount
