{-# LANGUAGE DataKinds #-}

module Main (main) where

import Bot.Agent.Core
import Bot.Core.Transcript (Transcript (..))
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Sequence as Seq
import Prelude (Show (show))
import qualified Streaming.Prelude as S hiding (show)
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup "agent core interaction-tree laws"
      [ testProperty "Tau t ≈ t" propTauInvisible
      , testProperty "Vis is congruent when every continuation is" propVisCongruence
      , testProperty "lift pure ≈ pure" propLiftPure
      , testProperty "fmap agrees with bind and pure" propFmapIsBind
      , testProperty "bind preserves Tau" propBindTau
      , testProperty "bind distributes through Vis" propBindVis
      , testProperty "Kleisli associativity" propKleisliAssociativity
      ]

-- QuickCheck generates the real core type. The wrapper exists only because a
-- function-valued 'Program' cannot have ordinary 'Show' or 'Arbitrary'
-- instances. Generated visible events have two continuations, and 'eutt'
-- checks both.
newtype ProgramCase = ProgramCase
  { program :: Program (Eff '[]) Int
  }

instance Show ProgramCase where
  show _ =
    "<agent program>"

instance Arbitrary ProgramCase where
  arbitrary =
    sized generateProgram

  shrink _ =
    []

generateProgram :: Int -> Gen ProgramCase
generateProgram 0 =
  ProgramCase . pure <$> arbitrary
generateProgram size =
  frequency
    [ (3, ProgramCase . pure <$> arbitrary)
    , (2, tauCase)
    , (2, outputCase)
    , (3, visibleCase)
    ]
  where
    smaller =
      (.program) <$> generateProgram (size - 1)
    branch =
      (.program) <$> generateProgram (size `div` 2)
    tauCase =
      ProgramCase . tau <$> smaller
    outputCase = do
      value <- arbitrary :: Gen Int
      next <- smaller
      pure (ProgramCase (emit [i|output:#{value}|] next))
    visibleCase =
      ProgramCase <$> (visible <$> arbitrary <*> arbitrary <*> branch <*> branch)

propTauInvisible :: ProgramCase -> Property
propTauInvisible ProgramCase{program} =
  tau program ~= program

propVisCongruence :: Bool -> Int -> ProgramCase -> ProgramCase -> Property
propVisCongruence useModel key ProgramCase{program = onLeft} ProgramCase{program = onRight} =
  visible useModel key (tau onLeft) (tau onRight)
    ~= visible useModel key onLeft onRight

propLiftPure :: Int -> Property
propLiftPure value =
  (lift (pure value) :: Program (Eff '[]) Int) ~= pure value

propFmapIsBind :: ProgramCase -> Fun Int Int -> Property
propFmapIsBind ProgramCase{program} generated =
  fmap f program
    ~= (program >>= pure . f)
  where
    f = applyFun generated

propBindTau :: ProgramCase -> Fun Int ProgramCase -> Property
propBindTau ProgramCase{program} generated =
  (tau program >>= next)
    ~= tau (program >>= next)
  where
    next = arrow generated

propBindVis
  :: Bool
  -> Int
  -> ProgramCase
  -> ProgramCase
  -> Fun Int ProgramCase
  -> Property
propBindVis
  useModel
  key
  ProgramCase{program = onLeft}
  ProgramCase{program = onRight}
  generated =
    (visible useModel key onLeft onRight >>= next)
      ~= visible useModel key (onLeft >>= next) (onRight >>= next)
    where
      next = arrow generated

propKleisliAssociativity
  :: ProgramCase
  -> Fun Int ProgramCase
  -> Fun Int ProgramCase
  -> Property
propKleisliAssociativity ProgramCase{program} generatedF generatedG =
  ((program >>= f) >>= g)
    ~= (program >>= \value -> f value >>= g)
  where
    f = arrow generatedF
    g = arrow generatedG

arrow :: Fun Int ProgramCase -> Int -> Program (Eff '[]) Int
arrow generated =
  (.program) . applyFun generated

tau :: Program (Eff '[]) result -> Program (Eff '[]) result
tau =
  Program . pure . Continues

emit :: Text -> Program (Eff '[]) result -> Program (Eff '[]) result
emit output next =
  Program do
    S.yield (ContentDelta output)
    next.observe

data Choice
  = ChooseEven
  | ChooseOdd
  deriving (Eq, Show, Enum, Bounded)

toolVisible
  :: Int
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
toolVisible key onEven onOdd =
  trigger (RunTools (toolRequest key)) >>= \turnState ->
    if even turnState.turn then onEven else onOdd

modelVisible
  :: Int
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
modelVisible key onFinal onTools =
  trigger (RunModel (emptyState key)) >>= \(_, answer) ->
    case answer of
      LLM.ChatFinalAnswer{} -> onFinal
      LLM.ChatToolRequest{} -> onTools

visible
  :: Bool
  -> Int
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
visible useModel
  | useModel = modelVisible
  | otherwise = toolVisible

-- Equality up to finite Tau: silently follow 'Continues', retain output and
-- visible-event order, and compare every response branch of a visible event.
(~=) :: (Eq result, Show result) => Program (Eff '[]) result -> Program (Eff '[]) result -> Property
left ~= right =
  counterexample "programs are not equivalent up to Tau" $
    eutt left right

eutt :: Eq result => Program (Eff '[]) result -> Program (Eff '[]) result -> Bool
eutt left right =
  case (observeEvent left, observeEvent right) of
    (Returned leftOutputs leftResult, Returned rightOutputs rightResult) ->
      leftOutputs == rightOutputs && leftResult == rightResult
    (ObservedTools leftOutputs leftKey leftContinue, ObservedTools rightOutputs rightKey rightContinue) ->
      leftOutputs == rightOutputs
        && leftKey == rightKey
        && all
          (\choice -> eutt (leftContinue (choiceState choice)) (rightContinue (choiceState choice)))
          [ChooseEven, ChooseOdd]
    (ObservedModel leftOutputs leftTurn leftContinue, ObservedModel rightOutputs rightTurn rightContinue) ->
      leftOutputs == rightOutputs
        && leftTurn == rightTurn
        && all
          (\response -> eutt (leftContinue response) (rightContinue response))
          modelResponses
    _ ->
      False

data ObservedEvent result
  = Returned ![ObservedOutput] !result
  | ObservedTools ![ObservedOutput] !Text !(TurnState -> Program (Eff '[]) result)
  | ObservedModel ![ObservedOutput] !Int !((TurnState, LLM.ChatAnswer) -> Program (Eff '[]) result)

observeEvent :: Program (Eff '[]) result -> ObservedEvent result
observeEvent =
  runPureEff . go []
  where
    go
      :: [ObservedOutput]
      -> Program (Eff '[]) value
      -> Eff '[] (ObservedEvent value)
    go prior program = do
      outputs S.:> step <- S.toList program.observe
      let observed = prior <> map observeOutput outputs
      case step of
        Finished result ->
          pure (Returned observed result)
        Continues next ->
          go observed next
        Visible (RunTools request) continue ->
          pure (ObservedTools observed request.toolContent continue)
        Visible (RunModel turnState) continue ->
          pure (ObservedModel observed turnState.turn continue)

modelResponses :: [(TurnState, LLM.ChatAnswer)]
modelResponses =
  [ (emptyState 0, LLM.ChatFinalAnswer "answer" Nothing)
  , (emptyState 1, LLM.ChatToolRequest "" (toolCall 0 :| []) Nothing)
  ]

data ObservedOutput
  = ObservedContent !Text
  | ObservedToolCalls ![(Text, Text)]
  | ObservedReplyBoundary
  deriving (Eq, Show)

observeOutput :: Output -> ObservedOutput
observeOutput = \case
  ContentDelta output ->
    ObservedContent output
  ToolCallNotification calls ->
    ObservedToolCalls [(call.id, call.name) | call <- toList calls]
  ReplyBoundary ->
    ObservedReplyBoundary

choiceState :: Choice -> TurnState
choiceState = \case
  ChooseEven ->
    emptyState 0
  ChooseOdd ->
    emptyState 1

toolRequest :: Int -> ToolRequest
toolRequest key =
  ToolRequest
    { agentState = emptyState 0
    , answered = emptyTranscript
    , toolContent = [i|#{key}|]
    , toolCalls = toolCall key :| []
    }

toolCall :: Int -> LLM.ToolCall
toolCall key =
  LLM.ToolCall
    { id = [i|call-#{key}|]
    , name = "algebra"
    , arguments = "{}"
    }

emptyState :: Int -> TurnState
emptyState currentTurn =
  TurnState
    { transcript = emptyTranscript
    , nextModelTranscript = Nothing
    , turn = currentTurn
    , modelTokenUsage = Nothing
    }

emptyTranscript :: Transcript
emptyTranscript =
  Transcript Seq.empty
