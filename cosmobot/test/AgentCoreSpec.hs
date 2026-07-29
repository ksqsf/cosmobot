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
  { program :: Program '[] Int
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
      ProgramCase <$> (visible <$> arbitrary <*> branch <*> branch)

propTauInvisible :: ProgramCase -> Property
propTauInvisible ProgramCase{program} =
  tau program ~= program

propVisCongruence :: Int -> ProgramCase -> ProgramCase -> Property
propVisCongruence key ProgramCase{program = onEven} ProgramCase{program = onOdd} =
  visible key (tau onEven) (tau onOdd)
    ~= visible key onEven onOdd

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
  :: Int
  -> ProgramCase
  -> ProgramCase
  -> Fun Int ProgramCase
  -> Property
propBindVis
  key
  ProgramCase{program = onEven}
  ProgramCase{program = onOdd}
  generated =
    (visible key onEven onOdd >>= next)
      ~= visible key (onEven >>= next) (onOdd >>= next)
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

arrow :: Fun Int ProgramCase -> Int -> Program '[] Int
arrow generated =
  (.program) . applyFun generated

tau :: Program '[] result -> Program '[] result
tau =
  Program . pure . Continues

emit :: Text -> Program '[] result -> Program '[] result
emit output next =
  Program do
    S.yield (ContentDelta output)
    next.observe

data Choice
  = ChooseEven
  | ChooseOdd
  deriving (Eq, Show, Enum, Bounded)

data Response
  = EvenResponse
  | OddResponse

trigger :: Int -> Program '[] Response
trigger key =
  Program . pure $
    NeedsTools
      (toolRequest key)
      (pure . responseFromState)

visible
  :: Int
  -> Program '[] result
  -> Program '[] result
  -> Program '[] result
visible key onEven onOdd =
  trigger key >>= \case
    EvenResponse -> onEven
    OddResponse -> onOdd

responseFromState :: TurnState -> Response
responseFromState turnState
  | even turnState.turn = EvenResponse
  | otherwise = OddResponse

-- Equality up to finite Tau: silently follow 'Continues', retain output and
-- visible-event order, and compare every response branch of a visible event.
(~=) :: (Eq result, Show result) => Program '[] result -> Program '[] result -> Property
left ~= right =
  counterexample "programs are not equivalent up to Tau" $
    eutt left right

eutt :: Eq result => Program '[] result -> Program '[] result -> Bool
eutt left right =
  case (observeVisible left, observeVisible right) of
    (Returned leftOutputs leftResult, Returned rightOutputs rightResult) ->
      leftOutputs == rightOutputs && leftResult == rightResult
    (Visible leftOutputs leftKey leftContinue, Visible rightOutputs rightKey rightContinue) ->
      leftOutputs == rightOutputs
        && leftKey == rightKey
        && all
          (\choice -> eutt (leftContinue (choiceState choice)) (rightContinue (choiceState choice)))
          [ChooseEven, ChooseOdd]
    _ ->
      False

data Visible result
  = Returned ![ObservedOutput] !result
  | Visible ![ObservedOutput] !Text !(TurnState -> Program '[] result)

observeVisible :: Program '[] result -> Visible result
observeVisible =
  runPureEff . go []
  where
    go prior program = do
      outputs S.:> step <- S.toList program.observe
      let observed = prior <> map observeOutput outputs
      case step of
        Finished result ->
          pure (Returned observed result)
        Continues next ->
          go observed next
        NeedsTools request continue ->
          pure (Visible observed request.toolContent continue)

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
    , toolCalls =
        LLM.ToolCall
          { id = [i|call-#{key}|]
          , name = "algebra"
          , arguments = "{}"
          }
          :| []
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
