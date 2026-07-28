{-# LANGUAGE DataKinds #-}
{-|
Module      : Bot.Agent.Middleware.Steering
Description : Inject user steering between agent model turns
Stability   : experimental
-}
module Bot.Agent.Middleware.Steering
  ( SteeringControl (..)
  , withSteering
  )
where

import Bot.Agent.Core
import Bot.Agent.Middleware.ToolResultCompaction (NextModelInput (..))
import Bot.Agent.Tools.Continuation (ContinuationState, emptyContinuationState)
import Bot.Core.Transcript (Transcript, appendUser)
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Streaming.Prelude as S

data SteeringControl es = SteeringControl
  { drain :: !(Eff es [Text])
  , complete :: !(Eff es (Maybe [Text]))
  }

withSteering
  :: ( HList.Has ContinuationState transient
     , HList.Put ContinuationState transient
     , HList.Has NextModelInput transient
     , HList.Put NextModelInput transient
     )
  => SteeringControl es
  -> AgentProgram transient context es
  -> AgentProgram transient context es
withSteering steering program =
  program
    { aroundModelTurn = \context agentState action -> do
        pending <- lift steering.drain
        let steeredState = injectSteers pending agentState
        decision <- program.aroundModelTurn context steeredState action
        case decision of
          ModelAnswered completion ->
            lift steering.complete >>= \case
              Nothing ->
                pure decision
              Just steers -> do
                S.yield AgentReplyBoundary
                pure . ModelContinues $
                  injectCompletedSteers steers steeredState completion
          _ ->
            pure decision
    }

injectSteers
  :: ( HList.Has ContinuationState transient
     , HList.Put ContinuationState transient
     , HList.Has NextModelInput transient
     , HList.Put NextModelInput transient
     )
  => [Text]
  -> AgentState transient
  -> AgentState transient
injectSteers [] agentState =
  agentState
injectSteers steers agentState =
  agentState
    { transcript = appendSteers steers agentState.transcript
    , transient =
        clearContinuations
          . updateNextModelInput (appendSteers steers)
          $ agentState.transient
    }

injectCompletedSteers
  :: (HList.Put ContinuationState transient, HList.Put NextModelInput transient)
  => [Text]
  -> AgentState transient
  -> AgentCompletion
  -> AgentState transient
injectCompletedSteers steers agentState completion =
  agentState
    { transcript = appendSteers steers completion.result.transcript
    , modelTokenUsage = completion.tokenUsage
    , transient =
        HList.put (NextModelInput Nothing)
          . clearContinuations
          $ agentState.transient
    }

appendSteers :: [Text] -> Transcript -> Transcript
appendSteers steers transcript =
  foldl' (flip appendUser) transcript steers

updateNextModelInput
  :: (HList.Has NextModelInput transient, HList.Put NextModelInput transient)
  => (Transcript -> Transcript)
  -> HList.HList transient
  -> HList.HList transient
updateNextModelInput update transient =
  HList.put
    (NextModelInput (update <$> (HList.get @NextModelInput transient).transcript))
    transient

clearContinuations
  :: HList.Put ContinuationState transient
  => HList.HList transient
  -> HList.HList transient
clearContinuations =
  HList.put emptyContinuationState
