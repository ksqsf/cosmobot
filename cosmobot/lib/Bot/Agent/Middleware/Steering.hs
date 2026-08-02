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
import Bot.Core.Message (MessageInput)
import Bot.Core.Transcript (Transcript, appendUserInput)
import Bot.Prelude
import qualified Streaming.Prelude as S

data SteeringControl es = SteeringControl
  { drain :: !(Eff es [MessageInput])
  , complete :: !(Eff es (Maybe [MessageInput]))
  }

withSteering
  :: SteeringControl es
  -> Runtime context (Eff es)
  -> Runtime context (Eff es)
withSteering steering program =
  program
    { aroundModelTurn = \context continue agentState action -> do
        pending <- lift steering.drain
        let steeredState = injectSteers pending agentState
        decision <- program.aroundModelTurn context continue steeredState action
        case decision of
          Finished completion ->
            lift steering.complete >>= \case
              Nothing ->
                pure decision
              Just steers -> do
                S.yield ReplyBoundary
                pure . Continues . continue $
                  injectCompletedSteers steers steeredState completion
          _ ->
            pure decision
    }

injectSteers
  :: [MessageInput]
  -> TurnState
  -> TurnState
injectSteers [] agentState =
  agentState
injectSteers steers agentState =
  agentState
    { transcript = appendSteers steers agentState.transcript
    , nextModelTranscript = appendSteers steers <$> agentState.nextModelTranscript
    }

injectCompletedSteers
  :: [MessageInput]
  -> TurnState
  -> Result
  -> TurnState
injectCompletedSteers steers agentState completion =
  agentState
    { transcript = appendSteers steers completion.transcript
    , nextModelTranscript = Nothing
    , modelTokenUsage = completion.tokenUsage
    }

appendSteers :: [MessageInput] -> Transcript -> Transcript
appendSteers steers transcript =
  foldl' (flip appendUserInput) transcript steers
