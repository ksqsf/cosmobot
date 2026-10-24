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

import qualified Bot.Effect.Agent as Agent
import qualified Effectful.Concurrent.Async as Async
import Bot.Agent.Types (BotRuntime)
import Bot.Chat.Transcript (appendUserInput)
import Bot.Agent.Core
import Bot.Core.Message (MessageInput)
import Bot.Core.Transcript (Transcript)
import Bot.Prelude
import qualified Streaming.Prelude as S

data SteeringControl es = SteeringControl
  { awaitPending :: !(Eff es ())
  , drain :: !(Eff es [MessageInput])
  , complete :: !(Eff es (Maybe [MessageInput]))
  }

withSteering
  :: (Agent.Agent :> es, Concurrent :> es)
  => SteeringControl es
  -> BotRuntime context (Eff es)
  -> BotRuntime context (Eff es)
withSteering steering program =
  program
    { aroundProgram = \finalRuntime ->
        program.aroundProgram finalRuntime . steerProgram completing
    , aroundToolCall = \turn call context action ->
        program.aroundToolCall turn call context (interruptWaits steering action)
    , aroundControlCall = \turn call context action ->
        program.aroundControlCall turn call context (interruptWaits steering action)
    , aroundModelTurn = \context agentState action -> do
        pending <- lift steering.drain
        let steeredState = injectSteers pending agentState
        program.aroundModelTurn context steeredState action
    }

  where
    completing = steering
      { complete = do
          pending <- Agent.hasPendingChildren program.runId
          if pending then pure Nothing else steering.complete
      }

interruptWaits
  :: (Agent.Agent :> es, Concurrent :> es)
  => SteeringControl es -> Eff es a -> Eff es a
interruptWaits steering = interpose (\localEnv -> \case
  Agent.Wait action ->
    localUnlift localEnv (ConcUnlift Persistent Unlimited) \unlift ->
      either (const Nothing) Just <$> Async.race steering.awaitPending (unlift action)
  operation -> passthrough localEnv operation)

steerProgram
  :: SteeringControl es
  -> Program (Eff es) Result
  -> Program (Eff es) Result
steerProgram steering (Program action) =
  Program do
    action >>= \case
      Finished result ->
        pure (Finished result)
      Continues next ->
        pure (Continues (steerProgram steering next))
      Visible event@(RunModel _) continue ->
        pure (Visible event (completeModel continue))
      Visible event continue ->
        pure (Visible event (steerProgram steering . continue))
  where
    completeModel continue response@(agentState, _) =
      Program do
        (continue response).observe >>= \case
          Finished completion ->
            lift steering.complete >>= \case
              Nothing ->
                pure (Finished completion)
              Just steers -> do
                S.yield ReplyBoundary
                pure . Continues . steerProgram steering $
                  runModel (injectCompletedSteers steers agentState completion) >>= continue
          Continues next ->
            pure (Continues (steerProgram steering next))
          Visible event next ->
            pure (Visible event (steerProgram steering . next))

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
