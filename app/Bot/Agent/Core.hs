{-|
Module      : Bot.Agent.Core
Description : Agent loop state and program middleware types
Stability   : experimental
-}
{-# LANGUAGE DataKinds #-}

module Bot.Agent.Core
  ( AgentCompletion (..)
  , AgentProgram (..)
  , AgentResult (..)
  , AgentStreamOutput (..)
  , AgentRun (..)
  , AgentState (..)
  , ModelDecision (..)
  , ModelTurn
  , MiddlewareContext
  , ToolTurn
  , ToolTurnState (..)
  , emptyAgentProgram
  , runAgentLoop
  )
where

import Bot.Agent.ToolRegistry (RunningTool)
import Bot.Agent.Types
import Bot.Core.Conversation
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList

data AgentResult = AgentResult
  { runId :: !Text
  , answer :: !Text
  , conversation :: !Conversation
  }

data AgentStreamOutput
  = AgentAnswerDelta !Text
  | AgentIntermediateMessage !Text !Conversation

data AgentCompletion = AgentCompletion
  { result :: !AgentResult
  , status :: !Text
  , finalText :: !Text
  , turnsUsed :: !Int
  }

-- | Per-agent-run tool environment.
--
-- The original 'tools' list is kept so an unexpected or currently disallowed
-- tool call can produce a precise error. 'exposedTools' is the subset whose
-- schemas were sent to the model. 'runningTools' are the per-run tool runners;
-- each tool gets one 'start' call here, so tools may keep state local to this
-- agent run without putting it in 'AgentContext'.
data AgentRun es = AgentRun
  { runId       :: !Text
  , context      :: AgentContext es
  , tools        :: [Tool es]
  , exposedTools :: [Tool es]
  , runningTools :: [RunningTool es]
  }

-- | Mutable position of the agent loop.
data AgentState = AgentState
  { conversation :: !Conversation
  , turn         :: !Int
  }

data ModelDecision
  = ModelAnswered !AgentCompletion
  | ModelNeedsTools !ToolTurnState

type ModelTurn es =
  AgentState -> Stream (Of AgentStreamOutput) (Eff es) ModelDecision

type ToolTurn es =
  ToolTurnState -> Eff es AgentState

type MiddlewareContext context =
  HList.HList context

-- | Runtime wiring for the agent algorithm.
--
-- The core loop stays as direct model/tool recursion, while cross-cutting
-- behavior gets named middleware boundaries. For example, conversation
-- compaction belongs in 'aroundModelTurn': it can rewrite state before the
-- next LLM request without changing tool execution or completion handling.
data AgentProgram (context :: [Type]) es = AgentProgram
  { -- | Immutable per-run tool and request context.
    agentRun :: AgentRun es
    -- | Wrap one complete agent run.
  , aroundAgentRun :: MiddlewareContext context -> Stream (Of AgentStreamOutput) (Eff es) AgentCompletion -> Stream (Of AgentStreamOutput) (Eff es) AgentCompletion
    -- | Wrap one complete model phase.
    --
    -- Use this for model-side middleware such as conversation compaction,
    -- timing, auditing, or exception-aware behavior around the streamed model
    -- request plus decision.
  , aroundModelTurn :: MiddlewareContext context -> AgentState -> (AgentState -> Stream (Of AgentStreamOutput) (Eff es) ModelDecision) -> Stream (Of AgentStreamOutput) (Eff es) ModelDecision
    -- | Wrap the whole tool phase.
    --
    -- Use this for cleanup, timing, timeout, auditing, or exception-aware
    -- behavior that must cover all tool calls in the phase.
  , aroundToolTurn :: MiddlewareContext context -> ToolTurnState -> Eff es AgentState -> Eff es AgentState
    -- | Wrap one model-requested tool call.
    --
    -- Use this for per-call observation, failure recovery, policy, or timing
    -- without replacing the default tool registry dispatch.
  , aroundToolCall :: Int -> LLM.ToolCall -> MiddlewareContext context -> Eff es ToolResult -> Eff es ToolResult
  }

data ToolTurnState = ToolTurnState
  { agentState :: !AgentState
  , answered   :: !Conversation
  , toolContent :: !Text
  , toolCalls  :: !(NonEmpty LLM.ToolCall)
  }

emptyAgentProgram :: AgentRun es -> AgentProgram context es
emptyAgentProgram agentRun =
  AgentProgram
    { agentRun
    , aroundAgentRun = \_ action -> action
    , aroundModelTurn = \_ agentState action -> action agentState
    , aroundToolTurn = \_ _ action -> action
    , aroundToolCall = \_ _ _ action -> action
    }

runAgentLoop
  :: AgentProgram context es
  -> MiddlewareContext context
  -> ModelTurn es
  -> ToolTurn es
  -> AgentState
  -> Stream (Of AgentStreamOutput) (Eff es) AgentCompletion
runAgentLoop program context modelTurn toolTurn agentState = do
  program.aroundModelTurn context agentState modelTurn >>= \case
    ModelAnswered completion ->
      pure completion
    ModelNeedsTools toolState -> do
      continuedState <- lift (toolTurn toolState)
      runAgentLoop program context modelTurn toolTurn continuedState
