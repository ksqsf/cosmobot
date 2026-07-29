{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : Bot.Agent.Core
Description : Agent loop state and program middleware types
Stability   : experimental
-}

module Bot.Agent.Core
  ( Result (..)
  , Output (..)
  , TurnState (..)
  , Program (..)
  , Runtime (..)
  , Step (..)
  , ToolRequest (..)
  )
where

import Bot.Agent.Tool (Tool)
import Bot.Agent.ToolRegistry (RunningTool)
import Bot.Agent.Types
import Bot.Core.Transcript
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList

data Result = Result
  { runId :: !Text
  , transcript :: !Transcript
  , status :: !Text
  , finalText :: !Text
  , turnsUsed :: !Int
  , tokenUsage :: !(Maybe LLM.TokenUsage)
  }

data Output
  = ContentDelta !Text
  | ToolCallNotification !(NonEmpty LLM.ToolCall)
  | ReplyBoundary

-- | Mutable position of the agent loop.
data TurnState = TurnState
  { transcript    :: !Transcript
  , nextModelTranscript :: !(Maybe Transcript)
  , turn         :: !Int
  , modelTokenUsage :: !(Maybe LLM.TokenUsage)
  }

newtype Program es result = Program
  { observe :: Stream (Of Output) (Eff es) (Step es result)
  }

data Step es result
  = Finished !result
  | NeedsTools
      !ToolRequest
      !(TurnState -> Program es result)
  | Continues !(Program es result)

instance Functor (Program es) where
  fmap f (Program action) =
    Program (fmap (mapStep f) action)

instance Applicative (Program es) where
  pure result =
    Program (pure (Finished result))

  function <*> argument =
    function >>= \f -> fmap f argument

instance Monad (Program es) where
  Program action >>= next =
    Program do
      action >>= \case
        Finished result ->
          let Program nextAction = next result
          in nextAction
        NeedsTools request continue ->
          pure (NeedsTools request ((>>= next) . continue))
        Continues program ->
          pure (Continues (program >>= next))

mapStep
  :: (a -> b)
  -> Step es a
  -> Step es b
mapStep f = \case
  Finished result ->
    Finished (f result)
  NeedsTools request continue ->
    NeedsTools request (fmap f . continue)
  Continues program ->
    Continues (fmap f program)

-- | Runtime wiring for the agent algorithm.
--
-- The core loop stays as direct model/tool recursion, while cross-cutting
-- behavior gets named middleware boundaries. For example, transcript
-- compaction belongs in 'aroundModelTurn': it can rewrite state before the
-- next LLM request without changing tool execution or completion handling.
data Runtime (context :: [Type]) es = Runtime
  { runId :: !Text
  , toolCallMetadata :: !ToolCallMetadata
  , context :: Context
    -- | All configured tools, including tools unavailable to this request.
  , tools :: [Tool es]
    -- | Tools whose schemas are visible to the model.
  , exposedTools :: [Tool es]
    -- | Per-run tool implementations.
  , runningTools :: [RunningTool es]
    -- | Maximum number of model-requested tool turns.
  , maxTurns :: !Int
    -- | Select the transcript sent to the next model request. Most programs
    -- use the canonical transcript; middleware may expose a one-shot view.
  , modelInputTranscript :: HList.HList context -> TurnState -> Eff es Transcript
    -- | Wrap the whole coinductive program.
    --
    -- The final runtime is supplied so middleware uses every installed
    -- bracket, including wrappers added later in the composition chain.
  , aroundProgram :: Runtime '[] es -> Program es Result -> Program es Result
    -- | Wrap one complete agent run.
  , aroundAgentRun :: HList.HList context -> Stream (Of Output) (Eff es) Result -> Stream (Of Output) (Eff es) Result
    -- | Wrap one complete model phase.
    --
    -- Use this for model-side middleware such as transcript compaction,
    -- timing, auditing, or exception-aware behavior around the streamed model
    -- request plus decision.
  , aroundModelTurn
      :: HList.HList context
      -> (TurnState -> Program es Result)
      -> TurnState
      -> (TurnState -> Stream (Of Output) (Eff es) (Step es Result))
      -> Stream (Of Output) (Eff es) (Step es Result)
    -- | Wrap the whole tool phase.
    --
    -- Use this for cleanup, timing, timeout, auditing, or exception-aware
    -- behavior that must cover all tool calls in the phase.
  , aroundToolTurn
      :: forall a.
         HList.HList context
      -> ToolRequest
      -> Eff es (TurnState, a)
      -> Eff es (TurnState, a)
    -- | Wrap one model-requested tool call.
    --
    -- Use this for per-call observation, failure recovery, policy, or timing
    -- without replacing the default tool registry dispatch.
  , aroundToolCall :: Int -> LLM.ToolCall -> HList.HList context -> Eff es ToolResult -> Eff es ToolResult
  }

data ToolRequest = ToolRequest
  { agentState :: !TurnState
  , answered   :: !Transcript
  , toolContent :: !Text
  , toolCalls  :: !(NonEmpty LLM.ToolCall)
  }
