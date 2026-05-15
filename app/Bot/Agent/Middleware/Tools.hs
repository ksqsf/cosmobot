{-|
Module      : Bot.Agent.Middleware.Tools
Description : Tool-related agent program middleware
Stability   : experimental
-}
{-# LANGUAGE DataKinds #-}

module Bot.Agent.Middleware.Tools
  ( ToolLimitContext (..)
  , withToolFailureRecovery
  , withToolLimit
  , withToolMessage
  )
where

import Bot.Agent.Conversation
  ( appendMessages
  , pausedToolResult
  )
import Bot.Agent.Core
import Bot.Agent.Middleware.Observation.Types
import Bot.Agent.Types
import Bot.Core.Conversation
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Data.Text as Text

newtype ToolLimitContext = ToolLimitContext
  { maxToolTurns :: Int
  }
  deriving (Eq, Show)

withToolLimit :: Log :> es => Int -> AgentProgram (ToolLimitContext ': context) es -> AgentProgram context es
withToolLimit maxTurns program =
  program
    { aroundAgentRun = \context action ->
        program.aroundAgentRun (toolLimitContext HList.:& context) action
    , aroundModelTurn = \context agentState action -> do
        decision <- program.aroundModelTurn (toolLimitContext HList.:& context) agentState action
        case decision of
          ModelNeedsTools ToolTurnState{answered, toolContent, toolCalls}
            | agentState.turn >= toolLimitContext.maxToolTurns -> do
                lift $ logInfo_ [i|Agent tool turn limit reached: #{show toolCalls :: String}|]
                ModelAnswered <$> handleToolLimit program.agentRun.runId agentState.turn toolContent toolCalls answered
          _ ->
            pure decision
    , aroundToolTurn = \context toolState action ->
        program.aroundToolTurn (toolLimitContext HList.:& context) toolState action
    , aroundToolCall = \turn call context action ->
        program.aroundToolCall turn call (toolLimitContext HList.:& context) action
    }
  where
    toolLimitContext =
      ToolLimitContext{maxToolTurns = max 1 maxTurns}

withToolFailureRecovery :: IOE :> es => AgentProgram context es -> AgentProgram context es
withToolFailureRecovery program =
  program
    { aroundToolCall = \turn call context action ->
        safeToolCall call (program.aroundToolCall turn call context action)
    }

withToolMessage :: (Chat.Chat :> es, HList.Has ObservationContext context) => AgentProgram context es -> AgentProgram context es
withToolMessage program =
  program
    { aroundToolCall = \turn call context action -> do
        announceNoisyTool program call context
        program.aroundToolCall turn call context action
    }

announceNoisyTool :: (Chat.Chat :> es, HList.Has ObservationContext context) => AgentProgram context es -> LLM.ToolCall -> MiddlewareContext context -> Eff es ()
announceNoisyTool program call context =
  case find ((== call.name) . (.name)) program.agentRun.tools of
    Just tool
      | tool.noisy ->
          void $ Chat.replyTo program.agentRun.context.message (toolMessageText call context)
    _ ->
      pure ()

toolMessageText :: HList.Has ObservationContext context => LLM.ToolCall -> MiddlewareContext context -> Text
toolMessageText call context =
  case (HList.get @ObservationContext context).auditToolUseId of
    Just auditId ->
      [i|正在调用 #{toolName} 工具...（id=#{auditId}）|]
    Nothing ->
      [i|正在调用 #{toolName} 工具...|]
  where
    toolName = call.name

-- | Pause before executing another tool turn.
--
-- The assistant message already contains tool calls, and OpenAI-compatible
-- chat history requires every tool call to be followed by a tool result. We
-- therefore append synthetic "paused" tool results so the saved conversation is
-- valid when the user later continues.
handleToolLimit
  :: Text
  -> Int
  -> Text
  -> NonEmpty LLM.ToolCall
  -> Conversation
  -> Stream (Of Text) (Eff es) AgentCompletion
handleToolLimit runId turn content calls answered = do
  let paused = appendMessages (toList (fmap pausedToolResult calls)) answered
      message = toolLimitMessage content calls
  pure AgentCompletion
    { result = AgentResult{runId, answer = message, conversation = paused}
    , status = "tool_limit"
    , finalText = message
    , turnsUsed = turn
    }

safeToolCall :: IOE :> es => LLM.ToolCall -> Eff es ToolResult -> Eff es ToolResult
safeToolCall call action =
  action `catch` \(err :: SomeException) ->
    if isAsyncException err
      then throwIO err
      else pure (toolText [i|Tool #{callName} failed: #{show err :: String}|])
  where
    callName = call.name

-- | User-facing pause text returned when the tool-turn budget is exhausted.
toolLimitMessage :: Text -> NonEmpty LLM.ToolCall -> Text
toolLimitMessage content calls
  | Text.null stripped =
      [i|已暂停：本次 agent 工具调用轮数已用完，尚未执行下一步工具调用：#{toolCallList calls}

如果需要继续，请直接回复下一条消息。|]
  | otherwise =
      [i|#{stripped}

已暂停：本次 agent 工具调用轮数已用完，尚未执行下一步工具调用：#{toolCallList calls}

如果需要继续，请直接回复下一条消息。|]
  where
    stripped = Text.strip content

toolCallList :: NonEmpty LLM.ToolCall -> Text
toolCallList calls =
  Text.intercalate ", " (toList (fmap (.name) calls))
