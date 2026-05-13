{-|
Module      : Bot.Agent.Middleware.Tools
Description : Tool-related agent program middleware
Stability   : experimental
-}

module Bot.Agent.Middleware.Tools
  ( withToolFailureRecovery
  , withToolLimit
  )
where

import Bot.Agent.Conversation
  ( appendMessages
  , pausedToolResult
  )
import Bot.Agent.Core
import Bot.Agent.Types
import Bot.Core.Conversation
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Text as Text

withToolLimit :: Log :> es => Int -> AgentProgram es -> AgentProgram es
withToolLimit maxTurns program =
  program
    { aroundModelTurn = \agentState action -> do
        decision <- program.aroundModelTurn agentState action
        case decision of
          ModelNeedsTools ToolTurnState{answered, toolContent, toolCalls}
            | agentState.turn >= max 1 maxTurns -> do
                lift $ logInfo_ [i|Agent tool turn limit reached: #{show toolCalls :: String}|]
                ModelAnswered <$> handleToolLimit program.agentRun.runId agentState.turn toolContent toolCalls answered
          _ ->
            pure decision
    }

withToolFailureRecovery :: IOE :> es => AgentProgram es -> AgentProgram es
withToolFailureRecovery program =
  program
    { aroundToolCall = \turn call action ->
        safeToolCall call (program.aroundToolCall turn call action)
    }

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
