{-|
Module      : Bot.Agent.Transcript
Description : Transcript shaping helpers for agent turns
Stability   : experimental
-}

module Bot.Agent.Transcript
  ( appendMessage
  , appendMessages
  , closeInterruptedToolCalls
  , pausedToolResult
  )
where

import Bot.Core.Transcript
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq

appendMessage :: LLM.ChatMessage -> Transcript -> Transcript
appendMessage message (Transcript messages) =
  Transcript (messages Seq.|> message)

appendMessages :: [LLM.ChatMessage] -> Transcript -> Transcript
appendMessages newMessages (Transcript messages) =
  Transcript (messages <> Seq.fromList newMessages)

-- | Synthetic tool result used when a real tool call is deliberately skipped.
pausedToolResult :: LLM.ToolCall -> LLM.ChatMessage
pausedToolResult call =
  LLM.toolResult call "Agent paused because the maximum tool turn limit was reached before this tool call could run. The user may continue the thread to resume the work."

-- | Repair history that ended after an assistant tool-call message.
--
-- This covers interruption between receiving tool calls and appending their
-- results. The next run can then send the transcript to the LLM without
-- violating the tool-calling message protocol.
closeInterruptedToolCalls :: Transcript -> Transcript
closeInterruptedToolCalls (Transcript messages) =
  Transcript (Seq.fromList (go (Foldable.toList messages)))
  where
    go [] = []
    go (message : rest)
      | message.role == "assistant" && not (null message.toolCalls) =
          let (toolResults, remaining) = span isToolResult rest
              existingIds = mapMaybe (.toolCallId) toolResults
              missingCalls = filter ((`notElem` existingIds) . (.id)) message.toolCalls
          in message : toolResults <> map pausedToolResult missingCalls <> go remaining
      | otherwise =
          message : go rest

    isToolResult message =
      message.role == "tool"
