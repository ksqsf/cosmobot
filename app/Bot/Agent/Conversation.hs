{-|
Module      : Bot.Agent.Conversation
Description : Conversation shaping helpers for agent turns
Stability   : experimental
-}

module Bot.Agent.Conversation
  ( appendMessage
  , appendMessages
  , closeInterruptedToolCalls
  , pausedToolResult
  )
where

import Bot.Core.Conversation
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq

appendMessage :: LLM.ChatMessage -> Conversation -> Conversation
appendMessage message (Conversation messages) =
  Conversation (messages Seq.|> message)

appendMessages :: [LLM.ChatMessage] -> Conversation -> Conversation
appendMessages newMessages (Conversation messages) =
  Conversation (messages <> Seq.fromList newMessages)

-- | Synthetic tool result used when a real tool call is deliberately skipped.
pausedToolResult :: LLM.ToolCall -> LLM.ChatMessage
pausedToolResult call =
  LLM.toolResult call "Agent paused because the maximum tool turn limit was reached before this tool call could run. The user may continue the conversation to resume the work."

-- | Repair history that ended after an assistant tool-call message.
--
-- This covers interruption between receiving tool calls and appending their
-- results. The next run can then send the conversation to the LLM without
-- violating the tool-calling message protocol.
closeInterruptedToolCalls :: Conversation -> Conversation
closeInterruptedToolCalls (Conversation messages) =
  Conversation (Seq.fromList (go (Foldable.toList messages)))
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
