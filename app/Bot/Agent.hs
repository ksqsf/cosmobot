{-|
Module      : Bot.Agent
Description : Agent loop and extensible tool framework
Stability   : experimental
-}

module Bot.Agent
  ( Tool (..)
  , AgentContext (..)
  , ToolConfig (..)
  , WebSearchApi (..)
  , defaultToolConfig
  , ToolResult (..)
  , runAgent
  , runAgentStreaming
  , defaultTools
  )
where

import Bot.Core.Conversation
import Bot.Agent.Tools (defaultTools)
import Bot.Agent.Types
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Streaming.Prelude as S

-- | Run an LLM/tool loop until the model answers or the tool turn limit is hit.
runAgent
  :: (LLM.LLM :> es, Log :> es, IOE :> es)
  => Int
  -> AgentContext es
  -> [Tool es]
  -> Conversation
  -> Eff es (Text, Conversation)
runAgent maxTurns context tools conversation =
  S.mapM_ (\_ -> pure ()) (runAgentStreaming maxTurns context tools conversation)

-- | Run an LLM/tool loop, streaming assistant text chunks from the final model turn.
runAgentStreaming
  :: (LLM.LLM :> es, Log :> es, IOE :> es)
  => Int
  -> AgentContext es
  -> [Tool es]
  -> Conversation
  -> Stream (Of Text) (Eff es) (Text, Conversation)
runAgentStreaming maxTurns context tools conversation = do
  agentRun <- lift (startAgentRun context tools)
  agentLoop agentRun AgentState
    { turnsLeft = max 1 maxTurns
    , conversation = closeInterruptedToolCalls conversation
    }

-- | Per-agent-run tool environment.
--
-- The original 'tools' list is kept so an unexpected or currently disallowed
-- tool call can produce a precise error. 'exposedTools' is the subset whose
-- schemas were sent to the model. 'runningTools' are the per-run tool runners;
-- each tool gets one 'start' call here, so tools may keep state local to this
-- agent run without putting it in 'AgentContext'.
data AgentRun es = AgentRun
  { context      :: AgentContext es
  , tools        :: [Tool es]
  , exposedTools :: [Tool es]
  , runningTools :: [RunningTool es]
  }

-- | Mutable position of the agent loop.
data AgentState = AgentState
  { turnsLeft    :: !Int
  , conversation :: !Conversation
  }

-- | A completed tool call as both an LLM-visible message and any chat message
-- ids that should alias the updated conversation snapshot.
data ToolExecution = ToolExecution
  { message    :: !LLM.ChatMessage
  , messageIds :: ![Maybe Integer]
  }

-- | Select tools visible to this request and start their per-run runners.
startAgentRun :: AgentContext es -> [Tool es] -> Eff es (AgentRun es)
startAgentRun context tools = do
  let exposedTools = filter (`toolAllowed` context) tools
  runningTools <- traverse (startToolRun context) exposedTools
  pure AgentRun{context, tools, exposedTools, runningTools}

-- | Main agent loop.
--
-- Each turn asks the model with the full conversation and currently exposed
-- tool schemas. A final assistant answer ends the loop. If the model asks for
-- tools, the assistant tool-call message is appended first, then matching tool
-- result messages are appended before the next model turn.
agentLoop
  :: (LLM.LLM :> es, Log :> es, IOE :> es)
  => AgentRun es
  -> AgentState
  -> Stream (Of Text) (Eff es) (Text, Conversation)
agentLoop agentRun agentState = do
  answer <- askNext agentRun agentState.conversation
  let answered = appendMessage (LLM.assistantAnswer answer) agentState.conversation
  case answer of
    LLM.ChatFinalAnswer{content} ->
      pure (content, answered)
    LLM.ChatToolRequest{content, toolCalls}
      | agentState.turnsLeft <= 1 ->
          handleToolLimit content toolCalls answered
      | otherwise -> do
          unless (Text.null content) (S.yield content)
          next <- lift (continueWithToolCalls agentRun answered toolCalls)
          agentLoop agentRun agentState{turnsLeft = agentState.turnsLeft - 1, conversation = next}

-- | Ask the LLM for the next assistant message.
askNext
  :: (LLM.LLM :> es, IOE :> es)
  => AgentRun es
  -> Conversation
  -> Stream (Of Text) (Eff es) LLM.ChatAnswer
askNext agentRun conversation =
  LLM.askWithToolsStreaming
    (map toolSchema agentRun.exposedTools)
    (Foldable.toList conversation.messages)

-- | Pause before executing another tool turn.
--
-- The assistant message already contains tool calls, and OpenAI-compatible
-- chat history requires every tool call to be followed by a tool result. We
-- therefore append synthetic "paused" tool results so the saved conversation is
-- valid when the user later continues.
handleToolLimit
  :: Log :> es
  => Text
  -> NonEmpty LLM.ToolCall
  -> Conversation
  -> Stream (Of Text) (Eff es) (Text, Conversation)
handleToolLimit content calls answered = do
  lift $ logInfo "Agent tool turn limit reached" calls
  let paused = appendMessages (toList (fmap pausedToolResult calls)) answered
  pure (toolLimitMessage content calls, paused)

-- | Execute requested tools, append their tool-result messages, and persist
-- aliases for any chat messages emitted by tools.
continueWithToolCalls
  :: IOE :> es
  => AgentRun es
  -> Conversation
  -> NonEmpty LLM.ToolCall
  -> Eff es Conversation
continueWithToolCalls agentRun answered calls = do
  executions <- traverse (executeToolCall agentRun) calls
  let next = appendMessages (toList (fmap (.message) executions)) answered
  traverse_ (\messageId -> agentRun.context.remember messageId next) (concatMap (.messageIds) (toList executions))
  pure next

-- | Run one tool call and convert failures into tool-visible text.
--
-- Tool failures must still produce a tool result message; otherwise the next
-- LLM request would contain an assistant tool call without its required result.
executeToolCall :: IOE :> es => AgentRun es -> LLM.ToolCall -> Eff es ToolExecution
executeToolCall agentRun call = do
  let callName = call.name
  result <-
    runToolCall agentRun call `catch` \(err :: SomeException) ->
      pure (toolText [i|Tool #{callName} failed: #{show err :: String}|])
  pure ToolExecution
    { message = LLM.toolResult call result.content
    , messageIds = result.messageIds
    }

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

toolSchema :: Tool es -> LLM.FunctionTool
toolSchema Tool{name, description, parameters} =
  LLM.FunctionTool
    { name = name
    , description = description
    , parameters = parameters
    }

-- | A tool runner bound to one agent run.
data RunningTool es = RunningTool
  { name :: !Text
  , run  :: Aeson.Value -> Eff es ToolResult
  }

-- | Start a tool for this agent run.
startToolRun :: AgentContext es -> Tool es -> Eff es (RunningTool es)
startToolRun context Tool{name, start} = do
  run <- start context
  pure RunningTool{name, run}

-- | Resolve a model tool call, decode its JSON arguments, and invoke the
-- per-run runner.
runToolCall :: AgentRun es -> LLM.ToolCall -> Eff es ToolResult
runToolCall agentRun call =
  case find ((== call.name) . (.name)) agentRun.runningTools of
    Nothing ->
      case find ((== call.name) . (.name)) agentRun.tools of
        Just tool | not (toolAllowed tool agentRun.context) ->
          pure (toolText [i|Permission denied for tool: #{callName}|])
        _ ->
          pure (toolText [i|Unknown tool: #{callName}|])
    Just tool ->
      case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 call.arguments) of
        Left err ->
          pure (toolText [i|Invalid JSON arguments for #{callName}: #{err}|])
        Right args ->
          tool.run args
  where
    callName = call.name

toolAllowed :: Tool es -> AgentContext es -> Bool
toolAllowed tool context =
  tool.allowed context

appendMessage :: LLM.ChatMessage -> Conversation -> Conversation
appendMessage message (Conversation messages) =
  Conversation (messages Seq.|> message)

appendMessages :: [LLM.ChatMessage] -> Conversation -> Conversation
appendMessages newMessages (Conversation messages) =
  Conversation (messages <> Seq.fromList newMessages)
