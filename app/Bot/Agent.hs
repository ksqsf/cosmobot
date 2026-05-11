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
runAgentStreaming maxTurns context tools conversation =
  loop (max 1 maxTurns) 0 (closeInterruptedToolCalls conversation)
  where
    exposedTools = filter (`toolAllowed` context) tools

    loop turnsLeft webFetchUses current = do
      answer <- LLM.askWithToolsStreaming (map toolSchema exposedTools) current.messages
      let answered = appendMessage (LLM.assistantAnswer answer) current
      case answer.toolCalls of
        [] ->
          pure (answer.content, answered)
        calls
          | turnsLeft <= 1 -> do
              lift $ logInfo "Agent tool turn limit reached" calls
              let paused = appendMessages (map pausedToolResult calls) answered
              pure (toolLimitMessage answer.content calls, paused)
          | otherwise -> do
              (results, nextWebFetchUses) <- lift $ executeCalls webFetchUses calls
              let next = appendMessages (map fst results) answered
              lift $ traverse_ (\messageId -> context.remember messageId next) (concatMap snd results)
              loop (turnsLeft - 1) nextWebFetchUses next

    executeCalls webFetchUses [] =
      pure ([], webFetchUses)
    executeCalls webFetchUses (call : calls) = do
      (result, nextWebFetchUses) <- execute webFetchUses call
      (rest, finalWebFetchUses) <- executeCalls nextWebFetchUses calls
      pure (result : rest, finalWebFetchUses)

    execute webFetchUses call = do
      let callName = call.name
          webFetchCall = callName == "web_fetch"
          webFetchLimit = context.toolConfig.webFetchMaxUses
      result <-
        if webFetchCall && maybe False (webFetchUses >=) webFetchLimit
          then pure (toolText [i|web_fetch use limit reached for this agent run: #{webFetchUses}.|])
          else runTool context tools call `catch` \(err :: SomeException) ->
            pure (toolText [i|Tool #{callName} failed: #{show err :: String}|])
      let nextWebFetchUses =
            if webFetchCall && maybe True (webFetchUses <) webFetchLimit
              then webFetchUses + 1
              else webFetchUses
      pure ((LLM.toolResult call result.content, result.messageIds), nextWebFetchUses)

toolLimitMessage :: Text -> [LLM.ToolCall] -> Text
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

toolCallList :: [LLM.ToolCall] -> Text
toolCallList calls =
  Text.intercalate ", " (map (.name) calls)

pausedToolResult :: LLM.ToolCall -> LLM.ChatMessage
pausedToolResult call =
  LLM.toolResult call "Agent paused because the maximum tool turn limit was reached before this tool call could run. The user may continue the conversation to resume the work."

closeInterruptedToolCalls :: Conversation -> Conversation
closeInterruptedToolCalls (Conversation messages) =
  Conversation (go messages)
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

runTool :: AgentContext es -> [Tool es] -> LLM.ToolCall -> Eff es ToolResult
runTool context tools call =
  case find ((== call.name) . (.name)) tools of
    Nothing ->
      pure (toolText [i|Unknown tool: #{callName}|])
    Just tool
      | not (toolAllowed tool context) ->
          pure (toolText [i|Permission denied for tool: #{callName}|])
      | otherwise ->
      case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 call.arguments) of
        Left err ->
          pure (toolText [i|Invalid JSON arguments for #{callName}: #{err}|])
        Right args ->
          tool.run context args
  where
    callName = call.name

toolAllowed :: Tool es -> AgentContext es -> Bool
toolAllowed tool context =
  tool.allowed context

appendMessage :: LLM.ChatMessage -> Conversation -> Conversation
appendMessage message (Conversation messages) =
  Conversation (messages <> [message])

appendMessages :: [LLM.ChatMessage] -> Conversation -> Conversation
appendMessages newMessages (Conversation messages) =
  Conversation (messages <> newMessages)
