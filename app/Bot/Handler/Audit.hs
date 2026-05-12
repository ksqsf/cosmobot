{-|
Module      : Bot.Handler.Audit
Description : Superuser audit commands for agent tool use
Stability   : experimental
-}

module Bot.Handler.Audit
  ( auditHandlers
  )
where

import Bot.Core.Message
import Bot.Core.Conversation
import Bot.Core.Route
import qualified Bot.Effect.AgentTrace as AgentTrace
import qualified Bot.Effect.Chat as Chat
import Bot.Prelude
import qualified Data.Text as Text
import Data.Time (FormatTime, defaultTimeLocale, formatTime)

auditHandlers
  :: (AgentTrace.AgentTrace :> es, Chat.Chat :> es, IOE :> es)
  => ConversationStore
  -> [RouteHandler es]
auditHandlers conversations =
  [ requireAuth isSuperuser (\message -> void $ Chat.replyTo message "只有 superuser 可以查看 audit。") $
      stopOn (command "!audit") (handleAudit conversations)
  ]

handleAudit
  :: (AgentTrace.AgentTrace :> es, Chat.Chat :> es, IOE :> es)
  => ConversationStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleAudit conversations message args =
  case parseAuditId args of
    Nothing
      | Text.toLower (Text.strip args) == "log" ->
          case message.replyToMessageId of
            Just parentId -> do
              records <- AgentTrace.queryConversationTrace parentId
              void $ Chat.replyTo message (renderConversationTraceLog parentId records)
            Nothing ->
              void $ Chat.replyTo message "用法：回复一条 agent conversation 消息并发送 !audit log"
      | Text.toLower (Text.strip args) == "all" ->
          case message.replyToMessageId of
            Just parentId -> do
              messageIds <- lookupConversationMessageIds conversations parentId
              records <- AgentTrace.queryConversationMessagesTrace messageIds
              void $ Chat.replyTo message (renderConversationToolUses parentId records)
            Nothing ->
              void $ Chat.replyTo message "用法：回复一条 agent conversation 消息并发送 !audit all"
      | Text.null (Text.strip args) ->
          case message.replyToMessageId of
            Just parentId -> do
              records <- AgentTrace.queryConversationTrace parentId
              void $ Chat.replyTo message (renderConversationToolUses parentId records)
            Nothing -> do
              toolUses <- AgentTrace.queryRecentToolUses 50
              void $ Chat.replyTo message (renderAuditList toolUses)
      | otherwise ->
          void $ Chat.replyTo message "用法：!audit、!audit all、!audit log 或 !audit <id>"
    Just auditId -> do
      detail <- AgentTrace.queryToolUse auditId
      void $ Chat.replyTo message (maybe [i|没有找到 audit id #{auditId}。|] renderAuditDetail detail)

parseAuditId :: Text -> Maybe Integer
parseAuditId =
  readMaybe . toString . Text.strip

renderAuditList :: [AgentTrace.ToolUseDetail] -> Text
renderAuditList [] =
  "最近没有 agent tool use。"
renderAuditList toolUses =
  Text.unlines (map renderToolUseLine toolUses)

renderToolUseLine :: AgentTrace.ToolUseDetail -> Text
renderToolUseLine toolUse =
  let auditId = toolUse.auditId
      toolName = toolUse.toolName
      toolCallId = toolUse.toolCallId
  in
  Text.unwords
    [ timestamp toolUse.occurredAt
    , [i|id=#{auditId}|]
    , [i|tool=#{toolName}|]
    , [i|request=#{toolCallId}|]
    , renderStatus toolUse.status
    ]

renderAuditDetail :: AgentTrace.ToolUseDetail -> Text
renderAuditDetail toolUse =
  let auditId = toolUse.auditId
      turn = toolUse.turn
  in
  Text.unlines
    [ [i|id: #{auditId}|]
    , "started_at: " <> timestamp toolUse.occurredAt
    , "finished_at: " <> maybe "(still running)" timestamp toolUse.finishedAt
    , "run_id: " <> toolUse.runId
    , [i|turn: #{turn}|]
    , "tool: " <> toolUse.toolName
    , "tool_request_id: " <> toolUse.toolCallId
    , "status: " <> renderStatus toolUse.status
    , "message_ids: " <> show toolUse.messageIds
    , "arguments:"
    , toolUse.arguments
    , "result:"
    , fromMaybe "(still running)" toolUse.result
    ]

renderConversationToolUses :: Integer -> [AgentTrace.AgentTraceRecord] -> Text
renderConversationToolUses parentId [] =
  [i|没有找到消息 #{parentId} 对应的 agent trace。|]
renderConversationToolUses _ records =
  case AgentTrace.toolUsesFromTraceRecords records of
    [] ->
      "该 agent trace 中没有 tool use。"
    toolUses ->
      Text.intercalate "\n" (map renderToolUseBlock toolUses)

renderToolUseBlock :: AgentTrace.ToolUseDetail -> Text
renderToolUseBlock toolUse =
  let auditId = toolUse.auditId
      turn = toolUse.turn
      resultChars = maybe 0 Text.length toolUse.result
  in
  Text.unlines
    [ [i|id: #{auditId}|]
    , "started_at: " <> timestamp toolUse.occurredAt
    , "finished_at: " <> maybe "(still running)" timestamp toolUse.finishedAt
    , "run_id: " <> toolUse.runId
    , [i|turn: #{turn}|]
    , "tool: " <> toolUse.toolName
    , "tool_request_id: " <> toolUse.toolCallId
    , "status: " <> renderStatus toolUse.status
    , [i|result_chars: #{resultChars}|]
    , "arguments:"
    , toolUse.arguments
    ]

renderConversationTraceLog :: Integer -> [AgentTrace.AgentTraceRecord] -> Text
renderConversationTraceLog parentId [] =
  [i|没有找到消息 #{parentId} 对应的 agent trace。|]
renderConversationTraceLog _ records =
  Text.unlines (map renderTraceRecord records)

renderTraceRecord :: AgentTrace.AgentTraceRecord -> Text
renderTraceRecord record =
  let eventId = renderRecordId record.id
  in
  Text.unwords
    [ timestamp record.occurredAt
    , [i|event_id=#{eventId}|]
    , renderTraceEvent record.id record.event
    ]

renderRecordId :: Maybe Integer -> Text
renderRecordId =
  maybe "unknown" show

renderTraceEvent :: Maybe Integer -> AgentTrace.AgentTraceEvent -> Text
renderTraceEvent recordId = \case
  AgentTrace.AgentRunStarted{runId, maxTurns, exposedTools} ->
    [i|run_started run=#{runId} max_turns=#{maxTurns} tools=#{Text.intercalate "," exposedTools}|]
  AgentTrace.ModelTurnStarted{runId, turn, messageCount} ->
    [i|model_turn_started run=#{runId} turn=#{turn} messages=#{messageCount}|]
  AgentTrace.ModelTurnFinished{runId, turn, answerKind, contentLength, toolCalls} ->
    [i|model_turn_finished run=#{runId} turn=#{turn} kind=#{answerKind} content_chars=#{contentLength} tool_calls=#{length toolCalls}|]
  AgentTrace.ToolCallStarted{runId, turn, toolCall} ->
    let toolName = toolCall.name
        toolCallId = toolCall.id
        auditId = renderRecordId recordId
    in [i|tool_started audit_id=#{auditId} run=#{runId} turn=#{turn} tool=#{toolName} request=#{toolCallId}|]
  AgentTrace.ToolCallFinished{runId, turn, toolCallId, toolName, status, resultLength} ->
    [i|tool_finished run=#{runId} turn=#{turn} tool=#{toolName} request=#{toolCallId} status=#{status} result_chars=#{resultLength}|]
  AgentTrace.AgentRunFinished{runId, status, finalLength, turnsUsed} ->
    [i|run_finished run=#{runId} status=#{status} final_chars=#{finalLength} turns=#{turnsUsed}|]
  AgentTrace.AgentRunInterrupted{runId, reason} ->
    [i|run_interrupted run=#{runId} reason=#{reason}|]
  AgentTrace.AgentConversationLinked{runId, linkedMessageId, parentMessageId} ->
    let parent = show parentMessageId :: Text
    in [i|conversation_linked run=#{runId} message=#{linkedMessageId} parent=#{parent}|]

renderStatus :: AgentTrace.ToolUseStatus -> Text
renderStatus = \case
  AgentTrace.ToolUseInProgress ->
    "running"
  AgentTrace.ToolUseFinished{status, durationMilliseconds} ->
    [i|finished(#{status}, #{durationMilliseconds}ms)|]
  AgentTrace.ToolUseInterrupted{reason, durationMilliseconds} ->
    [i|interrupted(#{reason}, #{durationMilliseconds}ms)|]

timestamp :: FormatTime t => t -> Text
timestamp =
  Text.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q UTC"
