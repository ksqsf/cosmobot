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
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import Bot.Prelude
import qualified Data.Text as Text
import Data.Time (FormatTime, defaultTimeLocale, formatTime)

auditHandlers
  :: (AgentAudit.AgentAudit :> es, Chat.Chat :> es, IOE :> es)
  => ConversationStore
  -> [RouteHandler es]
auditHandlers conversations =
  [ requireAuth isSuperuser (\message -> void $ Chat.replyTo message "只有 superuser 可以查看 audit。") $
      stopOn (command "!audit") (handleAudit conversations)
  ]

handleAudit
  :: (AgentAudit.AgentAudit :> es, Chat.Chat :> es, IOE :> es)
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
              records <- AgentAudit.queryConversationAudit parentId
              void $ Chat.replyTo message (renderConversationAuditLog parentId records)
            Nothing ->
              void $ Chat.replyTo message "用法：回复一条 agent conversation 消息并发送 !audit log"
      | Text.toLower (Text.strip args) == "all" ->
          case message.replyToMessageId of
            Just parentId -> do
              messageIds <- lookupConversationMessageIds conversations parentId
              records <- AgentAudit.queryConversationMessagesAudit messageIds
              void $ Chat.replyTo message (renderConversationToolUses parentId records)
            Nothing ->
              void $ Chat.replyTo message "用法：回复一条 agent conversation 消息并发送 !audit all"
      | Text.null (Text.strip args) ->
          case message.replyToMessageId of
            Just parentId -> do
              records <- AgentAudit.queryConversationAudit parentId
              void $ Chat.replyTo message (renderConversationToolUses parentId records)
            Nothing -> do
              toolUses <- AgentAudit.queryRecentToolUses 50
              void $ Chat.replyTo message (renderAuditList toolUses)
      | otherwise ->
          void $ Chat.replyTo message "用法：!audit、!audit all、!audit log 或 !audit <id>"
    Just auditId -> do
      detail <- AgentAudit.queryToolUse auditId
      void $ Chat.replyTo message (maybe [i|没有找到 audit id #{auditId}。|] renderAuditDetail detail)

parseAuditId :: Text -> Maybe Integer
parseAuditId =
  readMaybe . toString . Text.strip

renderAuditList :: [AgentAudit.ToolUseDetail] -> Text
renderAuditList [] =
  "最近没有 agent tool use。"
renderAuditList toolUses =
  Text.unlines (map renderToolUseLine toolUses)

renderToolUseLine :: AgentAudit.ToolUseDetail -> Text
renderToolUseLine toolUse =
  let auditId = toolUse.auditId
      toolName = toolUse.toolName
  in
  Text.unwords
    [ timestamp toolUse.occurredAt
    , [i|id=#{auditId}|]
    , [i|tool=#{toolName}|]
    , renderStatus toolUse.status
    ]

renderAuditDetail :: AgentAudit.ToolUseDetail -> Text
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
    , "status: " <> renderStatus toolUse.status
    , "message_ids: " <> show toolUse.messageIds
    , "arguments:"
    , toolUse.arguments
    , "result:"
    , fromMaybe "(still running)" toolUse.result
    ]

renderConversationToolUses :: Integer -> [AgentAudit.AgentAuditRecord] -> Text
renderConversationToolUses parentId [] =
  [i|没有找到消息 #{parentId} 对应的 agent audit。|]
renderConversationToolUses _ records =
  case AgentAudit.toolUsesFromAuditRecords records of
    [] ->
      "该 agent audit 中没有 tool use。"
    toolUses ->
      Text.intercalate "\n" (map renderToolUseBlock toolUses)

renderToolUseBlock :: AgentAudit.ToolUseDetail -> Text
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
    , "status: " <> renderStatus toolUse.status
    , [i|result_chars: #{resultChars}|]
    , "arguments:"
    , toolUse.arguments
    ]

renderConversationAuditLog :: Integer -> [AgentAudit.AgentAuditRecord] -> Text
renderConversationAuditLog parentId [] =
  [i|没有找到消息 #{parentId} 对应的 agent audit。|]
renderConversationAuditLog _ records =
  Text.unlines (map renderAuditRecord records)

renderAuditRecord :: AgentAudit.AgentAuditRecord -> Text
renderAuditRecord record =
  let eventId = renderRecordId record.id
  in
  Text.unwords
    [ timestamp record.occurredAt
    , [i|event_id=#{eventId}|]
    , renderAuditEvent record.id record.event
    ]

renderRecordId :: Maybe Integer -> Text
renderRecordId =
  maybe "unknown" show

renderAuditEvent :: Maybe Integer -> AgentAudit.AgentAuditEvent -> Text
renderAuditEvent recordId = \case
  AgentAudit.ToolCallStarted{runId, turn, toolCall} ->
    let toolName = toolCall.name
        auditId = renderRecordId recordId
    in [i|tool_started audit_id=#{auditId} run=#{runId} turn=#{turn} tool=#{toolName}|]
  AgentAudit.ToolCallFinished{runId, turn, toolName, status, resultLength} ->
    [i|tool_finished run=#{runId} turn=#{turn} tool=#{toolName} status=#{status} result_chars=#{resultLength}|]
  AgentAudit.AgentRunInterrupted{runId, reason} ->
    [i|run_interrupted run=#{runId} reason=#{reason}|]
  AgentAudit.AgentConversationLinked{runId, linkedMessageId, parentMessageId} ->
    let parent = show parentMessageId :: Text
    in [i|conversation_linked run=#{runId} message=#{linkedMessageId} parent=#{parent}|]

renderStatus :: AgentAudit.ToolUseStatus -> Text
renderStatus = \case
  AgentAudit.ToolUseInProgress ->
    "running"
  AgentAudit.ToolUseFinished{status, durationMilliseconds} ->
    [i|finished(#{status}, #{durationMilliseconds}ms)|]
  AgentAudit.ToolUseInterrupted{reason, durationMilliseconds} ->
    [i|interrupted(#{reason}, #{durationMilliseconds}ms)|]

timestamp :: FormatTime t => t -> Text
timestamp =
  Text.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q UTC"
