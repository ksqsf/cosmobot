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
import Bot.Core.Route
import Bot.Core.Thread
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Storage.Thread
import qualified Data.Text as Text
import Data.Time (FormatTime, defaultTimeLocale, formatTime)

auditHandlers
  :: (AgentAudit.AgentAudit :> es, Chat.Chat :> es, Storage.Storage :> es, Prim :> es)
  => ThreadStore
  -> [RouteHandler es]
auditHandlers threads =
  [ withHelp (RouteHelp "!stats" "Show cumulative agent statistics for a replied thread message (superuser only).") $
    requireAuth isSuperuser (\message -> void $ Chat.replyTo message "只有 superuser 可以查看 agent stats。") $
      stopOn (command "!stats") (handleStats threads)
  , withHelp (RouteHelp "!audit [all|log|<id>]" "Inspect agent tool-use audit records (superuser only).") $
    requireAuth isSuperuser (\message -> void $ Chat.replyTo message "只有 superuser 可以查看 audit。") $
      stopOn (command "!audit") (handleAudit threads)
  ]

handleStats
  :: (AgentAudit.AgentAudit :> es, Chat.Chat :> es, Storage.Storage :> es, Prim :> es)
  => ThreadStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleStats threads message args
  | not (Text.null (Text.strip args)) =
      void $ Chat.replyTo message "用法：回复一条 agent thread 消息并发送 !stats"
  | otherwise =
      case message.replyToMessageId of
        Nothing ->
          void $ Chat.replyTo message "用法：回复一条 agent thread 消息并发送 !stats"
        Just parentId -> do
          let parentKey = threadMessageKey message parentId
          messageIds <- lookupThreadMessageIds threads parentKey
          completed <- AgentAudit.queryThreadMessagesAudit (map (threadMessageKey message) messageIds)
          activeRunId <- lookupActiveThreadRunId threads parentKey
          active <- maybe (pure []) AgentAudit.queryRunAudit activeRunId
          let records = ordNubOn (.id) (linkedMessageAudit messageIds completed <> active)
          void $ Chat.replyTo message (renderThreadStats parentId activeRunId records)

handleAudit
  :: (AgentAudit.AgentAudit :> es, Chat.Chat :> es, Storage.Storage :> es, Prim :> es)
  => ThreadStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleAudit threads message args =
  case parseAuditId args of
    Nothing
      | Text.toLower (Text.strip args) == "log" ->
          case message.replyToMessageId of
            Just parentId -> do
              records <- AgentAudit.queryThreadAudit (threadMessageKey message parentId)
              void $ Chat.replyTo message (renderThreadAuditLog parentId records)
            Nothing ->
              void $ Chat.replyTo message "用法：回复一条 agent thread 消息并发送 !audit log"
      | Text.toLower (Text.strip args) == "all" ->
          case message.replyToMessageId of
            Just parentId -> do
              messageIds <- lookupThreadMessageIds threads (threadMessageKey message parentId)
              records <- AgentAudit.queryThreadMessagesAudit (map (threadMessageKey message) messageIds)
              void $ Chat.replyTo message (renderThreadToolUses parentId records)
            Nothing ->
              void $ Chat.replyTo message "用法：回复一条 agent thread 消息并发送 !audit all"
      | Text.null (Text.strip args) ->
          case message.replyToMessageId of
            Just parentId -> do
              records <- AgentAudit.queryThreadAudit (threadMessageKey message parentId)
              void $ Chat.replyTo message (renderThreadToolUses parentId records)
            Nothing -> do
              toolUses <- AgentAudit.queryRecentToolUses recentAuditLimit
              void $ Chat.replyTo message (renderAuditList toolUses)
      | otherwise ->
          void $ Chat.replyTo message "用法：!audit、!audit all、!audit log 或 !audit <id>"
    Just auditId -> do
      detail <- AgentAudit.queryToolUse auditId
      void $ Chat.replyTo message (maybe [i|没有找到 audit id #{auditId}。|] renderAuditDetail detail)

parseAuditId :: Text -> Maybe Integer
parseAuditId =
  readMaybe . toString . Text.strip

recentAuditLimit :: Int
recentAuditLimit =
  20

renderAuditList :: [AgentAudit.ToolUseDetail] -> Text
renderAuditList [] =
  "最近没有 agent tool use。"
renderAuditList toolUses =
  Text.unlines ("*Recent agent tool uses*" : map renderToolUseLine toolUses)

renderThreadStats :: MessageId -> Maybe Text -> [AgentAudit.AgentAuditRecord] -> Text
renderThreadStats parentId activeRunId records
  | null records, isNothing activeRunId =
      [i|没有找到消息 #{messageIdText parentId} 对应的 agent stats。|]
  | otherwise =
      Text.unlines $
        [ "*Thread stats*"
        , [i|- status: #{if isJust activeRunId then "active" else "complete" :: Text}|]
        , [i|- runs: #{length runIds}|]
        , [i|- model turns: #{length modelTurns}|]
        , [i|- tokens: #{totalTokens} total (#{promptTokens} prompt, #{completionTokens} completion#{unreportedSuffix})|]
        , renderCurrentTurn currentRunTurns
        , renderContext currentUsage peakPromptTokens
        , [i|- tool calls: #{length toolUses} (#{okTools} ok, #{failedTools} failed, #{interruptedTools} interrupted, #{length runningTools} running)|]
        ]
          <> [ "- running tools: " <> Text.intercalate ", " (map renderRunningTool runningTools)
             | not (null runningTools)
             ]
  where
    runIds =
      ordNub (map (AgentAudit.eventRunId . (.event)) records <> maybeToList activeRunId)
    modelTurns =
      [ (runId, tokenUsage)
      | AgentAudit.AgentAuditRecord{event = AgentAudit.ModelTurnFinished{runId, tokenUsage}} <- records
      ]
    allTurnUsage =
      map snd modelTurns
    reportedUsage =
      catMaybes allTurnUsage
    currentUsage =
      join (viaNonEmpty last allTurnUsage)
    currentRunTurns =
      case viaNonEmpty last runIds of
        Nothing ->
          []
        Just currentRunId ->
          [ tokenUsage
          | (runId, tokenUsage) <- modelTurns
          , runId == currentRunId
          ]
    promptTokens =
      sum (map (.promptTokens) reportedUsage)
    completionTokens =
      sum (map (.completionTokens) reportedUsage)
    totalTokens =
      sum (map (.totalTokens) reportedUsage)
    promptUsage =
      map (.promptTokens) reportedUsage
    peakPromptTokens =
      foldl' max 0 promptUsage
    unreportedTurns =
      length (filter isNothing allTurnUsage)
    unreportedSuffix :: Text
    unreportedSuffix
      | unreportedTurns == 0 =
          ""
      | otherwise =
          [i|; #{unreportedTurns} unreported turns|]
    toolUses =
      AgentAudit.toolUsesFromAuditRecords records
    okTools =
      length
        [ ()
        | AgentAudit.ToolUseDetail{status = AgentAudit.ToolUseFinished{status = "ok"}} <- toolUses
        ]
    failedTools =
      length
        [ ()
        | AgentAudit.ToolUseDetail{status = AgentAudit.ToolUseFinished{status}} <- toolUses
        , status /= "ok"
        ]
    interruptedTools =
      length
        [ ()
        | AgentAudit.ToolUseDetail{status = AgentAudit.ToolUseInterrupted{}} <- toolUses
        ]
    runningTools =
      [ toolUse
      | toolUse@AgentAudit.ToolUseDetail{status = AgentAudit.ToolUseInProgress} <- toolUses
      ]
    renderRunningTool toolUse =
      let toolName = toolUse.toolName
          auditId = toolUse.auditId
      in [i|`#{toolName}` (`id=#{auditId}`)|]

renderCurrentTurn :: [Maybe LLM.TokenUsage] -> Text
renderCurrentTurn [] =
  "- current turn: 0 total (0 prompt, 0 completion)"
renderCurrentTurn usages
  | null reported =
      "- current turn: unreported"
  | otherwise =
      [i|- current turn: #{totalTokens} total (#{promptTokens} prompt, #{completionTokens} completion#{unreportedSuffix})|]
  where
    reported =
      catMaybes usages
    promptTokens =
      sum (map (.promptTokens) reported)
    completionTokens =
      sum (map (.completionTokens) reported)
    totalTokens =
      sum (map (.totalTokens) reported)
    unreportedTurns =
      length (filter isNothing usages)
    unreportedSuffix :: Text
    unreportedSuffix
      | unreportedTurns == 0 =
          ""
      | otherwise =
          [i|; #{unreportedTurns} unreported model turns|]

renderContext :: Maybe LLM.TokenUsage -> Int -> Text
renderContext Nothing peakPromptTokens =
  [i|- context: unreported last / #{peakPromptTokens} peak prompt tokens|]
renderContext (Just usage) peakPromptTokens =
  let promptTokens = usage.promptTokens
  in [i|- context: #{promptTokens} last / #{peakPromptTokens} peak prompt tokens|]

linkedMessageAudit :: [MessageId] -> [AgentAudit.AgentAuditRecord] -> [AgentAudit.AgentAuditRecord]
linkedMessageAudit messageIds =
  concat . filter linksRequestedMessage . auditOccurrences
  where
    linksRequestedMessage records =
      any
        (\record -> case record.event of
          AgentAudit.AgentThreadLinked{linkedMessageId} ->
            linkedMessageId `elem` messageIds
          _ ->
            False
        )
        records

auditOccurrences :: [AgentAudit.AgentAuditRecord] -> [[AgentAudit.AgentAuditRecord]]
auditOccurrences =
  go []
  where
    go current [] =
      [reverse current | not (null current)]
    go current (record : rest) =
      case record.event of
        AgentAudit.AgentThreadLinked{} ->
          reverse (record : current) : go [] rest
        _ ->
          go (record : current) rest

renderToolUseLine :: AgentAudit.ToolUseDetail -> Text
renderToolUseLine toolUse =
  let auditId = toolUse.auditId
      toolName = toolUse.toolName
      occurredAt = timestamp toolUse.occurredAt
      status = renderStatus toolUse.status
  in
  [i|- #{occurredAt} id=#{auditId} tool=`#{toolName}` #{status}|]

renderAuditDetail :: AgentAudit.ToolUseDetail -> Text
renderAuditDetail toolUse =
  let auditId = toolUse.auditId
      occurredAt = timestamp toolUse.occurredAt
      finishedAt = maybe "(still running)" timestamp toolUse.finishedAt
      runId = toolUse.runId
      turn = toolUse.turn
      toolName = toolUse.toolName
      status = renderStatus toolUse.status
      messageIds = renderMessageIds toolUse.messageIds
      arguments = toolUse.arguments
      result = fromMaybe "(still running)" toolUse.result
  in
  Text.unlines
    [ [i|*Audit `#{auditId}`*|]
    , [i|- started: `#{occurredAt}`|]
    , [i|- finished: `#{finishedAt}`|]
    , [i|- run: `#{runId}`|]
    , [i|- turn: `#{turn}`|]
    , [i|- tool: `#{toolName}`|]
    , [i|- status: `#{status}`|]
    , [i|- message ids: `#{messageIds}`|]
    , ""
    , "*Arguments*"
    , fenced "json" arguments
    , "*Result*"
    , fenced "" result
    ]

renderThreadToolUses :: MessageId -> [AgentAudit.AgentAuditRecord] -> Text
renderThreadToolUses parentId [] =
  [i|没有找到消息 #{messageIdText parentId} 对应的 agent audit。|]
renderThreadToolUses _ records =
  case AgentAudit.toolUsesFromAuditRecords records of
    [] ->
      "该 agent audit 中没有 tool use。"
    toolUses ->
      Text.intercalate "\n" ("*Thread tool uses*" : map renderToolUseBlock toolUses)

renderToolUseBlock :: AgentAudit.ToolUseDetail -> Text
renderToolUseBlock toolUse =
  let auditId = toolUse.auditId
      toolName = toolUse.toolName
      status = renderStatus toolUse.status
      occurredAt = timestamp toolUse.occurredAt
      finishedAt = maybe "(still running)" timestamp toolUse.finishedAt
      runId = toolUse.runId
      turn = toolUse.turn
      resultChars = maybe 0 Text.length toolUse.result
      arguments = toolUse.arguments
  in
  Text.unlines
    [ [i|- `id=#{auditId}` `tool=#{toolName}` `#{status}`|]
    , [i|  - started: `#{occurredAt}`|]
    , [i|  - finished: `#{finishedAt}`|]
    , [i|  - run: `#{runId}` turn: `#{turn}` result chars: `#{resultChars}`|]
    , "  - arguments:"
    , indent (fenced "json" arguments)
    ]

renderThreadAuditLog :: MessageId -> [AgentAudit.AgentAuditRecord] -> Text
renderThreadAuditLog parentId [] =
  [i|没有找到消息 #{messageIdText parentId} 对应的 agent audit。|]
renderThreadAuditLog _ records =
  Text.unlines ("*Thread audit log*" : map renderAuditRecord records)

renderAuditRecord :: AgentAudit.AgentAuditRecord -> Text
renderAuditRecord record =
  let eventId :: Text
      eventId = show record.id
      occurredAt = timestamp record.occurredAt
      event = renderAuditEvent record.id record.event
  in
  [i|- `#{occurredAt}` `event_id=#{eventId}` #{event}|]

renderAuditEvent :: Integer -> AgentAudit.AgentAuditEvent -> Text
renderAuditEvent recordId = \case
  AgentAudit.ModelTurnFinished{runId, turn, answerKind, contentLength, toolCalls, tokenUsage} ->
    let usage = maybe "tokens=unreported" renderTokenUsage tokenUsage
    in [i|model_finished run=#{runId} turn=#{turn} kind=#{answerKind} content_chars=#{contentLength} tool_calls=#{length toolCalls} #{usage}|]
  AgentAudit.ToolCallStarted{runId, turn, toolCall} ->
    let toolName = toolCall.name
        auditId :: Text
        auditId = show recordId
    in [i|started audit_id=#{auditId} run=#{runId} turn=#{turn} tool=`#{toolName}`|]
  AgentAudit.ToolCallFinished{runId, turn, toolName, status, resultLength} ->
    [i|finished run=#{runId} turn=#{turn} tool=`#{toolName}` status=#{status} result_chars=#{resultLength}|]
  AgentAudit.AgentRunInterrupted{runId, reason} ->
    [i|run run=#{runId} reason=`#{reason}`|]
  AgentAudit.AgentThreadLinked{runId, linkedMessageId, parentMessageId} ->
    let parent = maybe "-" messageIdText parentMessageId
    in [i|`thread_linked` run=#{runId} message=#{messageIdText linkedMessageId} parent=#{parent}|]

renderTokenUsage :: LLM.TokenUsage -> Text
renderTokenUsage usage =
  let LLM.TokenUsage{promptTokens, completionTokens, totalTokens} = usage
  in [i|tokens=#{totalTokens} prompt=#{promptTokens} completion=#{completionTokens}|]

renderMessageIds :: [Maybe MessageId] -> Text
renderMessageIds messageIds =
  "[" <> Text.intercalate ", " (map (maybe "null" messageIdText) messageIds) <> "]"

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
  Text.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC"

fenced :: Text -> Text -> Text
fenced language body =
  Text.unlines
    [ "```" <> language
    , body
    , "```"
    ]

indent :: Text -> Text
indent =
  Text.unlines . map ("    " <>) . Text.lines
