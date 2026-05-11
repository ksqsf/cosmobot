{-|
Module      : Bot.Agent
Description : Agent loop and extensible tool framework
Stability   : experimental
-}

module Bot.Agent
  ( Tool (..)
  , AgentContext (..)
  , ToolResult (..)
  , runAgent
  , defaultTools
  )
where

import Bot.Conversation
import Bot.Agent.Tool
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Memory as Memory
import Bot.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import System.Directory
import System.FilePath
import System.IO.Error (userError)
import System.Posix.Signals (signalProcess, sigKILL)
import System.Process (createProcess, shell, std_out, std_err, StdStream(..), getPid, waitForProcess)
import System.Timeout (timeout)

-- | Run an LLM/tool loop until the model answers or the tool turn limit is hit.
runAgent
  :: (LLM.LLM :> es, Log :> es)
  => Int
  -> AgentContext es
  -> [Tool es]
  -> Conversation
  -> Eff es (Text, Conversation)
runAgent maxTurns context tools conversation =
  loop (max 1 maxTurns) (closeInterruptedToolCalls conversation)
  where
    exposedTools = filter (`toolAllowed` context) tools

    loop turnsLeft current = do
      answer <- LLM.askWithTools (map toolSchema exposedTools) current.messages
      let answered = appendMessage (LLM.assistantAnswer answer) current
      case answer.toolCalls of
        [] ->
          pure (answer.content, answered)
        calls
          | turnsLeft <= 1 -> do
              logInfo "Agent tool turn limit reached" calls
              let paused = appendMessages (map pausedToolResult calls) answered
              pure (toolLimitMessage answer.content calls, paused)
          | otherwise -> do
              results <- traverse execute calls
              let next = appendMessages (map fst results) answered
              traverse_ (\messageId -> context.remember messageId next) (concatMap snd results)
              loop (turnsLeft - 1) next

    execute call = do
      let callName = call.name
      result <- runTool context tools call `catch` \(err :: SomeException) ->
        pure (toolText [i|Tool #{callName} failed: #{show err :: String}|])
      pure (LLM.toolResult call result.content, result.messageIds)

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

everyone :: AgentContext es -> Bool
everyone _ =
  True

superuserOnly :: AgentContext es -> Bool
superuserOnly =
  (.superuser)

appendMessage :: LLM.ChatMessage -> Conversation -> Conversation
appendMessage message (Conversation messages) =
  Conversation (messages <> [message])

appendMessages :: [LLM.ChatMessage] -> Conversation -> Conversation
appendMessages newMessages (Conversation messages) =
  Conversation (messages <> newMessages)

-- | Built-in tools exposed to the model after per-message permission checks.
defaultTools :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, IOE :> es) => [Tool es]
defaultTools =
  [ listDirectoryTool
  , readFileTool
  , queryChatLogTool
  , generateImageTool
  , sendReplyTool
  , mentionUserTool
  , senderMemberInfoTool
  , memberInfoTool
  , listGroupMembersTool
  , currentMentionsTool
  , scheduleAgentActionTool
  , deleteScheduledAgentActionTool
  , listCurrentUserSchedulesTool
  , manageMemoryTool
  , runBashTool
  ]

manageMemoryTool :: IOE :> es => Tool es
manageMemoryTool = Tool
  { name = "manage_current_sender_memory"
  , description = "View, replace, or clear the persistent MEMORY.md for the current message sender. Use this when the sender asks to view or clear memory, or when the sender gives durable preferences such as a preferred name, style, language, stable personal facts, or recurring instructions. Keep memory concise: non-superusers must stay within 1000 characters; if an update is rejected, summarize it shorter and try again."
  , parameters = objectSchema
      [ fieldText "action" "One of: view, replace, clear."
      , fieldText "memory" "Complete replacement MEMORY.md content. Required only when action is replace."
      ]
      ["action"]
  , allowed = everyone
  , run = \context args ->
      case context.memoryConfig of
        Nothing ->
          pure (toolText "Memory is not configured.")
        Just cfg ->
          case AesonTypes.parseEither memoryArgs args of
            Left err ->
              pure (toolText (Text.pack err))
            Right (action, memory) ->
              runMemoryAction cfg context action memory
  }

listDirectoryTool :: IOE :> es => Tool es
listDirectoryTool = Tool
  { name = "list_directory"
  , description = "List files and directories under a path inside the bot working directory."
  , parameters = objectSchema
      [ fieldText "path" "Directory path relative to the bot working directory. Use \".\" for the working directory."
      ]
      ["path"]
  , allowed = superuserOnly
  , run = \_ -> withTextArg "path" \path -> do
      target <- resolveSafePath path
      isDir <- liftIO (doesDirectoryExist target)
      if not isDir
        then pure (toolText "Not a directory.")
        else do
          entries <- liftIO (listDirectory target)
          pure (toolText (jsonText entries))
  }

readFileTool :: IOE :> es => Tool es
readFileTool = Tool
  { name = "read_file"
  , description = "Read a UTF-8 text file inside the bot working directory."
  , parameters = objectSchema
      [ fieldText "path" "File path relative to the bot working directory."
      ]
      ["path"]
  , allowed = superuserOnly
  , run = \_ -> withTextArg "path" \path -> do
      target <- resolveSafePath path
      isFile <- liftIO (doesFileExist target)
      if not isFile
        then pure (toolText "Not a file.")
        else toolText <$> liftIO (Text.readFile target)
  }

queryChatLogTool :: ChatLog.ChatLog :> es => Tool es
queryChatLogTool = Tool
  { name = "query_current_chat_log"
  , description = "Return recent messages recorded in the current chat. Results are in chronological order and include sender ids, message ids, mentions, image urls, and text."
  , parameters = objectSchema
      [ fieldInteger "limit" "Maximum number of recent messages to return."
      , fieldBoolean "include_bot_messages" "Whether to include bot messages. Defaults to false."
      ]
      ["limit"]
  , allowed = everyone
  , run = \context args ->
      case AesonTypes.parseEither queryChatLogArgs args of
        Left err ->
          pure (toolText (Text.pack err))
        Right (limit, includeBotMessages) -> do
          entries <- ChatLog.queryChat context.message (fromInteger (max 0 limit)) includeBotMessages
          pure (toolText (jsonText entries))
  }

generateImageTool :: (Chat.Chat :> es, LLM.LLM :> es) => Tool es
generateImageTool = Tool
  { name = "generate_image"
  , description = "Generate an actual image from a prompt and send it to the current chat. Use this when the user asks to draw, create, or generate an image, including scheduled future image requests. After using this tool, keep the final answer brief and do not repeat the image URL."
  , parameters = objectSchema
      [ fieldText "prompt" "Image generation prompt. Include the user's visual requirements, style, subject, text, and constraints."
      ]
      ["prompt"]
  , allowed = everyone
  , run = \context -> withTextArg "prompt" \prompt -> do
      generated <- LLM.askImageWithHistory [LLM.userWithImages prompt context.message.imageUrls]
      case Chat.replyImageUrls generated of
        [] ->
          pure (toolText generated)
        _ -> do
          sent <- Chat.replyTo context.message generated
          context.recordBotMessage sent generated
          let sentText = show sent :: String
          pure (toolMessage sent [i|Generated and sent image message id: #{sentText}|])
  }

sendReplyTool :: Chat.Chat :> es => Tool es
sendReplyTool = Tool
  { name = "send_reply_to_current_chat"
  , description = "Send a reply message to the same chat as the current user message. Supports text and image URLs. Use image_urls when the user asks you to send an image found or generated elsewhere. Use only when the user asks you to send an additional message before the final answer."
  , parameters = objectSchema
      [ fieldText "text" "Message text to send. May be omitted when image_urls is non-empty."
      , fieldTextArray "image_urls" "Image URLs to send as images in the same reply. The platform must be able to fetch these URLs."
      ]
      []
  , allowed = everyone
  , run = \context args ->
      case AesonTypes.parseEither sendReplyArgs args of
        Left err ->
          pure (toolText (Text.pack err))
        Right body -> do
          sent <- Chat.replyTo context.message body
          context.recordBotMessage sent body
          let sentText = show sent :: String
          pure (toolMessage sent [i|Sent message id: #{sentText}|])
  }

mentionUserTool :: Chat.Chat :> es => Tool es
mentionUserTool = Tool
  { name = "mention_user"
  , description = "Send a reply in the current chat that mentions the given user id. On QQ this sends an actual at segment."
  , parameters = objectSchema
      [ fieldInteger "user_id" "Platform user id to mention."
      , fieldText "text" "Message text to send after the mention."
      ]
      ["user_id", "text"]
  , allowed = everyone
  , run = \context args ->
      case AesonTypes.parseEither mentionUserArgs args of
        Left err ->
          pure (toolText (Text.pack err))
        Right (userId, text) -> do
          sent <- Chat.mentionUser context.message userId text
          context.recordBotMessage sent text
          let sentText = show sent :: String
          pure (toolMessage sent [i|Sent mention message id: #{sentText}|])
  }

senderMemberInfoTool :: Chat.Chat :> es => Tool es
senderMemberInfoTool = Tool
  { name = "get_current_sender_member_info"
  , description = "Get platform-provided member information for the sender of the current message in the current group chat."
  , parameters = objectSchema [] []
  , allowed = everyone
  , run = \context _ -> do
      info <- Chat.getSenderMemberInfo context.message
      pure (toolText (maybe "No member information is available for this message." jsonText info))
  }

memberInfoTool :: Chat.Chat :> es => Tool es
memberInfoTool = Tool
  { name = "get_group_member_info"
  , description = "Get platform-provided member information for any user id in the current group chat."
  , parameters = objectSchema
      [ fieldInteger "user_id" "Platform user id to query in the current group."
      ]
      ["user_id"]
  , allowed = everyone
  , run = \context -> withIntegerArg "user_id" \userId -> do
      info <- Chat.getMemberInfo context.message userId
      pure (toolText (maybe "No member information is available for this user in the current chat." jsonText info))
  }

listGroupMembersTool :: Chat.Chat :> es => Tool es
listGroupMembersTool = Tool
  { name = "list_group_members"
  , description = "List members in the current group chat, including platform user ids and nicknames when available. QQ groups are supported. Telegram Bot API does not expose full member lists, so Telegram may return unavailable."
  , parameters = objectSchema [] []
  , allowed = everyone
  , run = \context _ -> do
      members <- Chat.listGroupMembers context.message
      pure (toolText (maybe "Group member listing is not available for this platform or chat." jsonText members))
  }

currentMentionsTool :: Tool es
currentMentionsTool = Tool
  { name = "get_current_message_mentions"
  , description = "Return platform user ids mentioned in the current message, in message order. On QQ these are QQ numbers from at segments."
  , parameters = objectSchema [] []
  , allowed = everyone
  , run = \context _ ->
      pure (toolText (jsonText context.message.mentions))
  }

scheduleAgentActionTool :: Scheduler.Scheduler :> es => Tool es
scheduleAgentActionTool = Tool
  { name = "schedule_agent_action"
  , description = "Schedule a future agent action in the current chat. The future action is processed through the same incoming message pipeline and replies to the current user message."
  , parameters = objectSchema
      [ fieldInteger "delay_seconds" "Delay before running the future agent action, in seconds."
      , fieldText "prompt" "Prompt for the future agent action."
      ]
      ["delay_seconds", "prompt"]
  , allowed = everyone
  , run = \context args ->
      case AesonTypes.parseEither scheduledActionArgs args of
        Left err ->
          pure (toolText (Text.pack err))
        Right (delaySeconds, prompt) -> do
          scheduled <- Scheduler.scheduleMessage delaySeconds (scheduledAgentMessage context delaySeconds prompt)
          if scheduled
            then pure (toolText [i|Scheduled agent action in #{delaySeconds} seconds.|])
            else pure (toolText "Could not schedule agent action: scheduler is at capacity.")
  }

deleteScheduledAgentActionTool :: Scheduler.Scheduler :> es => Tool es
deleteScheduledAgentActionTool = Tool
  { name = "delete_scheduled_agent_action"
  , description = "Delete a schedule using schedule ID. Only current user's schedules may be deleted."
  , parameters = objectSchema
    [ fieldInteger "schedule_id" "The schedule ID to be deleted."
    ]
    ["schedule_id"]
  , allowed = everyone
  , run = \context -> withIntegerArg "schedule_id" $ \scheduleId -> do
      ok <- Scheduler.deleteScheduledMessage context.message scheduleId
      if ok
        then pure (toolText [i|Schedule #{scheduleId} has been removed.|])
        else pure (toolText [i|Schedule #{scheduleId} is not available to the user.|])
  }

listCurrentUserSchedulesTool :: Scheduler.Scheduler :> es => Tool es
listCurrentUserSchedulesTool = Tool
  { name = "list_current_user_schedules"
  , description = "List pending scheduled agent actions created by the current user in the current chat. Returns schedule ids, remaining seconds, and scheduled prompts."
  , parameters = objectSchema [] []
  , allowed = everyone
  , run = \context _ -> do
      schedules <- Scheduler.listScheduledMessages context.message
      pure (toolText (jsonText (map scheduleSummary schedules)))
  }

runBashTool :: IOE :> es => Tool es
runBashTool = Tool
  { name = "run_bash"
  , description = "Run a bash script and obtain outputs; do not run malicious code"
  , parameters = objectSchema
      [ fieldText "script" "The bash script to be executed in the cwd"
      , fieldInteger "timeout_seconds" "Maximum seconds to wait before killing the process. Defaults to 30."
      ]
      ["script"]
  , allowed = superuserOnly
  , run = \_ -> withTextArg "script" \script -> do
      result <- liftIO $ runBashSafe (Text.unpack script)
      pure (toolText result)
  }

runBashSafe :: String -> IO Text
runBashSafe script = do
  let timeoutSeconds = 30
  (_, Just hOut, Just hErr, ph) <- createProcess
    (shell script) { std_out = CreatePipe, std_err = CreatePipe }
  outcome <- timeout (timeoutSeconds * 1_000_000) $ do
    stdoutText <- Text.hGetContents hOut
    stderrText <- Text.hGetContents hErr
    exitCode <- waitForProcess ph
    pure (exitCode, stdoutText, stderrText)
  case outcome of
    Nothing -> do
      mPid <- getPid ph
      traverse_ (signalProcess sigKILL) mPid
      _ <- waitForProcess ph
      pure $ "Script timed out after " <> Text.pack (show (timeoutSeconds :: Int)) <> " seconds and was killed."
    Just (exitCode, stdoutText, stderrText) ->
      pure $ Text.strip $ Text.unlines $ filter (not . Text.null)
        [ if Text.null stdoutText then "" else "stdout:\n" <> stdoutText
        , if Text.null stderrText then "" else "stderr:\n" <> stderrText
        , "exit code: " <> Text.pack (show exitCode)
        ]

data ScheduleSummary = ScheduleSummary
  { scheduleId :: !Integer
  , remainingSeconds :: !Int
  , prompt :: !Text
  }
  deriving (Show, Generic, Aeson.ToJSON)

scheduleSummary :: Scheduler.ScheduledMessage -> ScheduleSummary
scheduleSummary schedule =
  ScheduleSummary
    { scheduleId = schedule.scheduleId
    , remainingSeconds = schedule.remainingSeconds
    , prompt = scheduledPrompt schedule.message
    }

scheduledPrompt :: IncomingMessage -> Text
scheduledPrompt message =
  fromMaybe message.text (AesonTypes.parseMaybe parsePrompt message.raw)
  where
    parsePrompt =
      Aeson.withObject "scheduled action" (Aeson..: Key.fromText "prompt")

scheduledActionArgs :: Aeson.Value -> AesonTypes.Parser (Int, Text)
scheduledActionArgs =
  Aeson.withObject "scheduled action arguments" $ \o -> do
    delaySeconds <- o Aeson..: Key.fromText "delay_seconds"
    prompt <- o Aeson..: Key.fromText "prompt"
    pure (delaySeconds, prompt)

data MemoryAction
  = MemoryView
  | MemoryReplace
  | MemoryClear

memoryArgs :: Aeson.Value -> AesonTypes.Parser (MemoryAction, Maybe Text)
memoryArgs =
  Aeson.withObject "memory arguments" $ \o -> do
    actionText <- Text.toLower . Text.strip <$> o Aeson..: Key.fromText "action"
    memory <- fmap Text.strip <$> o Aeson..:? Key.fromText "memory"
    action <- case actionText of
      "view" ->
        pure MemoryView
      "replace" ->
        pure MemoryReplace
      "clear" ->
        pure MemoryClear
      _ ->
        fail "action must be one of: view, replace, clear"
    when (actionText == "replace" && maybe True Text.null memory) do
      fail "memory is required when action is replace"
    pure (action, memory)

runMemoryAction :: IOE :> es => Memory.MemoryConfig -> AgentContext es -> MemoryAction -> Maybe Text -> Eff es ToolResult
runMemoryAction cfg context action memory =
  case action of
    MemoryView -> do
      current <- Memory.loadSenderMemory cfg context.message
      pure (toolText (fromMaybe "No memory is stored for the current sender." current))
    MemoryReplace ->
      case memory of
        Nothing ->
          pure (toolText "memory is required when action is replace")
        Just content
          | not context.superuser && Text.length content > Memory.memoryLimitChars ->
              pure (toolText [i|Memory update rejected: memory is #{Text.length content} characters, over the #{Memory.memoryLimitChars} character limit. Please summarize it more concisely and try again.|])
          | otherwise -> do
              result <- Memory.replaceSenderMemory cfg context.message content
              pure (toolText (either identity (const "Memory updated.") result))
    MemoryClear -> do
      result <- Memory.clearSenderMemory cfg context.message
      pure (toolText (either identity (const "Memory cleared.") result))

scheduledAgentMessage :: AgentContext es -> Int -> Text -> IncomingMessage
scheduledAgentMessage context delaySeconds prompt =
  let original = context.message
      commandText = context.askCommand <> " " <> prompt
  in original
      { messageId = original.messageId
      , replyToMessageId = Nothing
      , mentions = original.mentions
      , mentionUsernames = original.mentionUsernames
      , imageUrls = []
      , text = commandText
      , raw = Aeson.object
          [ "type" Aeson..= Aeson.String "scheduled_agent_action"
          , "delay_seconds" Aeson..= delaySeconds
          , "prompt" Aeson..= prompt
          , "original_message" Aeson..= original.raw
          ]
      }

withTextArg :: Text -> (Text -> Eff es ToolResult) -> Aeson.Value -> Eff es ToolResult
withTextArg key action =
  either (pure . toolText . Text.pack) action . AesonTypes.parseEither parser
  where
    parser = Aeson.withObject "tool arguments" (Aeson..: Key.fromText key)

withIntegerArg :: Text -> (Integer -> Eff es ToolResult) -> Aeson.Value -> Eff es ToolResult
withIntegerArg key action =
  either (pure . toolText . Text.pack) action . AesonTypes.parseEither parser
  where
    parser = Aeson.withObject "tool arguments" (Aeson..: Key.fromText key)

queryChatLogArgs :: Aeson.Value -> AesonTypes.Parser (Integer, Bool)
queryChatLogArgs =
  Aeson.withObject "query chat log arguments" $ \o -> do
    limit <- o Aeson..: Key.fromText "limit"
    includeBotMessages <- fromMaybe False <$> o Aeson..:? Key.fromText "include_bot_messages"
    pure (limit, includeBotMessages)

mentionUserArgs :: Aeson.Value -> AesonTypes.Parser (Integer, Text)
mentionUserArgs =
  Aeson.withObject "mention user arguments" $ \o -> do
    userId <- o Aeson..: Key.fromText "user_id"
    text <- o Aeson..: Key.fromText "text"
    pure (userId, text)

sendReplyArgs :: Aeson.Value -> AesonTypes.Parser Text
sendReplyArgs =
  Aeson.withObject "send reply arguments" $ \o -> do
    text <- Text.strip . fromMaybe "" <$> o Aeson..:? Key.fromText "text"
    imageUrls <- map Text.strip . fromMaybe [] <$> o Aeson..:? Key.fromText "image_urls"
    let body = replyBodyWithImages text (filter (not . Text.null) imageUrls)
    when (Text.null body) do
      fail "Either text or image_urls must be provided."
    pure body

replyBodyWithImages :: Text -> [Text] -> Text
replyBodyWithImages text imageUrls =
  Text.strip $ Text.unlines $
    [ text | not (Text.null text) ]
      <> map ("[image] " <>) imageUrls

resolveSafePath :: IOE :> es => Text -> Eff es FilePath
resolveSafePath rawPath = do
  cwd <- liftIO getCurrentDirectory
  target <- liftIO (canonicalizePath (cwd </> Text.unpack rawPath))
  unless (cwd `isEqualOrParentOf` target) do
    throwIO (userError "Path escapes the bot working directory.")
  pure target

isEqualOrParentOf :: FilePath -> FilePath -> Bool
isEqualOrParentOf parent child =
  parent == child || addTrailingPathSeparator parent `isPrefixOf` child

fieldText :: Text -> Text -> (Text, Aeson.Value)
fieldText name description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "string"
      , "description" Aeson..= description
      ]
  )

fieldTextArray :: Text -> Text -> (Text, Aeson.Value)
fieldTextArray name description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "array"
      , "items" Aeson..= Aeson.object
          [ "type" Aeson..= Aeson.String "string"
          ]
      , "description" Aeson..= description
      ]
  )

fieldInteger :: Text -> Text -> (Text, Aeson.Value)
fieldInteger name description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "integer"
      , "minimum" Aeson..= (0 :: Int)
      , "description" Aeson..= description
      ]
  )

fieldBoolean :: Text -> Text -> (Text, Aeson.Value)
fieldBoolean name description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "boolean"
      , "description" Aeson..= description
      ]
  )

objectSchema :: [(Text, Aeson.Value)] -> [Text] -> Aeson.Value
objectSchema fields required =
  Aeson.object
    [ "type" Aeson..= Aeson.String "object"
    , "properties" Aeson..= Aeson.object
        [ Key.fromText name Aeson..= schema
        | (name, schema) <- fields
        ]
    , "required" Aeson..= required
    , "additionalProperties" Aeson..= False
    ]

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode
