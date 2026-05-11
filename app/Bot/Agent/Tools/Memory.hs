{-|
Module      : Bot.Agent.Tools.Memory
Description : Agent tools for persistent sender and chat memory
Stability   : experimental
-}

module Bot.Agent.Tools.Memory
  ( manageMemoryTool
  , manageChatMemoryTool
  )
where

import Bot.Agent.Types
import Bot.Agent.Tools.Common
import Bot.Core.Message
import qualified Bot.Memory as Memory
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

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
          withParsedToolArgs memoryArgs args \(action, memory) ->
            runMemoryAction senderMemoryScope cfg context action memory
  }

manageChatMemoryTool :: IOE :> es => Tool es
manageChatMemoryTool = Tool
  { name = "manage_current_chat_memory"
  , description = "View, replace, or clear the persistent MEMORY.md for the current chat/conversation. Use this when the user asks to view or clear chat memory, or when durable preferences, facts, norms, recurring instructions, or context apply to this chat as a whole rather than only to the current sender. Keep memory concise: non-superusers must stay within 1000 characters; if an update is rejected, summarize it shorter and try again."
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
          withParsedToolArgs memoryArgs args \(action, memory) ->
            runMemoryAction chatMemoryScope cfg context action memory
  }

data MemoryAction
  = MemoryView
  | MemoryReplace
  | MemoryClear

data MemoryScope es = MemoryScope
  { missingMessage :: !Text
  , updatedMessage :: !Text
  , clearedMessage :: !Text
  , loadMemory :: Memory.MemoryConfig -> IncomingMessage -> Eff es (Maybe Text)
  , replaceMemory :: Memory.MemoryConfig -> IncomingMessage -> Text -> Eff es (Either Text ())
  , clearMemory :: Memory.MemoryConfig -> IncomingMessage -> Eff es (Either Text ())
  }

senderMemoryScope :: IOE :> es => MemoryScope es
senderMemoryScope = MemoryScope
  { missingMessage = "No memory is stored for the current sender."
  , updatedMessage = "Memory updated."
  , clearedMessage = "Memory cleared."
  , loadMemory = Memory.loadSenderMemory
  , replaceMemory = Memory.replaceSenderMemory
  , clearMemory = Memory.clearSenderMemory
  }

chatMemoryScope :: IOE :> es => MemoryScope es
chatMemoryScope = MemoryScope
  { missingMessage = "No memory is stored for the current chat."
  , updatedMessage = "Chat memory updated."
  , clearedMessage = "Chat memory cleared."
  , loadMemory = Memory.loadChatMemory
  , replaceMemory = Memory.replaceChatMemory
  , clearMemory = Memory.clearChatMemory
  }

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

runMemoryAction :: IOE :> es => MemoryScope es -> Memory.MemoryConfig -> AgentContext es -> MemoryAction -> Maybe Text -> Eff es ToolResult
runMemoryAction scope cfg context action memory =
  case action of
    MemoryView -> do
      current <- scope.loadMemory cfg context.message
      pure (toolText (fromMaybe scope.missingMessage current))
    MemoryReplace ->
      case memory of
        Nothing ->
          pure (toolText "memory is required when action is replace")
        Just content
          | not context.superuser && Text.length content > Memory.memoryLimitChars ->
              pure (toolText [i|Memory update rejected: memory is #{Text.length content} characters, over the #{Memory.memoryLimitChars} character limit. Please summarize it more concisely and try again.|])
          | otherwise -> do
              result <- scope.replaceMemory cfg context.message content
              pure (toolText (either identity (const scope.updatedMessage) result))
    MemoryClear -> do
      result <- scope.clearMemory cfg context.message
      pure (toolText (either identity (const scope.clearedMessage) result))
