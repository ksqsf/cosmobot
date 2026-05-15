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
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Memory as MemoryStore
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

manageMemoryTool :: Memory.Memory :> es => Tool es
manageMemoryTool = Tool
  { name = "manage_current_sender_memory"
  , description = "View, replace, or clear the persistent MEMORY.md for the current message sender. Use this when the sender asks to view or clear memory, or when the sender gives durable preferences such as a preferred name, style, language, stable personal facts, or recurring instructions. Keep memory concise: non-superusers must stay within 1000 characters; if an update is rejected, summarize it shorter and try again."
  , parameters = objectSchema
      [ fieldText "action" "One of: view, replace, clear."
      , fieldText "memory" "Complete replacement MEMORY.md content. Required only when action is replace."
      ]
      ["action"]
  , noisy = False
  , allowed = everyone
  , start = \context -> pure \args ->
      withParsedToolArgs memoryArgs args \(action, memory) ->
        runMemoryAction senderMemoryScope context action memory
  }

manageChatMemoryTool :: Memory.Memory :> es => Tool es
manageChatMemoryTool = Tool
  { name = "manage_current_chat_memory"
  , description = "View, replace, or clear the persistent MEMORY.md for the current chat/conversation. Use this when the user asks to view or clear chat memory, or when durable preferences, facts, norms, recurring instructions, or context apply to this chat as a whole rather than only to the current sender. Keep memory concise: non-superusers must stay within 1000 characters; if an update is rejected, summarize it shorter and try again."
  , parameters = objectSchema
      [ fieldText "action" "One of: view, replace, clear."
      , fieldText "memory" "Complete replacement MEMORY.md content. Required only when action is replace."
      ]
      ["action"]
  , noisy = False
  , allowed = everyone
  , start = \context -> pure \args ->
      withParsedToolArgs memoryArgs args \(action, memory) ->
        runMemoryAction chatMemoryScope context action memory
  }

data MemoryAction
  = MemoryView
  | MemoryReplace
  | MemoryClear

data MemoryScope = MemoryScope
  { missingMessage :: !Text
  , updatedMessage :: !Text
  , clearedMessage :: !Text
  , scopeOf :: IncomingMessage -> Either Text MemoryStore.MemoryScope
  }

senderMemoryScope :: MemoryScope
senderMemoryScope = MemoryScope
  { missingMessage = "No memory is stored for the current sender."
  , updatedMessage = "Memory updated."
  , clearedMessage = "Memory cleared."
  , scopeOf = MemoryStore.senderMemoryScope
  }

chatMemoryScope :: MemoryScope
chatMemoryScope = MemoryScope
  { missingMessage = "No memory is stored for the current chat."
  , updatedMessage = "Chat memory updated."
  , clearedMessage = "Chat memory cleared."
  , scopeOf = MemoryStore.chatMemoryScope
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

runMemoryAction :: Memory.Memory :> es => MemoryScope -> AgentContext es -> MemoryAction -> Maybe Text -> Eff es ToolResult
runMemoryAction scope context action memory =
  case scope.scopeOf context.message of
    Left err ->
      pure (toolText err)
    Right memoryScope ->
      case action of
        MemoryView -> do
          current <- Memory.loadMemory memoryScope
          pure (toolText (fromMaybe scope.missingMessage current))
        MemoryReplace ->
          case memory of
            Nothing ->
              pure (toolText "memory is required when action is replace")
            Just content
              | not context.superuser && Text.length content > MemoryStore.memoryLimitChars ->
                  pure (toolText [i|Memory update rejected: memory is #{Text.length content} characters, over the #{MemoryStore.memoryLimitChars} character limit. Please summarize it more concisely and try again.|])
              | otherwise -> do
                  Memory.replaceMemory memoryScope content
                  pure (toolText scope.updatedMessage)
        MemoryClear -> do
          Memory.clearMemory memoryScope
          pure (toolText scope.clearedMessage)
