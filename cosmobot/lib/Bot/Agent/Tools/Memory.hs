{-|
Module      : Bot.Agent.Tools.Memory
Description : Agent tools for persistent sender and chat memory
Stability   : experimental
-}

module Bot.Agent.Tools.Memory
  ( senderMemoryTool
  , chatMemoryTool
  )
where

import Bot.Agent.Types
import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Core.Message
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Memory as MemoryStore
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

senderMemoryTool :: Memory.Memory :> es => Tool (Eff es)
senderMemoryTool = memoryTool
  "sender_memory"
  "View, replace, or clear persistent memory for the current sender. Use it for personal facts and preferences. Keep non-superuser memory within 1000 characters."
  senderMemoryScope

chatMemoryTool :: Memory.Memory :> es => Tool (Eff es)
chatMemoryTool = memoryTool
  "chat_memory"
  "View, replace, or clear persistent memory shared by the current chat. Keep non-superuser memory within 1000 characters."
  chatMemoryScope

memoryTool :: Memory.Memory :> es => Text -> Text -> MemoryScope -> Tool (Eff es)
memoryTool name description scope =
  noisy
  . withDescription description
  $ tool name
      (parsedArguments
        (objectSchema
          [ fieldText "action" "One of: view, replace, clear."
          , fieldText "memory" "Complete replacement MEMORY.md content. Required only when action is replace."
          , fieldText "message" "Short, single-line reason for this operation (72 characters maximum). Used as the Git commit subject when memory changes."
          ]
          ["action", "message"])
        memoryArgs)
      \(action, memory, message) -> do
        context <- askToolContext
        runMemoryAction scope context action memory message

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

memoryArgs :: Aeson.Value -> AesonTypes.Parser (MemoryAction, Maybe Text, MemoryStore.MemoryCommitMessage)
memoryArgs =
  Aeson.withObject "memory arguments" $ \o -> do
    actionText <- Text.toLower . Text.strip <$> o Aeson..: Key.fromText "action"
    memory <- fmap Text.strip <$> o Aeson..:? Key.fromText "memory"
    rawMessage <- o Aeson..: Key.fromText "message"
    message <- either (fail . Text.unpack) pure (MemoryStore.memoryCommitMessage rawMessage)
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
    pure (action, memory, message)

runMemoryAction :: Memory.Memory :> es => MemoryScope -> Context -> MemoryAction -> Maybe Text -> MemoryStore.MemoryCommitMessage -> Eff es ToolResult
runMemoryAction scope context action memory message =
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
                  Memory.replaceMemory memoryScope message content
                  pure (toolText scope.updatedMessage)
        MemoryClear -> do
          Memory.clearMemory memoryScope message
          pure (toolText scope.clearedMessage)
