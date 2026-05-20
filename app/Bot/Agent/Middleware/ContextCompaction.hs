{-|
Module      : Bot.Agent.Middleware.ContextCompaction
Description : Agent conversation compaction middleware
Stability   : experimental
-}
module Bot.Agent.Middleware.ContextCompaction
  ( withContextCompaction
  , withContextCompactionNotice
  )
where

import Bot.Agent.Core
import Bot.Agent.Types (AgentContext (..))
import Bot.Core.Conversation
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

compactionHardLimit :: Int
compactionHardLimit =
  50

recentMessageWindow :: Int
recentMessageWindow =
  20

compactionNoticeMessage :: Text
compactionNoticeMessage =
  "正在整理较早的对话上下文..."

withContextCompaction :: (LLM.LLM :> es) => AgentProgram context es -> AgentProgram context es
withContextCompaction program =
  withContextCompactionUsing (\_ -> pure ()) program

withContextCompactionNotice :: (Chat.Chat :> es, LLM.LLM :> es) => AgentProgram context es -> AgentProgram context es
withContextCompactionNotice program =
  withContextCompactionUsing
    (\_ -> void $ Chat.replyTo program.agentRun.context.message compactionNoticeMessage)
    program

withContextCompactionUsing :: (LLM.LLM :> es) => (AgentState -> Eff es ()) -> AgentProgram context es -> AgentProgram context es
withContextCompactionUsing notify program =
  program
    { aroundModelTurn = \context agentState action -> do
        compactedState <- lift (compactAgentState notify agentState)
        program.aroundModelTurn context compactedState action
    }

compactAgentState :: LLM.LLM :> es => (AgentState -> Eff es ()) -> AgentState -> Eff es AgentState
compactAgentState notify agentState@AgentState{conversation = Conversation messages}
  | Seq.length messages < compactionHardLimit =
      pure agentState
  | otherwise = do
      let (older, newer) = splitCompactablePrefix messages
      notify agentState
      summary <- summarizeMessages (Foldable.toList older)
      pure AgentState
        { conversation = Conversation (LLM.systemText (summaryMessage summary) Seq.<| newer)
        , turn = agentState.turn
        }

splitCompactablePrefix :: Seq.Seq LLM.ChatMessage -> (Seq.Seq LLM.ChatMessage, Seq.Seq LLM.ChatMessage)
splitCompactablePrefix messages =
  let cutoff = max 0 (Seq.length messages - recentMessageWindow)
      (older, newer) = Seq.splitAt cutoff messages
      (leadingToolResults, rest) = Seq.spanl ((== "tool") . (.role)) newer
  in (older <> leadingToolResults, rest)

summarizeMessages :: LLM.LLM :> es => [LLM.ChatMessage] -> Eff es Text
summarizeMessages messages =
  Text.strip <$> LLM.askWithHistory
    [ LLM.systemText summarySystemPrompt
    , LLM.userText [i|Summarize this chat transcript for future continuation. Preserve user goals, decisions, constraints, tool results, generated artifacts, unresolved tasks, and any facts needed to answer later follow-up messages.

Transcript JSON:
#{messagesJson messages}|]
    ]

summarySystemPrompt :: Text
summarySystemPrompt =
  "You compact chatbot conversation history into a durable continuation summary. Return a concise but complete summary. Do not invent facts. Keep identifiers, file paths, URLs, commands, and tool results precise."

summaryMessage :: Text -> Text
summaryMessage summary =
  Text.strip [i|The earlier conversation was compacted. Use this summary as context for the continuation:

#{summary}|]

messagesJson :: [LLM.ChatMessage] -> Text
messagesJson =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode
