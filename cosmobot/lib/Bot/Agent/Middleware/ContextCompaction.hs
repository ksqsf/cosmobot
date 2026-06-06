{-|
Module      : Bot.Agent.Middleware.ContextCompaction
Description : Agent transcript compaction middleware
Stability   : experimental
-}
module Bot.Agent.Middleware.ContextCompaction
  ( withContextCompaction
  , withContextCompactionNotice
  )
where

import Bot.Agent.Core
import Bot.Agent.Middleware.ToolResultCompaction (NextModelInput (..))
import Bot.Agent.Types (AgentContext (..))
import Bot.Core.Transcript
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

recentMessageWindow :: Int
recentMessageWindow =
  20

compactionNoticeMessage :: Text
compactionNoticeMessage =
  "正在整理较早的对话上下文..."

withContextCompaction
  :: (LLM.LLM :> es, HList.Has NextModelInput transient, HList.Put NextModelInput transient)
  => Int
  -> AgentProgram transient context es
  -> AgentProgram transient context es
withContextCompaction tokenThreshold program =
  withContextCompactionUsing tokenThreshold (\_ -> pure ()) program

withContextCompactionNotice
  :: (Chat.Chat :> es, LLM.LLM :> es, HList.Has NextModelInput transient, HList.Put NextModelInput transient)
  => Int
  -> AgentProgram transient context es
  -> AgentProgram transient context es
withContextCompactionNotice tokenThreshold program =
  withContextCompactionUsing
    tokenThreshold
    (\_ -> void $ Chat.replyTo program.agentRun.context.message compactionNoticeMessage)
    program

withContextCompactionUsing
  :: (LLM.LLM :> es, HList.Has NextModelInput transient, HList.Put NextModelInput transient)
  => Int
  -> (AgentState transient -> Eff es ())
  -> AgentProgram transient context es
  -> AgentProgram transient context es
withContextCompactionUsing tokenThreshold notify program =
  program
    { aroundModelTurn = \context agentState action -> do
        compactedState <- lift (compactAgentState tokenThreshold notify agentState)
        program.aroundModelTurn context compactedState action
    }

compactAgentState
  :: (LLM.LLM :> es, HList.Has NextModelInput transient, HList.Put NextModelInput transient)
  => Int
  -> (AgentState transient -> Eff es ())
  -> AgentState transient
  -> Eff es (AgentState transient)
compactAgentState tokenThreshold notify agentState
  | not (shouldCompact tokenThreshold agentState.modelTokenUsage) =
      pure agentState
  | otherwise = do
      let modelTranscript = selectedTranscript agentState
          (older, _) = compactableTranscriptParts modelTranscript
      if Seq.null older
        then pure agentState{modelTokenUsage = Nothing}
        else do
          notify agentState
          summary <- summarizeMessages (Foldable.toList older)
          let modelCompactedTranscript = compactTranscriptWithSummary summary modelTranscript
              canonicalCompactedTranscript = compactTranscriptWithSummary summary agentState.transcript
          pure AgentState
            { transcript = canonicalCompactedTranscript
            , turn = agentState.turn
            , modelTokenUsage = Nothing
            , transient = HList.put (NextModelInput (Just modelCompactedTranscript)) agentState.transient
            }

selectedTranscript :: HList.Has NextModelInput transient => AgentState transient -> Transcript
selectedTranscript agentState =
  fromMaybe agentState.transcript (HList.get @NextModelInput agentState.transient).transcript

compactableTranscriptParts :: Transcript -> (Seq.Seq LLM.ChatMessage, Seq.Seq LLM.ChatMessage)
compactableTranscriptParts (Transcript messages) =
  splitCompactablePrefix messages

compactTranscriptWithSummary :: Text -> Transcript -> Transcript
compactTranscriptWithSummary summary transcript =
  let (_, newer) = compactableTranscriptParts transcript
  in Transcript (LLM.systemText (summaryMessage summary) Seq.<| newer)

shouldCompact :: Int -> Maybe LLM.TokenUsage -> Bool
shouldCompact tokenThreshold usage =
  maybe False ((>= tokenThreshold) . (.totalTokens)) usage

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
  "You compact chatbot transcript into a durable continuation summary. Return a concise but complete summary. Do not invent facts. Keep identifiers, file paths, URLs, commands, and tool results precise."

summaryMessage :: Text -> Text
summaryMessage summary =
  Text.strip [i|The earlier transcript was compacted. Use this summary as context for the continuation:

#{summary}|]

messagesJson :: [LLM.ChatMessage] -> Text
messagesJson =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode
