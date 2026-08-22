{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Bot.Agent.Tools.Transcript
Description : Bounded access to the current canonical agent transcript
Stability   : experimental
-}

module Bot.Agent.Tools.Transcript
  ( transcriptTool
  )
where

import qualified Bot.Agent as Agent
import Bot.Agent.Tool
import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Core.Transcript (Transcript (..))
import qualified Bot.Effect.Agent as AgentEffect
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import qualified Bot.LLM.Types as LLMTypes
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Data.Sequence as Seq
import qualified Streaming.Prelude as S
import Text.Regex.TDFA (Regex, makeRegexM, matchTest)

data TranscriptCall = TranscriptCall
  { op :: !Text
  , startMessage :: !Int
  , messageCount :: !Int
  , pattern :: !(Maybe Text)
  , prompt :: !(Maybe Text)
  , caseSensitive :: !Bool
  , maxMatches :: !Int
  }

transcriptTool
  :: ( AgentEffect.Agent :> es
     , AgentAudit.AgentAudit :> es
     , Chat.Chat :> es
     , Concurrency.Concurrency :> es
     , LLM.LLM :> es
     , Media.Media :> es
     , KatipE :> es
     , Concurrent :> es
     , Prim :> es
     , IOE :> es
     )
  => Tool (Eff es)
transcriptTool =
  transcriptToolWith maxRecursiveDepth Nothing Nothing

transcriptToolWith
  :: ( AgentEffect.Agent :> es
     , AgentAudit.AgentAudit :> es
     , Chat.Chat :> es
     , Concurrency.Concurrency :> es
     , LLM.LLM :> es
     , Media.Media :> es
     , KatipE :> es
     , Concurrent :> es
     , Prim :> es
     , IOE :> es
     )
  => Int
  -> Maybe Transcript
  -> Maybe (Eff es UseLimit)
  -> Tool (Eff es)
transcriptToolWith depth boundTranscript sharedUseLimit =
  withDescription "Inspect the canonical transcript. info reports its size; search finds messages with a POSIX regular expression; read returns a bounded message range; query starts a bounded recursive analyst over a selected range. Message indexes are zero-based."
  $ toolWithRunState "transcript"
      (parsedArguments schema parseCall)
      (\_ -> maybe (newUseLimiter (Just maxRecursiveQueries)) pure sharedUseLimit)
      \checkRecursiveUse call -> do
        transcript <- maybe askToolTranscript pure boundTranscript
        context <- askToolContext
        metadata <- askToolCallMetadata
        raise (runCall depth checkRecursiveUse context metadata transcript call)
  where
    schema = objectSchema
      [ fieldText "op" "One of: info, search, read, query."
      , fieldIntegerMax "start_message" 1000000 "Zero-based first message; defaults to 0."
      , fieldIntegerMax "message_count" 200 "Number of messages; defaults to 20 and is capped at 200."
      , fieldText "pattern" "POSIX regular expression; required for search."
      , fieldText "prompt" "Analysis question; required for query."
      , fieldBoolean "case_sensitive" "Whether search is case-sensitive; defaults to true."
      , fieldIntegerMax "max_matches" 100 "Maximum search matches; defaults to 20."
      ]
      ["op"]

maxRecursiveDepth :: Int
maxRecursiveDepth = 3

maxRecursiveQueries :: Int
maxRecursiveQueries = 32

parseCall :: Aeson.Value -> AesonTypes.Parser TranscriptCall
parseCall = Aeson.withObject "transcript arguments" \object -> do
  op <- object Aeson..: Key.fromText "op"
  startMessage <- fromMaybe 0 <$> object Aeson..:? Key.fromText "start_message"
  messageCount <- fromMaybe 20 <$> object Aeson..:? Key.fromText "message_count"
  pattern <- object Aeson..:? Key.fromText "pattern"
  prompt <- object Aeson..:? Key.fromText "prompt"
  caseSensitive <- fromMaybe True <$> object Aeson..:? Key.fromText "case_sensitive"
  maxMatches <- fromMaybe 20 <$> object Aeson..:? Key.fromText "max_matches"
  unless (op `elem` ["info", "search", "read", "query"]) (fail "op must be info, search, read, or query")
  when (op == "search" && isNothing pattern) (fail "pattern is required for search")
  when (op == "query" && isNothing prompt) (fail "prompt is required for query")
  pure TranscriptCall
    { op
    , startMessage
    , messageCount = min 200 messageCount
    , pattern
    , prompt
    , caseSensitive
    , maxMatches = min 100 maxMatches
    }

runCall
  :: ( AgentEffect.Agent :> es
     , AgentAudit.AgentAudit :> es
     , Chat.Chat :> es
     , Concurrency.Concurrency :> es
     , LLM.LLM :> es
     , Media.Media :> es
     , KatipE :> es
     , Concurrent :> es
     , Prim :> es
     , IOE :> es
     )
  => Int
  -> Eff es UseLimit
  -> Context
  -> ToolCallMetadata
  -> Transcript
  -> TranscriptCall
  -> Eff es ToolResult
runCall depth checkRecursiveUse context metadata transcript call =
  case call.op of
    "info" -> pure . toolText . jsonText $ transcriptInfo transcript
    "read" -> pure . toolText . jsonText $ rangeResult transcript call
    "search" -> pure (searchTranscript transcript call)
    "query" ->
      checkRecursiveUse >>= \case
        UseLimitReached uses ->
          pure (toolFailure (permanentArgumentFailure "Recursive transcript query limit reached." [i|Recursive transcript query limit reached after #{uses} calls.|]))
        UseAllowed
          | depth <= 0 -> queryTranscript transcript call
          | otherwise -> recursiveQuery depth checkRecursiveUse context metadata transcript call
    _ -> pure (toolFailure (permanentArgumentFailure "Unknown transcript operation." "Unknown transcript operation."))

recursiveQuery
  :: ( AgentEffect.Agent :> es
     , AgentAudit.AgentAudit :> es
     , Chat.Chat :> es
     , Concurrency.Concurrency :> es
     , LLM.LLM :> es
     , Media.Media :> es
     , KatipE :> es
     , Concurrent :> es
     , Prim :> es
     , IOE :> es
     )
  => Int
  -> Eff es UseLimit
  -> Context
  -> ToolCallMetadata
  -> Transcript
  -> TranscriptCall
  -> Eff es ToolResult
recursiveQuery depth checkRecursiveUse context metadata transcript call = do
  let snapshotMessages = selectedMessages transcript call
      snapshot = Transcript (Seq.fromList snapshotMessages)
      childContext = context
        { systemContext = recursiveSystemPrompt
        }
      childTranscript = Transcript (Seq.fromList
        [ LLM.systemText recursiveSystemPrompt
        , LLM.userText (Text.unlines
            [ "Question: " <> fromMaybe "" call.prompt
            , [i|The bound transcript snapshot contains #{length snapshotMessages} messages.|]
            , "Use the transcript tool to inspect and decompose it, then answer the question."
            ])
        ])
      childTools = [transcriptToolWith (depth - 1) (Just snapshot) (Just checkRecursiveUse)]
      childMetadata runId = ToolCallMetadata
        { agentRunId = runId
        , originRunId = metadata.originRunId
        , resourceOwner = metadata.resourceOwner
        }
  answer <- Agent.withAgentMetadata childMetadata $
    Agent.withRun 6 ContextCompaction 1000000 childContext childTools \runtime -> do
      _ S.:> result <- S.toList (Agent.agentStream runtime childTranscript)
      pure result.finalText
  pure (toolText (jsonText (Aeson.object ["answer" Aeson..= answer])))

recursiveSystemPrompt :: Text
recursiveSystemPrompt =
  "Analyze a bound transcript snapshot. Use the transcript tool for exact evidence and recursive decomposition. Return only the answer to the question."

transcriptInfo :: Transcript -> Aeson.Value
transcriptInfo Transcript{messages} =
  Aeson.object
    [ "message_count" Aeson..= length messages
    , "character_count" Aeson..= characters
    , "estimated_tokens" Aeson..= max 1 (characters `div` 4)
    ]
  where
    characters = sum (map (Text.length . searchableMessageText) (toList messages))

rangeResult :: Transcript -> TranscriptCall -> Aeson.Value
rangeResult transcript call =
  Aeson.object
    [ "start_message" Aeson..= call.startMessage
    , "message_count" Aeson..= length selected
    , "messages" Aeson..= selected
    ]
  where
    selected = selectedMessages transcript call

selectedMessages :: Transcript -> TranscriptCall -> [LLM.ChatMessage]
selectedMessages Transcript{messages} call =
  take call.messageCount . drop call.startMessage $ toList messages

searchTranscript :: Transcript -> TranscriptCall -> ToolResult
searchTranscript Transcript{messages} call =
  case makeRegexM (Text.unpack effectivePattern) :: Either String Regex of
    Left err -> toolFailure (permanentArgumentFailure (toText err) ("Invalid regular expression: " <> toText err))
    Right regex -> toolText . jsonText . take call.maxMatches $
      [ Aeson.object
          [ "message" Aeson..= index
          , "role" Aeson..= message.role
          , "snippet" Aeson..= preview 1000 text
          ]
      | (index, message) <- zip [0 :: Int ..] (toList messages)
      , let text = searchableMessageText message
      , matchTest regex (Text.unpack (normalize text))
      ]
  where
    effectivePattern = normalize (fromMaybe "" call.pattern)
    normalize = if call.caseSensitive then id else Text.toCaseFold
    preview limit text
      | Text.length text <= limit = text
      | otherwise = Text.take limit text <> "..."

queryTranscript :: LLM.LLM :> es => Transcript -> TranscriptCall -> Eff es ToolResult
queryTranscript transcript call
  | Text.length excerpt > 200000 =
      pure (toolFailure (permanentArgumentFailure "Transcript query range is too large." "The selected transcript range exceeds 200000 characters; request a smaller range."))
  | otherwise = do
      answer <- LLM.askWithTools []
        [ LLM.systemText "Answer the user's question using only the supplied transcript excerpt. Do not call tools. State when the excerpt is insufficient."
        , LLM.userText (Text.unlines
            [ "Question: " <> fromMaybe "" call.prompt
            , "Transcript excerpt (JSON):"
            , excerpt
            ])
        ]
      pure . toolText . jsonText $ Aeson.object
        [ "answer" Aeson..= LLMTypes.chatAnswerContent answer
        , "token_usage" Aeson..= LLMTypes.chatAnswerTokenUsage answer
        ]
  where
    excerpt = jsonText (selectedMessages transcript call)

searchableMessageText :: LLM.ChatMessage -> Text
searchableMessageText message =
  Text.unlines
    ( message.role
    : maybeToList (contentText =<< message.content)
    <> map (\call -> call.name <> " " <> call.arguments) message.toolCalls
    <> maybeToList message.toolCallId
    )

contentText :: LLM.MessageContent -> Maybe Text
contentText = \case
  LLM.TextContent text -> Just text
  LLM.PartsContent parts -> nonBlank (Text.unlines (mapMaybe partText parts))
  where
    nonBlank text
      | Text.null text = Nothing
      | otherwise = Just text
    partText = \case
      LLM.TextPart text -> Just text
      LLM.ImageUrlPart url -> Just ("[image: " <> url <> "]")
