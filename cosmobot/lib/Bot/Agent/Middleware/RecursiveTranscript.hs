{-|
Module      : Bot.Agent.Middleware.RecursiveTranscript
Description : Externalize old model-visible transcript while preserving canonical history
Stability   : experimental
-}

module Bot.Agent.Middleware.RecursiveTranscript
  ( withRecursiveTranscript
  , externalizeTranscript
  )
where

import Bot.Agent.Core
import Bot.Agent.Middleware.Observation.Types (EventObservation (..))
import Bot.Agent.Types (Event (..))
import Bot.Core.Transcript (Transcript (..))
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

withRecursiveTranscript
  :: forall es context.
     HList.Has (EventObservation es) context
  => Int
  -> Runtime context (Eff es)
  -> Runtime context (Eff es)
withRecursiveTranscript tokenThreshold runtime =
  runtime
    { modelInputTranscript = \context turnState -> do
        transcript <- runtime.modelInputTranscript context turnState
        case externalizedTranscript tokenThreshold transcript of
          Nothing -> pure transcript
          Just projected -> do
            void $ (HList.get @(EventObservation es) context).observeAgentEvent
              RecursiveTranscriptFlushed
                { runId = runtime.runId
                , turn = turnState.turn
                }
            pure projected
    }

externalizeTranscript :: Int -> Transcript -> Transcript
externalizeTranscript tokenThreshold transcript =
  fromMaybe transcript (externalizedTranscript tokenThreshold transcript)

externalizedTranscript :: Int -> Transcript -> Maybe Transcript
externalizedTranscript tokenThreshold Transcript{messages}
  | hiddenTokenEstimate < tokenThreshold = Nothing
  | Seq.null hidden = Nothing
  | otherwise = Just (Transcript (systemPrefix <> Seq.singleton notice <> currentTurn))
  where
    messageList = toList messages
    systemCount = length (takeWhile ((== "system") . (.role)) messageList)
    currentTurnStart = fromMaybe (length messageList) (findLastUserIndex messageList)
    systemPrefix = Seq.take systemCount messages
    hidden = Seq.take (max 0 (currentTurnStart - systemCount)) (Seq.drop systemCount messages)
    currentTurn = Seq.drop currentTurnStart messages
    hiddenChars = sum (map (Text.length . searchableMessageText) (toList hidden))
    hiddenTokenEstimate = max 1 (hiddenChars `div` 4)
    notice = LLM.systemText . Text.unlines $
      [ "Earlier transcript messages have been externalized from this model view."
      , [i|Hidden message range: [#{systemCount}, #{currentTurnStart}); approximately #{hiddenTokenEstimate} tokens.|]
      , "Use the transcript tool with info, search, read, or query to inspect the canonical transcript."
      , "The transcript tool uses zero-based message indexes."
      ]

findLastUserIndex :: [LLM.ChatMessage] -> Maybe Int
findLastUserIndex =
  fmap fst . find ((== "user") . (.role) . snd) . reverse . zip [0 ..]

searchableMessageText :: LLM.ChatMessage -> Text
searchableMessageText message =
  Text.unlines (message.role : maybeToList (contentText =<< message.content))

contentText :: LLM.MessageContent -> Maybe Text
contentText = \case
  LLM.TextContent text -> Just text
  LLM.PartsContent parts ->
    nonBlank (Text.unlines (mapMaybe partText parts))
  where
    nonBlank text
      | Text.null text = Nothing
      | otherwise = Just text
    partText = \case
      LLM.TextPart text -> Just text
      LLM.ImageUrlPart url -> Just ("[image: " <> url <> "]")
