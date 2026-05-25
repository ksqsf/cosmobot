{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.LLM.OpenAI
Description : OpenAI-compatible LLM interpreter
Stability   : experimental
-}


module Bot.LLM.OpenAI
  ( runLLM
  )
where

import Bot.Prelude
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import Bot.LLM.OpenAI.Config
import qualified Bot.LLM.OpenAI.Retry as Retry
import qualified Bot.LLM.OpenAI.Transport as Transport
import Bot.LLM.Types
import qualified Data.Text as Text
import qualified Streaming.Prelude as S
import Effectful.FileSystem
import Effectful.Process
import Effectful.Timeout

-- | Interpret LLM requests through an OpenAI-compatible HTTP endpoint.
runLLM
  :: ( Fail :> es
     , Timeout :> es
     , FileSystem :> es
     , Process :> es
     , HTTP.HTTP :> es
     , Media.Media :> es
     , IOE :> es)
  => KatipE :> es
  => Config
  -> Eff (LLM.LLM : es) a
  -> Eff es a
runLLM cfg = interpret $ \localEnv operation ->
  localSeqLift localEnv \liftLocal ->
    case operation of
      LLM.AskStream messages ->
        pure $
          LLM.liftLocalStream liftLocal $
            do
              resolved <- lift (resolveChatMessages messages)
              Retry.retryLLMStreamRequest "LLM streaming request" $
                pure (Retry.validateTextStream (normalizeReplyResult (Transport.askOpenAIStreaming cfg resolved)))
      LLM.AskImageStream options messages ->
        pure $
          LLM.liftLocalStream liftLocal $
            do
              resolved <- lift (resolveChatMessages messages)
              Retry.retryLLMStreamRequest "LLM image streaming request" $
                pure (Retry.validateTextStream (normalizeReplyResult (Transport.askImageOpenAIStreaming cfg options resolved)))
      LLM.AskImageEditStream options prompt imageRefs maskRef ->
        pure $
          LLM.liftLocalStream liftLocal $
            do
              resolvedImageRefs <- lift (traverse Media.publicMediaRef imageRefs)
              resolvedMaskRef <- lift (traverse Media.publicMediaRef maskRef)
              Retry.retryLLMStreamRequest "LLM image edit streaming request" $
                pure (Retry.validateTextStream (normalizeReplyResult (Transport.askImageEditOpenAIStreaming cfg options prompt resolvedImageRefs resolvedMaskRef)))
      LLM.AskAudioStream options messages ->
        pure $
          LLM.liftLocalStream liftLocal $
            do
              resolved <- lift (resolveChatMessages messages)
              Retry.retryLLMStreamRequest "LLM audio streaming request" $
                pure (Retry.validateTextStream (normalizeReplyResult (Transport.askAudioOpenAIStreaming cfg options resolved)))
      LLM.AskToolsStream tools messages ->
        pure $
          LLM.liftLocalStream liftLocal $
            do
              resolved <- lift (resolveChatMessages messages)
              Retry.retryLLMStreamRequest "LLM streaming request" $
                pure (Retry.validateChatAnswerStream (Transport.askOpenAIWithToolsStreaming cfg tools resolved))

resolveChatMessages :: Media.Media :> es => [ChatMessage] -> Eff es [ChatMessage]
resolveChatMessages =
  traverse resolveChatMessage

resolveChatMessage :: Media.Media :> es => ChatMessage -> Eff es ChatMessage
resolveChatMessage message =
  case message.content of
    Just (PartsContent parts) -> do
      resolvedParts <- traverse resolveContentPart parts
      pure ChatMessage
        { role = message.role
        , content = Just (PartsContent resolvedParts)
        , toolCalls = message.toolCalls
        , toolCallId = message.toolCallId
        }
    _ ->
      pure message

resolveContentPart :: Media.Media :> es => ContentPart -> Eff es ContentPart
resolveContentPart = \case
  ImageUrlPart ref ->
    ImageUrlPart <$> Media.publicMediaRef ref
  part ->
    pure part

data ReplyNormalizeState
  = ReplyNormal !Text
  | ReplyCollectImage !Text

normalizeReplyResult :: Media.Media :> es => Stream (Of Text) (Eff es) Text -> Stream (Of Text) (Eff es) Text
normalizeReplyResult stream =
  normalizeReplyResultWith (ReplyNormal "") stream

normalizeReplyResultWith :: Media.Media :> es => ReplyNormalizeState -> Stream (Of Text) (Eff es) Text -> Stream (Of Text) (Eff es) Text
normalizeReplyResultWith normalizeState stream =
  lift (S.next stream) >>= \case
    Left result -> do
      flushReplyNormalizeState normalizeState
      lift (Media.normalizeReplyBody result)
    Right (chunk, rest) -> do
      nextState <- normalizeReplyChunk normalizeState chunk
      normalizeReplyResultWith nextState rest

normalizeReplyChunk :: Media.Media :> es => ReplyNormalizeState -> Text -> Stream (Of Text) (Eff es) ReplyNormalizeState
normalizeReplyChunk normalizeState chunk =
  case normalizeState of
    ReplyNormal pending ->
      processNormal (pending <> chunk)
    ReplyCollectImage pending ->
      processCollectedImage (pending <> chunk)

processNormal :: Media.Media :> es => Text -> Stream (Of Text) (Eff es) ReplyNormalizeState
processNormal text =
  case findImageDirectiveStart text of
    Just offset -> do
      let (prefix, imageAndRest) = Text.splitAt offset text
      yieldNonEmpty prefix
      processCollectedImage imageAndRest
    Nothing -> do
      let (ready, pending) = splitReadyText text
      yieldNonEmpty ready
      pure (ReplyNormal pending)

processCollectedImage :: Media.Media :> es => Text -> Stream (Of Text) (Eff es) ReplyNormalizeState
processCollectedImage text =
  case Text.breakOn "\n" text of
    (imageLine, restWithNewline)
      | Text.null restWithNewline ->
          pure (ReplyCollectImage imageLine)
      | otherwise -> do
          normalized <- lift (Media.normalizeReplyBody imageLine)
          yieldNonEmpty normalized
          S.yield "\n"
          processNormal (Text.drop 1 restWithNewline)

flushReplyNormalizeState :: Media.Media :> es => ReplyNormalizeState -> Stream (Of Text) (Eff es) ()
flushReplyNormalizeState = \case
  ReplyNormal pending ->
    yieldNonEmpty pending
  ReplyCollectImage pending -> do
    normalized <- lift (Media.normalizeReplyBody pending)
    yieldNonEmpty normalized

findImageDirectiveStart :: Text -> Maybe Int
findImageDirectiveStart =
  go 0
  where
    go offset text =
      case Text.breakOn imageDirectivePrefix text of
        (_, "") ->
          Nothing
        (before, _matched)
          | imageDirectiveAtLineStart before ->
              Just (offset + Text.length before)
          | otherwise ->
              let consumed = Text.length before + 1
              in go (offset + consumed) (Text.drop consumed text)

imageDirectiveAtLineStart :: Text -> Bool
imageDirectiveAtLineStart before =
  Text.all isHorizontalSpace (Text.takeWhileEnd (/= '\n') before)

splitReadyText :: Text -> (Text, Text)
splitReadyText text =
  Text.splitAt (max 0 (Text.length text - imageDirectiveOverlapChars)) text

imageDirectivePrefix :: Text
imageDirectivePrefix =
  "[image] "

imageDirectiveOverlapChars :: Int
imageDirectiveOverlapChars =
  Text.length imageDirectivePrefix + 1

isHorizontalSpace :: Char -> Bool
isHorizontalSpace char =
  char == ' ' || char == '\t' || char == '\r'

yieldNonEmpty :: Text -> Stream (Of Text) (Eff es) ()
yieldNonEmpty text =
  unless (Text.null text) (S.yield text)
