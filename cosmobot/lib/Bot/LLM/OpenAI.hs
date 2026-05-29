{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
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
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import Bot.LLM.OpenAI.Config
import qualified Bot.LLM.OpenAI.Retry as Retry
import qualified Bot.LLM.OpenAI.Transport as Transport
import Bot.LLM.Types
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Text as Text
import qualified Streaming.ByteString as Q
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
                pure (Retry.validateTextStream (askImageStreamingWithMedia cfg options resolved))
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

-- Image Generation Media Streaming

askImageStreamingWithMedia
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Timeout :> es, Media.Media :> es, FileSystem :> es, Process :> es, Fail :> es)
  => Config
  -> LLM.ImageRequestOptions
  -> [ChatMessage]
  -> Stream (Of Text) (Eff es) Text
askImageStreamingWithMedia cfg options messages =
  case cfg.imageProvider of
    Just provider@ImageProviderConfig{apiKey = Just key, canGenerate = True} ->
      streamGeneratedImageToMedia provider key options messages
    _ ->
      normalizeReplyResult (Transport.askImageOpenAIStreaming cfg options messages)

streamGeneratedImageToMedia
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Timeout :> es, Media.Media :> es)
  => ImageProviderConfig
  -> Text
  -> LLM.ImageRequestOptions
  -> [ChatMessage]
  -> Stream (Of Text) (Eff es) Text
streamGeneratedImageToMedia provider@ImageProviderConfig{baseUrl, model, requestTimeout} key options messages = do
  let requestPath = ["images", "generations"]
      request = Transport.imageGenerationStreamingRequestPayload provider options model (imagePromptFromMessages messages)
      mime = generatedImageMimeType
      sourceName = Just (generatedImageSourceName mime)
  ref <- lift $
    runTimedImageMediaStore requestTimeout $
      withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
        runInIO $
          Media.storeMediaObject Media.MediaObject
          { bytes = effByteStreamToResourceTIO runInIO $
              Transport.streamImageGenerationImageBytes baseUrl requestPath key (requestTimeout * 1000000) request
          , mimeType = mime
          , sourceName
          }
  case ref of
    Nothing ->
      lift (throwIO (LLMException "Image generation response could not be stored in media cache."))
    Just mediaRef -> do
      let answer = ReplyBody.imageDirective mediaRef
      S.yield answer
      pure answer

runTimedImageMediaStore :: (Timeout :> es, IOE :> es) => Int -> Eff es a -> Eff es a
runTimedImageMediaStore timeoutSeconds action = do
  result <- timeout (timeoutSeconds * 1000000) action
  case result of
    Just value ->
      pure value
    Nothing ->
      throwIO (LLMException [i|LLM image streaming request timed out after #{timeoutSeconds} seconds.|])

effByteStreamToResourceTIO
  :: (forall a. Eff es a -> IO a)
  -> Q.ByteStream (Eff es) ()
  -> Q.ByteStream (ResourceT IO) ()
effByteStreamToResourceTIO runInIO byteStream =
  Q.fromChunks (go (Q.toChunks byteStream))
  where
    go chunks = do
      next <- liftIO (runInIO (S.next chunks))
      case next of
        Left () ->
          pure ()
        Right (chunk, rest) -> do
          S.yield chunk
          go rest

generatedImageMimeType :: Text
generatedImageMimeType =
  "image/png"

generatedImageSourceName :: Text -> Text
generatedImageSourceName mime =
  case Text.toLower mime of
    "image/jpeg" -> "llm-image.jpg"
    "image/webp" -> "llm-image.webp"
    _ -> "llm-image.png"

imagePromptFromMessages :: [ChatMessage] -> Text
imagePromptFromMessages messages =
  case Text.strip (Text.intercalate "\n\n" (mapMaybe chatMessagePromptText messages)) of
    "" -> "Generate an image."
    prompt -> prompt

chatMessagePromptText :: ChatMessage -> Maybe Text
chatMessagePromptText message =
  case message.content of
    Just (TextContent text) ->
      nonEmptyText text
    Just (PartsContent parts) ->
      nonEmptyText (Text.unlines (map partPromptText parts))
    Nothing ->
      Nothing

partPromptText :: ContentPart -> Text
partPromptText = \case
  TextPart text ->
    text
  ImageUrlPart url ->
    "Input image: " <> url

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped

-- Reply Normalization

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
