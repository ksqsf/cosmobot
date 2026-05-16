{-|
Module      : Bot.Effect.LLM
Description : Query LLM
Stability   : experimental
-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bot.Effect.LLM
  ( -- * Effect
    LLM
  , ask
  , askWithHistory
  , askStreamingWithHistory
  , askImageWithHistory
  , askWithTools
  , askWithToolsStreaming
  , runLLM
  , runLLMWith
  , LLMException (..)
  , llmExceptionSummary

    -- * Configuration
  , Config (..)
  , ImageGenerationApi (..)
  , defaultConfig

    -- * Conversation values
  , ChatMessage (..)
  , MessageContent (..)
  , ContentPart (..)
  , ChatAnswer (..)
  , FunctionTool (..)
  , ToolCall (..)
  , userText
  , userWithImages
  , systemText
  , contextSystemPrompt
  , memorySystemPrompt
  , assistantText
  , assistantAnswer
  , toolResult
  )
where

import Bot.Prelude hiding (ask)
import Bot.Effect.LLM.Transport
  ( ChatAnswer (..)
  , ChatMessage (..)
  , Config (..)
  , ContentPart (..)
  , FunctionTool (..)
  , ImageGenerationApi (..)
  , LLMException (..)
  , MessageContent (..)
  , ToolCall (..)
  , assistantAnswer
  , assistantText
  , defaultConfig
  , llmExceptionSummary
  , memorySystemPrompt
  , contextSystemPrompt
  , systemText
  , toolResult
  , userText
  , userWithImages
  )
import qualified Bot.Effect.LLM.Retry as Retry
import qualified Bot.Effect.LLM.Transport as Transport
import qualified Streaming.Prelude as S

-- | Effect for text, image, and tool-calling LLM requests.
data LLM :: Effect where
  Ask :: [ChatMessage] -> LLM m Text
  AskStream :: [ChatMessage] -> LLM m (Stream (Of Text) m Text)
  AskImage :: [ChatMessage] -> LLM m Text
  AskTools :: [FunctionTool] -> [ChatMessage] -> LLM m ChatAnswer
  AskToolsStream :: [FunctionTool] -> [ChatMessage] -> LLM m (Stream (Of Text) m ChatAnswer)

type instance DispatchOf LLM = Dynamic

-- | Ask a one-shot text question without preserving history.
ask :: LLM :> es => Text -> Eff es Text
ask prompt = askWithHistory [userText prompt]

-- | Ask for a text answer using an explicit chat history.
askWithHistory :: LLM :> es => [ChatMessage] -> Eff es Text
askWithHistory = send . Ask

-- | Ask for a text answer while receiving response chunks.
askStreamingWithHistory :: (LLM :> es, IOE :> es) => [ChatMessage] -> Stream (Of Text) (Eff es) Text
askStreamingWithHistory messages = do
  stream <- lift (send (AskStream messages))
  stream

-- | Ask the configured image model to generate an image response.
askImageWithHistory :: LLM :> es => [ChatMessage] -> Eff es Text
askImageWithHistory = send . AskImage

-- | Ask with function tools and return both text and tool calls.
askWithTools :: LLM :> es => [FunctionTool] -> [ChatMessage] -> Eff es ChatAnswer
askWithTools tools messages =
  send (AskTools tools messages)

-- | Ask with function tools while receiving final-answer text chunks.
askWithToolsStreaming :: (LLM :> es, IOE :> es) => [FunctionTool] -> [ChatMessage] -> Stream (Of Text) (Eff es) ChatAnswer
askWithToolsStreaming tools messages = do
  stream <- lift (send (AskToolsStream tools messages))
  stream

-- | Interpret LLM requests through an OpenAI-compatible HTTP endpoint.
runLLM
  :: IOE :> es
  => Log :> es
  => Config
  -> Eff (LLM : es) a
  -> Eff es a
runLLM cfg = interpret $ \localEnv operation ->
  localSeqLift localEnv \liftLocal ->
    case operation of
      Ask messages ->
        Retry.retryLLMRequest "LLM request" (Transport.askOpenAI cfg messages)
      AskStream messages ->
        pure $
          liftLocalStream liftLocal $
            Retry.retryLLMStreamRequest "LLM streaming request" $
              pure (Retry.validateTextStream (Transport.askOpenAIStreaming cfg messages))
      AskImage messages ->
        Retry.retryLLMRequest "LLM image request" (Transport.askImageOpenAI cfg messages)
      AskTools tools messages ->
        Retry.retryLLMRequest "LLM request" (Transport.askOpenAIWithTools cfg tools messages >>= Retry.validateChatAnswer)
      AskToolsStream tools messages ->
        pure $
          liftLocalStream liftLocal $
            Retry.retryLLMStreamRequest "LLM streaming request" $
              pure (Retry.validateChatAnswerStream (Transport.askOpenAIWithToolsStreaming cfg tools messages))

runLLMWith
  :: ([ChatMessage] -> Eff es Text)
  -> ([ChatMessage] -> Stream (Of Text) (Eff es) Text)
  -> ([ChatMessage] -> Eff es Text)
  -> ([FunctionTool] -> [ChatMessage] -> Eff es ChatAnswer)
  -> ([FunctionTool] -> [ChatMessage] -> Stream (Of Text) (Eff es) ChatAnswer)
  -> Eff (LLM : es) a
  -> Eff es a
runLLMWith askText askTextStream askImage askTools askToolsStream = interpret $ \localEnv operation ->
  localSeqLift localEnv \liftLocal ->
    case operation of
      Ask messages -> askText messages
      AskStream messages -> pure (liftLocalStream liftLocal (askTextStream messages))
      AskImage messages -> askImage messages
      AskTools tools messages -> askTools tools messages
      AskToolsStream tools messages -> pure (liftLocalStream liftLocal (askToolsStream tools messages))

liftLocalStream
  :: Monad m
  => (forall x. Eff es x -> m x)
  -> Stream (Of a) (Eff es) r
  -> Stream (Of a) m r
liftLocalStream liftLocal stream = do
  next <- lift (liftLocal (S.next stream))
  case next of
    Left result ->
      pure result
    Right (chunk, rest) -> do
      S.yield chunk
      liftLocalStream liftLocal rest
