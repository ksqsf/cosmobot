{-|
Module      : Bot.Effect.LLM
Description : Query LLM
Stability   : experimental
-}

module Bot.Effect.LLM
  ( -- * Effect
    LLM (..)
  , ask
  , askWithHistory
  , askStreamingWithHistory
  , askImageWithHistory
  , askImageStreamingWithHistory
  , askImageEdit
  , askWithTools
  , askWithToolsStreaming
  , liftLocalStream
  , LLMException (..)
  , llmExceptionSummary

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
import Bot.LLM.Types
import qualified Streaming.Prelude as S

-- | Effect for text, image, and tool-calling LLM requests.
data LLM :: Effect where
  Ask :: [ChatMessage] -> LLM m Text
  AskStream :: [ChatMessage] -> LLM m (Stream (Of Text) m Text)
  AskImage :: [ChatMessage] -> LLM m Text
  AskImageStream :: [ChatMessage] -> LLM m (Stream (Of Text) m Text)
  AskImageEdit :: Text -> [Text] -> Maybe Text -> LLM m Text
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
askImageWithHistory messages =
  S.effects (askImageStreamingWithHistory messages)

-- | Ask the configured image model to generate an image response over the streaming transport.
askImageStreamingWithHistory :: LLM :> es => [ChatMessage] -> Stream (Of Text) (Eff es) Text
askImageStreamingWithHistory messages = do
  stream <- lift (send (AskImageStream messages))
  stream

-- | Ask the configured image model to edit one or more input images.
askImageEdit :: LLM :> es => Text -> [Text] -> Maybe Text -> Eff es Text
askImageEdit prompt imageRefs maskRef =
  send (AskImageEdit prompt imageRefs maskRef)

-- | Ask with function tools and return both text and tool calls.
askWithTools :: LLM :> es => [FunctionTool] -> [ChatMessage] -> Eff es ChatAnswer
askWithTools tools messages =
  send (AskTools tools messages)

-- | Ask with function tools while receiving assistant content chunks.
askWithToolsStreaming :: LLM :> es => [FunctionTool] -> [ChatMessage] -> Stream (Of Text) (Eff es) ChatAnswer
askWithToolsStreaming tools messages = do
  stream <- lift (send (AskToolsStream tools messages))
  stream

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
