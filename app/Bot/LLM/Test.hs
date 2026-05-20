{-|
Module      : Bot.LLM.Test
Description : Test interpreter for the LLM effect
Stability   : experimental
-}

module Bot.LLM.Test
  ( runLLMWith
  )
where

import Bot.Prelude
import qualified Bot.Effect.LLM as LLM
import Bot.LLM.Types
import qualified Streaming.Prelude as S

runLLMWith
  :: ([ChatMessage] -> Eff es Text)
  -> ([ChatMessage] -> Stream (Of Text) (Eff es) Text)
  -> ([ChatMessage] -> Eff es Text)
  -> (Text -> [Text] -> Maybe Text -> Eff es Text)
  -> ([FunctionTool] -> [ChatMessage] -> Eff es ChatAnswer)
  -> ([FunctionTool] -> [ChatMessage] -> Stream (Of Text) (Eff es) ChatAnswer)
  -> Eff (LLM.LLM : es) a
  -> Eff es a
runLLMWith askText askTextStream askImage askImageEditRequest askTools askToolsStream = interpret $ \localEnv operation ->
  localSeqLift localEnv \liftLocal ->
    case operation of
      LLM.Ask messages -> askText messages
      LLM.AskStream messages -> pure (LLM.liftLocalStream liftLocal (askTextStream messages))
      LLM.AskImage messages -> askImage messages
      LLM.AskImageStream messages -> pure (LLM.liftLocalStream liftLocal (textResultStream (askImage messages)))
      LLM.AskImageEdit prompt imageRefs maskRef -> askImageEditRequest prompt imageRefs maskRef
      LLM.AskTools tools messages -> askTools tools messages
      LLM.AskToolsStream tools messages -> pure (LLM.liftLocalStream liftLocal (askToolsStream tools messages))

textResultStream :: Monad m => m Text -> Stream (Of Text) m Text
textResultStream action = do
  answer <- lift action
  S.yield answer
  pure answer
