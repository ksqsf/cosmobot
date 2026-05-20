{-|
Module      : Bot.LLM.OpenAI
Description : OpenAI-compatible LLM interpreter
Stability   : experimental
-}

{-# LANGUAGE OverloadedLabels #-}

module Bot.LLM.OpenAI
  ( runLLM
  )
where

import Bot.Prelude
import qualified Bot.Effect.LLM as LLM
import Bot.LLM.OpenAI.Config
import qualified Bot.LLM.OpenAI.Retry as Retry
import qualified Bot.LLM.OpenAI.Transport as Transport
import Effectful.FileSystem
import Effectful.Process
import Effectful.Timeout

-- | Interpret LLM requests through an OpenAI-compatible HTTP endpoint.
runLLM
  :: ( Fail :> es
     , Timeout :> es
     , FileSystem :> es
     , Process :> es
     , IOE :> es)
  => Log :> es
  => Config
  -> Eff (LLM.LLM : es) a
  -> Eff es a
runLLM cfg = interpret $ \localEnv operation ->
  localSeqLift localEnv \liftLocal ->
    case operation of
      LLM.Ask messages ->
        Retry.retryLLMRequest "LLM request" (Transport.askOpenAI cfg messages)
      LLM.AskStream messages ->
        pure $
          LLM.liftLocalStream liftLocal $
            Retry.retryLLMStreamRequest "LLM streaming request" $
              pure (Retry.validateTextStream (Transport.askOpenAIStreaming cfg messages))
      LLM.AskImage options messages ->
        Retry.retryLLMRequest "LLM image request" (Transport.askImageOpenAI cfg options messages)
      LLM.AskImageStream options messages ->
        pure $
          LLM.liftLocalStream liftLocal $
            Retry.retryLLMStreamRequest "LLM image streaming request" $
              pure (Retry.validateTextStream (Transport.askImageOpenAIStreaming cfg options messages))
      LLM.AskImageEdit options prompt imageRefs maskRef ->
        Retry.retryLLMRequest "LLM image edit request" (Transport.askImageEditOpenAI cfg options prompt imageRefs maskRef)
      LLM.AskAudio options messages ->
        Retry.retryLLMRequest "LLM audio request" (Transport.askAudioOpenAI cfg options messages)
      LLM.AskAudioStream options messages ->
        pure $
          LLM.liftLocalStream liftLocal $
            Retry.retryLLMStreamRequest "LLM audio streaming request" $
              pure (Retry.validateTextStream (Transport.askAudioOpenAIStreaming cfg options messages))
      LLM.AskTools tools messages ->
        Retry.retryLLMRequest "LLM request" (Transport.askOpenAIWithTools cfg tools messages >>= Retry.validateChatAnswer)
      LLM.AskToolsStream tools messages ->
        pure $
          LLM.liftLocalStream liftLocal $
            Retry.retryLLMStreamRequest "LLM streaming request" $
              pure (Retry.validateChatAnswerStream (Transport.askOpenAIWithToolsStreaming cfg tools messages))
