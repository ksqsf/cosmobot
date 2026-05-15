{-|
Module      : Bot.Effect.LLM.Retry
Description : LLM retry and response validation policy
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Effect.LLM.Retry
  ( retryLLMRequest
  , validateChatAnswer
  , validateChatAnswerStream
  , validateTextStream
  )
where

import Bot.Prelude
import qualified Bot.Effect.LLM.Transport as Transport
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP

maxLLMRequestAttempts :: Int
maxLLMRequestAttempts =
  3

retryLLMRequest :: (IOE :> es, Log :> es) => Text -> Eff es a -> Eff es a
retryLLMRequest label action =
  go (1 :: Int)
  where
    go attempt =
      action `catch` \(err :: SomeException) ->
        if attempt < maxLLMRequestAttempts && retryableLLMFailure err
          then do
            logAttention_ [i|#{label} failed with #{Transport.llmExceptionSummary err}; retrying attempt #{attempt + 1}/#{maxLLMRequestAttempts}|]
            go (attempt + 1)
          else
            throwIO err

retryableLLMFailure :: SomeException -> Bool
retryableLLMFailure err =
  retryableHTTPFailure err || retryableEmptyResponse err

retryableHTTPFailure :: SomeException -> Bool
retryableHTTPFailure err =
  case Exception.fromException err of
    Just (HTTP.HttpExceptionRequest _ HTTP.ResponseTimeout) ->
      True
    Just (HTTP.HttpExceptionRequest _ HTTP.ConnectionTimeout) ->
      True
    _ ->
      False

retryableEmptyResponse :: SomeException -> Bool
retryableEmptyResponse err =
  case Exception.fromException err of
    Just (Transport.LLMException message) ->
      "empty" `Text.isInfixOf` Text.toLower message
    Nothing ->
      False

validateTextStream :: IOE :> es => Stream (Of Text) (Eff es) Text -> Stream (Of Text) (Eff es) Text
validateTextStream stream = do
  answer <- stream
  if Text.null (Text.strip answer)
    then lift $ throwIO (Transport.LLMException "OpenAI response was empty: no text output.")
    else pure answer

validateChatAnswerStream :: IOE :> es => Stream (Of Text) (Eff es) Transport.ChatAnswer -> Stream (Of Text) (Eff es) Transport.ChatAnswer
validateChatAnswerStream stream = do
  answer <- stream
  lift (validateChatAnswer answer)

validateChatAnswer :: IOE :> es => Transport.ChatAnswer -> Eff es Transport.ChatAnswer
validateChatAnswer answer =
  case answer of
    Transport.ChatFinalAnswer{content}
      | Text.null (Text.strip content) ->
          throwIO (Transport.LLMException "OpenAI response was empty: no text, image, or tool call output.")
    _ ->
      pure answer
