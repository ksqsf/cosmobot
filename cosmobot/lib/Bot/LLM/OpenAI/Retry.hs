{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.LLM.OpenAI.Retry
Description : LLM retry and response validation policy
Stability   : experimental
-}

module Bot.LLM.OpenAI.Retry
  ( retryLLMRequest
  , retryLLMStreamRequest
  )
where

import Bot.Prelude
import qualified Bot.LLM.Types as LLM
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP
import qualified Streaming.Prelude as S

maxLLMRequestAttempts :: Int
maxLLMRequestAttempts =
  3

retryLLMRequest :: (IOE :> es, KatipE :> es) => Text -> Eff es a -> Eff es a
retryLLMRequest label action =
  go (1 :: Int)
  where
    go attempt =
      action `catchSync` \err ->
        if attempt < maxLLMRequestAttempts && retryableLLMFailure err
          then do
            logWarning [i|#{label} failed with #{LLM.llmExceptionSummary err}; retrying attempt #{attempt + 1}/#{maxLLMRequestAttempts}|]
            go (attempt + 1)
          else
            throwIO err

retryLLMStreamRequest
  :: (IOE :> es, KatipE :> es)
  => Text
  -> Eff es (Stream (Of a) (Eff es) r)
  -> Stream (Of a) (Eff es) r
retryLLMStreamRequest label makeStream =
  go (1 :: Int)
  where
    go attempt = do
      stream <- lift makeStream
      consume attempt stream

    consume attempt stream = do
      next <- lift $
        S.next stream `catchSync` \err ->
          if attempt < maxLLMRequestAttempts && retryableLLMFailure err
            then do
              logWarning [i|#{label} failed with #{LLM.llmExceptionSummary err}; retrying attempt #{attempt + 1}/#{maxLLMRequestAttempts}|]
              S.next (go (attempt + 1))
            else
              throwIO err
      case next of
        Left result ->
          pure result
        Right (chunk, rest) -> do
          S.yield chunk
          consume attempt rest

retryableLLMFailure :: SomeException -> Bool
retryableLLMFailure err =
  retryableHTTPFailure err || retryableEmptyResponse err

retryableHTTPFailure :: SomeException -> Bool
retryableHTTPFailure err =
  case fromException err of
    Just (HTTP.HttpExceptionRequest _ HTTP.ResponseTimeout) ->
      True
    Just (HTTP.HttpExceptionRequest _ HTTP.ConnectionTimeout) ->
      True
    _ ->
      False

retryableEmptyResponse :: SomeException -> Bool
retryableEmptyResponse err =
  case fromException err of
    Just (LLM.LLMException message) ->
      "empty" `Text.isInfixOf` Text.toLower message
    Nothing ->
      False
