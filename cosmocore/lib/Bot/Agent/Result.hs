module Bot.Agent.Result
  ( module Bot.Agent.Failure
  , ToolResult (..), toolText, toolTextWithImages, toolFailure
  , toolResultContent, toolResultImageUrls, toolResultFailure
  ) where

import Bot.Agent.Failure
import Bot.Prelude

-- | One tool call outcome. Failures are still returned as tool results because
-- OpenAI-compatible history requires every requested tool call to have a
-- corresponding tool-result message.
data ToolResult
  = ToolSucceeded
      { content :: !Text
      , imageUrls :: ![Text]
      }
  | ToolFailed
      { failure :: !Failure
      }

toolText :: Text -> ToolResult
toolText content =
  ToolSucceeded content []

toolTextWithImages :: Text -> [Text] -> ToolResult
toolTextWithImages content imageUrls =
  ToolSucceeded content imageUrls

toolFailure :: Failure -> ToolResult
toolFailure failure =
  ToolFailed failure

toolResultContent :: ToolResult -> Text
toolResultContent = \case
  ToolSucceeded{content} ->
    content
  ToolFailed{failure} ->
    failure.userMessage

toolResultImageUrls :: ToolResult -> [Text]
toolResultImageUrls = \case
  ToolSucceeded{imageUrls} ->
    imageUrls
  ToolFailed{} ->
    []

toolResultFailure :: ToolResult -> Maybe Failure
toolResultFailure = \case
  ToolSucceeded{} ->
    Nothing
  ToolFailed{failure} ->
    Just failure
