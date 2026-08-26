{-|
Module      : Bot.Chat.Types
Description : Shared chat domain types
Stability   : experimental
-}

module Bot.Chat.Types
  ( MessageOutPolicy (..)
  , MessageOutResult (..)
  , Activity (..)
  )
where

import Bot.Core.Message
import Bot.Prelude

data MessageOutPolicy
  = EditableMessage !Int !Int
  | ChunkedMessage !Int

data MessageOutResult = MessageOutResult
  { responseId :: !(Maybe MessageId)
  , sentMessageResults :: ![Either Text MessageId]
  , answer :: !Text
  }
  deriving (Show)

data Activity
  = ReasoningStarted !Text !Int
  | ReasoningFinished !Text !Int !Text
  | ToolCallStarted !Text !Int !Text !Text
  | ToolCallFinished !Text !Int !Text !Text !Text
  deriving (Eq, Show)
