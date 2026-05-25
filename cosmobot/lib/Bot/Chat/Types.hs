{-|
Module      : Bot.Chat.Types
Description : Shared chat domain types
Stability   : experimental
-}

module Bot.Chat.Types
  ( ReplyStreamStyle (..)
  , ReplyStreamUpdate (..)
  )
where

import Bot.Core.Message
import Bot.Prelude

data ReplyStreamStyle
  = EditableReply !Int !Int
  | ChunkedReply !Int

data ReplyStreamUpdate = ReplyStreamUpdate
  { responseId :: !(Maybe MessageId)
  , sentResponseIds :: ![MessageId]
  , answer :: !Text
  }
  deriving (Show)
