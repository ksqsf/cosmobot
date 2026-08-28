{-|
Module      : Bot.Core.Session
Description : Platform-neutral durable session identity
Stability   : experimental
-}

module Bot.Core.Session
  ( SessionId (..)
  , sessionIdText
  ) where

import Bot.Prelude
import qualified Data.Aeson as Aeson

newtype SessionId = SessionId { unSessionId :: Text }
  deriving (Eq, Ord, Show)
    deriving (Aeson.ToJSON, Aeson.FromJSON) via Text

sessionIdText :: SessionId -> Text
sessionIdText (SessionId value) =
  value
