{-|
Module      : Bot.Agent.Middleware.Observation.Types
Description : Types carried by agent observation middleware
Stability   : experimental
-}
module Bot.Agent.Middleware.Observation.Types
  ( ObservationContext (..)
  , ToolResultObservation (..)
  , emptyObservationContext
  )
where

import Bot.Agent.Types (ToolResult)
import Bot.Prelude

newtype ObservationContext = ObservationContext
  { auditToolUseId :: Maybe Integer
  }
  deriving (Eq, Show)

newtype ToolResultObservation es = ToolResultObservation
  { observeToolResult :: ToolResult -> Eff es Text
  }

emptyObservationContext :: ObservationContext
emptyObservationContext =
  ObservationContext{auditToolUseId = Nothing}
