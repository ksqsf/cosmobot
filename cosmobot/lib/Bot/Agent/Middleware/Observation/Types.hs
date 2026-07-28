{-|
Module      : Bot.Agent.Middleware.Observation.Types
Description : Types carried by agent observation middleware
Stability   : experimental
-}
module Bot.Agent.Middleware.Observation.Types
  ( ObservationContext (..)
  , AgentEventObservation (..)
  , ToolResultObservation (..)
  , emptyObservationContext
  )
where

import Bot.Agent.Types (AgentEvent, ToolResult)
import Bot.Prelude

newtype AgentEventObservation es = AgentEventObservation
  { observeAgentEvent :: AgentEvent -> Eff es ObservationContext
  }

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
