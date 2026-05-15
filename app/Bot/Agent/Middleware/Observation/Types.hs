{-|
Module      : Bot.Agent.Middleware.Observation.Types
Description : Types carried by agent observation middleware
Stability   : experimental
-}
module Bot.Agent.Middleware.Observation.Types
  ( ObservationContext (..)
  , emptyObservationContext
  )
where

import Bot.Prelude

newtype ObservationContext = ObservationContext
  { auditToolUseId :: Maybe Integer
  }
  deriving (Eq, Show)

emptyObservationContext :: ObservationContext
emptyObservationContext =
  ObservationContext{auditToolUseId = Nothing}
