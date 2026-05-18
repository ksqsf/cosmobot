{-|
Module      : Bot.Agent.Failure
Description : Structured agent failure categories
Stability   : experimental
-}

module Bot.Agent.Failure
  ( AgentFailureCategory (..)
  , AgentFailure (..)
  , AgentException (..)
  , agentException
  , agentFailureFromException
  , agentFailureStatus
  , transientFailure
  , permanentArgumentFailure
  , permissionDeniedFailure
  , budgetExhaustedFailure
  , externalServiceFailure
  , uncertainSideEffectFailure
  )
where

import Bot.Prelude
import qualified Bot.Effect.LLM as LLM
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP

data AgentFailureCategory
  = TransientFailure
  | PermanentArgumentError
  | PermissionDenied
  | BudgetExhausted
  | ExternalServiceUnavailable
  | UncertainSideEffectState
  deriving (Eq, Show)

data AgentFailure = AgentFailure
  { category :: !AgentFailureCategory
  , userMessage :: !Text
  , detail :: !Text
  }
  deriving (Eq, Show)

newtype AgentException = AgentException
  { failure :: AgentFailure
  }
  deriving (Show)

instance Exception AgentException

agentException :: AgentFailureCategory -> Text -> Text -> AgentException
agentException category userMessage detail =
  AgentException AgentFailure{category, userMessage, detail}

transientFailure :: Text -> Text -> AgentException
transientFailure =
  agentException TransientFailure

permanentArgumentFailure :: Text -> Text -> AgentException
permanentArgumentFailure =
  agentException PermanentArgumentError

permissionDeniedFailure :: Text -> Text -> AgentException
permissionDeniedFailure =
  agentException PermissionDenied

budgetExhaustedFailure :: Text -> Text -> AgentException
budgetExhaustedFailure =
  agentException BudgetExhausted

externalServiceFailure :: Text -> Text -> AgentException
externalServiceFailure =
  agentException ExternalServiceUnavailable

uncertainSideEffectFailure :: Text -> Text -> AgentException
uncertainSideEffectFailure =
  agentException UncertainSideEffectState

agentFailureFromException :: SomeException -> AgentFailure
agentFailureFromException err =
  case Exception.fromException err of
    Just (AgentException failure) ->
      failure
    Nothing ->
      classifyException err

classifyException :: SomeException -> AgentFailure
classifyException err =
  case Exception.fromException err of
    Just httpErr ->
      httpFailure httpErr
    Nothing ->
      case Exception.fromException err of
        Just (LLM.LLMException message) ->
          llmFailure message
        Nothing ->
          AgentFailure
            { category = ExternalServiceUnavailable
            , userMessage = LLM.llmExceptionSummary err
            , detail = Text.pack (show err)
            }

httpFailure :: HTTP.HttpException -> AgentFailure
httpFailure httpErr =
  case httpErr of
    HTTP.HttpExceptionRequest _ HTTP.ResponseTimeout ->
      transient (LLM.llmExceptionSummary (toException httpErr))
    HTTP.HttpExceptionRequest _ HTTP.ConnectionTimeout ->
      transient (LLM.llmExceptionSummary (toException httpErr))
    _ ->
      AgentFailure
        { category = ExternalServiceUnavailable
        , userMessage = LLM.llmExceptionSummary (toException httpErr)
        , detail = Text.pack (show httpErr)
        }
  where
    transient message =
      AgentFailure
        { category = TransientFailure
        , userMessage = message
        , detail = Text.pack (show httpErr)
        }

llmFailure :: Text -> AgentFailure
llmFailure message =
  AgentFailure
    { category =
        if "empty" `Text.isInfixOf` Text.toLower message
          then TransientFailure
          else ExternalServiceUnavailable
    , userMessage = "LLM error: " <> message
    , detail = message
    }

agentFailureStatus :: AgentFailure -> Text
agentFailureStatus failure =
  case failure.category of
    TransientFailure ->
      "transient"
    PermanentArgumentError ->
      "permanent_argument_error"
    PermissionDenied ->
      "permission_denied"
    BudgetExhausted ->
      "budget_exhausted"
    ExternalServiceUnavailable ->
      "external_service_unavailable"
    UncertainSideEffectState ->
      "uncertain_side_effect_state"
