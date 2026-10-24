module Bot.Agent.Middleware.TranscriptSnapshot (withTranscriptSnapshots) where

import Bot.Agent.Core
import Bot.Agent.Transcript (appendMessage)
import Bot.Core.Transcript (Transcript)
import qualified Bot.LLM.Types as LLM
import Bot.Prelude

-- Keep the last completed model/tool boundary available even if the run is cancelled.
withTranscriptSnapshots
  :: Monad m
  => (Transcript -> m ())
  -> Runtime host metadata context m
  -> Runtime host metadata context m
withTranscriptSnapshots save runtime@Runtime{aroundToolTurn = toolTurn} = runtime
  { aroundModelTurn = \context turnState action -> do
      lift (save turnState.transcript)
      result@(next, answer) <- runtime.aroundModelTurn context turnState action
      lift (save (appendMessage (LLM.assistantAnswer answer) next.transcript))
      pure result
  , aroundToolTurn = \context request action -> do
      result@(next, _) <- toolTurn context request action
      save next.transcript
      pure result
  , aroundAgentRun = \context action -> do
      result <- runtime.aroundAgentRun context action
      lift (save result.transcript)
      pure result
  }
