{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Bot.Agent.Middleware.Python
  ( PythonInterpreter
  , interpretPython
  , withPythonInterpreter
  )
where

import Bot.Agent.Control (finishToolTurn)
import Bot.Agent.Core
import Bot.Agent.Program.Python
import Bot.Agent.Tool (toolName)
import Bot.Agent.Tools.Python
import Bot.Agent.Types
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList

type PythonInterpreter es =
  ToolRequest -> LLM.ToolCall -> PythonRequest -> Program (Eff es) PythonExit

withPythonInterpreter
  :: PythonInterpreter es
  -> Runtime context (Eff es)
  -> Runtime context (Eff es)
withPythonInterpreter interpreter runtime =
  runtime
    { aroundProgram = \finalRuntime@Runtime{aroundToolTurn = toolTurn} ->
        interpretPython
          interpreter
          (map toolName finalRuntime.exposedTools)
          toolTurn
          . runtime.aroundProgram finalRuntime
    }

interpretPython
  :: Monad m
  => (ToolRequest -> LLM.ToolCall -> PythonRequest -> Program m PythonExit)
  -> [Text]
  -> (forall a. HList.HList '[] -> ToolRequest -> m (TurnState, a) -> m (TurnState, a))
  -> Program m result
  -> Program m result
interpretPython interpreter exposedToolNames toolTurn =
  go
  where
    go (Program action) =
      Program do
        action >>= \case
          Finished result ->
            pure (Finished result)
          Continues next ->
            pure (Continues (go next))
          Visible (RunTools request) continue ->
            case exposedPythonCalls exposedToolNames request.toolCalls of
              [] ->
                pure (Visible (RunTools request) (go . continue))
              [call]
                | [_] <- toList request.toolCalls ->
                    handleCall request continue call
              _ ->
                rejectBatch request continue
          Visible event continue ->
            pure (Visible event (go . continue))

    handleCall request continue call =
      (do
        result <- case runPythonRequest call of
          Just (Right pythonRequest) ->
            pythonExitResult <$> interpreter request call pythonRequest
          Just (Left err) ->
            pure (argumentFailure [i|Invalid run_python arguments: #{err}|])
          Nothing ->
            pure (argumentFailure "Unknown Python control operation.")
        resume request continue ((call, result) :| [])
      ).observe

    rejectBatch request continue =
      (resume request continue $ request.toolCalls <&> \call ->
        (call, argumentFailure "run_python must be called alone; no tool call in this turn was executed.")
      ).observe

    resume request continue executions = do
      (nextState, _) <- lift $ finishToolTurn toolTurn request (pure (snd <$> executions))
      go (continue nextState)

exposedPythonCalls :: [Text] -> NonEmpty LLM.ToolCall -> [LLM.ToolCall]
exposedPythonCalls exposedToolNames =
  filter isExposedPython . toList
  where
    isExposedPython call =
      call.name == runPythonToolName
        && call.name `elem` exposedToolNames

argumentFailure :: Text -> ToolResult
argumentFailure message =
  toolFailure (permanentArgumentFailure message message)
