module Bot.Agent.Program.Python
  ( PythonExit (..)
  , pythonExitResult
  )
where

import Bot.Agent.Types
import Bot.Prelude

data PythonExit
  = PythonCompleted !Text
  | PythonFailed !Failure
  deriving stock (Eq, Show)

pythonExitResult :: PythonExit -> ToolResult
pythonExitResult = \case
  PythonCompleted content -> toolText content
  PythonFailed failure -> toolFailure failure
