module Bot.Agent.Tools.Python
  ( PythonRequest (..)
  , runPythonDescription
  , runPythonRequest
  , runPythonTool
  , runPythonToolName
  )
where

import Bot.Agent.Tool
import Bot.Agent.Tools.Common (requiredText, specialTag)
import Bot.Agent.Types
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

newtype PythonRequest = PythonRequest
  { code :: Text
  }
  deriving stock (Eq, Show)

runPythonToolName :: Text
runPythonToolName =
  "run_python"

runPythonTool :: Tool (Eff es)
runPythonTool =
  noisy
    . tagged [specialTag]
    . withDescription runPythonDescription
    $ tool runPythonToolName
        (requiredText "code" "Complete Python source to execute with compile(..., '<run_python>', 'exec').")
        \_ -> pure (toolFailure (permanentArgumentFailure unavailable unavailable))
  where
    unavailable = "run_python requires the structural Python interpreter."

runPythonRequest :: LLM.ToolCall -> Maybe (Either Text PythonRequest)
runPythonRequest call
  | call.name /= runPythonToolName = Nothing
  | otherwise = Just do
      value <- first Text.pack (Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 call.arguments))
      first Text.pack (AesonTypes.parseEither parser value)
  where
    parser =
      Aeson.withObject "run_python arguments" \object ->
        PythonRequest <$> object Aeson..: Key.fromText "code"

runPythonDescription :: Text
runPythonDescription = Text.unlines
  [ "Execute a fresh, sandboxed Python 3 program that can compose the other tools visible in this model turn. Use this when a task needs multiple tool calls with sequencing, branching, loops, aggregation, or recovery; call a single tool directly when no composition is needed. run_python must be the only tool call in its model-issued batch."
  , ""
  , "The environment contains the Python standard library and an in-memory `cosmobot` module: `run_tool(name, args_json)` returns `{'content': text}` or raises `RunToolException`; `run_tools([{'name': name, 'args': args}, ...])` runs a non-empty batch concurrently, preserves input order, and raises one `RunToolException` after all outcomes if any failed. `RunToolException` exposes `name`, `index`, `failure`, and `results`. Tool names and argument schemas are exactly the tools already visible in this model request; do not guess hidden tools."
  , ""
  , "`cosmobot.complete(content='')` and `raise cosmobot.BackToAgent(content)` immediately unwind Python after running `finally` blocks and return that exact text as this call's ordinary tool-result message for the next model phase. `cosmobot.fail(message)` similarly unwinds and fails this outer call. These control sentinels derive from `BaseException`, so `except Exception` does not catch them; deliberate `except BaseException` can."
  , ""
  , "Normal fall-through succeeds with captured stdout as the tool result; no output returns `Python completed successfully.` There is no last-expression evaluation. stderr is failure detail only. Unhandled exceptions, `sys.exit` (including zero), malformed protocol, dependency loss, and timeout fail the outer call. Nested tool side effects are real and are not rolled back if later Python code fails."
  , ""
  , "Each invocation starts a new interpreter and a fresh 64 MiB tmpfs at `/work`; cwd, HOME, and temporary files are inside it, and all globals/files disappear when the call ends. Caller files, the repository, host home/tmp/config/secrets, and writable host paths are not mounted. External non-loopback networking is allowed, host loopback is blocked, IPv6 is disabled, inbound forwarding is absent, and standard `file://` URL helpers are rejected. Child processes/threads are denied."
  , ""
  , "Fixed limits: 30 s wall time, 20 s CPU, 512 MiB address space, 64 open files, 1 MiB stdout, 64 KiB stderr, 8 KiB completion/control text, 4 MiB per JSON-RPC frame, 64 total nested tool calls, and at most 16 calls per `run_tools` batch. `run_python`, `tool_enable`, `capture_continuation`, and `resume_continuation` cannot be called from Python. Enable every needed tool tag in an earlier model turn before using run_python."
  ]
