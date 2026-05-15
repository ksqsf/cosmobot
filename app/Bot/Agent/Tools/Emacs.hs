{-|
Module      : Bot.Agent.Tools.Emacs
Description : Agent Emacs evaluation tool
Stability   : experimental
-}

module Bot.Agent.Tools.Emacs
  ( emacsEvalTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Prelude
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.IO.Error (userError)
import System.Process (StdStream (..), proc, readCreateProcessWithExitCode, waitForProcess, createProcess, std_err, std_out)
import System.Timeout (timeout)

emacsSocketName :: String
emacsSocketName =
  "cosmobot"

emacsEvalTool :: IOE :> es => Tool es
emacsEvalTool = Tool
  { name = "emacs_eval"
  , description = "Evaluate Emacs Lisp in a cosmobot-owned, persistent Emacs 30 daemon for coding, scripting, reading/writing files, starting subprocesses, managing terminals, recording temporary memory in buffers, etc. Prefer it to other tools if it uses less tokens, and always use it if there are multiple operations that can be batched."
  , parameters = objectSchema
      [ fieldText "expression" "Emacs Lisp expression to evaluate."
      , fieldInteger "timeout_seconds" "Maximum seconds to wait before returning a timeout. Defaults to 10."
      ]
      ["expression"]
  , noisy = False
  , allowed = superuserOnly
  , start = \_ -> pure \args ->
      withParsedToolArgs emacsEvalArgs args \(expression, timeoutSeconds) -> do
        result <- liftIO $ runEmacsEval timeoutSeconds expression
        pure (toolText result)
  }

runEmacsEval :: Int -> Text -> IO Text
runEmacsEval timeoutSeconds expression = do
  firstAttempt <- tryEval timeoutSeconds expression
  case firstAttempt of
    Right result ->
      pure result
    Left _ -> do
      startEmacsDaemon timeoutSeconds
      either Exception.throwIO pure =<< tryEval timeoutSeconds expression

tryEval :: Int -> Text -> IO (Either SomeException Text)
tryEval timeoutSeconds expression =
  Exception.try do
    let effectiveTimeout = max 1 timeoutSeconds
        process = proc "emacsclient" ["--socket-name", emacsSocketName, "--eval", Text.unpack expression]
    outcome <- timeout (effectiveTimeout * 1_000_000) (readCreateProcessWithExitCode process "")
    case outcome of
      Nothing ->
        Exception.throwIO (userError [i|emacs_eval timed out after #{effectiveTimeout} seconds.|])
      Just (exitCode, stdoutText, stderrText) ->
        pure (formatProcessResult "emacsclient" exitCode (Text.pack stdoutText) (Text.pack stderrText))

startEmacsDaemon :: Int -> IO ()
startEmacsDaemon timeoutSeconds = do
  let effectiveTimeout = max 1 timeoutSeconds
  (_, outHandle, errHandle, processHandle) <- createProcess
    (proc "emacs" ["-Q", "--daemon=" <> emacsSocketName])
      { std_out = CreatePipe
      , std_err = CreatePipe
      }
  outcome <- timeout (effectiveTimeout * 1_000_000) (waitForProcess processHandle)
  traverse_ hClose outHandle
  traverse_ hClose errHandle
  case outcome of
    Nothing ->
      Exception.throwIO (userError [i|emacs daemon startup timed out after #{effectiveTimeout} seconds.|])
    Just ExitSuccess ->
      pure ()
    Just exitCode ->
      Exception.throwIO (userError [i|emacs daemon startup failed: #{show exitCode :: String}|])

formatProcessResult :: Text -> ExitCode -> Text -> Text -> Text
formatProcessResult commandName exitCode stdoutText stderrText =
  Text.strip $ Text.unlines $ filter (not . Text.null)
    [ if Text.null stdoutText then "" else "stdout:\n" <> stdoutText
    , if Text.null stderrText then "" else "stderr:\n" <> stderrText
    , [i|exit code: #{show exitCode :: String}|]
    , [i|command: #{commandName}|]
    ]

emacsEvalArgs :: Aeson.Value -> AesonTypes.Parser (Text, Int)
emacsEvalArgs =
  Aeson.withObject "emacs eval arguments" $ \o -> do
    expression <- Text.strip <$> o Aeson..: Key.fromText "expression"
    timeoutSeconds <- fromMaybe 10 <$> o Aeson..:? Key.fromText "timeout_seconds"
    when (Text.null expression) do
      fail "expression must not be empty."
    when (timeoutSeconds <= 0) do
      fail "timeout_seconds must be positive."
    pure (expression, min 60 timeoutSeconds)
