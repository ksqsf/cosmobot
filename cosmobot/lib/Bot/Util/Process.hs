{-|
Module      : Bot.Util.Process
Description : Process output helpers
Stability   : experimental
-}

module Bot.Util.Process
  ( processOutputText
  , killProcessGroup
  , killProcessGroupByPid
  , withProcessGroup
  , readProcessGroupWithExitCode
  )
where

import Bot.Prelude
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.STM as STM
import qualified Effectful.Process as Process
import qualified Effectful.Process.Typed as TypedProcess
import System.Exit (ExitCode)
import System.Posix.Signals (signalProcess, signalProcessGroup, sigKILL)
import System.Process (Pid)

processOutputText :: Concurrent :> es => STM.STM LazyByteString.ByteString -> Eff es Text
processOutputText =
  fmap (TextEncoding.decodeUtf8Lenient . LazyByteString.toStrict) . STM.atomically

killProcessGroup :: (IOE :> es, Process.Process :> es) => Process.ProcessHandle -> Eff es ()
killProcessGroup processHandle = do
  mPid <- Process.getPid processHandle
  traverse_ killProcessGroupByPid mPid

killProcessGroupByPid :: IOE :> es => Pid -> Eff es ()
killProcessGroupByPid pid =
  ignoreIO $
    (liftIO $ signalProcessGroup sigKILL (fromIntegral pid))
      `catchSync` \_ ->
        liftIO $ signalProcess sigKILL pid

ignoreIO :: IOE :> es => Eff es () -> Eff es ()
ignoreIO action =
  action `catchSync` \_ -> pure ()

readProcessGroupWithExitCode
  :: (IOE :> es, Concurrent :> es, TypedProcess.TypedProcess :> es)
  => FilePath
  -> [String]
  -> Eff es (ExitCode, Text, Text)
readProcessGroupWithExitCode executable args = do
  let processConfig =
        TypedProcess.setCreateGroup True .
        TypedProcess.setStdin TypedProcess.closed .
        TypedProcess.setStdout TypedProcess.byteStringOutput .
        TypedProcess.setStderr TypedProcess.byteStringOutput $
        TypedProcess.proc executable args
  withProcessGroup processConfig \process -> do
    code <- TypedProcess.waitExitCode process
    stdoutText <- processOutputText (TypedProcess.getStdout process)
    stderrText <- processOutputText (TypedProcess.getStderr process)
    pure (code, stdoutText, stderrText)

withProcessGroup
  :: (IOE :> es, TypedProcess.TypedProcess :> es)
  => TypedProcess.ProcessConfig stdin stdout stderr
  -> (TypedProcess.Process stdin stdout stderr -> Eff es a)
  -> Eff es a
withProcessGroup processConfig =
  bracket (TypedProcess.startProcess processConfig) stop
  where
    stop process = do
      killProcessGroup (TypedProcess.unsafeProcessHandle process)
      TypedProcess.stopProcess process
