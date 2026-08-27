{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Cosmocode
  ( optionsInfo
  , runCosmocode
  ) where

import Cosmocode.RPC (Rpc, getSession, openSession, receiveServerEvent, sendChat)
import Cosmocode.RPC.WebSocket (runRpcWebSocket)
import Cosmocode.Terminal (Terminal, publishServerEvent, runSessionUi)
import Cosmocode.Terminal.Inline (runTerminalIO)
import Cosmocode.Types
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import qualified Effectful.Concurrent.Async as Async
import Effectful.Error.Static
import Effectful.Exception (displayException, trySync)
import Effectful.Prim (runPrim)
import Options.Applicative hiding (command, value)
import qualified Options.Applicative as Options
import System.Exit (die)

optionsInfo :: ParserInfo Options
optionsInfo = info (optionsParser <**> helper) $
  fullDesc <> progDesc "Connect to a Cosmobot console session" <> header "cosmocode"

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption (long "host" <> metavar "HOST" <> Options.value "127.0.0.1" <> showDefault)
  <*> option auto (long "port" <> metavar "PORT" <> Options.value 38765 <> showDefault)
  <*> (Text.pack <$> strOption (long "token" <> metavar "TOKEN" <> help "RPC bearer token"))
  <*> (resumeParser <|> pure NewSession)

resumeParser :: Parser Command
resumeParser = hsubparser $ Options.command "resume" $
  info (ResumeSession . Text.pack <$> argument str (metavar "SESSION_ID")) $
    progDesc "Resume an existing session"

runCosmocode :: IO ()
runCosmocode = do
  options <- execParser optionsInfo
  let sendMessage sessionId body =
        fmap (\case
          Left err -> Left (Text.pack (displayException err))
          Right result -> result)
          (trySync (sendChat sessionId body))
  result <- runEff . runPrim . runConcurrent $ trySync $
    runRpcWebSocket options.host options.port options.token $
      runTerminalIO sendMessage (runErrorNoCallStack (runApplication options))
  case result of
    Left err -> die ("cosmocode: " <> displayException err)
    Right (Left err) -> die ("cosmocode: " <> Text.unpack err)
    Right (Right ()) -> pure ()

runApplication
  :: (Rpc :> es, Terminal :> es, Concurrent :> es, Error Text :> es)
  => Options
  -> Eff es ()
runApplication options = do
  (sessionId, history) <- startSession options.command
  let server = Text.pack options.host <> ":" <> Text.pack (show options.port)
      model = initialModel server sessionId history
  Async.withAsync receiveEvents \_ -> runSessionUi model

startSession :: (Rpc :> es, Error Text :> es) => Command -> Eff es (Text, [SessionMessage])
startSession = \case
  NewSession -> (,[]) <$> (openSession >>= either throwError_ pure)
  ResumeSession sessionId -> do
    history <- getSession sessionId >>= either throwError_ pure
    maybe (throwError_ ("Session not found: " <> sessionId)) (pure . (sessionId,)) history

receiveEvents :: (Rpc :> es, Terminal :> es) => Eff es ()
receiveEvents = do
  outcome <- trySync receiveLoop
  publishServerEvent case outcome of
    Left err -> ConnectionClosed (Text.pack (displayException err))
    Right reason -> ConnectionClosed reason
  where
    receiveLoop =
      receiveServerEvent >>= \case
        Left err -> pure err
        Right event -> mapM_ publishServerEvent event >> receiveLoop
