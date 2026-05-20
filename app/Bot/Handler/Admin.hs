{-|
Module      : Bot.Handler.Admin
Description : Public administrative utility commands
Stability   : experimental
-}
{-# LANGUAGE TypeApplications #-}

module Bot.Handler.Admin
  ( adminHandlers
  )
where

import Bot.Core.Message
import Bot.Core.Route
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Storage as Storage
import Bot.Handler.Admin.Config
import Bot.Prelude
import qualified Bot.Storage.Lifecycle as Lifecycle
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent as Concurrent
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Unique as Unique
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Process as Process

adminHandlers :: (Chat.Chat :> es, Concurrent :> es, Storage.Storage :> es, Log :> es, IOE :> es) => AdminConfig -> [RouteHandler es]
adminHandlers cfg =
  [ pingRoute
  , titleRoute
  ] <> maybeToList (upgradeRoute <$> cfg.upgrade)

pingRoute :: Chat.Chat :> es => RouteHandler es
pingRoute =
  stopOn (command "!ping") handlePing

handlePing :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
handlePing message _ =
  void $ Chat.replyTo message "pong"

titleRoute :: Chat.Chat :> es => RouteHandler es
titleRoute =
  requireAuth
    isSuperuser
    (\message -> void $ Chat.replyTo message "只有 superuser 可以设置 title。")
    (stopOn (command "!title") handleTitle)

handleTitle :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
handleTitle message rawArgs =
  case parseTitleArgs rawArgs of
    Nothing ->
      void $ Chat.replyTo message "用法：!title <id> <title>"
    Just (userId, title)
      | message.kind /= ChatGroup ->
          void $ Chat.replyTo message "只能在群聊中设置 title。"
      | isNothing message.chatId ->
          void $ Chat.replyTo message "无法识别当前群聊，不能设置 title。"
      | otherwise -> do
          set <- Chat.setMemberTitle message userId title
          void $ Chat.replyTo message $
            if set
              then [i|已设置 #{userId} 的 title：#{title}|]
              else "设置 title 失败：当前平台可能不支持，或 bot 权限不足。"

parseTitleArgs :: Text -> Maybe (Integer, Text)
parseTitleArgs rawArgs = do
  let args = Text.strip rawArgs
      (rawUserId, rawTitle) = Text.break Char.isSpace args
      title = Text.strip rawTitle
  guard (not (Text.null rawUserId) && not (Text.null title))
  userId <- readMaybe (Text.unpack rawUserId)
  guard (userId > 0)
  pure (userId, title)

upgradeRoute :: (Chat.Chat :> es, Concurrent :> es, Storage.Storage :> es, Log :> es, IOE :> es) => UpgradeConfig -> RouteHandler es
upgradeRoute cfg =
  requireAuth
    isSuperuser
    (\message -> void $ Chat.replyTo message "只有 superuser 可以执行 upgrade。")
    (stopOn (command "!upgrade") \message _ -> handleUpgrade cfg message)

handleUpgrade :: (Chat.Chat :> es, Concurrent :> es, Storage.Storage :> es, Log :> es, IOE :> es) => UpgradeConfig -> IncomingMessage -> Eff es ()
handleUpgrade cfg message = do
  let scriptPath = cfg.script
  actionKey <- liftIO newLifecycleActionKey
  startupAction <- Lifecycle.enqueueStartupReply actionKey message "cosmobot 回来啦 (｡•̀ᴗ-)✧"
  result <- try @SomeException (liftIO (startUpgradeScript scriptPath))
  case result of
    Right running -> do
      void $ Chat.replyTo message [i|已启动 upgrade 脚本：#{Text.pack scriptPath}|]
      spawnTask (reportUpgradeScriptExit startupAction message running)
    Left err -> do
      Lifecycle.deleteStartupAction startupAction
      void $ Chat.replyTo message [i|upgrade 脚本启动失败：#{show err :: String}|]

data RunningUpgradeScript = RunningUpgradeScript
  { processHandle :: !Process.ProcessHandle
  , stdoutHandle :: !IO.Handle
  , stderrHandle :: !IO.Handle
  }

newLifecycleActionKey :: IO Text
newLifecycleActionKey = do
  unique <- Unique.newUnique
  pure [i|upgrade-#{Unique.hashUnique unique}|]

startUpgradeScript :: FilePath -> IO RunningUpgradeScript
startUpgradeScript scriptPath = do
  (_, Just stdoutHandle, Just stderrHandle, processHandle) <-
    Process.createProcess
      (Process.proc scriptPath [])
        { Process.std_in = Process.NoStream
        , Process.std_out = Process.CreatePipe
        , Process.std_err = Process.CreatePipe
        }
  pure RunningUpgradeScript{processHandle, stdoutHandle, stderrHandle}

reportUpgradeScriptExit
  :: (Chat.Chat :> es, Storage.Storage :> es, Log :> es, IOE :> es)
  => Lifecycle.StoredStartupAction
  -> IncomingMessage
  -> RunningUpgradeScript
  -> Eff es ()
reportUpgradeScriptExit startupAction message running = do
  (exitCode, stdoutText, stderrText) <- liftIO $ waitUpgradeScript running
  Lifecycle.deleteStartupAction startupAction
  void $ Chat.replyTo message (scriptExited exitCode stdoutText stderrText)

waitUpgradeScript :: RunningUpgradeScript -> IO (Exit.ExitCode, Text, Text)
waitUpgradeScript RunningUpgradeScript{processHandle, stdoutHandle, stderrHandle} = do
  stdoutVar <- MVar.newEmptyMVar
  stderrVar <- MVar.newEmptyMVar
  _ <- Concurrent.forkIO (readHandle stdoutHandle stdoutVar)
  _ <- Concurrent.forkIO (readHandle stderrHandle stderrVar)
  exitCode <- Process.waitForProcess processHandle
  stdoutText <- MVar.takeMVar stdoutVar
  stderrText <- MVar.takeMVar stderrVar
  pure (exitCode, stdoutText, stderrText)

readHandle :: IO.Handle -> MVar.MVar Text -> IO ()
readHandle outputHandle output =
  MVar.putMVar output . Text.pack =<< IO.hGetContents outputHandle

scriptExited :: Exit.ExitCode -> Text -> Text -> Text
scriptExited exitCode stdoutText stderrText =
  Text.strip [i|upgrade 脚本已退出，exitcode=#{exitCodeText exitCode}。#{scriptOutput stdoutText stderrText}|]

exitCodeText :: Exit.ExitCode -> Text
exitCodeText = \case
  Exit.ExitSuccess ->
    "0"
  Exit.ExitFailure code ->
    show code

scriptOutput :: Text -> Text -> Text
scriptOutput stdoutText stderrText =
  let sections =
        [ ("stdout", Text.strip stdoutText)
        , ("stderr", Text.strip stderrText)
        ]
  in Text.unlines
      [ name <> ":\n" <> Text.take 3000 body
      | (name, body) <- sections
      , not (Text.null body)
      ]
