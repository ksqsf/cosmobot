{-# LANGUAGE TypeApplications #-}
{-|
Module      : Bot.Handler.Admin
Description : Public administrative utility commands
Stability   : experimental
-}

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
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Unique as Unique
import qualified Effectful.Concurrent.MVar as MVar
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem.IO.ByteString as ByteStringFileSystem
import Effectful.Process (Process, ProcessHandle, StdStream (..), createProcess, proc, std_err, std_in, std_out, waitForProcess)
import qualified System.Exit as Exit
import System.IO.Error (userError)

adminHandlers :: (Chat.Chat :> es, Concurrent :> es, FileSystem :> es, Process :> es, Storage.Storage :> es, KatipE :> es, IOE :> es) => AdminConfig -> [RouteHandler es]
adminHandlers cfg =
  [ pingRoute
  , titleRoute
  , echoRoute
  ] <> maybeToList (upgradeRoute <$> cfg.upgrade)

pingRoute :: Chat.Chat :> es => RouteHandler es
pingRoute =
  stopOn (command "!ping") handlePing

handlePing :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
handlePing message _ =
  void $ Chat.replyTo message "pong"

echoRoute :: Chat.Chat :> es => RouteHandler es
echoRoute =
  stopOn (command "!echo") handleEcho

handleEcho :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
handleEcho message rawArgs = do
  void $ Chat.replyTo message rawArgs

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

parseTitleArgs :: Text -> Maybe (Text, Text)
parseTitleArgs rawArgs = do
  let args = Text.strip rawArgs
      (rawUserId, rawTitle) = Text.break Char.isSpace args
      userId = Text.strip rawUserId
      title = Text.strip rawTitle
  guard (not (Text.null userId) && userId /= "0" && not (Text.null title))
  pure (userId, title)

upgradeRoute :: (Chat.Chat :> es, Concurrent :> es, FileSystem :> es, Process :> es, Storage.Storage :> es, KatipE :> es, IOE :> es) => UpgradeConfig -> RouteHandler es
upgradeRoute cfg =
  requireAuth
    isSuperuser
    (\message -> void $ Chat.replyTo message "只有 superuser 可以执行 upgrade。")
    (stopOn (command "!upgrade") \message _ -> handleUpgrade cfg message)

handleUpgrade :: (Chat.Chat :> es, Concurrent :> es, FileSystem :> es, Process :> es, Storage.Storage :> es, KatipE :> es, IOE :> es) => UpgradeConfig -> IncomingMessage -> Eff es ()
handleUpgrade cfg message = do
  let scriptPath = cfg.script
  actionKey <- liftIO newLifecycleActionKey
  startupAction <- Lifecycle.enqueueStartupReply actionKey message "cosmobot 回来啦 (｡•̀ᴗ-)✧"
  result <- trySync (startUpgradeScript scriptPath)
  case result of
    Right running -> do
      void $ Chat.replyTo message [i|已启动 upgrade 脚本：#{Text.pack scriptPath}|]
      spawnTask (reportUpgradeScriptExit startupAction message running)
    Left err -> do
      Lifecycle.deleteStartupAction startupAction
      void $ Chat.replyTo message [i|upgrade 脚本启动失败：#{show err :: String}|]

data RunningUpgradeScript = RunningUpgradeScript
  { processHandle :: !ProcessHandle
  , stdoutHandle :: !Handle
  , stderrHandle :: !Handle
  }

newLifecycleActionKey :: IO Text
newLifecycleActionKey = do
  unique <- Unique.newUnique
  pure [i|upgrade-#{Unique.hashUnique unique}|]

startUpgradeScript :: Process :> es => FilePath -> Eff es RunningUpgradeScript
startUpgradeScript scriptPath = do
  (_, maybeStdoutHandle, maybeStderrHandle, processHandle) <-
    createProcess
      (proc scriptPath [])
        { std_in = NoStream
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  case (maybeStdoutHandle, maybeStderrHandle) of
    (Just stdoutHandle, Just stderrHandle) ->
      pure RunningUpgradeScript{processHandle, stdoutHandle, stderrHandle}
    _ ->
      throwIO (userError "upgrade script did not provide stdout/stderr handles.")

reportUpgradeScriptExit
  :: (Chat.Chat :> es, Concurrent :> es, FileSystem :> es, Process :> es, Storage.Storage :> es, KatipE :> es, IOE :> es)
  => Lifecycle.StoredStartupAction
  -> IncomingMessage
  -> RunningUpgradeScript
  -> Eff es ()
reportUpgradeScriptExit startupAction message running = do
  (exitCode, stdoutText, stderrText) <- waitUpgradeScript running
  Lifecycle.deleteStartupAction startupAction
  void $ Chat.replyTo message (scriptExited exitCode stdoutText stderrText)

waitUpgradeScript :: (Concurrent :> es, FileSystem :> es, Process :> es, IOE :> es) => RunningUpgradeScript -> Eff es (Exit.ExitCode, Text, Text)
waitUpgradeScript RunningUpgradeScript{processHandle, stdoutHandle, stderrHandle} = do
  stdoutVar <- MVar.newEmptyMVar
  stderrVar <- MVar.newEmptyMVar
  void $ forkIO (readHandle stdoutHandle stdoutVar)
  void $ forkIO (readHandle stderrHandle stderrVar)
  exitCode <- waitForProcess processHandle
  stdoutText <- MVar.takeMVar stdoutVar
  stderrText <- MVar.takeMVar stderrVar
  pure (exitCode, stdoutText, stderrText)

readHandle :: (FileSystem :> es, Concurrent :> es) => Handle -> MVar.MVar Text -> Eff es ()
readHandle outputHandle output = do
  bytes <- ByteStringFileSystem.hGetContents outputHandle
  MVar.putMVar output (TextEncoding.decodeUtf8 bytes)

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
