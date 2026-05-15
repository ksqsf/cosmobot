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
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified System.Process as Process

adminHandlers :: (Chat.Chat :> es, Storage.Storage :> es, IOE :> es) => AdminConfig -> [RouteHandler es]
adminHandlers cfg =
  pingRoute : maybeToList (upgradeRoute <$> cfg.upgrade)

pingRoute :: Chat.Chat :> es => RouteHandler es
pingRoute =
  stopOn (command "!ping") handlePing

handlePing :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
handlePing message _ =
  void $ Chat.replyTo message "pong"

upgradeRoute :: (Chat.Chat :> es, Storage.Storage :> es, IOE :> es) => UpgradeConfig -> RouteHandler es
upgradeRoute cfg =
  requireAuth
    isSuperuser
    (\message -> void $ Chat.replyTo message "只有 superuser 可以执行 upgrade。")
    (stopOn (command "!upgrade") \message _ -> handleUpgrade cfg message)

handleUpgrade :: (Chat.Chat :> es, Storage.Storage :> es, IOE :> es) => UpgradeConfig -> IncomingMessage -> Eff es ()
handleUpgrade cfg message = do
  let scriptPath = cfg.script
  Lifecycle.enqueueStartupReply message "cosmobot 重启完成啦 (｡•̀ᴗ-)✧"
  result <- liftIO $ Exception.try (startUpgradeScript scriptPath)
  case result of
    Right () ->
      void $ Chat.replyTo message [i|已启动 upgrade 脚本：#{Text.pack scriptPath}|]
    Left (err :: Exception.SomeException) ->
      void $ Chat.replyTo message [i|upgrade 脚本启动失败：#{show err :: String}|]

startUpgradeScript :: FilePath -> IO ()
startUpgradeScript scriptPath = do
  (_, _, _, processHandle) <-
    Process.createProcess
      (Process.proc scriptPath [])
        { Process.std_in = Process.NoStream
        , Process.std_out = Process.NoStream
        , Process.std_err = Process.NoStream
        }
  void $ Concurrent.forkIO (void (Process.waitForProcess processHandle))
