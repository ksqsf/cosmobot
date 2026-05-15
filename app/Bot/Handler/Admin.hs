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
import Bot.Handler.Admin.Config
import Bot.Prelude
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified System.Exit as Exit
import qualified System.Process as Process

adminHandlers :: (Chat.Chat :> es, IOE :> es) => AdminConfig -> [RouteHandler es]
adminHandlers cfg =
  pingRoute : maybeToList (upgradeRoute <$> cfg.upgrade)

pingRoute :: Chat.Chat :> es => RouteHandler es
pingRoute =
  stopOn (command "!ping") handlePing

handlePing :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
handlePing message _ =
  void $ Chat.replyTo message "pong"

upgradeRoute :: (Chat.Chat :> es, IOE :> es) => UpgradeConfig -> RouteHandler es
upgradeRoute cfg =
  requireAuth
    isSuperuser
    (\message -> void $ Chat.replyTo message "只有 superuser 可以执行 upgrade。")
    (stopOn (command "!upgrade") \message _ -> handleUpgrade cfg message)

handleUpgrade :: (Chat.Chat :> es, IOE :> es) => UpgradeConfig -> IncomingMessage -> Eff es ()
handleUpgrade cfg message = do
  let scriptPath = cfg.script
  void $ Chat.replyTo message [i|开始执行 upgrade 脚本：#{Text.pack scriptPath}|]
  result <- liftIO $ Exception.try (Process.readProcessWithExitCode scriptPath [] "")
  case result of
    Right (Exit.ExitSuccess, stdOut, stdErr) ->
      void $ Chat.replyTo message (upgradeSucceeded stdOut stdErr)
    Right (Exit.ExitFailure code, stdOut, stdErr) ->
      void $ Chat.replyTo message (upgradeFailed code stdOut stdErr)
    Left (err :: Exception.SomeException) ->
      void $ Chat.replyTo message [i|upgrade 脚本启动失败：#{show err :: String}|]

upgradeSucceeded :: String -> String -> Text
upgradeSucceeded stdOut stdErr =
  Text.strip [i|upgrade 脚本执行成功。#{scriptOutput stdOut stdErr}|]

upgradeFailed :: Int -> String -> String -> Text
upgradeFailed code stdOut stdErr =
  Text.strip [i|upgrade 脚本执行失败，退出码 #{code}。#{scriptOutput stdOut stdErr}|]

scriptOutput :: String -> String -> Text
scriptOutput stdOut stdErr =
  let out = Text.strip (Text.pack stdOut)
      err = Text.strip (Text.pack stdErr)
      sections :: [(Text, Text)]
      sections =
        [ ("stdout", out)
        , ("stderr", err)
        ]
  in Text.unlines
      [ name <> ":\n" <> Text.take 3000 body
      | (name, body) <- sections
      , not (Text.null body)
      ]
