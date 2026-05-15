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
import Bot.Prelude

adminHandlers :: Chat.Chat :> es => [RouteHandler es]
adminHandlers =
  [ pingRoute
  ]

pingRoute :: Chat.Chat :> es => RouteHandler es
pingRoute =
  stopOn (command "!ping") handlePing

handlePing :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
handlePing message _ =
  void $ Chat.replyTo message "pong"
