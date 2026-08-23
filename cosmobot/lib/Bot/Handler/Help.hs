{-|
Module      : Bot.Handler.Help
Description : Route-algebra generated command help
Stability   : experimental
-}

module Bot.Handler.Help
  ( helpHandlers
  , helpHandlersWith
  , renderHelp
  )
where

import Bot.Core.Route
import Bot.Core.Message (IncomingMessage)
import qualified Bot.Effect.Chat as Chat
import Bot.Prelude
import qualified Data.Text as Text

helpHandlers :: Chat.Chat :> es => [RouteHandler es] -> [RouteHandler es]
helpHandlers = helpHandlersWith (const (pure []))

helpHandlersWith
  :: Chat.Chat :> es
  => (IncomingMessage -> Eff es [RouteHelp])
  -> [RouteHandler es]
  -> [RouteHandler es]
helpHandlersWith dynamicHelp routes =
  [ helpRoute ]
  where
    helpRoute = withHelp helpMetadata $
      stopOn (command "!help") \message _ ->
        do
          dynamic <- dynamicHelp message
          void $ Chat.replyTo message (renderHelp (collectRouteHelp message (helpRoute : routes) <> dynamic))

helpMetadata :: RouteHelp
helpMetadata =
  RouteHelp "!help" "List available commands."

renderHelp :: [RouteHelp] -> Text
renderHelp entries =
  Text.unlines $ "Available commands:" : map renderEntry entries
  where
    renderEntry entry =
      "- `" <> entry.label <> "` — " <> entry.description
