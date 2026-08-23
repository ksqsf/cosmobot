{-|
Module      : Bot.Handler.Plugin
Description : Superuser plugin lifecycle commands and dynamic route gateway
Stability   : experimental
-}

module Bot.Handler.Plugin
  ( pluginHandlers
  , pluginRouteGateway
  )
where

import Bot.Core.Message
import Bot.Core.Route
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Plugin as Plugin
import qualified Bot.Plugin.Types as PluginTypes
import Bot.Prelude
import qualified Data.Text as Text

pluginHandlers :: (Chat.Chat :> es, Plugin.Plugin :> es) => [RouteHandler es]
pluginHandlers =
  [ lifecycleRoute "!plugin/load" "Load an installed plugin." Plugin.load renderLoaded
  , lifecycleRoute "!plugin/reload" "Reload a plugin." Plugin.reload renderLoaded
  , unloadRoute
  , listRoute
  ]

pluginRouteGateway :: Plugin.Plugin :> es => RouteHandler es
pluginRouteGateway =
  Route
    { help = Nothing
    , helpVisible = const True
    , decide = \message ->
        Plugin.dispatchRoute message <&> \case
          Nothing -> Skip
          Just PluginTypes.ContinueRouting -> ContinueWith (pure ())
          Just PluginTypes.StopRouting -> StopWith (pure ())
    }

lifecycleRoute
  :: (Chat.Chat :> es, Plugin.Plugin :> es)
  => Text
  -> Text
  -> (Text -> Eff es (Either Text PluginTypes.PluginStatus))
  -> (PluginTypes.PluginStatus -> Text)
  -> RouteHandler es
lifecycleRoute commandName description operation renderSuccess =
  withHelp (RouteHelp (commandName <> " <id>") (description <> " (superuser only).")) $
    requireAuth isSuperuser denied $
      stopOn (command commandName) \message rawId ->
        withPluginId message rawId operation renderSuccess

unloadRoute :: (Chat.Chat :> es, Plugin.Plugin :> es) => RouteHandler es
unloadRoute =
  withHelp (RouteHelp "!plugin/unload <id>" "Unload an optional plugin (superuser only).") $
    requireAuth isSuperuser denied $
      stopOn (command "!plugin/unload") \message rawId ->
        withPluginId message rawId Plugin.unload (const "Plugin unloaded.")

listRoute :: (Chat.Chat :> es, Plugin.Plugin :> es) => RouteHandler es
listRoute =
  withHelp (RouteHelp "!plugin/list" "List active plugins (superuser only).") $
    requireAuth isSuperuser denied $
      stopOn (command "!plugin/list") \message _ -> do
        active <- Plugin.statuses
        void $ Chat.replyTo message $
          if null active
            then "No active plugins."
            else Text.unlines (map renderStatus active)

withPluginId
  :: Chat.Chat :> es
  => IncomingMessage
  -> Text
  -> (Text -> Eff es (Either Text a))
  -> (a -> Text)
  -> Eff es ()
withPluginId message rawId operation renderSuccess =
  case validPluginId rawId of
    Nothing ->
      void $ Chat.replyTo message "Plugin id must contain only ASCII letters, digits, '_' or '-'."
    Just pluginId -> do
      result <- operation pluginId
      void $ Chat.replyTo message (either ("Plugin operation failed: " <>) renderSuccess result)

validPluginId :: Text -> Maybe Text
validPluginId raw = do
  let pluginId = Text.strip raw
  PluginTypes.PluginId value <- either (const Nothing) Just (PluginTypes.validatePluginId pluginId)
  pure value

denied :: Chat.Chat :> es => IncomingMessage -> Eff es ()
denied message =
  void $ Chat.replyTo message "Only superusers can manage plugins."

renderLoaded :: PluginTypes.PluginStatus -> Text
renderLoaded status =
  "Plugin loaded: " <> status.pluginId.unPluginId <> " generation " <> show status.generation <> "."

renderStatus :: PluginTypes.PluginStatus -> Text
renderStatus status =
  Text.unwords
    [ "-"
    , status.pluginId.unPluginId
    , "v" <> status.pluginVersion
    , "generation=" <> show status.generation
    , "routes=" <> show status.routeCount
    , "tools=" <> show status.toolCount
    , if status.required then "required" else "optional"
    , if status.sandboxed then "sandboxed" else "unsandboxed"
    ]
