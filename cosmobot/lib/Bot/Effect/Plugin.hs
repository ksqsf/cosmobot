{-|
Module      : Bot.Effect.Plugin
Description : External plugin capability facade
Stability   : experimental
-}

module Bot.Effect.Plugin
  ( Plugin (..)
  , PluginTool (..)
  , ToolInvocationResult (..)
  , ToolFailureKind (..)
  , statuses
  , load
  , unload
  , reload
  , dispatchRoute
  , helpEntries
  , toolSnapshot
  , invokeTool
  )
where

import Bot.Core.Message (IncomingMessage)
import Bot.Core.Route (RouteHelp)
import Bot.Plugin.Types (PluginStatus, RouteDisposition, ToolInvocationResult (..), ToolFailureKind (..))
import Bot.Prelude
import qualified Data.Aeson as Aeson

-- | Immutable handle captured when an agent run starts.
data PluginTool = PluginTool
  { pluginId :: !Text
  , generation :: !Int
  , name :: !Text
  , modelName :: !Text
  , description :: !Text
  , schema :: !Aeson.Value
  }
  deriving (Eq, Show)

data Plugin :: Effect where
  Statuses :: Plugin m [PluginStatus]
  Load :: Text -> Plugin m (Either Text PluginStatus)
  Unload :: Text -> Plugin m (Either Text ())
  Reload :: Text -> Plugin m (Either Text PluginStatus)
  DispatchRoute :: IncomingMessage -> Plugin m (Maybe RouteDisposition)
  HelpEntries :: IncomingMessage -> Plugin m [RouteHelp]
  ToolSnapshot :: Plugin m [PluginTool]
  InvokeTool :: PluginTool -> IncomingMessage -> Aeson.Value -> Plugin m ToolInvocationResult

type instance DispatchOf Plugin = Dynamic

statuses :: Plugin :> es => Eff es [PluginStatus]
statuses = send Statuses

load :: Plugin :> es => Text -> Eff es (Either Text PluginStatus)
load = send . Load

unload :: Plugin :> es => Text -> Eff es (Either Text ())
unload = send . Unload

reload :: Plugin :> es => Text -> Eff es (Either Text PluginStatus)
reload = send . Reload

dispatchRoute :: Plugin :> es => IncomingMessage -> Eff es (Maybe RouteDisposition)
dispatchRoute = send . DispatchRoute

helpEntries :: Plugin :> es => IncomingMessage -> Eff es [RouteHelp]
helpEntries = send . HelpEntries

toolSnapshot :: Plugin :> es => Eff es [PluginTool]
toolSnapshot = send ToolSnapshot

invokeTool
  :: Plugin :> es
  => PluginTool
  -> IncomingMessage
  -> Aeson.Value
  -> Eff es ToolInvocationResult
invokeTool tool message arguments =
  send (InvokeTool tool message arguments)
