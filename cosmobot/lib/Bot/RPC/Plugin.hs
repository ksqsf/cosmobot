{-|
Module      : Bot.RPC.Plugin
Description : Plugin lifecycle JSON-RPC contract
Stability   : experimental
-}

module Bot.RPC.Plugin
  ( PluginRpc (..)
  , pluginMethods
  , dispatchPluginRequest
  )
where

import qualified Bot.JSONRPC as RPC
import Bot.Plugin.Types (PluginId (..), PluginStatus (..))
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes

data PluginRpc m = PluginRpc
  { list :: m [PluginStatus]
  , load :: PluginId -> m (Either Text PluginStatus)
  , reload :: PluginId -> m (Either Text PluginStatus)
  , unload :: PluginId -> m (Either Text ())
  }

data LifecycleOperation = LoadPlugin | ReloadPlugin | UnloadPlugin

pluginMethods :: [Text]
pluginMethods = ["plugin.list", "plugin.load", "plugin.reload", "plugin.unload"]

dispatchPluginRequest
  :: Monad m
  => PluginRpc m
  -> RPC.RpcRequest
  -> m (Maybe (Either RPC.RpcError Aeson.Value))
dispatchPluginRequest callbacks request =
  case RPC.requestMethod request of
    "plugin.list" -> Just <$> case AesonTypes.parseEither parseNoParams (RPC.requestParams request) of
      Left err -> pure (invalidParams err)
      Right () -> do
        plugins <- callbacks.list
        pure (Right (Aeson.object ["plugins" Aeson..= map statusValue plugins]))
    "plugin.load" -> Just <$> lifecycle LoadPlugin callbacks.load
    "plugin.reload" -> Just <$> lifecycle ReloadPlugin callbacks.reload
    "plugin.unload" -> Just <$> case parsePluginId request of
      Left err -> pure (invalidParams err)
      Right pluginId -> callbacks.unload pluginId <&> \case
        Left failure -> Left (operationError UnloadPlugin failure)
        Right () -> Right (Aeson.object ["pluginId" Aeson..= pluginId, "unloaded" Aeson..= True])
    _ -> pure Nothing
  where
    lifecycle operation run = case parsePluginId request of
      Left err -> pure (invalidParams err)
      Right pluginId -> run pluginId <&> \case
        Left failure -> Left (operationError operation failure)
        Right status -> Right (statusValue status)

parseNoParams :: Aeson.Value -> AesonTypes.Parser ()
parseNoParams Aeson.Null = pure ()
parseNoParams value = Aeson.withObject "empty params" (\o -> unless (null o) (fail "params must be empty")) value

parsePluginId :: RPC.RpcRequest -> Either String PluginId
parsePluginId request =
  AesonTypes.parseEither
    (Aeson.withObject "plugin lifecycle params" (Aeson..: "pluginId"))
    (RPC.requestParams request)

statusValue :: PluginStatus -> Aeson.Value
statusValue status = Aeson.object
  [ "pluginId" Aeson..= status.pluginId
  , "version" Aeson..= status.pluginVersion
  , "generation" Aeson..= status.generation
  , "required" Aeson..= status.required
  , "sandboxed" Aeson..= status.sandboxed
  , "routeCount" Aeson..= status.routeCount
  , "toolCount" Aeson..= status.toolCount
  ]

invalidParams :: String -> Either RPC.RpcError value
invalidParams = Left . RPC.rpcError "invalid_params" . toText

operationError :: LifecycleOperation -> Text -> RPC.RpcError
operationError operation failure =
  RPC.rpcError "plugin_operation_failed" $ case failure of
    "plugin is already loaded" -> failure
    "plugin is not loaded" -> failure
    "required plugins cannot be unloaded" -> failure
    "plugin manager is shutting down" -> failure
    _ -> case operation of
      LoadPlugin -> "Plugin could not be loaded."
      ReloadPlugin -> "Plugin could not be reloaded."
      UnloadPlugin -> "Plugin could not be unloaded."
