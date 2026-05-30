{-|
Module      : Bot.RPC.Client
Description : Command-line JSON-RPC websocket client
Stability   : experimental
-}

module Bot.RPC.Client
  ( RpcClientCommand (..)
  , runRpcClientCommand
  )
where

import Bot.Prelude
import qualified Bot.RPC.Config as RPCConfig
import qualified Bot.RPC.Protocol as Protocol
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import qualified Network.WebSockets as WS
import qualified Toml
import Toml.Schema

data RpcClientCommand
  = RpcAuditRecent !Int
  | RpcAuditShow !Integer
  | RpcAuditThread !Text
  | RpcCall !Text !Aeson.Value
  deriving (Eq, Show)

newtype RpcClientFileConfig = RpcClientFileConfig
  { rpc :: RPCConfig.FileConfig
  }
  deriving (Show)

instance FromValue RpcClientFileConfig where
  fromValue = parseTableFromValue $
    RpcClientFileConfig
      <$> fmap (fromMaybe RPCConfig.defaultFileConfig) (optKey "rpc")

runRpcClientCommand :: FilePath -> RpcClientCommand -> IO ()
runRpcClientCommand configPath command = do
  cfg <- loadRpcClientConfig configPath
  when (Text.null cfg.token) $
    fail "rpc.token must be configured for RPC client authentication"
  let request = requestForCommand command
  WS.runClientWith cfg.host cfg.port "/rpc" WS.defaultConnectionOptions (authHeaders cfg) \conn -> do
    WS.sendTextData conn (Aeson.encode (requestValue request))
    responseBytes <- WS.receiveData conn
    responseValue <- case Aeson.eitherDecodeStrict' responseBytes of
      Left err ->
        fail [i|RPC response was not valid JSON: #{err}|]
      Right value ->
        pure (value :: Aeson.Value)
    LazyByteString.putStr (AesonPretty.encodePretty responseValue <> "\n")

loadRpcClientConfig :: FilePath -> IO RPCConfig.Config
loadRpcClientConfig path = do
  content <- TextIO.readFile path
  case Toml.decode content of
    Toml.Failure errors ->
      fail [i|Failed to parse #{path}: #{unlines (map toText errors)}|]
    Toml.Success warnings config_ -> do
      traverse_ (putStrLn . ("TOML warning: " <>)) warnings
      pure (RPCConfig.toRuntimeConfig (config_ :: RpcClientFileConfig).rpc)

requestForCommand :: RpcClientCommand -> Protocol.RpcRequest
requestForCommand = \case
  RpcAuditRecent limit ->
    rpcRequest "audit.recent" (Aeson.object ["limit" Aeson..= limit])
  RpcAuditShow auditId ->
    rpcRequest "audit.get" (Aeson.object ["audit_id" Aeson..= auditId])
  RpcAuditThread messageId ->
    rpcRequest "audit.thread" (Aeson.object ["message_id" Aeson..= messageId])
  RpcCall method params ->
    rpcRequest method params

rpcRequest :: Text -> Aeson.Value -> Protocol.RpcRequest
rpcRequest method params =
  Protocol.rpcRequest method params "cli-1"

requestValue :: Protocol.RpcRequest -> Aeson.Value
requestValue =
  Aeson.toJSON

authHeaders :: RPCConfig.Config -> WS.Headers
authHeaders RPCConfig.Config{token} =
  [("Authorization", "Bearer " <> TextEncoding.encodeUtf8 token)]
