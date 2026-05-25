{-|
Module      : Bot.RPC.Audit
Description : Agent audit JSON-RPC method handlers
Stability   : experimental
-}

module Bot.RPC.Audit
  ( auditRpcCallbacks
  )
where

import qualified Bot.Effect.AgentAudit as AgentAudit
import Bot.Prelude
import Bot.Core.Message (textMessageId)
import qualified Bot.RPC.Protocol as Protocol
import Bot.RPC.Server (RpcServerCallbacks (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes

auditRpcCallbacks :: AgentAudit.AgentAudit :> es => RpcServerCallbacks es
auditRpcCallbacks =
  RpcServerCallbacks
    { auditMethod = dispatchAuditMethod
    }

dispatchAuditMethod
  :: AgentAudit.AgentAudit :> es
  => Protocol.RpcRequest
  -> Eff es (Maybe (Either Protocol.RpcError Aeson.Value))
dispatchAuditMethod request =
  Just <$> case Protocol.requestMethod request of
    "audit.recent" ->
      parseParams request parseLimit \limit ->
        Aeson.toJSON <$> AgentAudit.queryRecentAuditRecords limit
    "audit.get" ->
      parseParams request parseAuditId \auditId ->
        Aeson.toJSON <$> AgentAudit.queryAuditRecord auditId
    "audit.conversation" ->
      parseParams request parseMessageId \messageId ->
        Aeson.toJSON <$> AgentAudit.queryConversationAudit (textMessageId messageId)
    "audit.conversation_messages" ->
      parseParams request parseMessageIds \messageIds ->
        Aeson.toJSON <$> AgentAudit.queryConversationMessagesAudit (map textMessageId messageIds)
    "audit.subscribe" ->
      pure (Right (Aeson.object ["subscribed" Aeson..= True]))
    _ ->
      let method = Protocol.requestMethod request
      in pure (Left (Protocol.rpcError "method_not_found" [i|Unknown RPC method: #{method}|]))

parseParams
  :: Protocol.RpcRequest
  -> (Aeson.Value -> AesonTypes.Parser a)
  -> (a -> Eff es Aeson.Value)
  -> Eff es (Either Protocol.RpcError Aeson.Value)
parseParams request parser action =
  case AesonTypes.parseEither parser (Protocol.requestParams request) of
    Left err ->
      pure (Left (Protocol.rpcError "invalid_params" (toText err)))
    Right value ->
      Right <$> action value

parseLimit :: Aeson.Value -> AesonTypes.Parser Int
parseLimit =
  Aeson.withObject "audit.recent params" \o ->
    fromMaybe 20 <$> o Aeson..:? "limit"

parseAuditId :: Aeson.Value -> AesonTypes.Parser Integer
parseAuditId =
  Aeson.withObject "audit.get params" \o ->
    o Aeson..: "audit_id" <|> o Aeson..: "id"

parseMessageId :: Aeson.Value -> AesonTypes.Parser Text
parseMessageId =
  Aeson.withObject "audit.conversation params" \o ->
    o Aeson..: "message_id"

parseMessageIds :: Aeson.Value -> AesonTypes.Parser [Text]
parseMessageIds =
  Aeson.withObject "audit.conversation_messages params" \o ->
    o Aeson..: "message_ids"
