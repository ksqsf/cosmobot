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
import Bot.Core.Message
import Bot.Core.Thread
import qualified Bot.JSONRPC as RPC
import Bot.RPC.Server (RpcServerCallbacks (..), noRpcServerCallbacks)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

auditRpcCallbacks :: AgentAudit.AgentAudit :> es => RpcServerCallbacks es
auditRpcCallbacks =
  noRpcServerCallbacks
    { auditMethod = dispatchAuditMethod
    , supportedMethods = ["audit.recent", "audit.search", "audit.get", "audit.run", "audit.thread", "audit.thread_messages"]
    }

dispatchAuditMethod
  :: AgentAudit.AgentAudit :> es
  => RPC.RpcRequest
  -> Eff es (Maybe (Either RPC.RpcError Aeson.Value))
dispatchAuditMethod request =
  Just <$> case RPC.requestMethod request of
    "audit.recent" ->
      parseParams request parseLimit \limit ->
        Aeson.toJSON <$> AgentAudit.queryRecentAuditRecords limit
    "audit.search" ->
      parseParams request parseSearch \(searchText, limit) ->
        Aeson.toJSON <$> AgentAudit.searchAuditRecords searchText limit
    "audit.get" ->
      parseParams request parseAuditId \auditId ->
        Aeson.toJSON <$> AgentAudit.queryAuditRecord auditId
    "audit.run" ->
      parseParams request parseRunId \runId ->
        Aeson.toJSON <$> AgentAudit.queryRunAudit runId
    "audit.thread" ->
      parseParams request parseMessageKey \messageKey ->
        Aeson.toJSON <$> AgentAudit.queryThreadAudit messageKey
    "audit.thread_messages" ->
      parseParams request parseMessageKeys \messageKeys ->
        Aeson.toJSON <$> AgentAudit.queryThreadMessagesAudit messageKeys
    _ ->
      let method = RPC.requestMethod request
      in pure (Left (RPC.rpcError "method_not_found" [i|Unknown RPC method: #{method}|]))

parseParams
  :: RPC.RpcRequest
  -> (Aeson.Value -> AesonTypes.Parser a)
  -> (a -> Eff es Aeson.Value)
  -> Eff es (Either RPC.RpcError Aeson.Value)
parseParams request parser action =
  case AesonTypes.parseEither parser (RPC.requestParams request) of
    Left err ->
      pure (Left (RPC.rpcError "invalid_params" (toText err)))
    Right value ->
      Right <$> action value

parseLimit :: Aeson.Value -> AesonTypes.Parser Int
parseLimit Aeson.Null =
  pure 20
parseLimit value =
  Aeson.withObject "audit.recent params" parse value
  where
    parse o = do
      limit <- fromMaybe 20 <$> o Aeson..:? "limit"
      if limit >= 1 && limit <= 500
        then pure limit
        else fail "limit must be between 1 and 500"

parseSearch :: Aeson.Value -> AesonTypes.Parser (Text, Int)
parseSearch =
  Aeson.withObject "audit.search params" \o -> do
    searchText <- Text.strip <$> o Aeson..: "query"
    limit <- fromMaybe 500 <$> o Aeson..:? "limit"
    when (Text.null searchText || Text.length searchText > 256) $
      fail "query must contain between 1 and 256 characters"
    when (limit < 1 || limit > 500) $
      fail "limit must be between 1 and 500"
    pure (searchText, limit)

parseAuditId :: Aeson.Value -> AesonTypes.Parser Integer
parseAuditId =
  Aeson.withObject "audit.get params" \o ->
    o Aeson..: "audit_id" <|> o Aeson..: "id"

parseRunId :: Aeson.Value -> AesonTypes.Parser Text
parseRunId =
  Aeson.withObject "audit.run params" \o -> do
    runId <- Text.strip <$> o Aeson..: "runId"
    if Text.null runId || Text.length runId > 256
      then fail "runId must contain between 1 and 256 characters"
      else pure runId

parseMessageKey :: Aeson.Value -> AesonTypes.Parser ThreadMessageKey
parseMessageKey =
  Aeson.withObject "audit.thread params" \o ->
    ThreadMessageKey
      <$> (o Aeson..: "platform" >>= parsePlatform)
      <*> (o Aeson..:? "chat_id" >>= traverse parseChatId)
      <*> (textMessageId <$> o Aeson..: "message_id")

parseMessageKeys :: Aeson.Value -> AesonTypes.Parser [ThreadMessageKey]
parseMessageKeys =
  Aeson.withObject "audit.thread_messages params" \o -> do
    platform <- o Aeson..: "platform" >>= parsePlatform
    chatId <- o Aeson..:? "chat_id" >>= traverse parseChatId
    messageIds <- o Aeson..: "message_ids"
    pure [ThreadMessageKey{platform, chatId, messageId = textMessageId messageId} | messageId <- messageIds]

parseChatId :: Aeson.Value -> AesonTypes.Parser Integer
parseChatId value =
  Aeson.withText "chat id" (maybe (fail "chat_id must be an integer") pure . readMaybe . toString) value
    <|> Aeson.parseJSON value

parsePlatform :: Text -> AesonTypes.Parser ChatPlatform
parsePlatform = \case
  "qq" -> pure PlatformQQ
  "telegram" -> pure PlatformTelegram
  "matrix" -> pure PlatformMatrix
  "discord" -> pure PlatformDiscord
  "rpc" -> pure PlatformRPC
  "acp" -> pure PlatformACP
  _ -> fail "platform must be one of: qq, telegram, matrix, discord, rpc, acp"
