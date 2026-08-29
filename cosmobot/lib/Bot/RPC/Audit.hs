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
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import qualified Bot.JSONRPC as RPC
import Bot.RPC.Server (RpcServerCallbacks (..), noRpcServerCallbacks)
import qualified Bot.Storage.Thread as Thread
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

auditRpcCallbacks :: (AgentAudit.AgentAudit :> es, Storage.Storage :> es) => RpcServerCallbacks es
auditRpcCallbacks =
  noRpcServerCallbacks
    { auditMethod = dispatchAuditMethod
    , supportedMethods = ["audit.recent", "audit.count", "audit.search", "audit.get", "audit.run", "audit.thread"]
    }

dispatchAuditMethod
  :: (AgentAudit.AgentAudit :> es, Storage.Storage :> es)
  => RPC.RpcRequest
  -> Eff es (Maybe (Either RPC.RpcError Aeson.Value))
dispatchAuditMethod request =
  Just <$> case RPC.requestMethod request of
    "audit.recent" ->
      parseParams request parseLimit \limit ->
        Aeson.toJSON <$> AgentAudit.queryRecentAuditRecords limit
    "audit.count" ->
      parseParams request parseNoParams \() ->
        Aeson.toJSON <$> AgentAudit.countAuditRecords
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
      parseParams request parseThreadId \threadId -> do
        rows <- Thread.loadThreadRowsByThreadId threadId
        if null rows
          then pure Aeson.Null
          else Aeson.toJSON <$> AgentAudit.queryThreadMessagesAudit (map (.messageKey) rows)
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

parseNoParams :: Aeson.Value -> AesonTypes.Parser ()
parseNoParams Aeson.Null = pure ()
parseNoParams value = Aeson.withObject "audit.count params" (const (pure ())) value

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

parseThreadId :: Aeson.Value -> AesonTypes.Parser Integer
parseThreadId =
  Aeson.withObject "audit.thread params" \o -> do
    threadId <- o Aeson..: "threadId"
    if threadId > 0 then pure threadId else fail "threadId must be positive"
