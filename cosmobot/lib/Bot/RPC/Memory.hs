{-|
Module      : Bot.RPC.Memory
Description : Persistent memory inspection and history JSON-RPC methods
Stability   : experimental
-}

module Bot.RPC.Memory
  ( memoryRpcCallbacks
  )
where

import Bot.Core.Message (ChatPlatform (..), chatPlatformKey)
import qualified Bot.Effect.Memory as Memory
import qualified Bot.JSONRPC as RPC
import qualified Bot.Memory as MemoryStore
import Bot.Prelude
import Bot.RPC.Server (RpcServerCallbacks (..), noRpcServerCallbacks)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Char (isHexDigit)
import qualified Data.Text as Text

memoryRpcCallbacks :: Memory.Memory :> es => RpcServerCallbacks es
memoryRpcCallbacks =
  noRpcServerCallbacks
    { memoryMethod = dispatchMemoryMethod
    , supportedMethods = ["memory.list", "memory.get", "memory.history", "memory.get_revision", "memory.revert"]
    }

dispatchMemoryMethod
  :: Memory.Memory :> es
  => RPC.RpcRequest
  -> Eff es (Maybe (Either RPC.RpcError Aeson.Value))
dispatchMemoryMethod request =
  case RPC.requestMethod request of
    "memory.list" -> Just <$> parseParams request parseNoParams (const listMemories)
    "memory.get" -> Just <$> parseParams request parseMemoryScope getMemory
    "memory.history" -> Just <$> parseParams request parseMemoryScope getMemoryHistory
    "memory.get_revision" -> Just <$> parseParams request parseMemoryRevision getMemoryRevision
    "memory.revert" -> Just <$> parseParams request parseMemoryRevision revertMemory
    _ -> pure Nothing

listMemories :: Memory.Memory :> es => Eff es Aeson.Value
listMemories = do
  entries <- Memory.listMemories
  pure $ Aeson.object ["memories" Aeson..= map memorySummaryValue entries]

getMemory :: Memory.Memory :> es => MemoryStore.MemoryScope -> Eff es Aeson.Value
getMemory scope = do
  content <- Memory.loadMemory scope
  pure $ maybe Aeson.Null (memoryDetailValue scope) content

getMemoryHistory :: Memory.Memory :> es => MemoryStore.MemoryScope -> Eff es Aeson.Value
getMemoryHistory scope = do
  history <- Memory.memoryHistory scope
  pure $ Aeson.object ["history" Aeson..= map memoryHistoryValue history]

getMemoryRevision :: Memory.Memory :> es => (MemoryStore.MemoryScope, MemoryStore.MemoryRevision) -> Eff es Aeson.Value
getMemoryRevision (scope, revision) = do
  content <- Memory.loadMemoryRevision scope revision
  pure $ maybe Aeson.Null (memoryDetailValue scope) content

revertMemory :: Memory.Memory :> es => (MemoryStore.MemoryScope, MemoryStore.MemoryRevision) -> Eff es Aeson.Value
revertMemory (scope, revision) = do
  Memory.revertMemory scope revision
  content <- Memory.loadMemory scope
  pure $ Aeson.object ["reverted" Aeson..= True, "memory" Aeson..= maybe Aeson.Null (memoryDetailValue scope) content]

memorySummaryValue :: MemoryStore.MemoryEntry -> Aeson.Value
memorySummaryValue entry =
  let (platform, scopeType, scopeId) = memoryScopeParts entry.scope
  in Aeson.object
      [ "platform" Aeson..= chatPlatformKey platform
      , "scope" Aeson..= scopeType
      , "scopeId" Aeson..= scopeId
      , "characters" Aeson..= Text.length entry.content
      ]

memoryDetailValue :: MemoryStore.MemoryScope -> Text -> Aeson.Value
memoryDetailValue scope content =
  let (platform, scopeType, scopeId) = memoryScopeParts scope
  in Aeson.object
      [ "platform" Aeson..= chatPlatformKey platform
      , "scope" Aeson..= scopeType
      , "scopeId" Aeson..= scopeId
      , "characters" Aeson..= Text.length content
      , "content" Aeson..= content
      ]

memoryHistoryValue :: MemoryStore.MemoryHistoryEntry -> Aeson.Value
memoryHistoryValue entry =
  Aeson.object
    [ "revision" Aeson..= entry.revision.value
    , "committedAt" Aeson..= entry.committedAt
    , "subject" Aeson..= entry.subject
    ]

memoryScopeParts :: MemoryStore.MemoryScope -> (ChatPlatform, Text, Text)
memoryScopeParts = \case
  MemoryStore.SenderMemory platform senderId -> (platform, "sender", senderId)
  MemoryStore.ChatMemory platform chatId -> (platform, "chat", toText (show chatId :: String))

parseParams
  :: RPC.RpcRequest
  -> (Aeson.Value -> AesonTypes.Parser a)
  -> (a -> Eff es Aeson.Value)
  -> Eff es (Either RPC.RpcError Aeson.Value)
parseParams request parser action =
  case AesonTypes.parseEither parser (RPC.requestParams request) of
    Left err -> pure (Left (RPC.rpcError "invalid_params" (toText err)))
    Right value -> Right <$> action value

parseNoParams :: Aeson.Value -> AesonTypes.Parser ()
parseNoParams Aeson.Null = pure ()
parseNoParams value = Aeson.withObject "empty params" (\o -> unless (null o) (fail "params must be empty")) value

parseMemoryScope :: Aeson.Value -> AesonTypes.Parser MemoryStore.MemoryScope
parseMemoryScope = Aeson.withObject "memory params" \o -> do
  platform <- o Aeson..: "platform" >>= parsePlatform
  scopeType <- o Aeson..: "scope"
  scopeId <- o Aeson..: "scopeId"
  unless (validScopeId scopeId) $
    fail "scopeId must be a non-empty path component"
  case scopeType :: Text of
    "sender" -> pure (MemoryStore.SenderMemory platform scopeId)
    "chat" -> maybe (fail "scopeId must be an integer for chat memory") (pure . MemoryStore.ChatMemory platform) (readMaybe (Text.unpack scopeId))
    _ -> fail "scope must be sender or chat"

validScopeId :: Text -> Bool
validScopeId value =
  not (Text.null value)
    && value `notElem` [".", ".."]
    && Text.all (`notElem` ['/', '\\']) value

parseMemoryRevision :: Aeson.Value -> AesonTypes.Parser (MemoryStore.MemoryScope, MemoryStore.MemoryRevision)
parseMemoryRevision value = do
  scope <- parseMemoryScope value
  revision <- Aeson.withObject "memory.revert params" (Aeson..: "revision") value
  unless (Text.length revision == 40 && Text.all isHexDigit revision) $
    fail "revision must be a full 40-character hexadecimal commit id"
  pure (scope, MemoryStore.MemoryRevision (Text.toLower revision))

parsePlatform :: Text -> AesonTypes.Parser ChatPlatform
parsePlatform = \case
  "qq" -> pure PlatformQQ
  "telegram" -> pure PlatformTelegram
  "matrix" -> pure PlatformMatrix
  "discord" -> pure PlatformDiscord
  "rpc" -> pure PlatformRPC
  "acp" -> pure PlatformACP
  _ -> fail "platform must be one of: qq, telegram, matrix, discord, rpc, acp"
