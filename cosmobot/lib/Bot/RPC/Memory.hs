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
import qualified Bot.Effect.Storage as Storage
import qualified Bot.JSONRPC as RPC
import qualified Bot.Memory as MemoryStore
import Bot.Prelude
import Bot.RPC.Server (RpcServerCallbacks (..), noRpcServerCallbacks)
import qualified Bot.Storage.Identity as Identity
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Char (isHexDigit)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

memoryRpcCallbacks :: (Memory.Memory :> es, Storage.Storage :> es) => RpcServerCallbacks es
memoryRpcCallbacks =
  noRpcServerCallbacks
    { memoryMethod = dispatchMemoryMethod
    , supportedMethods = ["memory.list", "memory.get", "memory.history", "memory.get_revision", "memory.revert"]
    }

dispatchMemoryMethod
  :: (Memory.Memory :> es, Storage.Storage :> es)
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

listMemories :: (Memory.Memory :> es, Storage.Storage :> es) => Eff es Aeson.Value
listMemories = do
  entries <- Memory.listMemories
  senderInfos <- Identity.loadSenderInfos [(platform, senderId) | entry <- entries, MemoryStore.SenderMemory platform senderId <- [entry.scope]]
  chatInfos <- Identity.loadChatInfos [(platform, chatId) | entry <- entries, MemoryStore.ChatMemory platform chatId <- [entry.scope]]
  pure $ Aeson.object ["memories" Aeson..= map (memorySummaryValue senderInfos chatInfos) entries]

getMemory :: (Memory.Memory :> es, Storage.Storage :> es) => MemoryStore.MemoryScope -> Eff es Aeson.Value
getMemory scope = do
  content <- Memory.loadMemory scope
  names <- loadMemoryIdentity scope
  pure $ maybe Aeson.Null (memoryDetailValue scope names) content

getMemoryHistory :: Memory.Memory :> es => MemoryStore.MemoryScope -> Eff es Aeson.Value
getMemoryHistory scope = do
  history <- Memory.memoryHistory scope
  pure $ Aeson.object ["history" Aeson..= map memoryHistoryValue history]

getMemoryRevision :: (Memory.Memory :> es, Storage.Storage :> es) => (MemoryStore.MemoryScope, MemoryStore.MemoryRevision) -> Eff es Aeson.Value
getMemoryRevision (scope, revision) = do
  content <- Memory.loadMemoryRevision scope revision
  names <- loadMemoryIdentity scope
  pure $ maybe Aeson.Null (memoryDetailValue scope names) content

revertMemory :: (Memory.Memory :> es, Storage.Storage :> es) => (MemoryStore.MemoryScope, MemoryStore.MemoryRevision) -> Eff es Aeson.Value
revertMemory (scope, revision) = do
  Memory.revertMemory scope revision
  content <- Memory.loadMemory scope
  names <- loadMemoryIdentity scope
  pure $ Aeson.object ["reverted" Aeson..= True, "memory" Aeson..= maybe Aeson.Null (memoryDetailValue scope names) content]

memorySummaryValue :: Map (ChatPlatform, Text) Identity.SenderInfo -> Map (ChatPlatform, Integer) (Maybe Text) -> MemoryStore.MemoryEntry -> Aeson.Value
memorySummaryValue senderInfos chatInfos entry =
  let (platform, scopeType, scopeId) = memoryScopeParts entry.scope
      (displayName, username) = memoryIdentity senderInfos chatInfos entry.scope
  in Aeson.object
      [ "platform" Aeson..= chatPlatformKey platform
      , "scope" Aeson..= scopeType
      , "scopeId" Aeson..= scopeId
      , "displayName" Aeson..= displayName
      , "username" Aeson..= username
      , "characters" Aeson..= Text.length entry.content
      ]

memoryDetailValue :: MemoryStore.MemoryScope -> (Maybe Text, Maybe Text) -> Text -> Aeson.Value
memoryDetailValue scope (displayName, username) content =
  let (platform, scopeType, scopeId) = memoryScopeParts scope
  in Aeson.object
      [ "platform" Aeson..= chatPlatformKey platform
      , "scope" Aeson..= scopeType
      , "scopeId" Aeson..= scopeId
      , "displayName" Aeson..= displayName
      , "username" Aeson..= username
      , "characters" Aeson..= Text.length content
      , "content" Aeson..= content
      ]

loadMemoryIdentity :: Storage.Storage :> es => MemoryStore.MemoryScope -> Eff es (Maybe Text, Maybe Text)
loadMemoryIdentity scope = case scope of
  MemoryStore.SenderMemory platform senderId -> do
    infos <- Identity.loadSenderInfos [(platform, senderId)]
    pure $ maybe (Nothing, Nothing) (\info -> (info.displayName, info.username)) (Map.lookup (platform, senderId) infos)
  MemoryStore.ChatMemory platform chatId -> do
    infos <- Identity.loadChatInfos [(platform, chatId)]
    pure (Map.lookup (platform, chatId) infos >>= id, Nothing)

memoryIdentity :: Map (ChatPlatform, Text) Identity.SenderInfo -> Map (ChatPlatform, Integer) (Maybe Text) -> MemoryStore.MemoryScope -> (Maybe Text, Maybe Text)
memoryIdentity senderInfos chatInfos = \case
  MemoryStore.SenderMemory platform senderId ->
    maybe (Nothing, Nothing) (\info -> (info.displayName, info.username)) (Map.lookup (platform, senderId) senderInfos)
  MemoryStore.ChatMemory platform chatId -> (Map.lookup (platform, chatId) chatInfos >>= id, Nothing)

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
