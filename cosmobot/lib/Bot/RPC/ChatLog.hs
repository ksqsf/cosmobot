{-|
Module      : Bot.RPC.ChatLog
Description : Chat-log inspection JSON-RPC methods
Stability   : experimental
-}

module Bot.RPC.ChatLog
  ( chatLogRpcCallbacks
  )
where

import Bot.Core.Message
import Bot.Core.Thread (ThreadMessageKey (..))
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Storage as Storage
import qualified Bot.JSONRPC as RPC
import Bot.Prelude
import Bot.RPC.Server (RpcServerCallbacks (..), noRpcServerCallbacks)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Bot.Storage.Identity as Identity
import qualified Bot.Storage.ChatLog as ChatLogStorage

chatLogRpcCallbacks
  :: (ChatLog.ChatLog :> es, Storage.Storage :> es)
  => (ThreadMessageKey -> Eff es (Maybe ThreadMessageKey))
  -> (ThreadMessageKey -> Eff es (Maybe Text))
  -> ([ThreadMessageKey] -> Eff es [(ThreadMessageKey, Integer)])
  -> (Text -> Eff es Text)
  -> RpcServerCallbacks es
chatLogRpcCallbacks parentMessage finalAssistantText threadIds publicMediaRef =
  noRpcServerCallbacks
    { chatLogMethod = dispatchChatLogMethod parentMessage finalAssistantText threadIds publicMediaRef
    , supportedMethods = ["chat_log.list", "chat_log.stats", "chat_log.window"]
    }

dispatchChatLogMethod
  :: (ChatLog.ChatLog :> es, Storage.Storage :> es)
  => (ThreadMessageKey -> Eff es (Maybe ThreadMessageKey))
  -> (ThreadMessageKey -> Eff es (Maybe Text))
  -> ([ThreadMessageKey] -> Eff es [(ThreadMessageKey, Integer)])
  -> (Text -> Eff es Text)
  -> RPC.RpcRequest
  -> Eff es (Maybe (Either RPC.RpcError Aeson.Value))
dispatchChatLogMethod parentMessage finalAssistantText threadIds publicMediaRef request =
  Just <$> case RPC.requestMethod request of
    "chat_log.list" ->
      parseParams request parseNoParams \() ->
        ChatLog.listChats >>= chatLogSummariesValue
    "chat_log.stats" ->
      parseParams request parseNoParams \() -> do
        (messages, platforms, chats) <- ChatLogStorage.loadChatLogStats
        pure $ Aeson.object
          [ "messages" Aeson..= messages
          , "platforms" Aeson..= platforms
          , "chats" Aeson..= chats
          ]
    "chat_log.window" ->
      parseParams request parseWindowParams \(scope, anchor, limit) ->
        queryWindowWithThreadFallback parentMessage finalAssistantText scope anchor limit >>= chatLogWindowValue threadIds publicMediaRef
    _ ->
      pure (Left (RPC.rpcError "method_not_found" [i|Unknown RPC method: #{RPC.requestMethod request}|]))

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
parseNoParams value = Aeson.withObject "chat_log.list params" (\o -> unless (null o) (fail "params must be empty")) value

parseWindowParams :: Aeson.Value -> AesonTypes.Parser (ChatLog.ChatLogScope, ChatLog.ChatLogWindowAnchor, Int)
parseWindowParams =
  Aeson.withObject "chat_log.window params" \o -> do
    platform <- o Aeson..: "platform" >>= parsePlatform
    kind <- o Aeson..: "kind" >>= parseKind
    chatId <- o Aeson..:? "chatId" >>= traverse parseChatId
    messageId <- o Aeson..:? "messageId" >>= traverse parseMessageId
    beforeRow <- o Aeson..:? "beforeRow"
    afterRow <- o Aeson..:? "afterRow"
    limit <- fromMaybe 50 <$> o Aeson..:? "limit"
    when (limit < 1 || limit > 200) $ fail "limit must be between 1 and 200"
    anchor <- case (messageId, beforeRow, afterRow) of
      (Just value, Nothing, Nothing) -> pure (ChatLog.AroundChatLogMessage value)
      (Nothing, Just value, Nothing) | value > 0 -> pure (ChatLog.BeforeChatLogRow value)
      (Nothing, Nothing, Just value) | value > 0 -> pure (ChatLog.AfterChatLogRow value)
      (Nothing, Nothing, Nothing) -> pure ChatLog.LatestChatLogWindow
      _ -> fail "provide only one of messageId, beforeRow, or afterRow"
    pure (ChatLog.ChatLogScope{platform, kind, chatId}, anchor, limit)

parseMessageId :: Text -> AesonTypes.Parser MessageId
parseMessageId value =
  let stripped = Text.strip value
  in if Text.null stripped || Text.length stripped > 512
      then fail "messageId must contain between 1 and 512 characters"
      else pure (textMessageId stripped)

parseChatId :: Aeson.Value -> AesonTypes.Parser ChatId
parseChatId = Aeson.parseJSON

parsePlatform :: Text -> AesonTypes.Parser ChatPlatform
parsePlatform = \case
  "PlatformQQ" -> pure PlatformQQ
  "PlatformTelegram" -> pure PlatformTelegram
  "PlatformMatrix" -> pure PlatformMatrix
  "PlatformDiscord" -> pure PlatformDiscord
  "PlatformRPC" -> pure PlatformRPC
  "PlatformACP" -> pure PlatformACP
  _ -> fail "unsupported chat platform"

parseKind :: Text -> AesonTypes.Parser ChatKind
parseKind = \case
  "ChatPrivate" -> pure ChatPrivate
  "ChatGroup" -> pure ChatGroup
  "ChatChannel" -> pure ChatChannel
  value
    | Just unknown <- Text.stripPrefix "ChatUnknown:" value -> pure (ChatUnknown unknown)
    | otherwise -> fail "unsupported chat kind"

chatLogSummariesValue :: Storage.Storage :> es => [ChatLog.ChatLogSummary] -> Eff es Aeson.Value
chatLogSummariesValue summaries = do
  names <- Identity.loadScopedChatInfos [(scope.platform, scope.kind, chatId) | summary <- summaries, let scope = summary.scope, chatId <- maybeToList scope.chatId]
  pure $ Aeson.object ["chats" Aeson..= map (chatLogSummaryValue names) summaries]

chatLogSummaryValue :: Map (ChatPlatform, ChatKind, ChatId) (Maybe Text) -> ChatLog.ChatLogSummary -> Aeson.Value
chatLogSummaryValue names summary =
  Aeson.object
    [ "scope" Aeson..= chatLogScopeValue summary.scope
    , "chatDisplayName" Aeson..= lookupChatDisplayName names summary.scope
    , "messageCount" Aeson..= summary.messageCount
    , "latestAt" Aeson..= summary.latestAt
    ]

chatLogWindowValue
  :: Storage.Storage :> es
  => ([ThreadMessageKey] -> Eff es [(ThreadMessageKey, Integer)])
  -> (Text -> Eff es Text)
  -> ChatLog.ChatLogWindow
  -> Eff es Aeson.Value
chatLogWindowValue resolveThreadIds publicMediaRef window = do
  associations <- Map.fromList <$> resolveThreadIds (ordNub (concatMap (itemMessageKeys window.scope) window.entries))
  entries <- traverse (resolveItemMedia publicMediaRef) window.entries
  senders <- Identity.loadSenderInfos [(entry.platform, senderId) | item <- entries, let entry = item.entry, senderId <- maybeToList entry.senderId]
  chats <- Identity.loadScopedChatInfos [(window.scope.platform, window.scope.kind, chatId) | chatId <- maybeToList window.scope.chatId]
  pure $
    Aeson.object
      [ "scope" Aeson..= chatLogScopeValue window.scope
      , "chatDisplayName" Aeson..= lookupChatDisplayName chats window.scope
      , "entries" Aeson..= map (chatLogItemValue senders associations window.scope) entries
      , "hasOlder" Aeson..= window.hasOlder
      , "hasNewer" Aeson..= window.hasNewer
      , "anchorFound" Aeson..= window.anchorFound
      , "anchorMessageId" Aeson..= window.anchorMessageId
      ]

chatLogScopeValue :: ChatLog.ChatLogScope -> Aeson.Value
chatLogScopeValue scope =
  Aeson.object
    [ "platform" Aeson..= scope.platform
    , "kind" Aeson..= chatKindValue scope.kind
    , "chatId" Aeson..= scope.chatId
    ]

chatLogItemValue :: Map (ChatPlatform, Text) Identity.SenderInfo -> Map ThreadMessageKey Integer -> ChatLog.ChatLogScope -> ChatLog.ChatLogItem -> Aeson.Value
chatLogItemValue senders associations scope item =
  Aeson.object
    [ "rowId" Aeson..= item.rowId
    , "entry" Aeson..= chatLogEntryValue senders item.entry
    , "threadId" Aeson..= viaNonEmpty head (mapMaybe (`Map.lookup` associations) (itemMessageKeys scope item))
    ]

itemMessageKeys :: ChatLog.ChatLogScope -> ChatLog.ChatLogItem -> [ThreadMessageKey]
itemMessageKeys scope item =
  [ ThreadMessageKey scope.platform scope.chatId messageId
  | messageId <- catMaybes [item.entry.messageId, item.entry.replyToMessageId]
  ]

resolveItemMedia :: Applicative f => (Text -> f Text) -> ChatLog.ChatLogItem -> f ChatLog.ChatLogItem
resolveItemMedia resolve item =
  (\imageUrls files -> item{ChatLog.entry = item.entry{ChatLog.imageUrls = imageUrls, ChatLog.files = files}})
    <$> traverse resolve item.entry.imageUrls
    <*> traverse (\file -> (\ref -> file{ref}) <$> resolve file.ref) item.entry.files

chatLogEntryValue :: Map (ChatPlatform, Text) Identity.SenderInfo -> ChatLog.ChatLogEntry -> Aeson.Value
chatLogEntryValue senders entry =
  Aeson.object
    [ "recordedAt" Aeson..= entry.recordedAt
    , "platform" Aeson..= entry.platform
    , "kind" Aeson..= chatKindValue entry.kind
    , "chatId" Aeson..= entry.chatId
    , "senderId" Aeson..= entry.senderId
    , "senderUsername" Aeson..= entry.senderUsername
    , "senderDisplayName" Aeson..= lookupSenderDisplayName senders entry
    , "messageId" Aeson..= entry.messageId
    , "replyToMessageId" Aeson..= entry.replyToMessageId
    , "isBot" Aeson..= entry.isBot
    , "mentions" Aeson..= entry.mentions
    , "mentionUsernames" Aeson..= entry.mentionUsernames
    , "imageUrls" Aeson..= entry.imageUrls
    , "files" Aeson..= entry.files
    , "text" Aeson..= entry.text
    ]

lookupChatDisplayName :: Map (ChatPlatform, ChatKind, ChatId) (Maybe Text) -> ChatLog.ChatLogScope -> Maybe Text
lookupChatDisplayName names scope =
  scope.chatId >>= \chatId -> Map.lookup (scope.platform, scope.kind, chatId) names >>= id

lookupSenderDisplayName :: Map (ChatPlatform, Text) Identity.SenderInfo -> ChatLog.ChatLogEntry -> Maybe Text
lookupSenderDisplayName senders entry =
  entry.senderDisplayName <|> (entry.senderId >>= \senderId -> Map.lookup (entry.platform, senderId) senders >>= (.displayName))

chatKindValue :: ChatKind -> Text
chatKindValue = \case
  ChatPrivate -> "ChatPrivate"
  ChatGroup -> "ChatGroup"
  ChatChannel -> "ChatChannel"
  ChatUnknown value -> "ChatUnknown:" <> value

queryWindowWithThreadFallback
  :: ChatLog.ChatLog :> es
  => (ThreadMessageKey -> Eff es (Maybe ThreadMessageKey))
  -> (ThreadMessageKey -> Eff es (Maybe Text))
  -> ChatLog.ChatLogScope
  -> ChatLog.ChatLogWindowAnchor
  -> Int
  -> Eff es ChatLog.ChatLogWindow
queryWindowWithThreadFallback parentMessage finalAssistantText scope anchor limit = do
  direct <- ChatLog.queryWindow scope anchor limit
  case anchor of
    ChatLog.AroundChatLogMessage messageId
      | not direct.anchorFound -> findParentWindow direct (64 :: Int) (ThreadMessageKey scope.platform scope.chatId messageId)
    _ -> pure direct
  where
    findParentWindow fallback 0 _ = pure fallback
    findParentWindow fallback remaining key = parentMessage key >>= \case
      Nothing -> findLegacyWindow fallback key
      Just parent -> do
        candidate <- ChatLog.queryWindow scope (ChatLog.AroundChatLogMessage parent.messageId) limit
        if candidate.anchorFound
          then pure candidate
          else findParentWindow fallback (remaining - 1) parent
    findLegacyWindow fallback key = finalAssistantText key >>= \case
      Nothing -> pure fallback
      Just answer -> ChatLog.findLegacyReplyAnchor scope answer >>= \case
        Nothing -> pure fallback
        Just messageId -> ChatLog.queryWindow scope (ChatLog.AroundChatLogMessage messageId) limit
