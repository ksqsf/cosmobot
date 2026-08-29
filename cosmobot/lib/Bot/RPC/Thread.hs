{-|
Module      : Bot.RPC.Thread
Description : Read-only inspection of persisted conversation threads
Stability   : experimental
-}

module Bot.RPC.Thread
  ( threadRpcCallbacks
  )
where

import Bot.Core.Thread (ThreadMessageKey (..))
import Bot.Core.Message (ChatPlatform (..))
import Bot.Core.Transcript (Transcript (..))
import qualified Bot.Effect.AgentAudit as AgentAudit
import Bot.Effect.Concurrency (Id (..))
import qualified Bot.LLM.Types as LLM
import qualified Bot.Effect.Storage as Storage
import qualified Bot.JSONRPC as RPC
import Bot.Prelude
import Bot.RPC.Server (RpcServerCallbacks (..), noRpcServerCallbacks)
import qualified Bot.Storage.Thread as Thread
import qualified Bot.Storage.Identity as Identity
import qualified Bot.Storage.ChatLog as ChatLog
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

threadRpcCallbacks
  :: (AgentAudit.AgentAudit :> es, Storage.Storage :> es)
  => Eff es [Thread.ActiveThreadInspection]
  -> (Id -> Eff es Bool)
  -> RpcServerCallbacks es
threadRpcCallbacks inspectActive haltActive =
  noRpcServerCallbacks
    { threadMethod = dispatchThreadMethod inspectActive haltActive
    , supportedMethods = ["thread.list", "thread.get", "thread.resolve_run", "thread.active", "thread.halt"]
    }

dispatchThreadMethod
  :: (AgentAudit.AgentAudit :> es, Storage.Storage :> es)
  => Eff es [Thread.ActiveThreadInspection]
  -> (Id -> Eff es Bool)
  -> RPC.RpcRequest
  -> Eff es (Maybe (Either RPC.RpcError Aeson.Value))
dispatchThreadMethod inspectActive haltActive request =
  Just <$> case RPC.requestMethod request of
    "thread.list" ->
      parseParams request parseThreadListParams \params -> do
        rows <- Thread.loadThreadIndexRows
        let summaries = filter (\summary -> maybe True (== summary.rootKey.platform) params.platform) (threadSummaries rows)
        -- ponytail: text search scans root blobs; add an indexed preview column if this becomes a sustained hot path.
        searchTails <- if Text.null params.query then pure [] else Thread.loadThreadRowsByIds (map (.latestRowId) summaries)
        let searchPreviews = Map.fromList [(row.rowId, threadRowPreview row) | row <- searchTails]
            filtered = filter (summaryMatches params.query searchPreviews) summaries
            page = take params.limit (drop params.offset filtered)
        pageTails <- if Text.null params.query then Thread.loadThreadRowsByIds (map (.latestRowId) page) else pure []
        let previews = searchPreviews <> Map.fromList [(row.rowId, threadRowPreview row) | row <- pageTails]
        chatInfos <- Identity.loadChatInfos [(summary.rootKey.platform, chatId) | summary <- page, chatId <- maybeToList summary.rootKey.chatId]
        pure $ Aeson.object
          [ "threads" Aeson..= map (summaryValue chatInfos previews) page
          , "total" Aeson..= length filtered
          , "nodes" Aeson..= sum (map (.nodeCount) filtered)
          , "leaves" Aeson..= sum (map (.leafCount) filtered)
          , "platforms" Aeson..= Set.size (Set.fromList (map (.rootKey.platform) filtered))
          ]
    "thread.get" ->
      parseParams request parseThreadId \threadId -> do
        rows <- Thread.loadThreadRowsByThreadId threadId
        auditRecords <- AgentAudit.queryThreadMessagesAudit (map (.messageKey) rows)
        let logicalRows = collapseAliases (linkedMessageKeys auditRecords) rows
        inputMessageKeys <- ChatLog.loadReplyMessageKeys (map (.messageKey) logicalRows)
        chatInfos <- Identity.loadChatInfos [(row.messageKey.platform, chatId) | row <- take 1 rows, chatId <- maybeToList row.messageKey.chatId]
        pure $ case nonEmpty logicalRows of
          Nothing -> Aeson.Null
          Just selectedRows -> threadValue chatInfos inputMessageKeys threadId selectedRows
    "thread.resolve_run" ->
      parseParams request parseRunId \runId -> do
        active <- find ((== runId) . (.runId)) <$> inspectActive
        records <- AgentAudit.queryRunAudit runId
        threadId <- maybe (pure Nothing) Thread.loadThreadIdByMessageKey (latestLinkedMessageKey records)
        pure $ Aeson.object
          [ "threadId" Aeson..= threadId
          , "taskId" Aeson..= ((.taskId.unId) <$> active)
          ]
    "thread.active" ->
      parseParams request parseNoParams \() -> do
        active <- inspectActive
        pure $ Aeson.object ["threads" Aeson..= map activeThreadValue active]
    "thread.halt" ->
      parseParams request parseTaskId \taskId -> do
        halted <- haltActive (Id taskId)
        pure $ Aeson.object ["taskId" Aeson..= taskId, "halted" Aeson..= halted]
    _ ->
      let method = RPC.requestMethod request
      in pure (Left (RPC.rpcError "method_not_found" [i|Unknown RPC method: #{method}|]))

data ThreadSummary = ThreadSummary
  { threadId :: !Integer
  , latestRowId :: !Integer
  , rootKey :: !ThreadMessageKey
  , latestKey :: !ThreadMessageKey
  , nodeCount :: !Int
  , leafCount :: !Int
  }

data ThreadListParams = ThreadListParams
  { offset :: !Int
  , limit :: !Int
  , query :: !Text
  , platform :: !(Maybe ChatPlatform)
  }

threadSummaries :: [Thread.ThreadIndexRow] -> [ThreadSummary]
threadSummaries =
  sortOn (Down . (.threadId)) . mapMaybe (uncurry summarize) . Map.toList .
    Map.fromListWith (<>) . map (\row -> (resolvedThreadId row, [row]))

summarize :: Integer -> [Thread.ThreadIndexRow] -> Maybe ThreadSummary
summarize threadId rows = do
  root <- extremum List.minimumBy (filter (isNothing . (.parentMessageKey)) rows) <|> extremum List.minimumBy rows
  latest <- extremum List.maximumBy rows
  let parentKeys = Set.fromList (mapMaybe (.parentMessageKey) rows)
  pure ThreadSummary
    { threadId
    , latestRowId = latest.rowId
    , rootKey = root.messageKey
    , latestKey = latest.messageKey
    , nodeCount = length rows
    , leafCount = length (filter (not . (`Set.member` parentKeys) . (.messageKey)) rows)
    }

resolvedThreadId :: Thread.ThreadIndexRow -> Integer
resolvedThreadId row =
  fromMaybe row.rowId row.threadStorageId

summaryValue :: Map (ChatPlatform, Integer) (Maybe Text) -> Map Integer Text -> ThreadSummary -> Aeson.Value
summaryValue chatInfos previews summary =
  Aeson.object
    [ "threadId" Aeson..= summary.threadId
    , "latestPreview" Aeson..= Map.findWithDefault "" summary.latestRowId previews
    , "rootKey" Aeson..= summary.rootKey
    , "latestKey" Aeson..= summary.latestKey
    , "chatDisplayName" Aeson..= (summary.rootKey.chatId >>= \chatId -> Map.lookup (summary.rootKey.platform, chatId) chatInfos >>= id)
    , "nodeCount" Aeson..= summary.nodeCount
    , "leafCount" Aeson..= summary.leafCount
    ]

summaryMatches :: Text -> Map Integer Text -> ThreadSummary -> Bool
summaryMatches rawQuery previews summary
  | Text.null query = True
  | otherwise = query `Text.isInfixOf` Text.toCaseFold searchable
  where
    query = Text.toCaseFold (Text.strip rawQuery)
    searchable = Text.unwords
      [ toText (show summary.threadId :: String)
      , toText (show summary.rootKey.platform :: String)
      , maybe "" (toText . (show :: Integer -> String)) summary.rootKey.chatId
      , Map.findWithDefault "" summary.latestRowId previews
      ]

threadValue :: Map (ChatPlatform, Integer) (Maybe Text) -> Map ThreadMessageKey ThreadMessageKey -> Integer -> NonEmpty Thread.ThreadRow -> Aeson.Value
threadValue chatInfos inputMessageKeys threadId rows =
  Aeson.object
    [ "summary" Aeson..= maybe Aeson.Null (summaryValue chatInfos previews) summary
    , "nodes" Aeson..= map (nodeValue inputMessageKeys) logicalRows
    ]
  where
    logicalRows = toList rows
    summary = summarize threadId (map indexRow logicalRows)
    previews = Map.fromList [(row.rowId, threadRowPreview row) | row <- logicalRows]

-- Multiple platform messages emitted by one agent turn are reply aliases for
-- the same logical conversation node. Prefer its audit-linked response as the
-- canonical link and redirect child links to it. Legacy rows without that
-- audit event fall back to the last alias.
collapseAliases :: Set ThreadMessageKey -> [Thread.ThreadRow] -> [Thread.ThreadRow]
collapseAliases linkedKeys rows =
  sortOn (.rowId)
    [ remapParent canonical
    | aliasGroup <- Map.elems groups
    , let canonical = canonicalRow aliasGroup
    ]
  where
    groups = Map.fromListWith (<>)
      [ ((row.parentMessageKey, row.messagesJson), [row])
      | row <- rows
      ]
    aliases = Map.fromList
      [ (row.messageKey, canonical.messageKey)
      | aliasGroup <- Map.elems groups
      , let canonical = canonicalRow aliasGroup
      , row <- aliasGroup
      ]
    canonicalRow aliasGroup = List.maximumBy (comparing (.rowId)) case filter ((`Set.member` linkedKeys) . (.messageKey)) aliasGroup of
      [] -> aliasGroup
      linked -> linked
    remapParent :: Thread.ThreadRow -> Thread.ThreadRow
    remapParent row =
      Thread.ThreadRow row.rowId row.messageKey row.threadStorageId
        (row.parentMessageKey <&> \parent -> Map.findWithDefault parent parent aliases)
        row.messagesJson

linkedMessageKeys :: [AgentAudit.AgentAuditRecord] -> Set ThreadMessageKey
linkedMessageKeys records = Set.fromList
  [ linkedMessageKey
  | record <- records
  , AgentAudit.AgentThreadLinked{linkedMessageKey = Just linkedMessageKey} <- [record.event]
  ]

nodeValue :: Map ThreadMessageKey ThreadMessageKey -> Thread.ThreadRow -> Aeson.Value
nodeValue inputMessageKeys row =
  Aeson.object
    [ "messageKey" Aeson..= row.messageKey
    , "inputMessageKey" Aeson..= Map.lookup row.messageKey inputMessageKeys
    , "parentMessageKey" Aeson..= row.parentMessageKey
    , "messages" Aeson..= decodeMessages row.messagesJson
    ]

decodeMessages :: Text -> Aeson.Value
decodeMessages encoded =
  fromMaybe (Aeson.Array mempty) $ Aeson.decode $ LazyByteString.fromStrict $ TextEncoding.encodeUtf8 encoded

threadRowPreview :: Thread.ThreadRow -> Text
threadRowPreview row =
  fromMaybe "" $ do
    messages <- Aeson.decodeStrict' (TextEncoding.encodeUtf8 row.messagesJson) :: Maybe [LLM.ChatMessage]
    let visibleMessages = filter ((/= "synthetic") . (.role)) messages
    message <- find ((== "user") . (.role)) (reverse visibleMessages) <|> viaNonEmpty last visibleMessages
    readableMessageContent message.content

readableMessageContent :: Maybe LLM.MessageContent -> Maybe Text
readableMessageContent = \case
  Just (LLM.TextContent text) -> nonEmptyPreview text
  Just (LLM.PartsContent parts) -> nonEmptyPreview $ Text.unwords
    [ text
    | LLM.TextPart text <- parts
    ]
  Nothing -> Nothing
  where
    nonEmptyPreview = nonEmptyText . Text.unwords . Text.words
    nonEmptyText text = text <$ guard (not (Text.null text))

activeThreadValue :: Thread.ActiveThreadInspection -> Aeson.Value
activeThreadValue active =
  Aeson.object
    [ "taskId" Aeson..= active.taskId.unId
    , "runId" Aeson..= active.runId
    , "prompt" Aeson..= active.prompt
    , "parentMessageKey" Aeson..= active.parentMessageKey
    , "messageKeys" Aeson..= active.messageKeys
    , "pendingSteers" Aeson..= active.pendingSteers
    , "messages" Aeson..= toList active.transcript.messages
    ]

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
parseNoParams value = Aeson.withObject "thread.list params" (\o -> unless (null o) (fail "params must be empty")) value

parseThreadListParams :: Aeson.Value -> AesonTypes.Parser ThreadListParams
parseThreadListParams Aeson.Null = pure (ThreadListParams 0 25 "" Nothing)
parseThreadListParams value = Aeson.withObject "thread.list params" parse value
  where
    parse o = do
      offset <- fromMaybe 0 <$> o Aeson..:? "offset"
      limit <- fromMaybe 25 <$> o Aeson..:? "limit"
      query <- Text.strip . fromMaybe "" <$> o Aeson..:? "query"
      platform <- o Aeson..:? "platform" >>= traverse parsePlatform
      unless (offset >= 0) (fail "offset must be non-negative")
      unless (limit >= 1 && limit <= 200) (fail "limit must be between 1 and 200")
      unless (Text.length query <= 256) (fail "query must be at most 256 characters")
      pure ThreadListParams{offset, limit, query, platform}

parsePlatform :: Text -> AesonTypes.Parser ChatPlatform
parsePlatform = \case
  "qq" -> pure PlatformQQ
  "telegram" -> pure PlatformTelegram
  "matrix" -> pure PlatformMatrix
  "discord" -> pure PlatformDiscord
  "rpc" -> pure PlatformRPC
  "acp" -> pure PlatformACP
  _ -> fail "platform must be one of: qq, telegram, matrix, discord, rpc, acp"

parseThreadId :: Aeson.Value -> AesonTypes.Parser Integer
parseThreadId =
  Aeson.withObject "thread.get params" \o -> do
    threadId <- o Aeson..: "threadId"
    if threadId > 0 then pure threadId else fail "threadId must be positive"

parseRunId :: Aeson.Value -> AesonTypes.Parser Text
parseRunId =
  Aeson.withObject "thread.resolve_run params" \o -> do
    runId <- Text.strip <$> o Aeson..: "runId"
    if Text.null runId || Text.length runId > 256
      then fail "runId must contain between 1 and 256 characters"
      else pure runId

parseTaskId :: Aeson.Value -> AesonTypes.Parser Integer
parseTaskId =
  Aeson.withObject "thread.halt params" \o -> do
    taskId <- o Aeson..: "taskId"
    if taskId > 0 then pure taskId else fail "taskId must be positive"

extremum
  :: ((Thread.ThreadIndexRow -> Thread.ThreadIndexRow -> Ordering) -> [Thread.ThreadIndexRow] -> Thread.ThreadIndexRow)
  -> [Thread.ThreadIndexRow]
  -> Maybe Thread.ThreadIndexRow
extremum choose = viaNonEmpty (choose (comparing (.rowId)) . toList)

indexRow :: Thread.ThreadRow -> Thread.ThreadIndexRow
indexRow row = Thread.ThreadIndexRow
  { rowId = row.rowId
  , messageKey = row.messageKey
  , threadStorageId = row.threadStorageId
  , parentMessageKey = row.parentMessageKey
  }

latestLinkedMessageKey :: [AgentAudit.AgentAuditRecord] -> Maybe ThreadMessageKey
latestLinkedMessageKey =
  listToMaybe . mapMaybe linkedKey . reverse
  where
    linkedKey AgentAudit.AgentAuditRecord{event = AgentAudit.AgentThreadLinked{linkedMessageKey}} = linkedMessageKey
    linkedKey _ = Nothing
