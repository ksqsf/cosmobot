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
import Bot.Effect.Concurrency (Id (..))
import qualified Bot.LLM.Types as LLM
import qualified Bot.Effect.Storage as Storage
import qualified Bot.JSONRPC as RPC
import Bot.Prelude
import Bot.RPC.Server (RpcServerCallbacks (..), noRpcServerCallbacks)
import qualified Bot.Storage.Thread as Thread
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

threadRpcCallbacks
  :: Storage.Storage :> es
  => Eff es [Thread.ActiveThreadInspection]
  -> (Id -> Eff es Bool)
  -> RpcServerCallbacks es
threadRpcCallbacks inspectActive haltActive =
  noRpcServerCallbacks
    { threadMethod = dispatchThreadMethod inspectActive haltActive
    , supportedMethods = ["thread.list", "thread.get", "thread.active", "thread.halt"]
    }

dispatchThreadMethod
  :: Storage.Storage :> es
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
        searchRoots <- if Text.null params.query then pure [] else Thread.loadThreadRowsByIds (map (.rootRowId) summaries)
        let searchPreviews = Map.fromList [(row.rowId, threadRowPreview row) | row <- searchRoots]
            filtered = filter (summaryMatches params.query searchPreviews) summaries
            page = take params.limit (drop params.offset filtered)
        pageRoots <- if Text.null params.query then Thread.loadThreadRowsByIds (map (.rootRowId) page) else pure []
        let previews = searchPreviews <> Map.fromList [(row.rowId, threadRowPreview row) | row <- pageRoots]
        pure $ Aeson.object
          [ "threads" Aeson..= map (summaryValue previews) page
          , "total" Aeson..= length filtered
          , "nodes" Aeson..= sum (map (.nodeCount) filtered)
          , "leaves" Aeson..= sum (map (.leafCount) filtered)
          , "platforms" Aeson..= Set.size (Set.fromList (map (.rootKey.platform) filtered))
          ]
    "thread.get" ->
      parseParams request parseThreadId \threadId -> do
        rows <- Thread.loadThreadRowsByThreadId threadId
        pure $ case nonEmpty rows of
          Nothing -> Aeson.Null
          Just selectedRows -> threadValue threadId selectedRows
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
  , rootRowId :: !Integer
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
    , rootRowId = root.rowId
    , rootKey = root.messageKey
    , latestKey = latest.messageKey
    , nodeCount = length rows
    , leafCount = length (filter (not . (`Set.member` parentKeys) . (.messageKey)) rows)
    }

resolvedThreadId :: Thread.ThreadIndexRow -> Integer
resolvedThreadId row =
  fromMaybe row.rowId row.threadStorageId

summaryValue :: Map Integer Text -> ThreadSummary -> Aeson.Value
summaryValue previews summary =
  Aeson.object
    [ "threadId" Aeson..= summary.threadId
    , "rootPreview" Aeson..= Map.findWithDefault "" summary.rootRowId previews
    , "rootKey" Aeson..= summary.rootKey
    , "latestKey" Aeson..= summary.latestKey
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
      , Map.findWithDefault "" summary.rootRowId previews
      ]

threadValue :: Integer -> NonEmpty Thread.ThreadRow -> Aeson.Value
threadValue threadId rows =
  Aeson.object
    [ "summary" Aeson..= maybe Aeson.Null (summaryValue previews) summary
    , "nodes" Aeson..= map nodeValue (sortOn (.rowId) (toList rows))
    ]
  where
    summary = summarize threadId (map indexRow (toList rows))
    previews = Map.fromList [(row.rowId, threadRowPreview row) | row <- toList rows]

nodeValue :: Thread.ThreadRow -> Aeson.Value
nodeValue row =
  Aeson.object
    [ "messageKey" Aeson..= row.messageKey
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
    message <- find ((== "user") . (.role)) (reverse messages) <|> viaNonEmpty last messages
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
