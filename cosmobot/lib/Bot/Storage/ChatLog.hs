{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Storage.ChatLog
Description : Persistent chat log storage
Stability   : experimental
-}

module Bot.Storage.ChatLog
  ( persistRecord
  , queryStored
  , queryCurrentSenderStored
  , listStoredChats
  , queryStoredWindow
  , loadReplyMessageKeys
  , findLegacyReplyAnchor
  )
where

import Bot.ChatLog.Record
import Bot.ChatLog.Types
import Bot.Core.Message
import Bot.Core.Thread (ThreadMessageKey (..))
import Bot.Prelude
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Storage.Identity as Identity
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time (getCurrentTime)
import qualified Database.Selda.Backend as SeldaBackend
import qualified Database.Selda.SQLite as SeldaSQLite

data ChatLogRow = ChatLogRow
  { id :: ID ChatLogRow
  , platform_key :: Text
  , kind_key :: Text
  , chat_id :: Maybe Text
  , sender_id :: Maybe Text
  , sender_username :: Maybe Text
  , sender_display_name :: Maybe Text
  , message_id :: Maybe Text
  , reply_to_message_id :: Maybe Text
  , is_bot :: Bool
  , mentions :: Text
  , mention_usernames :: Text
  , image_urls :: Text
  , files_json :: Text
  , body_text :: Text
  , recorded_at :: Maybe UTCTime
  }
  deriving (Generic)

instance SqlRow ChatLogRow

chatLogRows :: Table ChatLogRow
chatLogRows =
  table "chat_log"
    [ #id :- autoPrimary
    , #platform_key :- index
    , #kind_key :- index
    , #chat_id :- index
    , #sender_id :- index
    ]

ensureChatLogTable :: Storage.Storage :> es => Eff es ()
ensureChatLogTable = do
  runSelda $ transaction do
    tryCreateTable chatLogRows
    migrateChatLogFiles
    migrateChatLogDisplayNames
  Identity.ensureIdentityTables

migrateChatLogFiles :: SeldaT SeldaSQLite.SQLite IO ()
migrateChatLogFiles =
  SeldaBackend.withBackend \backend -> liftIO do
    (_, rows) <- SeldaBackend.runStmt backend "PRAGMA table_info(chat_log)" []
    let columns = [name | _ : SeldaBackend.SqlString name : _ <- rows]
    unless ("files_json" `elem` columns) $
      void (SeldaBackend.runStmt backend "ALTER TABLE chat_log ADD COLUMN files_json TEXT NOT NULL DEFAULT '[]'" [])

migrateChatLogDisplayNames :: SeldaT SeldaSQLite.SQLite IO ()
migrateChatLogDisplayNames =
  SeldaBackend.withBackend \backend -> liftIO do
    (_, rows) <- SeldaBackend.runStmt backend "PRAGMA table_info(chat_log)" []
    let columns = [name | _ : SeldaBackend.SqlString name : _ <- rows]
    unless ("sender_display_name" `elem` columns) $
      void (SeldaBackend.runStmt backend "ALTER TABLE chat_log ADD COLUMN sender_display_name TEXT" [])

persistRecord :: (IOE :> es, KatipE :> es, Storage.Storage :> es) => ChatLogRecord -> Eff es ()
persistRecord record = do
  ensureChatLogTable
  recordedAt <- liftIO getCurrentTime
  runSelda (transaction do
      Identity.rememberIncomingIdentityRows recordedAt (chatLogRecordMessage record)
      insert_ chatLogRows [chatLogRow (sanitizeChatLogEntry (chatLogEntry recordedAt record))])
    `catchSync` \err ->
      $(logError) [i|Failed to persist chat log entry: #{show err :: String}|]

queryStored :: Storage.Storage :> es => IncomingMessage -> Maybe Text -> Int -> Bool -> ChatLogTimeRange -> Eff es [ChatLogEntry]
queryStored message sender limitCount includeBotMessages timeRange = do
  ensureChatLogTable
  rows <- runSelda $
    query $
      queryLimit 0 (max 0 limitCount) do
        row <- select chatLogRows
        restrict (chatLogMatches message includeBotMessages row .&& optionalSenderMatches sender row .&& timeRangeMatches timeRange row)
        order (row ! #id) descending
        pure row
  pure (map (chatLogEntryFromRow message) (reverse rows))

queryCurrentSenderStored :: Storage.Storage :> es => IncomingMessage -> SenderChatLogScope -> [[Text]] -> Int -> ChatLogTimeRange -> Eff es [ChatLogEntry]
queryCurrentSenderStored message scope keywords limitCount timeRange = do
  ensureChatLogTable
  case (message.senderId, keywordLikePatterns keywords) of
    (Just _, patterns@(_ : _))
      | scope == SenderChatLogGlobal || isJust message.chatId -> do
          rows <- runSelda $
            query $
              queryLimit 0 (boundedChatLogLimit limitCount) do
                row <- select chatLogRows
                restrict (currentSenderChatLogMatches scope message patterns row .&& timeRangeMatches timeRange row)
                order (row ! #id) descending
                pure row
          pure (map (chatLogEntryFromRow message) rows)
    _ ->
      pure []

listStoredChats :: Storage.Storage :> es => Eff es [ChatLogSummary]
listStoredChats = do
  ensureChatLogTable
  rows <- runSelda $ query $ aggregate do
    row <- select chatLogRows
    platformKey' <- groupBy (row ! #platform_key)
    kindKey' <- groupBy (row ! #kind_key)
    chatId' <- groupBy (row ! #chat_id)
    pure (platformKey' :*: kindKey' :*: chatId' :*: count (row ! #id) :*: max_ (row ! #recorded_at))
  pure $ sortOn (Down . (.latestAt)) $ mapMaybe summaryFromRow rows

queryStoredWindow
  :: Storage.Storage :> es
  => ChatLogScope
  -> ChatLogWindowAnchor
  -> Int
  -> Eff es ChatLogWindow
queryStoredWindow scope anchor requestedLimit = do
  ensureChatLogTable
  anchorRow <- case anchor of
    AroundChatLogMessage messageId -> findMessageRow scope messageId
    _ -> pure Nothing
  let limitCount = min 200 (max 1 requestedLimit)
      resolvedAnchor = case anchor of
        AroundChatLogMessage _ -> maybe ResolvedLatestChatLogWindow (AroundChatLogRow . rowIdInteger . (.id)) anchorRow
        LatestChatLogWindow -> ResolvedLatestChatLogWindow
        BeforeChatLogRow rowId -> ResolvedBeforeChatLogRow rowId
        AfterChatLogRow rowId -> ResolvedAfterChatLogRow rowId
  rows <- queryWindowRows scope resolvedAnchor limitCount
  let items = map (chatLogItemFromRow scope) rows
  (hasOlder, hasNewer) <- case items of
    [] -> pure (False, False)
    firstItem : rest ->
      (,) <$> hasRowsBefore scope firstItem.rowId <*> hasRowsAfter scope (foldl' (\_ item -> item) firstItem rest).rowId
  pure ChatLogWindow
    { scope
    , entries = items
    , hasOlder
    , hasNewer
    , anchorFound = case anchor of AroundChatLogMessage _ -> isJust anchorRow; _ -> True
    , anchorMessageId = case anchor of AroundChatLogMessage messageId | isJust anchorRow -> Just messageId; _ -> Nothing
    }

-- | Resolve stored platform replies back to the messages they answered.
-- Message ids are queried together with their platform/chat scope because
-- they are not globally unique.
loadReplyMessageKeys :: Storage.Storage :> es => [ThreadMessageKey] -> Eff es (Map ThreadMessageKey ThreadMessageKey)
loadReplyMessageKeys requested = do
  ensureChatLogTable
  Map.fromList . concat <$> traverse loadScope (Map.toList scopes)
  where
    scopes = Map.fromListWith (<>)
      [ ((key.platform, key.chatId), [key.messageId])
      | key <- ordNub requested
      ]
    loadScope ((platform, chatId), messageIds) = do
      rows <- runSelda $ query do
        row <- select chatLogRows
        restrict $
          row ! #platform_key .== literal (platformKey platform)
            .&& chatIdMatches chatId row
            .&& row ! #message_id `isIn` map (literal . Just . messageIdText) messageIds
        pure (row ! #message_id :*: row ! #reply_to_message_id)
      pure
        [ ( ThreadMessageKey platform chatId (textMessageId messageId)
          , ThreadMessageKey platform chatId (textMessageId replyToMessageId)
          )
        | Just messageId :*: Just replyToMessageId <- rows
        ]

data ResolvedChatLogWindowAnchor
  = ResolvedLatestChatLogWindow
  | ResolvedBeforeChatLogRow !Integer
  | ResolvedAfterChatLogRow !Integer
  | AroundChatLogRow !Integer

queryWindowRows :: Storage.Storage :> es => ChatLogScope -> ResolvedChatLogWindowAnchor -> Int -> Eff es [ChatLogRow]
queryWindowRows scope anchor limitCount = case anchor of
  ResolvedLatestChatLogWindow -> reverse <$> queryDescending scope Nothing limitCount
  ResolvedBeforeChatLogRow rowId -> reverse <$> queryDescending scope (Just rowId) limitCount
  ResolvedAfterChatLogRow rowId -> queryAscending scope rowId limitCount
  AroundChatLogRow rowId -> do
    let olderLimit = limitCount `div` 2 + 1
    older <- reverse <$> queryDescending scope (Just (rowId + 1)) olderLimit
    newer <- queryAscending scope rowId (limitCount - length older)
    pure (older <> newer)

queryDescending :: Storage.Storage :> es => ChatLogScope -> Maybe Integer -> Int -> Eff es [ChatLogRow]
queryDescending scope beforeRow limitCount = runSelda $ query $ queryLimit 0 limitCount do
  row <- select chatLogRows
  restrict (scopeMatches scope row .&& maybe true (\rowId -> row ! #id .< literal (chatLogRowId rowId)) beforeRow)
  order (row ! #id) descending
  pure row

queryAscending :: Storage.Storage :> es => ChatLogScope -> Integer -> Int -> Eff es [ChatLogRow]
queryAscending scope afterRow limitCount = runSelda $ query $ queryLimit 0 limitCount do
  row <- select chatLogRows
  restrict (scopeMatches scope row .&& row ! #id .> literal (chatLogRowId afterRow))
  order (row ! #id) ascending
  pure row

findMessageRow :: Storage.Storage :> es => ChatLogScope -> MessageId -> Eff es (Maybe ChatLogRow)
findMessageRow scope messageId = do
  rows <- runSelda $ query $ queryLimit 0 1 do
    row <- select chatLogRows
    restrict (scopeMatches scope row .&& row ! #message_id .== literal (Just (messageIdText messageId)))
    order (row ! #id) descending
    pure row
  pure (viaNonEmpty head rows)

-- | Compatibility for bot rows written before sent message ids were persisted.
-- Exact reply-body matching is intentionally only a fallback for those legacy rows.
findLegacyReplyAnchor :: Storage.Storage :> es => ChatLogScope -> Text -> Eff es (Maybe MessageId)
findLegacyReplyAnchor scope body = do
  ensureChatLogTable
  rows <- runSelda $ query $ queryLimit 0 1 do
    row <- select chatLogRows
    restrict (scopeMatches scope row .&& row ! #is_bot .== literal True .&& row ! #body_text .== literal body)
    order (row ! #id) descending
    pure (row ! #reply_to_message_id)
  pure (textMessageId <$> (join (viaNonEmpty head rows)))

hasRowsBefore :: Storage.Storage :> es => ChatLogScope -> Integer -> Eff es Bool
hasRowsBefore scope rowId = not . null <$> queryDescending scope (Just rowId) 1

hasRowsAfter :: Storage.Storage :> es => ChatLogScope -> Integer -> Eff es Bool
hasRowsAfter scope rowId = not . null <$> queryAscending scope rowId 1

scopeMatches :: forall (backend :: Type). ChatLogScope -> Row backend ChatLogRow -> Col backend Bool
scopeMatches scope row =
  row ! #platform_key .== literal (platformKey scope.platform)
    .&& row ! #kind_key .== literal (kindKey scope.kind)
    .&& maybe (isNull (row ! #chat_id)) (\chatId -> row ! #chat_id .== literal (Just (chatIdText chatId))) scope.chatId

summaryFromRow :: Text :*: Text :*: Maybe Text :*: Int :*: Maybe (Maybe UTCTime) -> Maybe ChatLogSummary
summaryFromRow (platformKey' :*: kindKey' :*: chatId' :*: messageCount :*: latestAt) = do
  platform <- platformFromKey platformKey'
  pure ChatLogSummary
    { scope = ChatLogScope{platform, kind = kindFromKey kindKey', chatId = textChatId <$> chatId'}
    , messageCount
    , latestAt = join latestAt
    }

chatLogItemFromRow :: ChatLogScope -> ChatLogRow -> ChatLogItem
chatLogItemFromRow scope row = ChatLogItem
  { rowId = rowIdInteger row.id
  , entry = chatLogEntryFromScope scope row
  }

rowIdInteger :: ID ChatLogRow -> Integer
rowIdInteger = fromIntegral . fromId

chatLogRowId :: Integer -> ID ChatLogRow
chatLogRowId = toId . fromIntegral

boundedChatLogLimit :: Int -> Int
boundedChatLogLimit =
  min 100 . max 0

chatLogRow :: ChatLogEntry -> ChatLogRow
chatLogRow entry =
  ChatLogRow
    { id = def
    , platform_key = platformKey entry.platform
    , kind_key = kindKey entry.kind
    , chat_id = chatIdText <$> entry.chatId
    , sender_id = entry.senderId
    , sender_username = entry.senderUsername
    , sender_display_name = entry.senderDisplayName
    , message_id = messageIdText <$> entry.messageId
    , reply_to_message_id = messageIdText <$> entry.replyToMessageId
    , is_bot = entry.isBot
    , mentions = encodeTextList entry.mentions
    , mention_usernames = encodeTextList entry.mentionUsernames
    , image_urls = encodeTextList entry.imageUrls
    , files_json = encodeFiles entry.files
    , body_text = entry.text
    , recorded_at = entry.recordedAt
    }

chatLogEntryFromRow :: IncomingMessage -> ChatLogRow -> ChatLogEntry
chatLogEntryFromRow context row =
  chatLogEntryFromScope ChatLogScope{platform = context.platform, kind = context.kind, chatId = context.chatId} row

chatLogEntryFromScope :: ChatLogScope -> ChatLogRow -> ChatLogEntry
chatLogEntryFromScope scope row =
  ChatLogEntry
    { recordedAt = row.recorded_at
    , platform = scope.platform
    , kind = scope.kind
    , chatId = textChatId <$> row.chat_id
    , senderId = row.sender_id
    , senderUsername = row.sender_username
    , senderDisplayName = row.sender_display_name
    , messageId = textMessageId <$> row.message_id
    , replyToMessageId = textMessageId <$> row.reply_to_message_id
    , isBot = row.is_bot
    , mentions = decodeTextList row.mentions
    , mentionUsernames = decodeTextList row.mention_usernames
    , imageUrls = decodeTextList row.image_urls
    , files = decodeFiles row.files_json
    , text = row.body_text
    }

chatLogMatches :: forall (backend :: Type). IncomingMessage -> Bool -> Row backend ChatLogRow -> Col backend Bool
chatLogMatches message includeBotMessages row =
  row ! #platform_key .== literal (platformKey message.platform)
    .&& row ! #kind_key .== literal (kindKey message.kind)
    .&& chatIdMatches message.chatId row
    .&& botVisibilityMatches includeBotMessages row

chatIdMatches :: forall (backend :: Type). Maybe ChatId -> Row backend ChatLogRow -> Col backend Bool
chatIdMatches Nothing row =
  isNull (row ! #chat_id)
chatIdMatches (Just chatId) row =
  row ! #chat_id .== literal (Just (chatIdText chatId))

botVisibilityMatches :: forall (backend :: Type). Bool -> Row backend ChatLogRow -> Col backend Bool
botVisibilityMatches True _ =
  true
botVisibilityMatches False row =
  row ! #is_bot .== literal False

timeRangeMatches :: forall (backend :: Type). ChatLogTimeRange -> Row backend ChatLogRow -> Col backend Bool
timeRangeMatches timeRange row =
  maybe true (\since -> row ! #recorded_at .> literal (Just since)) timeRange.since
    .&& maybe true (\before -> row ! #recorded_at .< literal (Just before)) timeRange.before

currentSenderChatLogMatches :: forall (backend :: Type). SenderChatLogScope -> IncomingMessage -> [Text] -> Row backend ChatLogRow -> Col backend Bool
currentSenderChatLogMatches scope message patterns row =
  row ! #platform_key .== literal (platformKey message.platform)
    .&& senderChatLogScopeMatches scope message row
    .&& botVisibilityMatches False row
    .&& maybe false (`senderIdMatches` row) message.senderId
    .&& keywordPatternsMatch patterns row

senderChatLogScopeMatches :: forall (backend :: Type). SenderChatLogScope -> IncomingMessage -> Row backend ChatLogRow -> Col backend Bool
senderChatLogScopeMatches SenderChatLogGlobal _ _ =
  true
senderChatLogScopeMatches SenderChatLogChat message row =
  row ! #kind_key .== literal (kindKey message.kind)
    .&& chatIdMatches message.chatId row

optionalSenderMatches :: forall (backend :: Type). Maybe Text -> Row backend ChatLogRow -> Col backend Bool
optionalSenderMatches Nothing _ =
  true
optionalSenderMatches (Just senderId) row =
  senderIdMatches senderId row

senderIdMatches :: forall (backend :: Type). Text -> Row backend ChatLogRow -> Col backend Bool
senderIdMatches senderId row =
  row ! #sender_id .== literal (Just senderId)

keywordPatternsMatch :: forall (backend :: Type). [Text] -> Row backend ChatLogRow -> Col backend Bool
keywordPatternsMatch [] _ =
  false
keywordPatternsMatch (pattern : rest) row =
  foldl' (.||) (row ! #body_text `like` literal pattern)
    [ row ! #body_text `like` literal nextPattern
    | nextPattern <- rest
    ]

keywordLikePatterns :: [[Text]] -> [Text]
keywordLikePatterns =
  map \keyword -> "%" <> Text.intercalate "%" keyword <> "%"

platformKey :: ChatPlatform -> Text
platformKey =
  show

platformFromKey :: Text -> Maybe ChatPlatform
platformFromKey = \case
  "PlatformQQ" -> Just PlatformQQ
  "PlatformTelegram" -> Just PlatformTelegram
  "PlatformMatrix" -> Just PlatformMatrix
  "PlatformDiscord" -> Just PlatformDiscord
  "PlatformRPC" -> Just PlatformRPC
  "PlatformACP" -> Just PlatformACP
  _ -> Nothing

kindKey :: ChatKind -> Text
kindKey (ChatUnknown value) = value
kindKey value = show value

kindFromKey :: Text -> ChatKind
kindFromKey = \case
  "ChatPrivate" -> ChatPrivate
  "ChatGroup" -> ChatGroup
  "ChatChannel" -> ChatChannel
  value -> ChatUnknown value

encodeTextList :: [Text] -> Text
encodeTextList =
  Text.intercalate "\n"

decodeTextList :: Text -> [Text]
decodeTextList value
  | Text.null value = []
  | otherwise = Text.splitOn "\n" value

encodeFiles :: [MessageFile] -> Text
encodeFiles =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

decodeFiles :: Text -> [MessageFile]
decodeFiles =
  fromMaybe [] . Aeson.decodeStrict' . TextEncoding.encodeUtf8
