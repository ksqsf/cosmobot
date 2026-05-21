{-|
Module      : Bot.RPC.Storage
Description : Durable storage for local RPC chat sessions
Stability   : experimental
-}
{-# LANGUAGE OverloadedLabels #-}

module Bot.RPC.Storage
  ( StoredChatMessage (..)
  , StoredChatSession (..)
  , ensureRpcTables
  , createSession
  , listSessions
  , loadSession
  , loadSessionHistory
  , insertMessage
  , renameSession
  , deleteSession
  , forkSession
  , nextMessageId
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

data StoredChatSession = StoredChatSession
  { sessionId :: !Text
  , label :: !(Maybe Text)
  , parentSessionId :: !(Maybe Text)
  , parentMessageId :: !(Maybe MessageId)
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data StoredChatMessage = StoredChatMessage
  { sessionId :: !Text
  , messageId :: !MessageId
  , sender :: !Text
  , text :: !Text
  , imageUrls :: ![Text]
  , replyToMessageId :: !(Maybe MessageId)
  , parentMessageId :: !(Maybe MessageId)
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data RpcSessionRow = RpcSessionRow
  { id :: ID RpcSessionRow
  , session_id :: Text
  , label :: Maybe Text
  , parent_session_id :: Maybe Text
  , parent_message_id :: Maybe Text
  }
  deriving (Generic)

instance SqlRow RpcSessionRow

data RpcMessageRow = RpcMessageRow
  { id :: ID RpcMessageRow
  , session_id :: Text
  , message_id :: Text
  , sender :: Text
  , body :: Text
  , image_urls_json :: Text
  , reply_to_message_id :: Maybe Text
  , parent_message_id :: Maybe Text
  }
  deriving (Generic)

instance SqlRow RpcMessageRow

rpcSessionRows :: Table RpcSessionRow
rpcSessionRows =
  table "rpc_chat_sessions"
    [ #id :- autoPrimary
    , #session_id :- unique
    , #parent_session_id :- index
    , #parent_message_id :- index
    ]

rpcMessageRows :: Table RpcMessageRow
rpcMessageRows =
  table "rpc_chat_messages"
    [ #id :- autoPrimary
    , #session_id :- index
    , #message_id :- unique
    , #parent_message_id :- index
    ]

ensureRpcTables :: Storage.Storage :> es => Eff es ()
ensureRpcTables =
  runSelda do
    tryCreateTable rpcSessionRows
    tryCreateTable rpcMessageRows

createSession :: Storage.Storage :> es => Maybe Text -> Eff es StoredChatSession
createSession label = do
  ensureRpcTables
  sessions <- listSessions
  let sessionId = nextSessionId (fromMaybe "session" (label >>= nonEmptyText)) (map (.sessionId) sessions)
      stored = StoredChatSession{sessionId, label = label >>= nonEmptyText, parentSessionId = Nothing, parentMessageId = Nothing}
  runSelda $
    insert_ rpcSessionRows [sessionRow stored]
  pure stored

listSessions :: Storage.Storage :> es => Eff es [StoredChatSession]
listSessions = do
  ensureRpcTables
  rows <- runSelda $
    query do
      row <- select rpcSessionRows
      order (row ! #id) ascending
      pure row
  pure (map sessionFromRow rows)

loadSession :: Storage.Storage :> es => Text -> Eff es (Maybe StoredChatSession)
loadSession targetSessionId = do
  ensureRpcTables
  rows <- runSelda $
    query $
      queryLimit 0 1 do
        row <- select rpcSessionRows
        restrict (row ! #session_id .== literal targetSessionId)
        pure row
  pure (sessionFromRow <$> viaNonEmpty head rows)

loadSessionHistory :: Storage.Storage :> es => Text -> Eff es [StoredChatMessage]
loadSessionHistory targetSessionId = do
  current <- loadOwnMessages targetSessionId
  loadSession targetSessionId >>= \case
    Nothing ->
      pure []
    Just session
      | Just parentSession <- session.parentSessionId
      , Just parentMessage <- session.parentMessageId -> do
          parentHistory <- loadSessionHistory parentSession
          pure (takeThrough parentMessage parentHistory <> current)
      | otherwise ->
          pure current

insertMessage :: Storage.Storage :> es => StoredChatMessage -> Eff es ()
insertMessage message = do
  ensureRpcTables
  runSelda $
    insert_ rpcMessageRows [messageRow message]

renameSession :: Storage.Storage :> es => Text -> Text -> Eff es (Maybe StoredChatSession)
renameSession targetSessionId newLabel = do
  ensureRpcTables
  runSelda $
    update_ rpcSessionRows
      (\row -> row ! #session_id .== literal targetSessionId)
      (\row -> row `with` [#label := literal (nonEmptyText newLabel)])
  loadSession targetSessionId

deleteSession :: Storage.Storage :> es => Text -> Eff es Bool
deleteSession targetSessionId = do
  existed <- isJust <$> loadSession targetSessionId
  when existed do
    runSelda do
      deleteFrom_ rpcMessageRows \row ->
        row ! #session_id .== literal targetSessionId
      deleteFrom_ rpcSessionRows \row ->
        row ! #session_id .== literal targetSessionId
  pure existed

forkSession :: Storage.Storage :> es => Text -> MessageId -> Maybe Text -> Eff es (Maybe StoredChatSession)
forkSession sourceSessionId sourceMessageId requestedLabel = do
  source <- loadSession sourceSessionId
  sourceMessages <- loadSessionHistory sourceSessionId
  case (source, find ((== sourceMessageId) . (.messageId)) sourceMessages) of
    (Just sourceSession, Just _) -> do
      sessions <- listSessions
      let base = fromMaybe (sourceLabelBase sourceSession <> "-fork") (requestedLabel >>= nonEmptyText)
          sessionId = nextSessionId base (map (.sessionId) sessions)
          stored = StoredChatSession
            { sessionId
            , label = requestedLabel >>= nonEmptyText
            , parentSessionId = Just sourceSessionId
            , parentMessageId = Just sourceMessageId
            }
      runSelda $
        insert_ rpcSessionRows [sessionRow stored]
      pure (Just stored)
    _ ->
      pure Nothing

nextMessageId :: Storage.Storage :> es => Eff es MessageId
nextMessageId = do
  ensureRpcTables
  rows <- runSelda $
    query do
      row <- select rpcMessageRows
      pure (row ! #message_id)
  let nextNumber = Foldable.maximum (0 : map messageNumber rows) + 1
  pure (textMessageId ("rpc-" <> show nextNumber))

loadOwnMessages :: Storage.Storage :> es => Text -> Eff es [StoredChatMessage]
loadOwnMessages targetSessionId = do
  ensureRpcTables
  rows <- runSelda $
    query do
      row <- select rpcMessageRows
      restrict (row ! #session_id .== literal targetSessionId)
      order (row ! #id) ascending
      pure row
  pure (map messageFromRow rows)

sessionRow :: StoredChatSession -> RpcSessionRow
sessionRow session =
  RpcSessionRow
    { id = def
    , session_id = session.sessionId
    , label = session.label
    , parent_session_id = session.parentSessionId
    , parent_message_id = messageIdText <$> session.parentMessageId
    }

sessionFromRow :: RpcSessionRow -> StoredChatSession
sessionFromRow row =
  StoredChatSession
    { sessionId = row.session_id
    , label = row.label
    , parentSessionId = row.parent_session_id
    , parentMessageId = textMessageId <$> row.parent_message_id
    }

messageRow :: StoredChatMessage -> RpcMessageRow
messageRow message =
  RpcMessageRow
    { id = def
    , session_id = message.sessionId
    , message_id = messageIdText message.messageId
    , sender = message.sender
    , body = message.text
    , image_urls_json = encodeImageUrls message.imageUrls
    , reply_to_message_id = messageIdText <$> message.replyToMessageId
    , parent_message_id = messageIdText <$> message.parentMessageId
    }

messageFromRow :: RpcMessageRow -> StoredChatMessage
messageFromRow row =
  StoredChatMessage
    { sessionId = row.session_id
    , messageId = textMessageId row.message_id
    , sender = row.sender
    , text = row.body
    , imageUrls = decodeImageUrls row.image_urls_json
    , replyToMessageId = textMessageId <$> row.reply_to_message_id
    , parentMessageId = textMessageId <$> row.parent_message_id
    }

encodeImageUrls :: [Text] -> Text
encodeImageUrls =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

decodeImageUrls :: Text -> [Text]
decodeImageUrls value =
  fromMaybe [] (Aeson.decodeStrict' (TextEncoding.encodeUtf8 value))

takeThrough :: MessageId -> [StoredChatMessage] -> [StoredChatMessage]
takeThrough target =
  go
  where
    go [] =
      []
    go (message : rest)
      | message.messageId == target =
          [message]
      | otherwise =
          message : go rest

nextSessionId :: Text -> [Text] -> Text
nextSessionId base existing =
  let prefix = base <> "-"
      nextNumber = Foldable.maximum (0 : mapMaybe (Text.stripPrefix prefix >=> readMaybe . Text.unpack) existing) + 1
  in prefix <> show (nextNumber :: Integer)

sourceLabelBase :: StoredChatSession -> Text
sourceLabelBase session =
  fromMaybe session.sessionId session.label

messageNumber :: Text -> Integer
messageNumber value =
  fromMaybe 0 (Text.stripPrefix "rpc-" value >>= readMaybe . Text.unpack)

nonEmptyText :: Text -> Maybe Text
nonEmptyText value =
  let stripped = Text.strip value
  in stripped <$ guard (not (Text.null stripped))
