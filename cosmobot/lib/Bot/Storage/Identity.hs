{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.Storage.Identity
Description : Latest platform chat and sender display information
Stability   : experimental
-}

module Bot.Storage.Identity
  ( SenderInfo (..)
  , ensureIdentityTables
  , rememberIncomingIdentityRows
  , loadSenderInfos
  , loadChatInfos
  , loadScopedChatInfos
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Storage.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Database.Selda.SQLite as SeldaSQLite

data SenderInfo = SenderInfo
  { displayName :: !(Maybe Text)
  , username :: !(Maybe Text)
  }
  deriving (Eq, Show)

data SenderInfoRow = SenderInfoRow
  { identity_key :: !Text
  , platform_key :: !Text
  , sender_id :: !Text
  , display_name :: !(Maybe Text)
  , username :: !(Maybe Text)
  , updated_at :: !UTCTime
  }
  deriving (Generic)

instance SqlRow SenderInfoRow

data ChatInfoRow = ChatInfoRow
  { identity_key :: !Text
  , platform_key :: !Text
  , kind_key :: !Text
  , chat_id :: !Int.Int64
  , display_name :: !(Maybe Text)
  , updated_at :: !UTCTime
  }
  deriving (Generic)

instance SqlRow ChatInfoRow

senderInfoRows :: Table SenderInfoRow
senderInfoRows = table "sender_info"
  [ #identity_key :- primary
  , #platform_key :- index
  , #sender_id :- index
  ]

chatInfoRows :: Table ChatInfoRow
chatInfoRows = table "chat_info"
  [ #identity_key :- primary
  , #platform_key :- index
  , #chat_id :- index
  ]

ensureIdentityTables :: Storage.Storage :> es => Eff es ()
ensureIdentityTables = runSelda $ transaction do
  tryCreateTable senderInfoRows
  tryCreateTable chatInfoRows

rememberIncomingIdentityRows :: UTCTime -> IncomingMessage -> SeldaT SeldaSQLite.SQLite IO ()
rememberIncomingIdentityRows updatedAt message = do
  for_ message.senderId \senderId -> do
    let key = senderKey message.platform senderId
    existing <- viaNonEmpty head <$> query do
      row <- select senderInfoRows
      restrict (row ! #identity_key .== literal key)
      pure row
    deleteFrom_ senderInfoRows (\row -> row ! #identity_key .== literal key)
    insert_ senderInfoRows
      [ SenderInfoRow
          { identity_key = key
          , platform_key = chatPlatformKey message.platform
          , sender_id = senderId
          , display_name = nonEmptyText message.senderGlobalDisplayName <|> (existing >>= (.display_name))
          , username = nonEmptyText message.senderUsername <|> (existing >>= (.username))
          , updated_at = updatedAt
          }
      ]
  for_ message.chatId \chatId -> do
    let key = scopedChatKey message.platform message.kind chatId
    existing <- viaNonEmpty head <$> query do
      row <- select chatInfoRows
      restrict (row ! #identity_key .== literal key)
      pure row
    deleteFrom_ chatInfoRows (\row -> row ! #identity_key .== literal key)
    insert_ chatInfoRows
      [ ChatInfoRow
          { identity_key = key
          , platform_key = chatPlatformKey message.platform
          , kind_key = chatKindKey message.kind
          , chat_id = fromIntegral chatId
          , display_name = nonEmptyText message.chatDisplayName <|> (existing >>= (.display_name))
          , updated_at = updatedAt
          }
      ]

loadSenderInfos :: Storage.Storage :> es => [(ChatPlatform, Text)] -> Eff es (Map (ChatPlatform, Text) SenderInfo)
loadSenderInfos requested = do
  ensureIdentityTables
  let keys = ordNub (map (uncurry senderKey) requested)
  rows <- runSelda $ concat <$> traverse (\batch -> query do
    row <- select senderInfoRows
    restrict (foldr (.||) (literal False) [row ! #identity_key .== literal key | key <- batch])
    pure row) (identityBatches keys)
  pure $ Map.fromList
    [ ((platform, row.sender_id), SenderInfo row.display_name row.username)
    | row <- rows
    , Just platform <- [chatPlatformFromKey row.platform_key]
    ]

loadChatInfos :: Storage.Storage :> es => [(ChatPlatform, Integer)] -> Eff es (Map (ChatPlatform, Integer) (Maybe Text))
loadChatInfos requested = do
  ensureIdentityTables
  let keys = ordNub requested
  rows <- runSelda $ concat <$> traverse (\batch -> query do
    row <- select chatInfoRows
    restrict (foldr (.||) (literal False)
      [ row ! #platform_key .== literal (chatPlatformKey platform)
          .&& row ! #chat_id .== literal (fromIntegral chatId)
      | (platform, chatId) <- batch
      ])
    order (row ! #updated_at) ascending
    pure row) (identityBatches keys)
  pure $ Map.fromList
    [ ((platform, fromIntegral row.chat_id), row.display_name)
    | row <- rows
    , Just platform <- [chatPlatformFromKey row.platform_key]
    ]

loadScopedChatInfos
  :: Storage.Storage :> es
  => [(ChatPlatform, ChatKind, Integer)]
  -> Eff es (Map (ChatPlatform, ChatKind, Integer) (Maybe Text))
loadScopedChatInfos requested = do
  ensureIdentityTables
  let keys = ordNub requested
      identityKeys = map (\(platform, kind, chatId) -> scopedChatKey platform kind chatId) keys
  rows <- runSelda $ concat <$> traverse (\batch -> query do
    row <- select chatInfoRows
    restrict (foldr (.||) (literal False) [row ! #identity_key .== literal key | key <- batch])
    pure row) (identityBatches identityKeys)
  pure $ Map.fromList
    [ (key, row.display_name)
    | key@(platform, kind, chatId) <- keys
    , row <- rows
    , row.identity_key == scopedChatKey platform kind chatId
    ]

senderKey :: ChatPlatform -> Text -> Text
senderKey platform senderId = chatPlatformKey platform <> ":" <> senderId

scopedChatKey :: ChatPlatform -> ChatKind -> Integer -> Text
scopedChatKey platform kind chatId =
  Text.intercalate ":" [chatPlatformKey platform, chatKindKey kind, Text.pack (show chatId)]

chatKindKey :: ChatKind -> Text
chatKindKey = \case
  ChatPrivate -> "private"
  ChatGroup -> "group"
  ChatChannel -> "channel"
  ChatUnknown value -> "unknown:" <> value

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText value = do
  text <- Text.strip <$> value
  text <$ guard (not (Text.null text))

identityBatches :: [a] -> [[a]]
identityBatches [] = []
identityBatches values = batch : identityBatches rest
  where
    (batch, rest) = splitAt 20 values
