{-# LANGUAGE OverloadedLabels #-}

{-|
Module      : Bot.Storage.Resource
Description : Durable resource registrations
Stability   : experimental
-}
module Bot.Storage.Resource
  ( StoredResource (..)
  , loadResources
  , loadResourceLifetimes
  , saveResource
  , deleteResource
  , renameResource
  , setResourceLifetime
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Resource.Types
import Bot.Storage.Prelude

data StoredResource = StoredResource
  { resourceId :: !ResourceId
  , resourceType :: !Text
  , owner :: !ResourceOwner
  , sessionId :: !(Maybe Text)
  , payload :: !Text
  }
  deriving stock (Eq, Show)

data ResourceRow = ResourceRow
  { resource_id :: Text
  , resource_type :: Text
  , owner_platform :: Text
  , owner_chat_id :: Text
  , owner_sender_id :: Text
  , session_id :: Maybe Text
  , payload :: Text
  }
  deriving stock (Generic)

instance SqlRow ResourceRow

data ResourceLifetimeRow = ResourceLifetimeRow
  { resource_id :: Text
  , ttl_seconds :: Int
  , expires_at :: UTCTime
  }
  deriving stock (Generic)

instance SqlRow ResourceLifetimeRow

resources :: Table ResourceRow
resources =
  table "resources"
    [ #resource_id :- primary
    , #resource_type :- index
    , #owner_platform :- index
    , #owner_chat_id :- index
    , #owner_sender_id :- index
    ]

resourceLifetimes :: Table ResourceLifetimeRow
resourceLifetimes =
  table "resource_lifetimes"
    [ #resource_id :- primary
    ]

loadResources :: Storage.Storage :> es => Eff es [StoredResource]
loadResources = do
  ensureTable
  rows <- runSelda $ query (select resources)
  pure (mapMaybe fromRow rows)

loadResourceLifetimes :: Storage.Storage :> es => Eff es [(ResourceId, (Int, UTCTime))]
loadResourceLifetimes = do
  ensureTable
  rows <- runSelda $ query (select resourceLifetimes)
  pure [(row.resource_id, (row.ttl_seconds, row.expires_at)) | row <- rows]

saveResource :: Storage.Storage :> es => StoredResource -> Eff es ()
saveResource resource = do
  ensureTable
  runSelda do
    deleteFrom_ resources \row -> row ! #resource_id .== literal resource.resourceId
    insert_ resources [toRow resource]

deleteResource :: Storage.Storage :> es => ResourceId -> Eff es ()
deleteResource resourceId = do
  ensureTable
  runSelda do
    deleteFrom_ resourceLifetimes \row -> row ! #resource_id .== literal resourceId
    deleteFrom_ resources \row -> row ! #resource_id .== literal resourceId

renameResource :: Storage.Storage :> es => ResourceId -> ResourceId -> Eff es ()
renameResource oldId newId = do
  ensureTable
  runSelda do
    update_ resources
      (\row -> row ! #resource_id .== literal oldId)
      (\row -> row `with` [#resource_id := literal newId])
    update_ resourceLifetimes
      (\row -> row ! #resource_id .== literal oldId)
      (\row -> row `with` [#resource_id := literal newId])

setResourceLifetime :: Storage.Storage :> es => ResourceId -> Maybe (Int, UTCTime) -> Eff es ()
setResourceLifetime resourceId lifetime = do
  ensureTable
  runSelda do
    deleteFrom_ resourceLifetimes \row -> row ! #resource_id .== literal resourceId
    for_ lifetime \(ttlSeconds, expiresAt) ->
      insert_ resourceLifetimes [ResourceLifetimeRow resourceId ttlSeconds expiresAt]

ensureTable :: Storage.Storage :> es => Eff es ()
ensureTable = runSelda do
  tryCreateTable resources
  tryCreateTable resourceLifetimes

toRow :: StoredResource -> ResourceRow
toRow resource = ResourceRow
  { resource_id = resource.resourceId
  , resource_type = resource.resourceType
  , owner_platform = chatPlatformKey resource.owner.platform
  , owner_chat_id = resource.owner.chatId
  , owner_sender_id = resource.owner.senderId
  , session_id = resource.sessionId
  , payload = resource.payload
  }

fromRow :: ResourceRow -> Maybe StoredResource
fromRow row = do
  platform <- platformFromKey row.owner_platform
  pure StoredResource
    { resourceId = row.resource_id
    , resourceType = row.resource_type
    , owner = ResourceOwner
        { platform
        , chatId = row.owner_chat_id
        , senderId = row.owner_sender_id
        }
    , sessionId = row.session_id
    , payload = row.payload
    }

platformFromKey :: Text -> Maybe ChatPlatform
platformFromKey = \case
  "qq" -> Just PlatformQQ
  "telegram" -> Just PlatformTelegram
  "matrix" -> Just PlatformMatrix
  "discord" -> Just PlatformDiscord
  "rpc" -> Just PlatformRPC
  "acp" -> Just PlatformACP
  _ -> Nothing
