{-|
Module      : Bot.Storage.Attachment.Internal
Description : Selda rows and transaction helpers for RPC attachment storage
Stability   : experimental
-}
{-# LANGUAGE OverloadedLabels #-}

module Bot.Storage.Attachment.Internal
  ( AttachmentUpload (..)
  , StoredAttachment (..)
  , StoredAttachmentRef (..)
  , ensureAttachmentTables
  , insertAttachment
  , loadAttachmentById
  , claimUnreferencedAttachment
  , attachmentRef
  , resolveAndClaimAttachmentRefs
  , releaseAttachmentUsesAndClaimOrphans
  )
where

import Bot.Prelude
import qualified Bot.Effect.Storage as Storage
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Database.Selda.SQLite as SeldaSQLite

data AttachmentUpload = AttachmentUpload
  { name :: !Text
  , mediaType :: !Text
  , kind :: !Text
  , bytes :: !ByteString.ByteString
  }
  deriving (Eq, Show)

data StoredAttachment = StoredAttachment
  { attachmentId :: !Text
  , name :: !Text
  , mediaType :: !Text
  , kind :: !Text
  , size :: !Int
  , path :: !FilePath
  , refCount :: !Int
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data StoredAttachmentRef = StoredAttachmentRef
  { attachmentId :: !Text
  , name :: !Text
  , mediaType :: !Text
  , kind :: !Text
  , size :: !Int
  , url :: !Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data AttachmentRow = AttachmentRow
  { id :: ID AttachmentRow
  , attachment_id :: Text
  , name :: Text
  , media_type :: Text
  , kind :: Text
  , size_bytes :: Int
  , path :: Text
  , ref_count :: Int
  }
  deriving (Generic)

instance SqlRow AttachmentRow

attachmentRows :: Table AttachmentRow
attachmentRows =
  table "attachments"
    [ #id :- autoPrimary
    , #attachment_id :- unique
    ]

ensureAttachmentTables :: Storage.Storage :> es => Eff es ()
ensureAttachmentTables =
  runSelda $
    tryCreateTable attachmentRows

insertAttachment :: StoredAttachment -> SeldaT SeldaSQLite.SQLite IO ()
insertAttachment attachment =
  insert_ attachmentRows [attachmentRow attachment]

loadAttachmentById :: Text -> SeldaT SeldaSQLite.SQLite IO (Maybe StoredAttachment)
loadAttachmentById targetAttachmentId = do
  rows <-
    query $
      queryLimit 0 1 do
        row <- select attachmentRows
        restrict (row ! #attachment_id .== literal targetAttachmentId)
        pure row
  pure (attachmentFromRow <$> viaNonEmpty head rows)

claimUnreferencedAttachment :: Text -> SeldaT SeldaSQLite.SQLite IO (Maybe StoredAttachment)
claimUnreferencedAttachment targetAttachmentId = do
  rows <-
    query $
      queryLimit 0 1 do
        row <- select attachmentRows
        restrict (row ! #attachment_id .== literal targetAttachmentId .&& row ! #ref_count .== literal 0)
        pure row
  case viaNonEmpty head rows of
    Nothing ->
      pure Nothing
    Just row -> do
      deleteFrom_ attachmentRows \candidate ->
        candidate ! #attachment_id .== literal targetAttachmentId .&& candidate ! #ref_count .== literal 0
      pure (Just (attachmentFromRow row))

attachmentRef :: StoredAttachment -> StoredAttachmentRef
attachmentRef attachment =
  StoredAttachmentRef
    { attachmentId = attachment.attachmentId
    , name = attachment.name
    , mediaType = attachment.mediaType
    , kind = attachment.kind
    , size = attachment.size
    , url = "/attachments/" <> attachment.attachmentId
    }

resolveAndClaimAttachmentRefs :: [Text] -> SeldaT SeldaSQLite.SQLite IO (Either Text [StoredAttachmentRef])
resolveAndClaimAttachmentRefs attachmentIds = do
  rows <- query do
    row <- select attachmentRows
    restrict (row ! #attachment_id `isIn` map literal canonicalIds)
    pure row
  case attachmentRefsFromRows canonicalIds rows of
    Left err ->
      pure (Left err)
    Right refs -> do
      traverse_ claimAttachmentUse (attachmentUseCounts canonicalIds)
      pure (Right refs)
  where
    canonicalIds = ordNub attachmentIds

releaseAttachmentUsesAndClaimOrphans :: [Text] -> SeldaT SeldaSQLite.SQLite IO [StoredAttachment]
releaseAttachmentUsesAndClaimOrphans attachmentIds = do
  let canonicalIds = ordNub attachmentIds
  traverse_ releaseAttachmentUse (attachmentUseCounts attachmentIds)
  orphans <- query do
    row <- select attachmentRows
    restrict (row ! #attachment_id `isIn` map literal canonicalIds .&& row ! #ref_count .== literal 0)
    pure row
  unless (null orphans) $
    deleteFrom_ attachmentRows \row ->
      row ! #attachment_id `isIn` map (literal . (.attachment_id)) orphans
  pure (map attachmentFromRow orphans)

attachmentRow :: StoredAttachment -> AttachmentRow
attachmentRow attachment =
  AttachmentRow
    { id = def
    , attachment_id = attachment.attachmentId
    , name = attachment.name
    , media_type = attachment.mediaType
    , kind = attachment.kind
    , size_bytes = attachment.size
    , path = Text.pack attachment.path
    , ref_count = attachment.refCount
    }

attachmentFromRow :: AttachmentRow -> StoredAttachment
attachmentFromRow row =
  StoredAttachment
    { attachmentId = row.attachment_id
    , name = row.name
    , mediaType = row.media_type
    , kind = row.kind
    , size = row.size_bytes
    , path = Text.unpack row.path
    , refCount = row.ref_count
    }

attachmentRefFromRow :: AttachmentRow -> StoredAttachmentRef
attachmentRefFromRow =
  attachmentRef . attachmentFromRow

attachmentRefsFromRows :: [Text] -> [AttachmentRow] -> Either Text [StoredAttachmentRef]
attachmentRefsFromRows requested rows =
  traverse resolve requested
  where
    byId =
      Map.fromList [(row.attachment_id, row) | row <- rows]

    resolve attachmentId =
      maybe
        (Left [i|Unknown attachment: #{attachmentId}|])
        (Right . attachmentRefFromRow)
        (Map.lookup attachmentId byId)

claimAttachmentUse :: (Text, Int) -> SeldaT SeldaSQLite.SQLite IO ()
claimAttachmentUse (attachmentId, useCount) =
  update_ attachmentRows
    (\row -> row ! #attachment_id .== literal attachmentId)
    (\row -> row `with` [#ref_count := row ! #ref_count + literal useCount])

releaseAttachmentUse :: (Text, Int) -> SeldaT SeldaSQLite.SQLite IO ()
releaseAttachmentUse (attachmentId, useCount) =
  update_ attachmentRows
    (\row -> row ! #attachment_id .== literal attachmentId .&& row ! #ref_count .>= literal useCount)
    (\row -> row `with` [#ref_count := row ! #ref_count - literal useCount])

attachmentUseCounts :: [Text] -> [(Text, Int)]
attachmentUseCounts attachmentIds =
  Map.toList $
    Map.fromListWith (+) [(attachmentId, 1 :: Int) | attachmentId <- attachmentIds]
