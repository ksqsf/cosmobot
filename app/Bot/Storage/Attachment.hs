{-|
Module      : Bot.Storage.Attachment
Description : Durable metadata and file ownership for RPC attachments
Stability   : experimental
-}
{-# LANGUAGE OverloadedLabels #-}

module Bot.Storage.Attachment
  ( AttachmentConfig (..)
  , AttachmentUpload (..)
  , StoredAttachment (..)
  , StoredAttachmentRef (..)
  , ensureAttachmentTables
  , storeAttachment
  , loadAttachment
  , deleteUnreferencedAttachment
  , addAttachmentRefs
  , releaseAttachmentRefs
  )
where

import Bot.Prelude
import qualified Bot.Effect.Storage as Storage
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Unique as Unique
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import System.FilePath ((<.>), (</>), takeExtension)

data AttachmentConfig = AttachmentConfig
  { directory :: !FilePath
  , maxBytes :: !Int
  }
  deriving (Eq, Show)

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

storeAttachment
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es)
  => AttachmentConfig
  -> AttachmentUpload
  -> Eff es (Either Text StoredAttachmentRef)
storeAttachment cfg upload = do
  let actualSize = ByteString.length upload.bytes
      limit = cfg.maxBytes
  if actualSize > limit
    then pure (Left [i|attachment is #{actualSize} bytes; limit is #{limit} bytes|])
    else do
      FileSystem.createDirectoryIfMissing True cfg.directory
      attachmentId <- newAttachmentId
      let path = attachmentPath cfg upload.name attachmentId
      FileSystemByteString.writeFile path upload.bytes
      let stored = StoredAttachment
            { attachmentId
            , name = cleanName upload.name
            , mediaType = cleanMediaType upload.mediaType
            , kind = cleanKind upload.kind upload.mediaType
            , size = actualSize
            , path
            , refCount = 0
            }
      ensureAttachmentTables
      runSelda $
        insert_ attachmentRows [attachmentRow stored]
      pure (Right (attachmentRef stored))

loadAttachment :: Storage.Storage :> es => Text -> Eff es (Maybe StoredAttachment)
loadAttachment targetAttachmentId = do
  ensureAttachmentTables
  rows <- runSelda $
    query $
      queryLimit 0 1 do
        row <- select attachmentRows
        restrict (row ! #attachment_id .== literal targetAttachmentId)
        pure row
  pure (attachmentFromRow <$> viaNonEmpty head rows)

deleteUnreferencedAttachment
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es)
  => Text
  -> Eff es Bool
deleteUnreferencedAttachment targetAttachmentId = do
  loadAttachment targetAttachmentId >>= \case
    Nothing ->
      pure False
    Just attachment
      | attachment.refCount > 0 ->
          pure False
      | otherwise -> do
          exists <- FileSystem.doesFileExist attachment.path
          when exists $
            FileSystem.removeFile attachment.path
          runSelda $
            deleteFrom_ attachmentRows \row ->
              row ! #attachment_id .== literal targetAttachmentId .&& row ! #ref_count .== literal 0
          pure True

addAttachmentRefs :: Storage.Storage :> es => [Text] -> Eff es ()
addAttachmentRefs attachmentIds =
  updateRefCounts 1 attachmentIds

releaseAttachmentRefs :: Storage.Storage :> es => [Text] -> Eff es ()
releaseAttachmentRefs attachmentIds =
  updateRefCounts (-1) attachmentIds

updateRefCounts :: Storage.Storage :> es => Int -> [Text] -> Eff es ()
updateRefCounts delta attachmentIds = do
  ensureAttachmentTables
  traverse_ updateOne (ordNub attachmentIds)
  where
    updateOne attachmentId =
      runSelda $
        update_ attachmentRows
          (\row -> row ! #attachment_id .== literal attachmentId)
          (\row -> row `with` [#ref_count := row ! #ref_count + literal delta])

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

newAttachmentId :: IOE :> es => Eff es Text
newAttachmentId = do
  uniq <- liftIO Unique.newUnique
  pure [i|att-#{Unique.hashUnique uniq}|]

attachmentPath :: AttachmentConfig -> Text -> Text -> FilePath
attachmentPath cfg originalName attachmentId =
  cfg.directory </> Text.unpack attachmentId <.> extension
  where
    rawExtension = Text.pack (dropWhile (== '.') (takeExtension (Text.unpack originalName)))
    extension =
      Text.unpack $
        if Text.all validExtensionChar rawExtension && not (Text.null rawExtension)
          then rawExtension
          else "bin"

validExtensionChar :: Char -> Bool
validExtensionChar char =
  (char >= 'a' && char <= 'z')
    || (char >= 'A' && char <= 'Z')
    || (char >= '0' && char <= '9')

cleanName :: Text -> Text
cleanName value =
  case Text.strip value of
    "" -> "attachment"
    stripped -> stripped

cleanMediaType :: Text -> Text
cleanMediaType value =
  case Text.strip value of
    "" -> "application/octet-stream"
    stripped -> stripped

cleanKind :: Text -> Text -> Text
cleanKind kind mediaType =
  case Text.toLower (Text.strip kind) of
    "image" -> "image"
    "audio" -> "audio"
    "file" -> "file"
    _ | "image/" `Text.isPrefixOf` media -> "image"
      | "audio/" `Text.isPrefixOf` media -> "audio"
      | otherwise -> "file"
  where
    media = Text.toLower (Text.strip mediaType)
