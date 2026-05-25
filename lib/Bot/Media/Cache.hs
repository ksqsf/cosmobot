{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.Media.Cache
Description : Local content-addressed media cache
Stability   : experimental
-}


module Bot.Media.Cache
  ( CachedMedia (..)
  , CacheConfig (..)
  , cacheMediaObject
  , loadCachedMedia
  , loadMediaFileInfo
  , loadCachedMediaByRef
  , loadCachedMediaBySource
  , listMediaFiles
  , mediaCacheStats
  , mediaIdForFileId
  , parseMediaId
  , isMediaId
  , gcMediaCache
  , gcMediaCacheRetaining
  , loadPlatformRef
  , storePlatformRef
  , extensionFor
  , contentDigest
  )
where

import Bot.Effect.Media (MediaCacheStats (..), MediaFileInfo (..), MediaObject (..))
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Media.Mime as Mime
import Bot.Prelude
import Bot.Storage.Prelude
import Crypto.Hash (Digest, SHA256, hash)
import qualified Crypto.Random as CryptoRandom
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Time.Clock.POSIX as POSIX
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import System.FilePath ((<.>), (</>), takeExtension)

data CacheConfig = CacheConfig
  { directory :: !FilePath
  }
  deriving (Show, Eq)

data CachedMedia = CachedMedia
  { fileId :: !Text
  , digest :: !Text
  , mimeType :: !Text
  , sourceName :: !(Maybe Text)
  , path :: !FilePath
  , size :: !Int
  }
  deriving (Show, Eq)

data MediaObjectRow = MediaObjectRow
  { file_id :: Text
  , digest :: Text
  , mime_type :: Text
  , source_name :: Maybe Text
  , path :: Text
  , size_bytes :: Int
  , created_at_unix :: Int
  , last_used_at_unix :: Int
  }
  deriving (Generic)

instance SqlRow MediaObjectRow

data MediaSourceRow = MediaSourceRow
  { source_ref :: Text
  , file_id :: Text
  }
  deriving (Generic)

instance SqlRow MediaSourceRow

data MediaPlatformRefRow = MediaPlatformRefRow
  { platform_key :: Text
  , scope_key :: Text
  , file_id :: Text
  , platform_ref :: Text
  , created_at_unix :: Int
  , last_used_at_unix :: Int
  }
  deriving (Generic)

instance SqlRow MediaPlatformRefRow

mediaObjectRows :: Table MediaObjectRow
mediaObjectRows =
  table "media_files"
    [ #file_id :- primary
    , #digest :- index
    ]

mediaSourceRows :: Table MediaSourceRow
mediaSourceRows =
  table "media_sources"
    [ #source_ref :- primary
    ]

mediaPlatformRefRows :: Table MediaPlatformRefRow
mediaPlatformRefRows =
  table "media_platform_refs"
    [ #platform_key :- index
    , #scope_key :- index
    , #file_id :- index
    ]

cacheMediaObject
  :: (Storage.Storage :> es, FileSystem :> es, IOE :> es)
  => CacheConfig
  -> Maybe Text
  -> MediaObject
  -> Eff es CachedMedia
cacheMediaObject cfg sourceRef mediaObject = do
  ensureMediaCacheTables
  case sourceRef of
    Just ref -> do
      cached <- lookupCachedSource cfg ref
      case cached of
        Just media ->
          pure media
        Nothing ->
          lookupCachedDigest cfg (contentDigest mediaObject.bytes) >>= \case
            Just media -> do
              linkSourceRef ref media.fileId
              pure media
            Nothing ->
              storeMediaObject cfg sourceRef mediaObject
    Nothing ->
      lookupCachedDigest cfg (contentDigest mediaObject.bytes) >>= \case
        Just media ->
          pure media
        Nothing ->
          storeMediaObject cfg sourceRef mediaObject

loadCachedMedia :: (Storage.Storage :> es, FileSystem :> es, IOE :> es) => CacheConfig -> Text -> Eff es (Maybe CachedMedia)
loadCachedMedia _ targetFileId = do
  ensureMediaCacheTables
  rows <- runSelda $
    query do
      object <- select mediaObjectRows
      restrict (object ! #file_id .== literal targetFileId)
      pure object
  existing <- filterM (FileSystem.doesFileExist . Text.unpack . (.path)) rows
  for_ (viaNonEmpty head existing) \row ->
    touchMediaFile row.file_id
  pure (mediaObjectRowToCached <$> viaNonEmpty head existing)

loadCachedMediaByRef :: (Storage.Storage :> es, FileSystem :> es, IOE :> es) => CacheConfig -> Text -> Eff es (Maybe CachedMedia)
loadCachedMediaByRef cfg ref =
  case parseMediaId ref of
    Nothing ->
      pure Nothing
    Just fileId ->
      loadCachedMedia cfg fileId

loadCachedMediaBySource :: (Storage.Storage :> es, FileSystem :> es, IOE :> es) => CacheConfig -> Text -> Eff es (Maybe CachedMedia)
loadCachedMediaBySource =
  lookupCachedSource

loadMediaFileInfo :: (Storage.Storage :> es, FileSystem :> es) => CacheConfig -> Text -> Eff es (Maybe MediaFileInfo)
loadMediaFileInfo _ targetFileId = do
  ensureMediaCacheTables
  rows <- runSelda $
    query $
      queryLimit 0 1 do
        object <- select mediaObjectRows
        restrict (object ! #file_id .== literal targetFileId)
        pure object
  traverse mediaObjectRowToInfo (viaNonEmpty head rows)

listMediaFiles :: (Storage.Storage :> es, FileSystem :> es) => CacheConfig -> Eff es [MediaFileInfo]
listMediaFiles _ = do
  ensureMediaCacheTables
  rows <- runSelda $
    query do
      object <- select mediaObjectRows
      order (object ! #created_at_unix) descending
      pure object
  traverse mediaObjectRowToInfo rows

mediaCacheStats :: (Storage.Storage :> es, FileSystem :> es) => CacheConfig -> Eff es MediaCacheStats
mediaCacheStats cfg = do
  files <- listMediaFiles cfg
  sourceCount <- runSelda do
    rows <- query (select mediaSourceRows)
    pure (length rows)
  platformRefCount <- runSelda do
    rows <- query (select mediaPlatformRefRows)
    pure (length rows)
  let existingFiles = length (filter (.exists) files)
      missingFiles = length files - existingFiles
      totalBytes = sum [file.size | file <- files, file.exists]
  pure MediaCacheStats
    { files = length files
    , existingFiles
    , missingFiles
    , totalBytes
    , sources = sourceCount
    , platformRefs = platformRefCount
    }

lookupCachedDigest :: (Storage.Storage :> es, FileSystem :> es, IOE :> es) => CacheConfig -> Text -> Eff es (Maybe CachedMedia)
lookupCachedDigest _ targetDigest = do
  ensureMediaCacheTables
  rows <- runSelda $
    query do
      object <- select mediaObjectRows
      restrict (object ! #digest .== literal targetDigest)
      pure object
  existing <- filterM (FileSystem.doesFileExist . Text.unpack . (.path)) rows
  for_ (viaNonEmpty head existing) \row ->
    touchMediaFile row.file_id
  pure (mediaObjectRowToCached <$> viaNonEmpty head existing)

linkSourceRef :: Storage.Storage :> es => Text -> Text -> Eff es ()
linkSourceRef ref fileId = do
  ensureMediaCacheTables
  runSelda do
    deleteFrom_ mediaSourceRows \candidate ->
      candidate ! #source_ref .== literal ref
    insert_ mediaSourceRows [MediaSourceRow{source_ref = ref, file_id = fileId}]

loadPlatformRef :: (Storage.Storage :> es, FileSystem :> es, IOE :> es) => CacheConfig -> Text -> Text -> Text -> Eff es (Maybe Text)
loadPlatformRef cfg platform scope ref =
  case parseMediaId ref of
    Nothing ->
      pure Nothing
    Just fileId -> do
      cached <- loadCachedMedia cfg fileId
      case cached of
        Nothing ->
          pure Nothing
        Just _ -> do
          ensureMediaCacheTables
          rows <- runSelda $
            query do
              row <- select mediaPlatformRefRows
              restrict (row ! #platform_key .== literal platform)
              restrict (row ! #scope_key .== literal scope)
              restrict (row ! #file_id .== literal fileId)
              pure row
          for_ (viaNonEmpty head rows) \_ ->
            touchPlatformRef platform scope fileId
          pure ((.platform_ref) <$> viaNonEmpty head rows)

storePlatformRef :: (Storage.Storage :> es, IOE :> es) => Text -> Text -> Text -> Text -> Eff es ()
storePlatformRef platform scope ref platformRef =
  case parseMediaId ref of
    Nothing ->
      pure ()
    Just fileId -> do
      ensureMediaCacheTables
      now <- liftIO (round <$> POSIX.getPOSIXTime)
      runSelda do
        deleteFrom_ mediaPlatformRefRows \row ->
          row ! #platform_key .== literal platform
            .&& row ! #scope_key .== literal scope
            .&& row ! #file_id .== literal fileId
        insert_ mediaPlatformRefRows
          [ MediaPlatformRefRow
              { platform_key = platform
              , scope_key = scope
              , file_id = fileId
              , platform_ref = platformRef
              , created_at_unix = now
              , last_used_at_unix = now
              }
          ]

lookupCachedSource :: (Storage.Storage :> es, FileSystem :> es, IOE :> es) => CacheConfig -> Text -> Eff es (Maybe CachedMedia)
lookupCachedSource _ ref = do
  rows <- runSelda $
    query do
      source <- select mediaSourceRows
      object <- select mediaObjectRows
      restrict (source ! #source_ref .== literal ref)
      restrict (source ! #file_id .== object ! #file_id)
      pure object
  existing <- filterM (FileSystem.doesFileExist . Text.unpack . (.path)) rows
  for_ (viaNonEmpty head existing) \row ->
    touchMediaFile row.file_id
  pure (mediaObjectRowToCached <$> viaNonEmpty head existing)

storeMediaObject
  :: (Storage.Storage :> es, FileSystem :> es, IOE :> es)
  => CacheConfig
  -> Maybe Text
  -> MediaObject
  -> Eff es CachedMedia
storeMediaObject cfg sourceRef mediaObject = do
  let digest = contentDigest mediaObject.bytes
      relativePath = Text.unpack digest <.> Text.unpack (Text.dropWhile (== '.') (extensionFor mediaObject))
      finalPath = cfg.directory </> relativePath
  FileSystem.createDirectoryIfMissing True cfg.directory
  exists <- FileSystem.doesFileExist finalPath
  unless exists $
    FileSystemByteString.writeFile finalPath mediaObject.bytes
  fileId <- newFileId
  now <- liftIO (round <$> POSIX.getPOSIXTime)
  let row = MediaObjectRow
        { file_id = fileId
        , digest
        , mime_type = mediaObject.mimeType
        , source_name = mediaObject.sourceName
        , path = Text.pack finalPath
        , size_bytes = StrictByteString.length mediaObject.bytes
        , created_at_unix = now
        , last_used_at_unix = now
        }
  ensureMediaCacheTables
  runSelda do
    insert_ mediaObjectRows [row]
    for_ sourceRef \ref -> do
      deleteFrom_ mediaSourceRows \candidate ->
        candidate ! #source_ref .== literal ref
      insert_ mediaSourceRows [MediaSourceRow{source_ref = ref, file_id = fileId}]
  pure (mediaObjectRowToCached row)

gcMediaCache
  :: (Storage.Storage :> es, FileSystem :> es, IOE :> es)
  => CacheConfig
  -> Int
  -> Eff es Int
gcMediaCache cfg maxAgeSeconds =
  gcMediaCacheRetaining cfg maxAgeSeconds Set.empty

gcMediaCacheRetaining
  :: (Storage.Storage :> es, FileSystem :> es, IOE :> es)
  => CacheConfig
  -> Int
  -> Set.Set Text
  -> Eff es Int
gcMediaCacheRetaining _ maxAgeSeconds retainedFileIds = do
  ensureMediaCacheTables
  now <- currentUnixSeconds
  let cutoff = now - max 0 maxAgeSeconds
  objects <- runSelda $ query (select mediaObjectRows)
  let expired =
        [ object
        | object <- objects
        , object.last_used_at_unix < cutoff
        , not (Set.member object.file_id retainedFileIds)
        ]
      expiredIds = Set.fromList (map (.file_id) expired)
      retainedPaths =
        Set.fromList
          [ object.path
          | object <- objects
          , not (Set.member object.file_id expiredIds)
          ]
      removable =
        [ object
        | object <- expired
        , not (Set.member object.path retainedPaths)
        ]
      expiredFileIds = map (.file_id) expired
  traverse_ removeCachedFile removable
  runSelda $ transaction do
    for_ expiredFileIds \fileId -> do
      deleteFrom_ mediaPlatformRefRows \row ->
        row ! #file_id .== literal fileId
      deleteFrom_ mediaSourceRows \row ->
        row ! #file_id .== literal fileId
      deleteFrom_ mediaObjectRows \row ->
        row ! #file_id .== literal fileId
  pure (length expired)

removeCachedFile :: FileSystem :> es => MediaObjectRow -> Eff es ()
removeCachedFile row = do
  let filePath = Text.unpack row.path
  exists <- FileSystem.doesFileExist filePath
  when exists (FileSystem.removeFile filePath)
    `catchSync` \_ ->
      pure ()

ensureMediaCacheTables :: Storage.Storage :> es => Eff es ()
ensureMediaCacheTables =
  runSelda do
    tryCreateTable mediaObjectRows
    tryCreateTable mediaSourceRows
    tryCreateTable mediaPlatformRefRows

touchMediaFile :: (Storage.Storage :> es, IOE :> es) => Text -> Eff es ()
touchMediaFile fileId = do
  now <- currentUnixSeconds
  runSelda $
    update_
      mediaObjectRows
      (\row -> row ! #file_id .== literal fileId)
      (\row -> row `with` [#last_used_at_unix := literal now])

touchPlatformRef :: (Storage.Storage :> es, IOE :> es) => Text -> Text -> Text -> Eff es ()
touchPlatformRef platform scope fileId = do
  now <- currentUnixSeconds
  runSelda $
    update_
      mediaPlatformRefRows
      ( \row ->
          row ! #platform_key .== literal platform
            .&& row ! #scope_key .== literal scope
            .&& row ! #file_id .== literal fileId
      )
      (\row -> row `with` [#last_used_at_unix := literal now])

currentUnixSeconds :: IOE :> es => Eff es Int
currentUnixSeconds =
  liftIO (round <$> POSIX.getPOSIXTime)

mediaObjectRowToCached :: MediaObjectRow -> CachedMedia
mediaObjectRowToCached row =
  CachedMedia
    { fileId = row.file_id
    , digest = row.digest
    , mimeType = row.mime_type
    , sourceName = row.source_name
    , path = Text.unpack row.path
    , size = row.size_bytes
    }

mediaObjectRowToInfo :: FileSystem :> es => MediaObjectRow -> Eff es MediaFileInfo
mediaObjectRowToInfo row = do
  exists <- FileSystem.doesFileExist (Text.unpack row.path)
  pure MediaFileInfo
    { fileId = row.file_id
    , ref = mediaIdForFileId row.file_id
    , digest = row.digest
    , mimeType = row.mime_type
    , sourceName = row.source_name
    , path = Text.unpack row.path
    , size = row.size_bytes
    , createdAtUnix = row.created_at_unix
    , lastUsedAtUnix = row.last_used_at_unix
    , exists
    }

contentDigest :: StrictByteString.ByteString -> Text
contentDigest bytes =
  Text.pack (show (hash bytes :: Digest SHA256))

mediaIdForFileId :: Text -> Text
mediaIdForFileId fileId =
  "media:" <> fileId

parseMediaId :: Text -> Maybe Text
parseMediaId ref = do
  fileId <- Text.stripPrefix "media:" (Text.strip ref)
  guard (isValidFileId fileId)
  pure fileId
  where
    isValidFileId fileId =
      "mf_" `Text.isPrefixOf` fileId &&
        Text.length fileId >= 10 &&
        Text.all isFileIdChar fileId

    isFileIdChar char =
      (char >= 'a' && char <= 'z') ||
        (char >= 'A' && char <= 'Z') ||
        (char >= '0' && char <= '9') ||
        char == '-' ||
        char == '_'

isMediaId :: Text -> Bool
isMediaId =
  isJust . parseMediaId

newFileId :: IOE :> es => Eff es Text
newFileId = do
  bytes <- liftIO (CryptoRandom.getRandomBytes 16 :: IO StrictByteString.ByteString)
  pure ("mf_" <> TextEncoding.decodeUtf8 (Base64URL.encodeUnpadded bytes))

extensionFor :: MediaObject -> Text
extensionFor mediaObject =
  case mediaObject.sourceName >>= extensionFromName of
    Just ext -> ext
    Nothing -> Mime.extensionFromMime mediaObject.mimeType

extensionFromName :: Text -> Maybe Text
extensionFromName name =
  let ext = Text.pack (takeExtension (Text.unpack name))
  in if Text.null ext then Nothing else Just ext
