{-|
Module      : Bot.Media.Cache
Description : Local content-addressed media cache
Stability   : experimental
-}

{-# LANGUAGE OverloadedLabels #-}

module Bot.Media.Cache
  ( CachedMedia (..)
  , CacheConfig (..)
  , cacheMediaObject
  , loadCachedMedia
  , mediaIdForFileId
  , parseMediaId
  , isMediaId
  , loadPlatformRef
  , storePlatformRef
  , extensionFor
  , mimeFromName
  , contentDigest
  )
where

import Bot.Effect.Media (MediaObject (..))
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Storage.Prelude
import Crypto.Hash (Digest, SHA256, hash)
import qualified Crypto.Random as CryptoRandom
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Base64.URL as Base64URL
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

loadCachedMedia :: (Storage.Storage :> es, FileSystem :> es) => CacheConfig -> Text -> Eff es (Maybe CachedMedia)
loadCachedMedia _ targetFileId = do
  ensureMediaCacheTables
  rows <- runSelda $
    query do
      object <- select mediaObjectRows
      restrict (object ! #file_id .== literal targetFileId)
      pure object
  existing <- filterM (FileSystem.doesFileExist . Text.unpack . (.path)) rows
  pure (mediaObjectRowToCached <$> viaNonEmpty head existing)

lookupCachedDigest :: (Storage.Storage :> es, FileSystem :> es) => CacheConfig -> Text -> Eff es (Maybe CachedMedia)
lookupCachedDigest _ targetDigest = do
  ensureMediaCacheTables
  rows <- runSelda $
    query do
      object <- select mediaObjectRows
      restrict (object ! #digest .== literal targetDigest)
      pure object
  existing <- filterM (FileSystem.doesFileExist . Text.unpack . (.path)) rows
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

lookupCachedSource :: (Storage.Storage :> es, FileSystem :> es) => CacheConfig -> Text -> Eff es (Maybe CachedMedia)
lookupCachedSource _ ref = do
  rows <- runSelda $
    query do
      source <- select mediaSourceRows
      object <- select mediaObjectRows
      restrict (source ! #source_ref .== literal ref)
      restrict (source ! #file_id .== object ! #file_id)
      pure object
  existing <- filterM (FileSystem.doesFileExist . Text.unpack . (.path)) rows
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
        }
  ensureMediaCacheTables
  runSelda do
    insert_ mediaObjectRows [row]
    for_ sourceRef \ref -> do
      deleteFrom_ mediaSourceRows \candidate ->
        candidate ! #source_ref .== literal ref
      insert_ mediaSourceRows [MediaSourceRow{source_ref = ref, file_id = fileId}]
  pure (mediaObjectRowToCached row)

ensureMediaCacheTables :: Storage.Storage :> es => Eff es ()
ensureMediaCacheTables =
  runSelda do
    tryCreateTable mediaObjectRows
    tryCreateTable mediaSourceRows
    tryCreateTable mediaPlatformRefRows

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
    Nothing -> extensionFromMime mediaObject.mimeType

extensionFromName :: Text -> Maybe Text
extensionFromName name =
  let ext = Text.pack (takeExtension (Text.unpack name))
  in if Text.null ext then Nothing else Just ext

extensionFromMime :: Text -> Text
extensionFromMime mime =
  case Text.toLower (Text.takeWhile (/= ';') mime) of
    "image/jpeg" -> ".jpg"
    "image/png" -> ".png"
    "image/webp" -> ".webp"
    "image/gif" -> ".gif"
    "audio/mpeg" -> ".mp3"
    "audio/wav" -> ".wav"
    "audio/ogg" -> ".ogg"
    _ -> ".bin"

mimeFromName :: Text -> Text
mimeFromName name =
  case Text.toLower (Text.pack (takeExtension (Text.unpack name))) of
    ".jpg" -> "image/jpeg"
    ".jpeg" -> "image/jpeg"
    ".png" -> "image/png"
    ".webp" -> "image/webp"
    ".gif" -> "image/gif"
    ".mp3" -> "audio/mpeg"
    ".wav" -> "audio/wav"
    ".ogg" -> "audio/ogg"
    _ -> "application/octet-stream"
