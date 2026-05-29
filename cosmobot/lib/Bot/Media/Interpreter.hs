{-|
Module      : Bot.Media.Interpreter
Description : Media effect interpreter backed by the local cache and optional S3 publishing
Stability   : experimental
-}

module Bot.Media.Interpreter
  ( runMedia
  )
where

import Bot.Effect.Media
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Media.Cache as Cache
import qualified Bot.Media.Config as MediaConfig
import qualified Bot.Media.Object as MediaObject
import qualified Bot.Media.S3 as S3
import Bot.Prelude
import qualified Data.Text as Text
import Effectful.FileSystem (FileSystem)
import Effectful.Process (Process)
import qualified Network.HTTP.Client as Client

data Runtime = Runtime
  { cfg :: !MediaConfig.Config
  , manager :: !Client.Manager
  , s3 :: !S3.Runtime
  }

runMedia
  :: (IOE :> es, KatipE :> es, FileSystem :> es, Process :> es, Fail :> es, HTTP.HTTP :> es, Storage.Storage :> es)
  => MediaConfig.Config
  -> Eff (Media : es) a
  -> Eff es a
runMedia cfg inner = do
  manager <- HTTP.manager
  s3 <- S3.newRuntime manager cfg
  let runtime = Runtime{cfg, manager, s3}
  interpret
    ( \_ -> \case
        StoreMediaObject mediaObject ->
          Just <$> cacheObject runtime Nothing mediaObject
        StoreMediaObjectFromSource sourceRef mediaObject ->
          Just <$> cacheObject runtime (Just sourceRef) mediaObject
        MediaRefForSource sourceRef ->
          fmap (Cache.mediaIdForFileId . (.fileId)) <$> Cache.loadCachedMediaBySource (cacheConfig runtime) sourceRef
        GetMediaFileInfo fileId ->
          Cache.loadMediaFileInfo (cacheConfig runtime) fileId
        ListMediaFiles ->
          Cache.listMediaFiles (cacheConfig runtime)
        GetMediaCacheStats ->
          Cache.mediaCacheStats (cacheConfig runtime)
        GcMediaCache maxAgeSeconds retainedFileIds ->
          Cache.gcMediaCacheRetaining (cacheConfig runtime) maxAgeSeconds retainedFileIds
        NormalizeMediaRef ref ->
          normalizeRef runtime ref
        PublicMediaRef ref ->
          publicRef runtime ref
        LocalMediaPath ref ->
          localPath runtime ref
        PlatformMediaRef platform scope ref ->
          Cache.loadPlatformRef (cacheConfig runtime) platform scope ref
        StorePlatformMediaRef platform scope ref platformRef ->
          Cache.storePlatformRef platform scope ref platformRef
    )
    inner

normalizeRef :: (IOE :> es, KatipE :> es, FileSystem :> es, Process :> es, Fail :> es, Storage.Storage :> es) => Runtime -> Text -> Eff es Text
normalizeRef runtime ref
  | Cache.isMediaId ref =
      pure ref
  | "data:image/" `Text.isPrefixOf` Text.strip ref =
      case MediaObject.decodeDataMediaObject ref of
        Nothing -> do
          logError "Skipping invalid data:image media reference"
          pure ref
        Just mediaObject ->
          cacheObject runtime Nothing mediaObject
  | "http://" `Text.isPrefixOf` Text.toLower (Text.strip ref) ||
      "https://" `Text.isPrefixOf` Text.toLower (Text.strip ref) = do
      downloaded <- (Just <$> MediaObject.downloadObject runtime.manager ref) `catchSync` \err -> do
        logError [i|Remote media download failed: #{show err :: String}|]
        pure Nothing
      maybe (pure ref) (cacheObject runtime (Just (Text.strip ref))) downloaded
  | "file://" `Text.isPrefixOf` Text.strip ref = do
      mediaObject <- (Just <$> MediaObject.fileObject ref) `catchSync` \err -> do
        logError [i|Local media read failed: #{show err :: String}|]
        pure Nothing
      maybe (pure ref) (cacheObject runtime (Just (Text.strip ref))) mediaObject
  | otherwise =
      pure ref

publicRef :: (IOE :> es, KatipE :> es, FileSystem :> es, Process :> es, Fail :> es, Storage.Storage :> es) => Runtime -> Text -> Eff es Text
publicRef runtime ref =
  case Cache.parseMediaId ref of
    Nothing -> normalizeRef runtime ref >>= publicRef runtime
    Just fileId ->
      Cache.loadCachedMedia (cacheConfig runtime) fileId >>= \case
        Nothing ->
          pure ref
        Just cached ->
          S3.ensurePublicObject runtime.s3 cached

localPath :: (FileSystem :> es, Storage.Storage :> es, IOE :> es) => Runtime -> Text -> Eff es (Maybe FilePath)
localPath runtime ref =
  case Cache.parseMediaId ref of
    Nothing ->
      pure Nothing
    Just fileId ->
      fmap (.path) <$> Cache.loadCachedMedia (cacheConfig runtime) fileId

cacheObject :: (IOE :> es, KatipE :> es, FileSystem :> es, Process :> es, Fail :> es, Storage.Storage :> es) => Runtime -> Maybe Text -> MediaObject -> Eff es Text
cacheObject runtime sourceRef mediaObject = do
  prepared <- prepareMediaObject runtime mediaObject
  cached <- Cache.cacheMediaObject (cacheConfig runtime) sourceRef prepared
  pure (Cache.mediaIdForFileId cached.fileId)

prepareMediaObject :: (IOE :> es, KatipE :> es, FileSystem :> es, Process :> es, Fail :> es) => Runtime -> MediaObject -> Eff es MediaObject
prepareMediaObject _ mediaObject =
  pure mediaObject

cacheConfig :: Runtime -> Cache.CacheConfig
cacheConfig runtime =
  Cache.CacheConfig
    { directory = runtime.cfg.cacheDir
    }
