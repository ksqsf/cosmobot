{-|
Module      : Bot.Media.S3
Description : S3-backed media normalization interpreter
Stability   : experimental
-}

{-# LANGUAGE OverloadedLabels #-}

module Bot.Media.S3
  ( runMediaS3
  )
where

import qualified Amazonka as AWS
import qualified Amazonka.Auth as AWSAuth
import qualified Amazonka.S3 as S3
import qualified Amazonka.S3.Lens as S3Lens
import Bot.Effect.Media
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Media.Cache as Cache
import qualified Bot.Media.Config as MediaConfig
import Bot.Media.S3.Config
import Bot.Prelude
import qualified Bot.Util.HTTP as Http
import qualified Bot.Util.Image as Image
import Control.Lens ((.~), (?~))
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTPHeader
import System.FilePath (takeFileName)
import System.IO.Error (ioError, userError)

data Runtime = Runtime
  { cfg :: !MediaConfig.Config
  , manager :: !HTTP.Manager
  , env :: !(Maybe AWS.Env)
  }

runMediaS3
  :: (IOE :> es, KatipE :> es, FileSystem :> es, Storage.Storage :> es)
  => MediaConfig.Config
  -> Eff (Media : es) a
  -> Eff es a
runMediaS3 cfg inner = do
  manager <- liftIO Http.newTlsManager
  env <- if cfg.s3.enabled then Just <$> createAmazonkaEnv manager cfg.s3 else pure Nothing
  let runtime = Runtime{cfg, manager, env}
  interpret
    ( \_ -> \case
        StoreMediaObject mediaObject ->
          Just <$> cacheObject runtime Nothing mediaObject
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

createAmazonkaEnv :: IOE :> es => HTTP.Manager -> Config -> Eff es AWS.Env
createAmazonkaEnv manager cfg = do
  baseEnv <- liftIO (AWS.newEnvFromManager manager (pure . credentials cfg))
  endpoint <- liftIO (traverse parseEndpoint cfg.endpoint)
  pure $ configureEndpoint cfg endpoint baseEnv{AWS.region = AWS.Region' cfg.region}
  where
    credentials Config{accessKeyId = Just accessKeyId, secretAccessKey = Just secretAccessKey} =
      AWSAuth.fromKeys (AWS.AccessKey (TextEncoding.encodeUtf8 accessKeyId)) (AWS.SecretKey (TextEncoding.encodeUtf8 secretAccessKey))
    credentials _ =
      AWSAuth.fromKeys "" ""

parseEndpoint :: Text -> IO (Bool, StrictByteString.ByteString, Int)
parseEndpoint endpoint = do
  request <- HTTP.parseRequest (Text.unpack endpoint)
  pure (HTTP.secure request, HTTP.host request, HTTP.port request)

configureEndpoint :: Config -> Maybe (Bool, StrictByteString.ByteString, Int) -> AWS.Env -> AWS.Env
configureEndpoint cfg endpoint env =
  let service = configureAddressing cfg (configureEndpointService endpoint S3.defaultService)
  in AWS.configureService service env

configureEndpointService :: Maybe (Bool, StrictByteString.ByteString, Int) -> AWS.Service -> AWS.Service
configureEndpointService Nothing service =
  service
configureEndpointService (Just (secure, host, port)) service =
  AWS.setEndpoint secure host port service

configureAddressing :: Config -> AWS.Service -> AWS.Service
configureAddressing cfg service =
  case Text.toLower (Text.strip cfg.addressingStyle) of
    "path" ->
      service & AWS.service_s3AddressingStyle .~ AWS.S3AddressingStylePath
    "virtual" ->
      service & AWS.service_s3AddressingStyle .~ AWS.S3AddressingStyleVirtual
    _ ->
      service & AWS.service_s3AddressingStyle .~ AWS.S3AddressingStyleAuto

normalizeRef :: (IOE :> es, KatipE :> es, FileSystem :> es, Storage.Storage :> es) => Runtime -> Text -> Eff es Text
normalizeRef runtime ref
  | Cache.isMediaId ref =
      pure ref
  | "data:image/" `Text.isPrefixOf` Text.strip ref =
      case decodeDataMediaObject ref of
        Nothing -> do
          logInfo "Skipping invalid data:image media reference"
          pure ref
        Just mediaObject ->
          cacheObject runtime Nothing mediaObject
  | "http://" `Text.isPrefixOf` Text.toLower (Text.strip ref) ||
      "https://" `Text.isPrefixOf` Text.toLower (Text.strip ref) = do
      downloaded <- (Just <$> downloadObject runtime.manager ref) `catchSync` \err -> do
        logInfo [i|Remote media download skipped: #{show err :: String}|]
        pure Nothing
      maybe (pure ref) (cacheObject runtime (Just (Text.strip ref))) downloaded
  | "file://" `Text.isPrefixOf` Text.strip ref = do
      mediaObject <- (Just <$> fileObject ref) `catchSync` \err -> do
        logInfo [i|Local media read skipped: #{show err :: String}|]
        pure Nothing
      maybe (pure ref) (cacheObject runtime (Just (Text.strip ref))) mediaObject
  | otherwise =
      pure ref

publicRef :: (IOE :> es, KatipE :> es, FileSystem :> es, Storage.Storage :> es) => Runtime -> Text -> Eff es Text
publicRef runtime ref =
  case Cache.parseMediaId ref of
    Nothing -> normalizeRef runtime ref >>= publicRef runtime
    Just fileId ->
      Cache.loadCachedMedia (cacheConfig runtime) fileId >>= \case
        Nothing ->
          pure ref
        Just cached ->
          ensurePublicObject runtime cached

localPath :: (FileSystem :> es, Storage.Storage :> es, IOE :> es) => Runtime -> Text -> Eff es (Maybe FilePath)
localPath runtime ref =
  case Cache.parseMediaId ref of
    Nothing ->
      pure Nothing
    Just fileId ->
      fmap (.path) <$> Cache.loadCachedMedia (cacheConfig runtime) fileId

cacheObject :: (IOE :> es, FileSystem :> es, Storage.Storage :> es) => Runtime -> Maybe Text -> MediaObject -> Eff es Text
cacheObject runtime sourceRef mediaObject = do
  cached <- Cache.cacheMediaObject (cacheConfig runtime) sourceRef mediaObject
  pure (Cache.mediaIdForFileId cached.fileId)

ensurePublicObject :: (IOE :> es, KatipE :> es, FileSystem :> es, Storage.Storage :> es) => Runtime -> Cache.CachedMedia -> Eff es Text
ensurePublicObject runtime cached =
  case publicObjectUrl runtime.cfg cached of
    Nothing ->
      pure (Cache.mediaIdForFileId cached.fileId)
    Just url -> do
      when runtime.cfg.s3.enabled do
        void (storeObject runtime cached)
      pure url

storeObject :: (IOE :> es, KatipE :> es, FileSystem :> es, Storage.Storage :> es) => Runtime -> Cache.CachedMedia -> Eff es (Maybe Text)
storeObject runtime cached =
  storeObjectUnsafe runtime cached `catchSync` \err -> do
    logInfo [i|S3 media upload skipped: #{show err :: String}|]
    pure Nothing

storeObjectUnsafe :: (IOE :> es, KatipE :> es, FileSystem :> es, Storage.Storage :> es) => Runtime -> Cache.CachedMedia -> Eff es (Maybe Text)
storeObjectUnsafe Runtime{cfg, env = Nothing} _
  | not cfg.s3.enabled =
      pure Nothing
  | otherwise =
      liftIO (ioError (userError (Text.unpack (missingS3ConfigMessage cfg.s3))))
storeObjectUnsafe Runtime{cfg, env = Just env} cached
  | not cfg.s3.enabled =
      pure Nothing
  | otherwise = do
      ensureS3Config cfg.s3
      bytes <- FileSystemByteString.readFile cached.path
      let key = objectKey cfg.s3 cached
          mime = cached.mimeType
          request =
            S3.newPutObject (S3.BucketName (fromMaybe "" cfg.s3.bucket)) (S3.ObjectKey key) (AWS.toBody bytes)
              & S3Lens.putObject_contentType ?~ mime
              & setPublicAcl cfg.s3
      logInfo [i|S3 media upload: key=#{key} mime=#{mime}|]
      _ <- liftIO $ AWS.runResourceT (AWS.send env request)
      pure (publicObjectUrl cfg cached)

setPublicAcl :: Config -> S3.PutObject -> S3.PutObject
setPublicAcl cfg request
  | cfg.publicReadAcl =
      request & S3Lens.putObject_acl ?~ S3.ObjectCannedACL_Public_read
  | otherwise =
      request

ensureS3Config :: IOE :> es => Config -> Eff es ()
ensureS3Config cfg
  | missingS3ConfigMessage cfg == "" = pure ()
  | otherwise = liftIO (ioError (userError (Text.unpack (missingS3ConfigMessage cfg))))

missingS3ConfigMessage :: Config -> Text
missingS3ConfigMessage cfg =
  case missing of
    [] -> ""
    fields -> "S3 media storage is enabled but missing config keys: " <> Text.intercalate ", " fields
  where
    missing =
      [ name
      | (name, Nothing) <-
          [ ("bucket", cfg.bucket)
          , ("access_key_id", cfg.accessKeyId)
          , ("secret_access_key", cfg.secretAccessKey)
          ]
      ]

decodeDataMediaObject :: Text -> Maybe MediaObject
decodeDataMediaObject ref = do
  bytes <- Image.decodeDataImageReference ref
  let mime = fromMaybe "image/png" (dataImageMime ref)
  pure MediaObject
    { bytes
    , mimeType = mime
    , sourceName = Nothing
    }

dataImageMime :: Text -> Maybe Text
dataImageMime ref =
  Text.stripPrefix "data:" (Text.strip ref)
    <&> Text.takeWhile (/= ';')
    >>= nonEmptyText

downloadObject :: IOE :> es => HTTP.Manager -> Text -> Eff es MediaObject
downloadObject manager ref = do
  request <- liftIO (HTTP.parseRequest (Text.unpack ref))
  response <- liftIO (HTTP.httpLbs request manager)
  let bytes = LazyByteString.toStrict (HTTP.responseBody response)
      mime = responseMime response
  pure MediaObject
    { bytes
    , mimeType = mime
    , sourceName = Just (TextEncoding.decodeUtf8 (StrictByteString.takeWhile (/= 63) (HTTP.path request)))
    }

responseMime :: HTTP.Response body -> Text
responseMime response =
  fromMaybe "application/octet-stream" do
    raw <- List.lookup HTTPHeader.hContentType (HTTP.responseHeaders response)
    nonEmptyText (Text.takeWhile (/= ';') (TextEncoding.decodeUtf8 raw))

fileObject :: (IOE :> es, FileSystem :> es) => Text -> Eff es MediaObject
fileObject ref = do
  path <- case Text.stripPrefix "file://" ref of
    Just path -> pure (Text.unpack path)
    Nothing -> liftIO (ioError (userError [i|Invalid file media reference: #{ref}|]))
  bytes <- FileSystemByteString.readFile path
  pure MediaObject
    { bytes
    , mimeType = mimeFromName (Text.pack path)
    , sourceName = Just (Text.pack (takeFileName path))
    }

objectKey :: Config -> Cache.CachedMedia -> Text
objectKey cfg mediaObject =
  let ext = Cache.extensionFor MediaObject{bytes = "", mimeType = mediaObject.mimeType, sourceName = mediaObject.sourceName}
      prefix = Text.dropWhileEnd (== '/') cfg.prefix
  in prefix <> "/" <> mediaObject.digest <> ext

mimeFromName :: Text -> Text
mimeFromName name =
  Cache.mimeFromName name

publicObjectUrl :: MediaConfig.Config -> Cache.CachedMedia -> Maybe Text
publicObjectUrl cfg cached = do
  base <- cfg.publicBaseUrl
  pure (Text.dropWhileEnd (== '/') base <> "/" <> objectKey cfg.s3 cached)

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped

cacheConfig :: Runtime -> Cache.CacheConfig
cacheConfig runtime =
  Cache.CacheConfig
    { directory = runtime.cfg.cacheDir
    }
