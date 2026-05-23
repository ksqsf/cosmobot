{-|
Module      : Bot.Media.S3
Description : S3 media publishing
Stability   : experimental
-}

{-# LANGUAGE OverloadedLabels #-}

module Bot.Media.S3
  ( Runtime
  , newRuntime
  , ensurePublicObject
  )
where

import qualified Amazonka as AWS
import qualified Amazonka.Auth as AWSAuth
import qualified Amazonka.S3 as S3
import qualified Amazonka.S3.Lens as S3Lens
import Bot.Effect.Media (MediaObject (..))
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Media.Cache as Cache
import qualified Bot.Media.Config as MediaConfig
import Bot.Media.S3.Config
import Bot.Prelude
import Control.Lens ((.~), (?~), (^.))
import qualified Data.ByteString as StrictByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTPStatus
import System.IO.Error (ioError, userError)

data Runtime = Runtime
  { cfg :: !MediaConfig.Config
  , env :: !(Maybe AWS.Env)
  }

newRuntime :: IOE :> es => HTTP.Manager -> MediaConfig.Config -> Eff es Runtime
newRuntime manager cfg = do
  env <- if cfg.s3.enabled then Just <$> createAmazonkaEnv manager cfg.s3 else pure Nothing
  pure Runtime{cfg, env}

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
      let key = objectKey cfg.s3 cached
      objectExists env cfg.s3 key >>= \case
        True -> do
          logDebug [i|S3 media upload skipped; object already exists: key=#{key}|]
          pure (publicObjectUrl cfg cached)
        False -> do
          uploadObject env cfg.s3 key cached
          pure (publicObjectUrl cfg cached)

objectExists :: IOE :> es => AWS.Env -> Config -> Text -> Eff es Bool
objectExists env cfg key = do
  result <- trySync (liftIO $ AWS.runResourceT (AWS.send env request))
  case result of
    Right _ ->
      pure True
    Left err
      | Just (AWS.ServiceError serviceError) <- fromException err
      , serviceError ^. AWS.serviceError_status == HTTPStatus.status404 ->
          pure False
      | otherwise ->
          throwIO err
  where
    request =
      S3.newHeadObject (S3.BucketName (fromMaybe "" cfg.bucket)) (S3.ObjectKey key)

uploadObject :: (IOE :> es, KatipE :> es, FileSystem :> es) => AWS.Env -> Config -> Text -> Cache.CachedMedia -> Eff es ()
uploadObject env cfg key cached = do
  bytes <- FileSystemByteString.readFile cached.path
  let mime = cached.mimeType
      request =
        S3.newPutObject (S3.BucketName (fromMaybe "" cfg.bucket)) (S3.ObjectKey key) (AWS.toBody bytes)
          & S3Lens.putObject_contentType ?~ mime
          & setPublicAcl cfg
  logInfo [i|S3 media upload: key=#{key} mime=#{mime}|]
  void $ liftIO $ AWS.runResourceT (AWS.send env request)

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

objectKey :: Config -> Cache.CachedMedia -> Text
objectKey cfg mediaObject =
  let ext = Cache.extensionFor MediaObject{bytes = "", mimeType = mediaObject.mimeType, sourceName = mediaObject.sourceName}
      prefix = Text.dropWhileEnd (== '/') cfg.prefix
  in prefix <> "/" <> mediaObject.digest <> ext

publicObjectUrl :: MediaConfig.Config -> Cache.CachedMedia -> Maybe Text
publicObjectUrl cfg cached = do
  base <- cfg.publicBaseUrl
  pure (Text.dropWhileEnd (== '/') base <> "/" <> objectKey cfg.s3 cached)
