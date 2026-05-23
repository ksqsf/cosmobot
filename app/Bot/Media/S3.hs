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
import Bot.Media.S3.Config
import Bot.Prelude
import qualified Bot.Util.HTTP as Http
import qualified Bot.Util.Image as Image
import Control.Lens ((.~), (?~))
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTPHeader
import System.FilePath (takeExtension, takeFileName)
import System.IO.Error (ioError, userError)

data Runtime = Runtime
  { cfg :: !Config
  , manager :: !HTTP.Manager
  , env :: !(Maybe AWS.Env)
  }

runMediaS3
  :: (IOE :> es, Log :> es, FileSystem :> es)
  => Config
  -> Eff (Media : es) a
  -> Eff es a
runMediaS3 cfg inner = do
  manager <- liftIO Http.newTlsManager
  env <- if cfg.enabled then Just <$> createAmazonkaEnv manager cfg else pure Nothing
  interpret
    ( \_ -> \case
        StoreMediaObject mediaObject ->
          storeObject Runtime{cfg, manager, env} mediaObject
        NormalizeMediaRef ref ->
          normalizeRef Runtime{cfg, manager, env} ref
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

normalizeRef :: (IOE :> es, Log :> es, FileSystem :> es) => Runtime -> Text -> Eff es Text
normalizeRef runtime ref
  | not runtime.cfg.enabled =
      pure ref
  | isAlreadyPublicObject runtime.cfg ref =
      pure ref
  | "data:image/" `Text.isPrefixOf` Text.strip ref =
      case decodeDataMediaObject ref of
        Nothing -> do
          logInfo_ "Skipping invalid data:image media reference"
          pure ref
        Just mediaObject ->
          fromMaybe ref <$> storeObject runtime mediaObject
  | "http://" `Text.isPrefixOf` Text.toLower (Text.strip ref) ||
      "https://" `Text.isPrefixOf` Text.toLower (Text.strip ref) = do
      downloaded <- (Just <$> downloadObject runtime.manager ref) `catchSync` \err -> do
        logInfo_ [i|Remote media download skipped: #{show err :: String}|]
        pure Nothing
      maybe (pure ref) (fmap (fromMaybe ref) . storeObject runtime) downloaded
  | "file://" `Text.isPrefixOf` Text.strip ref = do
      mediaObject <- (Just <$> fileObject ref) `catchSync` \err -> do
        logInfo_ [i|Local media read skipped: #{show err :: String}|]
        pure Nothing
      maybe (pure ref) (fmap (fromMaybe ref) . storeObject runtime) mediaObject
  | otherwise =
      pure ref

storeObject :: (IOE :> es, Log :> es) => Runtime -> MediaObject -> Eff es (Maybe Text)
storeObject runtime mediaObject =
  storeObjectUnsafe runtime mediaObject `catchSync` \err -> do
    logInfo_ [i|S3 media upload skipped: #{show err :: String}|]
    pure Nothing

storeObjectUnsafe :: (IOE :> es, Log :> es) => Runtime -> MediaObject -> Eff es (Maybe Text)
storeObjectUnsafe Runtime{cfg, env = Nothing} _
  | not cfg.enabled =
      pure Nothing
  | otherwise =
      liftIO (ioError (userError (Text.unpack (missingS3ConfigMessage cfg))))
storeObjectUnsafe Runtime{cfg, env = Just env} mediaObject
  | not cfg.enabled =
      pure Nothing
  | otherwise = do
      ensureS3Config cfg
      let key = objectKey cfg mediaObject
          mime = mediaObject.mimeType
          request =
            S3.newPutObject (S3.BucketName (fromMaybe "" cfg.bucket)) (S3.ObjectKey key) (AWS.toBody mediaObject.bytes)
              & S3Lens.putObject_contentType ?~ mime
              & setPublicAcl cfg
      logInfo_ [i|S3 media upload: key=#{key} mime=#{mime}|]
      _ <- liftIO $ AWS.runResourceT (AWS.send env request)
      pure (Just (publicObjectUrl cfg key))

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
          , ("public_base_url", cfg.publicBaseUrl)
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

objectKey :: Config -> MediaObject -> Text
objectKey cfg mediaObject =
  let ext = extensionFor mediaObject
      prefix = Text.dropWhileEnd (== '/') cfg.prefix
  in prefix <> "/" <> contentDigest mediaObject.bytes <> ext

contentDigest :: StrictByteString.ByteString -> Text
contentDigest bytes =
  Text.pack (show (hash bytes :: Digest SHA256))

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

publicObjectUrl :: Config -> Text -> Text
publicObjectUrl cfg key =
  Text.dropWhileEnd (== '/') (fromMaybe "" cfg.publicBaseUrl) <> "/" <> key

isAlreadyPublicObject :: Config -> Text -> Bool
isAlreadyPublicObject cfg ref =
  maybe False (\base -> Text.dropWhileEnd (== '/') base `Text.isPrefixOf` ref) cfg.publicBaseUrl

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped
