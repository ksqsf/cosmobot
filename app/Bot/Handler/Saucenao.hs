{-|
Module      : Bot.Handler.Saucenao
Description : SauceNAO command handler
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Handler.Saucenao
  ( saucenaoHandlers
  )
where

import qualified Bot.Effect.Chat as Chat
import Bot.Core.Route
import Bot.Core.Message
import Bot.Handler.Saucenao.Config
import qualified Bot.Util.HTTP as Http
import Bot.Util.Multipart
import Bot.Prelude
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Network.HTTP.Req
import Network.HTTP.Client (RequestBody(RequestBodyBS))
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import qualified Text.URI as URI
import System.IO.Error (ioError, userError)

saucenaoCommand :: Text
saucenaoCommand =
  "!saucenao"

-- | Routes for SauceNAO reverse image search commands.
saucenaoHandlers
  :: (Chat.Chat :> es, Log :> es, IOE :> es, Concurrent :> es)
  => SaucenaoConfig
  -> [RouteHandler es]
saucenaoHandlers saucenaoCfg =
  [ saucenaoRoute saucenaoCfg
  ]

saucenaoRoute
  :: (Chat.Chat :> es, Log :> es, IOE :> es, Concurrent :> es)
  => SaucenaoConfig
  -> RouteHandler es
saucenaoRoute saucenaoCfg =
  stopOn (command saucenaoCommand) \message _ -> do
    logInfo_ [i|matched saucenao route: #{incomingMessageLogLine message}|]
    spawnTask (sendSaucenaoResults saucenaoCfg message)

sendSaucenaoResults
  :: (Chat.Chat :> es, Log :> es, IOE :> es)
  => SaucenaoConfig
  -> IncomingMessage
  -> Eff es ()
sendSaucenaoResults cfg message =
  handleError do
    case cfg.apiKey of
      Nothing ->
        void $ Chat.replyTo message "SauceNAO 需要配置 API key。"
      Just _ -> do
        referenced <- fetchReferencedMessage message
        case referenced >>= nonEmptyImageUrls of
          Nothing ->
            void $ Chat.replyTo message "请回复一条包含图片的消息。"
          Just imageUrls -> do
            results <- traverse (searchOne cfg) imageUrls
            let body = Text.intercalate "\n\n" (mapMaybe renderResult results)
            if Text.null body
              then void $ Chat.replyTo message "没有找到相似度大于 90% 的结果。"
              else void $ Chat.replyTo message body
  where
    handleError action =
      action `catchSync` \err -> do
        logInfo_ [i|SauceNAO search failed: #{show err :: String}|]
        void $ Chat.replyTo message "SauceNAO 搜索失败。"

fetchReferencedMessage
  :: Chat.Chat :> es
  => IncomingMessage
  -> Eff es (Maybe ReferencedMessage)
fetchReferencedMessage message =
  traverse (Chat.getMessageContent message) message.replyToMessageId <&> join

nonEmptyImageUrls :: ReferencedMessage -> Maybe [Text]
nonEmptyImageUrls referenced =
  viaNonEmpty toList referenced.imageUrls

searchOne
  :: IOE :> es
  => SaucenaoConfig
  -> Text
  -> Eff es (Maybe SearchResult)
searchOne cfg imageUrl = do
  result <- liftIO (searchImage cfg imageUrl)
  pure (result >>= highSimilarityResult)

data SearchResult = SearchResult
  { similarity :: !Double
  , thumbnail  :: !(Maybe Text)
  , urls       :: ![Text]
  , title      :: !(Maybe Text)
  , source     :: !(Maybe Text)
  }
  deriving (Show, Generic, Aeson.ToJSON)

searchImage :: SaucenaoConfig -> Text -> IO (Maybe SearchResult)
searchImage cfg imageUrl = do
  imageBytes <- downloadImage imageUrl
  value <- Http.runReq $
    responseBody <$> do
      body <- reqBodyMultipart (multipartParts cfg imageBytes)
      req POST saucenaoUrl body jsonResponse saucenaoRequestOptions
  case join (AesonTypes.parseMaybe saucenaoApiError value) of
    Just err -> ioError (userError (Text.unpack err))
    Nothing  -> pure ()
  pure (join (AesonTypes.parseMaybe firstResult value))

saucenaoUrl :: Url 'Https
saucenaoUrl =
  https "saucenao.com" /: "search.php"

downloadImage :: Text -> IO ByteString.ByteString
downloadImage imageUrl
  | Just path <- Text.stripPrefix "file://" imageUrl =
      ByteString.readFile (Text.unpack path)
  | otherwise = do
      uri <- URI.mkURI imageUrl
      case useHttpsURI uri of
        Nothing ->
          ioError (userError [i|Unsupported SauceNAO image URL: #{imageUrl}|])
        Just (url, options) -> Http.runReq $
          responseBody <$> req GET url NoReqBody bsResponse (options <> saucenaoRequestOptions)

multipartParts :: SaucenaoConfig -> ByteString.ByteString -> [Multipart.Part]
multipartParts cfg imageBytes =
  [ textPart "output_type" "2"
  , textPart "numres" "1"
  , textPart "db" "999"
  , imagePart imageBytes
  ]
    <> maybePart "api_key" cfg.apiKey

imagePart :: ByteString.ByteString -> Multipart.Part
imagePart imageBytes =
  Multipart.partFileRequestBody "file" (imageFilename imageBytes) (RequestBodyBS imageBytes)

imageFilename :: ByteString.ByteString -> FilePath
imageFilename imageBytes =
  "image." <> imageExtension imageBytes

imageExtension :: ByteString.ByteString -> String
imageExtension imageBytes
  | ByteString.pack [0x89, 0x50, 0x4e, 0x47] `ByteString.isPrefixOf` imageBytes = "png"
  | ByteString.pack [0xff, 0xd8, 0xff] `ByteString.isPrefixOf` imageBytes = "jpg"
  | ByteString.pack [0x47, 0x49, 0x46, 0x38] `ByteString.isPrefixOf` imageBytes = "gif"
  | ByteString.pack [0x42, 0x4d] `ByteString.isPrefixOf` imageBytes = "bmp"
  | isWebP imageBytes = "webp"
  | otherwise = "jpg"

isWebP :: ByteString.ByteString -> Bool
isWebP imageBytes =
  ByteString.pack [0x52, 0x49, 0x46, 0x46] `ByteString.isPrefixOf` imageBytes &&
    ByteString.pack [0x57, 0x45, 0x42, 0x50] == ByteString.take 4 (ByteString.drop 8 imageBytes)

saucenaoRequestOptions :: Option scheme
saucenaoRequestOptions =
  header "User-Agent" (TextEncoding.encodeUtf8 saucenaoUserAgent)

saucenaoUserAgent :: Text
saucenaoUserAgent =
  "cosmobot/0.1 (+https://github.com/ksqsf/cosmobot)"

firstResult :: Aeson.Value -> AesonTypes.Parser (Maybe SearchResult)
firstResult =
  Aeson.withObject "SauceNAOResponse" $ \o -> do
    results <- fromMaybe [] <$> o Aeson..:? "results"
    traverse parseResult (viaNonEmpty head results)

saucenaoApiError :: Aeson.Value -> AesonTypes.Parser (Maybe Text)
saucenaoApiError =
  Aeson.withObject "SauceNAOResponse" $ \o -> do
    sauceHeader <- o Aeson..: "header"
    status <- sauceHeader Aeson..:? "status" :: AesonTypes.Parser (Maybe Int)
    message <- sauceHeader Aeson..:? "message"
    shortRemaining <- sauceHeader Aeson..:? "short_remaining" :: AesonTypes.Parser (Maybe Int)
    longRemaining <- sauceHeader Aeson..:? "long_remaining" :: AesonTypes.Parser (Maybe Int)
    pure case (status, message, shortRemaining, longRemaining) of
      (Just value, Just msg, _, _) | value < 0 ->
        Just ("SauceNAO API error: " <> msg)
      (_, Just msg, _, _) | "anonymous account" `Text.isInfixOf` Text.toLower msg ->
        Just ("SauceNAO API error: " <> msg)
      (_, _, Just value, _) | value <= 0 ->
        Just "SauceNAO API short-term rate limit exceeded."
      (_, _, _, Just value) | value <= 0 ->
        Just "SauceNAO API daily rate limit exceeded."
      _ ->
        Nothing

parseResult :: Aeson.Value -> AesonTypes.Parser SearchResult
parseResult =
  Aeson.withObject "SauceNAOResult" $ \o -> do
    resultHeader <- o Aeson..: "header"
    similarity <- resultHeader Aeson..: "similarity" >>= parseSimilarity
    thumbnail <- resultHeader Aeson..:? "thumbnail"
    data_ <- o Aeson..: "data"
    urls <- fromMaybe [] <$> data_ Aeson..:? "ext_urls"
    title <- data_ Aeson..:? "title"
    source <- data_ Aeson..:? "source"
    pure SearchResult
      { similarity = similarity
      , thumbnail = thumbnail
      , urls = urls
      , title = title
      , source = source
      }

parseSimilarity :: Text -> AesonTypes.Parser Double
parseSimilarity text =
  maybe (fail [i|Invalid SauceNAO similarity: #{text}|]) pure $
    readMaybe (toString (Text.strip text))

highSimilarityResult :: SearchResult -> Maybe SearchResult
highSimilarityResult result
  | result.similarity > 90 = Just result
  | otherwise              = Nothing

renderResult :: Maybe SearchResult -> Maybe Text
renderResult result = do
  match <- result
  resultUrl <- viaNonEmpty head match.urls
  let similarityText = show match.similarity :: Text
  Just $ Text.unlines $
    [ "相似度：" <> similarityText <> "%"
    , resultUrl
    ]
      <> [ ReplyBody.imageDirective thumbnail | thumbnail <- maybeToList match.thumbnail ]
