{-|
Module      : Bot.Util.HTTP
Description : Small HTTP request helpers
Stability   : experimental
-}

module Bot.Util.HTTP
  ( httpsEndpointUrl
  , streamingJsonPostRequest
  )
where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Req
import System.IO.Error (ioError, userError)
import qualified Text.URI as URI

httpsEndpointUrl :: Text -> [Text] -> IO (Url 'Https, Option 'Https)
httpsEndpointUrl endpoint path = do
  uri <- URI.mkURI endpoint
  case useHttpsURI uri of
    Nothing ->
      ioError (userError [i|Unsupported HTTPS endpoint URL: #{endpoint}. Use a full HTTPS base URL.|])
    Just (url, options) ->
      pure (foldl' (/:) url path, options)

streamingJsonPostRequest :: Aeson.ToJSON body => Text -> [Text] -> Text -> body -> IO HTTP.Request
streamingJsonPostRequest endpoint path apiKey body = do
  base <- HTTP.parseRequest (Text.unpack (Text.dropWhileEnd (== '/') endpoint <> "/" <> Text.intercalate "/" path))
  pure base
    { HTTP.method = "POST"
    , HTTP.requestHeaders =
        [ ("Authorization", ByteString.pack [i|Bearer #{apiKey}|])
        , ("Content-Type", "application/json")
        ]
    , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode body)
    , HTTP.responseTimeout = HTTP.responseTimeoutNone
    }
