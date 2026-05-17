{-|
Module      : Bot.Util.HTTP
Description : Small HTTP request helpers
Stability   : experimental
-}

module Bot.Util.HTTP
  ( httpsEndpointUrl
  , newNoRequiredEmsTlsManager
  , noRequiredEmsHttpConfig
  , runReqWithoutRequiredEMS
  , streamingJsonPostRequest
  )
where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import Network.Connection (TLSSettings (..))
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPTLS
import Network.HTTP.Req
import qualified Network.TLS as TLS
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

newNoRequiredEmsTlsManager :: IO HTTP.Manager
newNoRequiredEmsTlsManager =
  HTTPTLS.newTlsManagerWith (HTTPTLS.mkManagerSettings noRequiredEmsTlsSettings Nothing)

noRequiredEmsHttpConfig :: HTTP.Manager -> HttpConfig
noRequiredEmsHttpConfig manager =
  defaultHttpConfig
    { httpConfigAltManager = Just manager
    }

runReqWithoutRequiredEMS :: Req a -> IO a
runReqWithoutRequiredEMS action = do
  manager <- newNoRequiredEmsTlsManager
  runReq (noRequiredEmsHttpConfig manager) action

noRequiredEmsTlsSettings :: TLSSettings
noRequiredEmsTlsSettings =
  TLSSettingsSimple
    { settingDisableCertificateValidation = False
    , settingDisableSession = False
    , settingUseServerName = True
    , settingClientSupported =
        TLS.defaultSupported
          { TLS.supportedExtendedMainSecret = TLS.AllowEMS
          }
    }

streamingJsonPostRequest :: Aeson.ToJSON body => Text -> [Text] -> Text -> Int -> body -> IO HTTP.Request
streamingJsonPostRequest endpoint path apiKey timeoutMicros body = do
  base <- HTTP.parseRequest (Text.unpack (endpointText endpoint path))
  pure base
    { HTTP.method = "POST"
    , HTTP.requestHeaders =
        [ ("Authorization", ByteString.pack [i|Bearer #{apiKey}|])
        , ("Content-Type", "application/json")
        ]
    , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode body)
    , HTTP.responseTimeout = HTTP.responseTimeoutMicro timeoutMicros
    }

endpointText :: Text -> [Text] -> Text
endpointText endpoint path =
  case path of
    [] -> Text.dropWhileEnd (== '/') endpoint
    _  -> Text.dropWhileEnd (== '/') endpoint <> "/" <> Text.intercalate "/" path
