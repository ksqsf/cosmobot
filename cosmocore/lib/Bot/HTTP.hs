{-|
Module      : Bot.HTTP
Description : Shared HTTP client interpreter
Stability   : experimental
-}

module Bot.HTTP
  ( runHTTP
  , retryHttpConfig
  , streamingJsonPostRequest
  )
where

import Bot.Prelude
import qualified Control.Retry as Retry
import qualified Network.HTTP.Types.Status as HTTPStatus
import qualified Bot.Effect.HTTP as HTTP
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import Network.Connection (TLSSettings (..))
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as ClientTLS
import Network.HTTP.Req (HttpConfig (..))
import qualified Network.HTTP.Req as Req
import qualified Network.TLS as TLS

runHTTP :: IOE :> es => Eff (HTTP.HTTP : es) a -> Eff es a
runHTTP inner = do
  sharedManager <- liftIO newTlsManager
  interpret
    ( \_ -> \case
        HTTP.Manager ->
          pure sharedManager
        HTTP.RunReq action ->
          liftIO $ Req.runReq (withSharedManager sharedManager retryHttpConfig) action
        HTTP.RunReqWithConfig config action ->
          liftIO $ Req.runReq (withSharedManager sharedManager config) action
        HTTP.OpenResponse request ->
          liftIO $ Client.responseOpen request sharedManager
    )
    inner

-- Retry each API request at most three times, after 1, 2, and 4 seconds.
retryHttpConfig :: HttpConfig
retryHttpConfig = Req.defaultHttpConfig
  { httpConfigRetryPolicy = Retry.exponentialBackoff 1000000 <> Retry.limitRetries 3
  , httpConfigRetryJudge = \status response ->
      HTTPStatus.statusCode (Client.responseStatus response) `elem` [502, 503]
        || httpConfigRetryJudge Req.defaultHttpConfig status response
  }

withSharedManager :: Client.Manager -> HttpConfig -> HttpConfig
withSharedManager sharedManager config =
  config
    { httpConfigAltManager = Just sharedManager
    }

newTlsManager :: IO Client.Manager
newTlsManager =
  ClientTLS.newTlsManagerWith
    (ClientTLS.mkManagerSettings tlsSettings Nothing)
      { Client.managerConnCount = sharedManagerConnectionCount
      }

sharedManagerConnectionCount :: Int
sharedManagerConnectionCount =
  64

tlsSettings :: TLSSettings
tlsSettings =
  TLSSettingsSimple
    { settingDisableCertificateValidation = False
    , settingDisableSession = False
    , settingUseServerName = True
    , settingClientSupported =
        TLS.defaultSupported
          { TLS.supportedExtendedMainSecret = TLS.AllowEMS
          }
    }

streamingJsonPostRequest :: Aeson.ToJSON body => Text -> [Text] -> Text -> Int -> body -> IO Client.Request
streamingJsonPostRequest endpoint path apiKey timeoutMicros body = do
  base <- Client.parseRequest (Text.unpack (endpointText endpoint path))
  pure base
    { Client.method = "POST"
    , Client.requestHeaders =
        [ ("Authorization", ByteString.pack [i|Bearer #{apiKey}|])
        , ("Content-Type", "application/json")
        ]
    , Client.requestBody = Client.RequestBodyLBS (Aeson.encode body)
    , Client.responseTimeout = Client.responseTimeoutMicro timeoutMicros
    }

endpointText :: Text -> [Text] -> Text
endpointText endpoint path =
  case path of
    [] -> Text.dropWhileEnd (== '/') endpoint
    _  -> Text.dropWhileEnd (== '/') endpoint <> "/" <> Text.intercalate "/" path
