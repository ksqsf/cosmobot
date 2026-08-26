{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.Matrix.Protocol
Description : Matrix Client-Server protocol implementation
Stability   : experimental
-}

module Bot.Chat.Driver.Matrix.Protocol where

import Bot.Chat.Driver.Matrix.Types (Config (..))
import qualified Bot.Effect.Matrix as Matrix
import Bot.Util.Aeson
import Bot.Prelude
import qualified Bot.Effect.HTTP as HTTP
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString as StrictByteString
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.Prim.IORef as IORef
import GHC.Clock (getMonotonicTimeNSec)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Req
import qualified Network.HTTP.Types.Header as HTTPHeader
import qualified Network.HTTP.Types.Status as HTTPStatus
import qualified Data.ByteString.Streaming.HTTP as StreamingHTTP
import qualified Streaming.ByteString as Q
import System.IO.Error (ioError, userError)
import qualified Text.URI as URI

newtype MatrixRoomId = MatrixRoomId Text
  deriving (Show, Eq, Ord)

newtype MatrixEventId = MatrixEventId Text
  deriving (Show, Eq, Ord)
    deriving (Aeson.ToJSON, Aeson.FromJSON) via Text

newtype MatrixReplyTo = MatrixReplyTo MatrixEventId
  deriving (Show, Eq)

matrixRoomIdText :: MatrixRoomId -> Text
matrixRoomIdText (MatrixRoomId roomId) =
  roomId

matrixEventIdText :: MatrixEventId -> Text
matrixEventIdText (MatrixEventId eventId) =
  eventId

matrixRoomId :: Text -> MatrixRoomId
matrixRoomId =
  MatrixRoomId

matrixEventId :: Text -> MatrixEventId
matrixEventId =
  MatrixEventId

instance IsString MatrixRoomId where
  fromString =
    matrixRoomId . Text.pack

instance IsString MatrixEventId where
  fromString =
    matrixEventId . Text.pack

data MatrixDriver = MatrixDriver
  { config :: !Config
  , auth :: !MatrixAuth
  , eventIds :: !(IORef.IORef (Map Text MatrixEventId))
  , streamTextMessages :: !(IORef.IORef (Map MatrixEventId MatrixEditMessageRequest))
  , directRoomIds :: !(IORef.IORef (Set MatrixRoomId))
  , joinedMemberCounts :: !(IORef.IORef (Map MatrixRoomId Int))
  }

newMatrixDriver
  :: (Concurrent :> es, Prim :> es)
  => Config
  -> Eff es MatrixDriver
newMatrixDriver cfg = do
  eventIds <- IORef.newIORef Map.empty
  streamTextMessages <- IORef.newIORef Map.empty
  directRoomIdsRef <- IORef.newIORef (Set.fromList (matrixRoomId <$> cfg.directRooms))
  joinedMemberCountsRef <- IORef.newIORef Map.empty
  authState <- IORef.newIORef (initialMatrixAuthState cfg)
  refreshLock <- MVar.newMVar ()
  let auth = MatrixAuth cfg authState refreshLock
  pure MatrixDriver
    { config = cfg
    , auth
    , eventIds
    , streamTextMessages
    , directRoomIds = directRoomIdsRef
    , joinedMemberCounts = joinedMemberCountsRef
    }

runMatrixClient
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Maybe MatrixDriver
  -> Eff (Matrix.Matrix : es) a
  -> Eff es a
runMatrixClient driver =
  interpret \_ -> \case
    Matrix.MatrixClientCall request ->
      maybe
        (liftIO (ioError (userError "Matrix driver is not configured.")))
        (`matrixClientRequest` request)
        driver

matrixClientRequest
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> Matrix.MatrixClientRequest
  -> Eff es Aeson.Value
matrixClientRequest driver Matrix.MatrixClientRequest{method, path, query, body} =
  case (method, body) of
    (Matrix.MatrixGet, Nothing) -> matrixClientJsonCall driver "client request" "client request" requestOptions GET requestUrl NoReqBody
    (Matrix.MatrixDelete, Nothing) -> matrixClientJsonCall driver "client request" "client request" requestOptions DELETE requestUrl NoReqBody
    (Matrix.MatrixPost, Just value) -> matrixClientJsonCall driver "client request" "client request" requestOptions POST requestUrl (ReqBodyJson value)
    (Matrix.MatrixPut, Just value) -> matrixClientJsonCall driver "client request" "client request" requestOptions PUT requestUrl (ReqBodyJson value)
    _ -> liftIO (ioError (userError "Invalid Matrix client request body."))
  where
    requestUrl :: forall scheme. Url scheme -> Url scheme
    requestUrl baseUrl = List.foldl' (/:) baseUrl path
    requestOptions :: forall scheme. Option scheme -> Option scheme
    requestOptions options = List.foldl' (\current (key, value) -> current <> (key =: value)) options query

data MatrixAuth = MatrixAuth
  { authConfig :: !Config
  , authState :: !(IORef.IORef MatrixAuthState)
  , authRefreshLock :: !(MVar.MVar ())
  }

data MatrixAuthState = MatrixAuthState
  { authAccessToken :: !(Maybe Text)
  , authRefreshToken :: !(Maybe Text)
  , authRefreshAtMilliseconds :: !(Maybe Integer)
  }
  deriving (Show, Eq, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (PrefixedSnakeJSON "auth" MatrixAuthState)

data MatrixDownloadedMedia = MatrixDownloadedMedia
  { downloadedBytes :: !(Q.ByteStream (ResourceT IO) ())
  , downloadedMimeType :: !Text
  , downloadedName :: !(Maybe Text)
  }

data MatrixSync = MatrixSync
  { syncSince :: Maybe Text
  , syncMode :: !MatrixSyncMode
  }

data MatrixSyncMode
  = MatrixInitialSync
  | MatrixLongPollSync

newtype MatrixJoinedMembers = MatrixJoinedMembers
  { joinedMembersRoomId :: MatrixRoomId
  }

data MatrixSendMessage = MatrixSendMessage
  { sendMessageRoomId :: !MatrixRoomId
  , sendMessageRequest :: !SendMessageRequest
  }

data MatrixUploadMedia = MatrixUploadMedia
  { uploadMediaPath :: !FilePath
  , uploadMediaFileName :: !Text
  , uploadMediaMime :: !Text
  }

data MatrixSendFile = MatrixSendFile
  { sendFileRoomId :: !Text
  , sendFileReplyTo :: !(Maybe MatrixReplyTo)
  , sendFileMessage :: !MatrixFileMessage
  }

data MatrixEditMessage = MatrixEditMessage
  { editMessageRoomId :: !MatrixRoomId
  , editMessageRequest :: !MatrixEditMessageRequest
  }

data MatrixCompleteStreamMessage = MatrixCompleteStreamMessage
  { completeStreamMessageRoomId :: !MatrixRoomId
  , completeStreamMessageRequest :: !MatrixEditMessageRequest
  }

data MatrixRedactEvent = MatrixRedactEvent
  { redactRoomId :: !Text
  , redactEventId :: !MatrixEventId
  }

data MatrixFetchEvent = MatrixFetchEvent
  { fetchEventRoomId :: !MatrixRoomId
  , fetchEventId :: !MatrixEventId
  }

data MatrixFetchMember = MatrixFetchMember
  { fetchMemberRoomId :: !MatrixRoomId
  , fetchMemberUserId :: !Text
  }

newtype MatrixFetchProfile = MatrixFetchProfile
  { fetchProfileUserId :: Text
  }

newtype MatrixDownloadMedia = MatrixDownloadMedia
  { downloadMediaRef :: Text
  }

data MatrixSetTyping = MatrixSetTyping
  { typingRoomId :: !MatrixRoomId
  , typingUserId :: !Text
  , typingTimeoutMs :: !Int
  }

newtype MatrixJoinRoom = MatrixJoinRoom
  { joinRoomId :: MatrixRoomId
  }

class MatrixAPI request where
  type MatrixResponse request

  call
    :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
    => MatrixDriver
    -> request
    -> Eff es (MatrixResponse request)

instance MatrixAPI MatrixSync where
  type MatrixResponse MatrixSync = SyncResponse

  call driver request@MatrixSync{syncSince} = do
    let sinceLabel :: Text
        sinceLabel = maybe "<initial>" (const "<next_batch>") syncSince
    matrixClientJsonCall driver "sync" [i|sync since=#{sinceLabel}|] (matrixSyncOptions request)
      GET
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "sync")
      NoReqBody

instance MatrixAPI MatrixJoinedMembers where
  type MatrixResponse MatrixJoinedMembers = JoinedMembersResponse

  call driver MatrixJoinedMembers{joinedMembersRoomId} =
    matrixClientJsonCall driver "joined_members" [i|joined_members room=#{joinedMembersRoomId}|] matrixApiOptions
      GET
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText joinedMembersRoomId /: "joined_members")
      NoReqBody

instance MatrixAPI MatrixJoinRoom where
  type MatrixResponse MatrixJoinRoom = Aeson.Value

  call driver MatrixJoinRoom{joinRoomId} =
    matrixClientJsonCall driver "join room" [i|join room=#{joinRoomId}|] matrixApiOptions
      POST
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText joinRoomId /: "join")
      (ReqBodyJson (Aeson.object []))

instance MatrixAPI MatrixSendMessage where
  type MatrixResponse MatrixSendMessage = SendMessageResponse

  call driver MatrixSendMessage{sendMessageRoomId, sendMessageRequest} = do
    txnId <- liftIO (show <$> getMonotonicTimeNSec)
    matrixClientJsonCall driver "send m.room.message" "send m.room.message" matrixApiOptions
      PUT
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText sendMessageRoomId /: "send" /: "m.room.message" /: txnId)
      (ReqBodyJson sendMessageRequest)

instance MatrixAPI MatrixUploadMedia where
  type MatrixResponse MatrixUploadMedia = MatrixUploadResponse

  call driver MatrixUploadMedia{uploadMediaPath, uploadMediaFileName, uploadMediaMime} =
    matrixClientJsonCall driver "upload media" "upload media" (matrixUploadOptions uploadMediaFileName uploadMediaMime)
      POST
      (\baseUrl -> baseUrl /: "_matrix" /: "media" /: "v3" /: "upload")
      (ReqBodyFile uploadMediaPath)

instance MatrixAPI MatrixSendFile where
  type MatrixResponse MatrixSendFile = SendMessageResponse

  call driver MatrixSendFile{sendFileRoomId, sendFileReplyTo, sendFileMessage = fileMessage} = do
    txnId <- liftIO (show <$> getMonotonicTimeNSec)
    let mediaMsgtype = fileMessage.msgtype
        request = MatrixFileMessageRequest
          { message = fileMessage
          , replyRelation = sendFileReplyTo
          , streamMetadata = MatrixStreamMetadata True
          }
    matrixClientJsonCall driver [i|send #{mediaMsgtype}|] [i|send #{mediaMsgtype}|] matrixApiOptions
      PUT
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: sendFileRoomId /: "send" /: "m.room.message" /: txnId)
      (ReqBodyJson request)

instance MatrixAPI MatrixEditMessage where
  type MatrixResponse MatrixEditMessage = SendMessageResponse

  call driver MatrixEditMessage{editMessageRoomId, editMessageRequest} = do
    txnId <- liftIO (show <$> getMonotonicTimeNSec)
    matrixClientJsonCall driver "edit m.room.message" "edit m.room.message" matrixApiOptions
      PUT
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText editMessageRoomId /: "send" /: "m.room.message" /: txnId)
      (ReqBodyJson editMessageRequest)

instance MatrixAPI MatrixCompleteStreamMessage where
  type MatrixResponse MatrixCompleteStreamMessage = SendMessageResponse

  call driver MatrixCompleteStreamMessage{completeStreamMessageRoomId, completeStreamMessageRequest} = do
    txnId <- liftIO (show <$> getMonotonicTimeNSec)
    matrixClientJsonCall driver "complete stream m.room.message" "complete stream m.room.message" matrixApiOptions
      PUT
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText completeStreamMessageRoomId /: "send" /: "m.room.message" /: txnId)
      (ReqBodyJson completeStreamMessageRequest)

instance MatrixAPI MatrixRedactEvent where
  type MatrixResponse MatrixRedactEvent = Aeson.Value

  call driver MatrixRedactEvent{redactRoomId, redactEventId} = do
    txnId <- liftIO (show <$> getMonotonicTimeNSec)
    let request = RedactEventRequest{reason = Nothing}
    matrixClientJsonCall driver "redact event" "redact event" matrixApiOptions
      PUT
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: redactRoomId /: "redact" /: matrixEventIdText redactEventId /: txnId)
      (ReqBodyJson request)

instance MatrixAPI MatrixFetchEvent where
  type MatrixResponse MatrixFetchEvent = Event

  call driver MatrixFetchEvent{fetchEventRoomId, fetchEventId} =
    matrixClientJsonCall driver "room event" [i|room event room=#{fetchEventRoomId}|] matrixApiOptions
      GET
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText fetchEventRoomId /: "event" /: matrixEventIdText fetchEventId)
      NoReqBody

instance MatrixAPI MatrixFetchMember where
  type MatrixResponse MatrixFetchMember = MatrixMember

  call driver MatrixFetchMember{fetchMemberRoomId, fetchMemberUserId} = do
    content :: MatrixMemberContent <-
      matrixClientJsonCall driver "room member" [i|room member room=#{fetchMemberRoomId}|] matrixApiOptions
        GET
        (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText fetchMemberRoomId /: "state" /: "m.room.member" /: fetchMemberUserId)
        NoReqBody
    pure (matrixMemberFromContent fetchMemberUserId content)

instance MatrixAPI MatrixFetchProfile where
  type MatrixResponse MatrixFetchProfile = MatrixProfile

  call driver MatrixFetchProfile{fetchProfileUserId} = do
    profile :: MatrixProfileContent <-
      matrixClientJsonCall driver "profile" "profile" matrixApiOptions
        GET
        (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "profile" /: fetchProfileUserId)
        NoReqBody
    pure MatrixProfile
      { profileUserId = fetchProfileUserId
      , profileDisplayName = profile.profileContentDisplayName
      , profileAvatarUrl = profile.profileContentAvatarUrl
      }

instance MatrixAPI MatrixDownloadMedia where
  type MatrixResponse MatrixDownloadMedia = MatrixDownloadedMedia

  call driver MatrixDownloadMedia{downloadMediaRef} =
    case parseMxcUri downloadMediaRef of
      Nothing ->
        liftIO (ioError (userError [i|Invalid Matrix media URI: #{downloadMediaRef}|]))
      Just (serverName, mediaId) -> katipAddContext (sl "matrix_method" ("authenticated media download" :: Text)) do
        $(logDebug) [i|Matrix API request: authenticated media download #{downloadMediaRef}|]
        withMatrixAccessToken driver.auth \token -> do
          manager <- HTTP.manager
          request <- liftIO (matrixMediaDownloadRequest driver.config token serverName mediaId)
          downloadedMimeType <- matrixReq "authenticated media download" (probeMatrixMediaDownload request manager)
          pure MatrixDownloadedMedia
            { downloadedBytes = matrixResponseByteStream request manager
            , downloadedMimeType
            , downloadedName = Just mediaId
            }

instance MatrixAPI MatrixSetTyping where
  type MatrixResponse MatrixSetTyping = Aeson.Value

  call driver MatrixSetTyping{typingRoomId, typingUserId, typingTimeoutMs} = do
    let request = SetTypingRequest typingTimeoutMs
    matrixClientJsonCall driver "set typing" [i|set typing room=#{matrixRoomIdText typingRoomId} user=#{typingUserId}|] matrixApiOptions
      PUT
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText typingRoomId /: "typing" /: typingUserId)
      (ReqBodyJson request)

initialMatrixAuthState :: Config -> MatrixAuthState
initialMatrixAuthState _cfg =
  -- It's unnecessary to log in here. /sync should handle it.
  MatrixAuthState
    { authAccessToken = Nothing
    , authRefreshToken = Nothing
    , authRefreshAtMilliseconds = Nothing
    }

matrixAuthAvailable :: Prim :> es => MatrixDriver -> Eff es Bool
matrixAuthAvailable driver = do
  currentAuthState <- IORef.readIORef driver.auth.authState
  pure (isJust currentAuthState.authAccessToken || isJust currentAuthState.authRefreshToken)

maybeCall
  :: (MatrixAPI request, HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> request
  -> Eff es (Maybe (MatrixResponse request))
maybeCall driver request = do
  available <- matrixAuthAvailable driver
  if available
    then Just <$> call driver request
    else pure Nothing

eitherCall
  :: (MatrixAPI request, HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Text
  -> MatrixDriver
  -> request
  -> Eff es (Either Text (MatrixResponse request))
eitherCall label driver request = do
  available <- matrixAuthAvailable driver
  if available
    then first (matrixUserFacingExceptionText label) <$> trySync (call driver request)
    else pure (Left [i|Matrix #{label} requires a configured access token, refresh token, or login credentials.|])

withMatrixAccessToken
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixAuth
  -> (Text -> Eff es a)
  -> Eff es a
withMatrixAccessToken auth action = do
  currentAuthState <- IORef.readIORef auth.authState
  now <- monotonicMilliseconds
  token <- case currentAuthState.authAccessToken of
    Just accessToken
      | maybe True (now <) currentAuthState.authRefreshAtMilliseconds ->
          pure accessToken
      | otherwise ->
          refreshMatrixAccessToken auth accessToken
    Nothing ->
      refreshMatrixAccessToken auth ""
  action token `catch` \(err :: MatrixApiException) ->
    if matrixAuthTokenRejected err
      then do
        refreshed <- refreshMatrixAccessToken auth token
        action refreshed
      else
        throwIO err

refreshMatrixAccessToken
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixAuth
  -> Text
  -> Eff es Text
refreshMatrixAccessToken auth expiredToken =
  MVar.withMVar auth.authRefreshLock \_ -> do
    currentAuthState <- IORef.readIORef auth.authState
    case currentAuthState.authAccessToken of
      Just token | token /= expiredToken ->
        pure token
      _ ->
        case currentAuthState.authRefreshToken of
          Nothing ->
            reloginMatrixAccessToken auth
          Just refreshToken -> do
            $(logInfo) "Matrix access token refreshing"
            let refreshWithToken = do
                  response <- refreshMatrixToken auth.authConfig refreshToken
                  now <- monotonicMilliseconds
                  let refreshedState = MatrixAuthState
                        { authAccessToken = Just response.refreshedAccessToken
                        , authRefreshToken = response.refreshedRefreshToken <|> currentAuthState.authRefreshToken
                        , authRefreshAtMilliseconds = matrixRefreshAtMilliseconds now response.refreshedExpiresInMs
                        }
                  IORef.writeIORef auth.authState refreshedState
                  pure response.refreshedAccessToken
            refreshWithToken `catchSync` \err -> do
              $(logWarning) [i|Matrix refresh token failed; logging in again: #{displayException err}|]
              reloginMatrixAccessToken auth

reloginMatrixAccessToken
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixAuth
  -> Eff es Text
reloginMatrixAccessToken auth =
  case (auth.authConfig.loginUser, auth.authConfig.loginPassword) of
    (Just user, Just password) -> do
      $(logInfo) [i|Matrix logging in again; attempts=#{matrixReloginAttempts}|]
      retryingMatrixLogin auth user password
    _ ->
      throwIO (userError "Matrix access token expired and no refresh token or login credentials are configured.")

retryingMatrixLogin
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixAuth
  -> Text
  -> Text
  -> Eff es Text
retryingMatrixLogin auth user password =
  attemptLogin 1
  where
    attemptLogin attempt =
      loginOnce `catchSync` \err ->
        if attempt < matrixReloginAttempts
          then do
            $(logWarning) [i|Matrix relogin attempt #{attempt}/#{matrixReloginAttempts} failed: #{displayException err}; retrying in #{matrixReloginDelaySeconds} seconds|]
            threadDelay matrixReloginDelayMicroseconds
            attemptLogin (attempt + 1)
          else do
            $(logError) [i|Matrix relogin attempt #{attempt}/#{matrixReloginAttempts} failed: #{displayException err}|]
            throwIO err

    loginOnce = do
      response <- matrixLogin auth.authConfig user password
      storeMatrixLoginResponse auth response
      pure response.loginAccessToken

storeMatrixLoginResponse :: (IOE :> es, Prim :> es) => MatrixAuth -> MatrixLoginResponse -> Eff es ()
storeMatrixLoginResponse auth response = do
  now <- monotonicMilliseconds
  IORef.writeIORef auth.authState MatrixAuthState
    { authAccessToken = Just response.loginAccessToken
    , authRefreshToken = response.loginRefreshToken
    , authRefreshAtMilliseconds = matrixRefreshAtMilliseconds now response.loginExpiresInMs
    }

matrixRefreshAtMilliseconds :: Integer -> Maybe Integer -> Maybe Integer
matrixRefreshAtMilliseconds now expiresInMilliseconds =
  expiresInMilliseconds <&> \expiresIn ->
    now + max 0 (expiresIn - matrixRefreshMarginMilliseconds)

matrixRefreshMarginMilliseconds :: Integer
matrixRefreshMarginMilliseconds =
  60000

monotonicMilliseconds :: IOE :> es => Eff es Integer
monotonicMilliseconds =
  fromIntegral . (`div` 1000000) <$> liftIO getMonotonicTimeNSec

matrixAuthTokenRejected :: MatrixApiException -> Bool
matrixAuthTokenRejected = \case
  MatrixApiException _ status err ->
    HTTPStatus.statusCode status == 401 && err.errcode `elem` ["M_UNKNOWN_TOKEN", "M_FORBIDDEN"]
  MatrixTransportException{} ->
    False

data MatrixApiException
  = MatrixApiException !Text !HTTPStatus.Status !MatrixErrorResponse
  | MatrixTransportException !Text !Text
  deriving (Show, Eq)

instance Exception MatrixApiException where
  displayException =
    Text.unpack . matrixApiExceptionMessage

matrixApiExceptionMessage :: MatrixApiException -> Text
matrixApiExceptionMessage = \case
  MatrixApiException method status err ->
    [i|Matrix API request failed (#{method}): HTTP #{HTTPStatus.statusCode status} #{matrixErrorResponseText err}|]
  MatrixTransportException method message ->
    [i|Matrix API request failed (#{method}): #{message}|]

matrixUserFacingExceptionText :: Text -> SomeException -> Text
matrixUserFacingExceptionText label err =
  case fromException err of
    Just matrixErr ->
      matrixUserFacingApiError label matrixErr
    Nothing ->
      [i|Matrix #{label} failed: #{displayException err}|]

matrixUserFacingApiError :: Text -> MatrixApiException -> Text
matrixUserFacingApiError label = \case
  MatrixApiException _ status err
    | matrixAuthTokenRejected (MatrixApiException label status err) ->
        [i|Matrix #{label} failed: Matrix authentication failed after retrying login.|]
    | otherwise ->
        [i|Matrix #{label} failed: HTTP #{HTTPStatus.statusCode status}.|]
  MatrixTransportException{} ->
    [i|Matrix #{label} failed: Matrix transport error.|]

matrixErrorResponseText :: MatrixErrorResponse -> Text
matrixErrorResponseText err =
  Text.intercalate "; " $
    [ err.errcode <> maybe "" (": " <>) err.matrixError
    ]
      <> maybe [] (\retry -> [[i|retry_after_ms=#{retry}|]]) err.retryAfterMs
      <> if err.softLogout then ["soft_logout=true"] else []

matrixApiException :: Text -> HttpException -> MatrixApiException
matrixApiException method = \case
  VanillaHttpException (Client.HttpExceptionRequest _ (Client.StatusCodeException response body)) ->
    case Aeson.eitherDecodeStrict body of
      Right err ->
        MatrixApiException method (Client.responseStatus response) err
      Left parseErr ->
        MatrixTransportException method [i|HTTP #{HTTPStatus.statusCode (Client.responseStatus response)} with non-Matrix error body: #{parseErr}|]
  VanillaHttpException (Client.HttpExceptionRequest _ content) ->
    MatrixTransportException method [i|HTTP transport error: #{show content :: String}|]
  VanillaHttpException err ->
    MatrixTransportException method [i|HTTP error: #{show err :: String}|]
  JsonHttpException message ->
    MatrixTransportException method [i|JSON error: #{message}|]

matrixReq :: IOE :> es => Text -> Eff es a -> Eff es a
matrixReq method action =
  action `catch` \(err :: HttpException) ->
    throwIO (matrixApiException method err)

matrixUnauthenticatedCall
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => Config
  -> Text
  -> Text
  -> (forall scheme. Option scheme -> Option scheme)
  -> (forall scheme. Url scheme -> Option scheme -> Req response)
  -> Eff es response
matrixUnauthenticatedCall cfg method logMessage addOptions buildRequest = katipAddContext (sl "matrix_method" method) do
  $(logDebug) [i|Matrix API request: #{logMessage}|]
  withMatrixBaseUrl cfg.homeserver \baseUrl baseOptions ->
    matrixReq method $
      HTTP.runReqWithConfig matrixHttpConfig $
        buildRequest baseUrl (addOptions baseOptions)

matrixUnauthenticatedJsonCall
  :: ( HTTP.HTTP :> es
     , IOE :> es
     , KatipE :> es
     , Aeson.FromJSON response
     , HttpMethod method
     , HttpBody body
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
     )
  => Config
  -> Text
  -> Text
  -> (forall scheme. Option scheme -> Option scheme)
  -> method
  -> (forall scheme. Url scheme -> Url scheme)
  -> body
  -> Eff es response
matrixUnauthenticatedJsonCall cfg method logMessage addOptions httpMethod buildUrl body =
  responseBody <$> matrixUnauthenticatedCall cfg method logMessage addOptions \baseUrl options ->
    req httpMethod (buildUrl baseUrl) body jsonResponse options

matrixClientJsonCall
  :: ( HTTP.HTTP :> es
     , IOE :> es
     , KatipE :> es
     , Concurrent :> es
     , Prim :> es
     , Aeson.FromJSON response
     , HttpMethod method
     , HttpBody body
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
     )
  => MatrixDriver
  -> Text
  -> Text
  -> (forall scheme. Option scheme -> Option scheme)
  -> method
  -> (forall scheme. Url scheme -> Url scheme)
  -> body
  -> Eff es response
matrixClientJsonCall driver method logMessage addOptions httpMethod buildUrl body =
  withMatrixAccessToken driver.auth \token ->
    matrixUnauthenticatedJsonCall driver.config method logMessage
      (\baseOptions -> matrixApiOptions (addOptions (baseOptions <> matrixAuth token)))
      httpMethod buildUrl body

matrixApiOptions :: Option scheme -> Option scheme
matrixApiOptions baseOptions =
  baseOptions
    <> responseTimeout matrixApiResponseTimeoutMicroseconds

matrixUploadOptions :: Text -> Text -> Option scheme -> Option scheme
matrixUploadOptions fileName mime baseOptions =
  baseOptions
    <> header "Content-Type" (TextEncoding.encodeUtf8 mime)
    <> "filename" =: fileName

matrixApiOptionsNoAuth :: Option scheme -> Option scheme
matrixApiOptionsNoAuth baseOptions =
  baseOptions
    <> responseTimeout matrixApiResponseTimeoutMicroseconds

matrixMediaDownloadRequest :: Config -> Text -> Text -> Text -> IO Client.Request
matrixMediaDownloadRequest cfg token serverName mediaId = do
  request <- Client.parseRequest (Text.unpack (matrixEndpointText cfg.homeserver ["_matrix", "client", "v1", "media", "download", serverName, mediaId]))
  pure request
    { Client.requestHeaders =
        ( "Authorization"
        , ByteString.pack [i|Bearer #{token}|]
        ) : Client.requestHeaders request
    , Client.responseTimeout = Client.responseTimeoutMicro matrixMediaDownloadResponseTimeoutMicroseconds
    }

matrixResponseByteStream :: Client.Request -> Client.Manager -> Q.ByteStream (ResourceT IO) ()
matrixResponseByteStream request manager = do
  response <- lift (StreamingHTTP.http request manager)
  Client.responseBody response

probeMatrixMediaDownload :: IOE :> es => Client.Request -> Client.Manager -> Eff es Text
probeMatrixMediaDownload request manager =
  bracket
    (liftIO $ Client.responseOpen (matrixMediaProbeRequest request) manager)
    (liftIO . Client.responseClose)
    \response -> do
      let status = Client.responseStatus response
      if HTTPStatus.statusIsSuccessful status
        then do
          void $ liftIO $ Client.brRead (Client.responseBody response)
          pure (matrixResponseMime response)
        else do
          body <- liftIO $ Client.brRead (Client.responseBody response)
          throwIO (matrixDownloadStatusException status body)

matrixResponseMime :: Client.Response body -> Text
matrixResponseMime response =
  fromMaybe "application/octet-stream" do
    raw <- List.lookup HTTPHeader.hContentType (Client.responseHeaders response)
    let mime = Text.takeWhile (/= ';') (TextEncoding.decodeUtf8 raw)
    mime <$ guard (not (Text.null mime))

matrixMediaProbeRequest :: Client.Request -> Client.Request
matrixMediaProbeRequest request =
  request
    { Client.requestHeaders =
        ("Range", "bytes=0-0") : filter ((/= "Range") . fst) request.requestHeaders
    }

matrixDownloadStatusException :: HTTPStatus.Status -> StrictByteString.ByteString -> MatrixApiException
matrixDownloadStatusException status body =
  case Aeson.eitherDecodeStrict body of
    Right err ->
      MatrixApiException "authenticated media download" status err
    Left parseErr ->
      MatrixTransportException "authenticated media download" [i|HTTP #{HTTPStatus.statusCode status} with non-Matrix error body: #{parseErr}|]

matrixEndpointText :: Text -> [Text] -> Text
matrixEndpointText homeserver path =
  Text.dropWhileEnd (== '/') homeserver <> "/" <> Text.intercalate "/" path

matrixSyncOptions :: MatrixSync -> Option scheme -> Option scheme
matrixSyncOptions MatrixSync{syncSince, syncMode} baseOptions =
  baseOptions
    <> responseTimeout matrixSyncResponseTimeoutMicroseconds
    <> "timeout" =: matrixSyncTimeoutMilliseconds syncMode
    <> maybe mempty ("since" =:) syncSince

newtype MatrixRefreshRequest = MatrixRefreshRequest
  { requestRefreshToken :: Text
  }
  deriving (Generic)
    deriving Aeson.ToJSON via (PrefixedSnakeJSON "request" MatrixRefreshRequest)

data MatrixRefreshResponse = MatrixRefreshResponse
  { refreshedAccessToken :: !Text
  , refreshedRefreshToken :: !(Maybe Text)
  , refreshedExpiresInMs :: !(Maybe Integer)
  }
  deriving (Show, Eq, Generic)
    deriving Aeson.FromJSON via (PrefixedSnakeJSON "refreshed" MatrixRefreshResponse)

data MatrixErrorResponse = MatrixErrorResponse
  { errcode :: !Text
  , matrixError :: !(Maybe Text)
  , retryAfterMs :: !(Maybe Integer)
  , softLogout :: !Bool
  }
  deriving (Show, Eq)

instance Aeson.FromJSON MatrixErrorResponse where
  parseJSON = Aeson.withObject "MatrixErrorResponse" \o ->
    MatrixErrorResponse
      <$> o Aeson..: "errcode"
      <*> o Aeson..:? "error"
      <*> o Aeson..:? "retry_after_ms"
      <*> o Aeson..:? "soft_logout" Aeson..!= False

data MatrixLoginIdentifier = MatrixLoginIdentifier
  { loginIdentifierType :: !Text
  , loginIdentifierUser :: !Text
  }

instance Aeson.ToJSON MatrixLoginIdentifier where
  toJSON MatrixLoginIdentifier{loginIdentifierType, loginIdentifierUser} =
    Aeson.object
      [ "type" Aeson..= loginIdentifierType
      , "user" Aeson..= loginIdentifierUser
      ]

data MatrixLoginRequest = MatrixLoginRequest
  { loginIdentifier :: !MatrixLoginIdentifier
  , loginPassword :: !Text
  , loginDeviceId :: !(Maybe Text)
  , loginInitialDeviceDisplayName :: !(Maybe Text)
  , loginRefreshToken :: !Bool
  }

instance Aeson.ToJSON MatrixLoginRequest where
  toJSON MatrixLoginRequest{loginIdentifier, loginPassword, loginDeviceId, loginInitialDeviceDisplayName, loginRefreshToken} =
    Aeson.object
      [ "type" Aeson..= ("m.login.password" :: Text)
      , "identifier" Aeson..= loginIdentifier
      , "password" Aeson..= loginPassword
      , "device_id" Aeson..= loginDeviceId
      , "initial_device_display_name" Aeson..= loginInitialDeviceDisplayName
      , "refresh_token" Aeson..= loginRefreshToken
      ]

data MatrixLoginResponse = MatrixLoginResponse
  { loginUserId :: !Text
  , loginDeviceId :: !(Maybe Text)
  , loginAccessToken :: !Text
  , loginRefreshToken :: !(Maybe Text)
  , loginExpiresInMs :: !(Maybe Integer)
  }
  deriving (Show, Eq, Generic)
    deriving Aeson.FromJSON via (PrefixedSnakeJSON "login" MatrixLoginResponse)

matrixLogin :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => Config -> Text -> Text -> Eff es MatrixLoginResponse
matrixLogin cfg user password = do
  let request = MatrixLoginRequest
        { loginIdentifier = MatrixLoginIdentifier
            { loginIdentifierType = "m.id.user"
            , loginIdentifierUser = user
            }
        , loginPassword = password
        , loginDeviceId = cfg.deviceId
        , loginInitialDeviceDisplayName = Just "cosmobot"
        , loginRefreshToken = True
        }
  matrixUnauthenticatedJsonCall cfg "login" "login" matrixApiOptionsNoAuth
    POST
    (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "login")
    (ReqBodyJson request)

refreshMatrixToken :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => Config -> Text -> Eff es MatrixRefreshResponse
refreshMatrixToken cfg refreshToken = do
  let request = MatrixRefreshRequest refreshToken
  matrixUnauthenticatedJsonCall cfg "refresh" "refresh access token" matrixApiOptionsNoAuth
    POST
    (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "refresh")
    (ReqBodyJson request)

parseMxcUri :: Text -> Maybe (Text, Text)
parseMxcUri ref = do
  rest <- Text.stripPrefix "mxc://" (Text.strip ref)
  let (serverName, mediaWithSlash) = Text.breakOn "/" rest
  mediaId <- Text.stripPrefix "/" mediaWithSlash
  guard (not (Text.null serverName))
  guard (not (Text.null mediaId))
  pure (serverName, mediaId)

withMatrixBaseUrl :: IOE :> es => Text -> (forall scheme. Url scheme -> Option scheme -> Eff es a) -> Eff es a
withMatrixBaseUrl homeserver action = do
  uri <- URI.mkURI homeserver
  case useURI uri of
    Nothing ->
      liftIO (ioError (userError [i|Unsupported Matrix homeserver URL: #{homeserver}. Use a full HTTP or HTTPS base URL.|]))
    Just (Left (baseUrl, baseOptions)) ->
      action baseUrl baseOptions
    Just (Right (baseUrl, baseOptions)) ->
      action baseUrl baseOptions

matrixAuth :: Text -> Option scheme
matrixAuth token =
  header "Authorization" (ByteString.pack [i|Bearer #{token}|])

matrixHttpConfig :: HttpConfig
matrixHttpConfig =
  defaultHttpConfig
    { httpConfigRetryJudge = \_ _ -> False
    , httpConfigRetryJudgeException = \_ _ -> False
    }

nonEmptyMatrixBody :: Text -> Text
nonEmptyMatrixBody body
  | Text.null (Text.strip body) = " "
  | otherwise = body

data RoomEvent = RoomEvent
  { roomId :: !MatrixRoomId
  , roomIsDirect :: !Bool
  , event :: !Event
  }
  deriving (Show)

data SyncResponse = SyncResponse
  { nextBatch :: !Text
  , rooms :: !Rooms
  , accountData :: !AccountData
  }
  deriving (Show, Generic)

instance Aeson.FromJSON SyncResponse where
  parseJSON = Aeson.withObject "SyncResponse" \o ->
    SyncResponse
      <$> o Aeson..: "next_batch"
      <*> o Aeson..:? "rooms" Aeson..!= Rooms Map.empty Map.empty
      <*> o Aeson..:? "account_data" Aeson..!= AccountData []

newtype AccountData = AccountData
  { directRooms :: [Text]
  }
  deriving (Show, Generic)

instance Aeson.FromJSON AccountData where
  parseJSON = Aeson.withObject "AccountData" \o -> do
    events <- o Aeson..:? "events" Aeson..!= []
    pure AccountData
      { directRooms = concatMap accountDataEventDirectRooms events
      }

data AccountDataEvent = AccountDataEvent
  { accountDataEventType :: !Text
  , accountDataEventContent :: !Aeson.Value
  }
  deriving (Show, Generic)

instance Aeson.FromJSON AccountDataEvent where
  parseJSON = Aeson.withObject "AccountDataEvent" \o ->
    AccountDataEvent
      <$> o Aeson..: "type"
      <*> o Aeson..:? "content" Aeson..!= Aeson.Object mempty

accountDataEventDirectRooms :: AccountDataEvent -> [Text]
accountDataEventDirectRooms event
  | event.accountDataEventType == "m.direct" =
      concat (fromMaybe [] (Aeson.parseMaybe parseDirectRooms event.accountDataEventContent))
  | otherwise =
      []
  where
    parseDirectRooms :: Aeson.Value -> Aeson.Parser [[Text]]
    parseDirectRooms =
      Aeson.withObject "m.direct content" \o ->
        traverse Aeson.parseJSON (AesonKeyMap.elems o)

data Rooms = Rooms
  { join :: Map Text JoinedRoom
  , invite :: Map Text Aeson.Value
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Rooms where
  parseJSON = Aeson.withObject "Rooms" \o ->
    Rooms
      <$> o Aeson..:? "join" Aeson..!= Map.empty
      <*> o Aeson..:? "invite" Aeson..!= Map.empty

data JoinedRoom = JoinedRoom
  { timeline :: Timeline
  , summary :: RoomSummary
  }
  deriving (Show, Generic)

instance Aeson.FromJSON JoinedRoom where
  parseJSON = Aeson.withObject "JoinedRoom" \o ->
    JoinedRoom
      <$> o Aeson..:? "timeline" Aeson..!= Timeline []
      <*> o Aeson..:? "summary" Aeson..!= RoomSummary Nothing

newtype RoomSummary = RoomSummary
  { joinedMemberCount :: Maybe Int
  }
  deriving (Show, Generic)

instance Aeson.FromJSON RoomSummary where
  parseJSON = Aeson.withObject "RoomSummary" \o ->
    RoomSummary <$> o Aeson..:? "m.joined_member_count"

newtype JoinedMembersResponse = JoinedMembersResponse
  { joinedMembers :: Map Text MatrixMember
  }
  deriving (Show, Generic)

instance Aeson.FromJSON JoinedMembersResponse where
  parseJSON = Aeson.withObject "JoinedMembersResponse" \o -> do
    joined <- o Aeson..:? "joined" Aeson..!= Map.empty
    pure (JoinedMembersResponse (Map.mapWithKey matrixMemberFromJoined joined))

data MatrixMember = MatrixMember
  { memberUserId :: !Text
  , memberDisplayName :: !(Maybe Text)
  , memberAvatarUrl :: !(Maybe Text)
  , memberMembership :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON MatrixMember where
  toJSON MatrixMember{memberUserId, memberDisplayName, memberAvatarUrl, memberMembership} =
    Aeson.object
      [ "user_id" Aeson..= memberUserId
      , "displayname" Aeson..= memberDisplayName
      , "avatar_url" Aeson..= memberAvatarUrl
      , "membership" Aeson..= memberMembership
      ]

data MatrixMemberContent = MatrixMemberContent
  { memberContentDisplayName :: !(Maybe Text)
  , memberContentAvatarUrl :: !(Maybe Text)
  , memberContentMembership :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON MatrixMemberContent where
  parseJSON = Aeson.withObject "MatrixMemberContent" \o -> do
    displayName <- o Aeson..:? "displayname"
    joinedDisplayName <- o Aeson..:? "display_name"
    MatrixMemberContent
      <$> pure (displayName <|> joinedDisplayName)
      <*> o Aeson..:? "avatar_url"
      <*> o Aeson..:? "membership"

matrixMemberFromContent :: Text -> MatrixMemberContent -> MatrixMember
matrixMemberFromContent userId content =
  MatrixMember
    { memberUserId = userId
    , memberDisplayName = content.memberContentDisplayName
    , memberAvatarUrl = content.memberContentAvatarUrl
    , memberMembership = content.memberContentMembership
    }

matrixMemberFromJoined :: Text -> MatrixMemberContent -> MatrixMember
matrixMemberFromJoined =
  matrixMemberFromContent

data MatrixProfile = MatrixProfile
  { profileUserId :: !Text
  , profileDisplayName :: !(Maybe Text)
  , profileAvatarUrl :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

data MatrixProfileContent = MatrixProfileContent
  { profileContentDisplayName :: !(Maybe Text)
  , profileContentAvatarUrl :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON MatrixProfileContent where
  parseJSON = Aeson.withObject "MatrixProfileContent" \o ->
    MatrixProfileContent
      <$> o Aeson..:? "displayname"
      <*> o Aeson..:? "avatar_url"

newtype Timeline = Timeline
  { events :: [Event]
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Timeline where
  parseJSON = Aeson.withObject "Timeline" \o ->
    Timeline <$> o Aeson..:? "events" Aeson..!= []

data Event = Event
  { type_ :: !Text
  , sender :: !Text
  , eventId :: !(Maybe MatrixEventId)
  , content :: !EventContent
  , raw :: !Aeson.Value
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Event where
  parseJSON value = Aeson.withObject "Event" parse value
    where
      parse o = do
        type_ <- o Aeson..: "type"
        sender <- o Aeson..: "sender"
        eventId <- fmap matrixEventId <$> o Aeson..:? "event_id"
        content <- o Aeson..:? "content" Aeson..!= EventContent Nothing Nothing [] Nothing
        pure Event{type_, sender, eventId, content, raw = value}

data EventContent = EventContent
  { msgtype :: !(Maybe Text)
  , body :: !(Maybe Text)
  , mentions :: ![Text]
  , replyToEventId :: !(Maybe MatrixEventId)
  }
  deriving (Show, Generic)

instance Aeson.FromJSON EventContent where
  parseJSON = Aeson.withObject "EventContent" \o -> do
    msgtype <- o Aeson..:? "msgtype"
    body <- o Aeson..:? "body"
    mentions <- o Aeson..:? "m.mentions" Aeson..!= MatrixMentions []
    replyToEventId <- o Aeson..:? "m.relates_to" Aeson..!= MatrixRelatesTo Nothing
    pure EventContent
      { msgtype
      , body
      , mentions = mentions.userIds
      , replyToEventId = replyToEventId.inReplyToEventId
      }

newtype MatrixMentions = MatrixMentions
  { userIds :: [Text]
  }
  deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON MatrixMentions)

instance Aeson.FromJSON MatrixMentions where
  parseJSON = Aeson.withObject "MatrixMentions" \o ->
    MatrixMentions <$> o Aeson..:? "user_ids" Aeson..!= []

newtype MatrixRelatesTo = MatrixRelatesTo
  { inReplyToEventId :: Maybe MatrixEventId
  }
  deriving (Show, Generic)

instance Aeson.FromJSON MatrixRelatesTo where
  parseJSON = Aeson.withObject "MatrixRelatesTo" \o -> do
    inReplyTo <- o Aeson..:? "m.in_reply_to" Aeson..!= MatrixInReplyTo Nothing
    pure (MatrixRelatesTo inReplyTo.replyEventId)

instance Aeson.ToJSON MatrixRelatesTo where
  toJSON MatrixRelatesTo{inReplyToEventId} =
    Aeson.object
      [ "m.in_reply_to" Aeson..= MatrixInReplyTo inReplyToEventId
      ]

newtype MatrixInReplyTo = MatrixInReplyTo
  { replyEventId :: Maybe MatrixEventId
  }
  deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (PrefixedSnakeJSON "reply" MatrixInReplyTo)

data SendMessageRequest = SendMessageRequest
  { msgtype :: !Text
  , body :: !Text
  , formattedBody :: !(Maybe Text)
  , replyRelation :: !(Maybe MatrixReplyTo)
  , mentions :: !MatrixMentions
  , streamMetadata :: !MatrixStreamMetadata
  }
  deriving (Show, Generic)

instance Aeson.ToJSON SendMessageRequest where
  toJSON SendMessageRequest{msgtype, body, formattedBody, replyRelation, mentions, streamMetadata} =
    Aeson.object $
      [ "msgtype" Aeson..= msgtype
      , "body" Aeson..= body
      , "m.mentions" Aeson..= mentions
      , matrixStreamMetadataPair streamMetadata
      ]
        <> matrixFormattedBodyFields formattedBody
        <> maybe [] (\(MatrixReplyTo eventId) -> ["m.relates_to" Aeson..= MatrixRelatesTo (Just eventId)]) replyRelation

newtype MatrixStreamMetadata = MatrixStreamMetadata
  { streamComplete :: Bool
  }
  deriving (Show, Generic)

instance Aeson.FromJSON MatrixStreamMetadata where
  parseJSON = Aeson.withObject "MatrixStreamMetadata" \o ->
    MatrixStreamMetadata <$> o Aeson..: "complete"

instance Aeson.ToJSON MatrixStreamMetadata where
  toJSON MatrixStreamMetadata{streamComplete} =
    Aeson.object ["complete" Aeson..= streamComplete]

matrixStreamMetadataPair :: MatrixStreamMetadata -> Aeson.Pair
matrixStreamMetadataPair metadata =
  "com.pfeiwu.ai.stream" Aeson..= metadata

newtype MatrixUploadResponse = MatrixUploadResponse
  { contentUri :: Text
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON MatrixUploadResponse)

data MatrixFileInfo = MatrixFileInfo
  { mimetype :: !Text
  , size :: !Integer
  }
  deriving (Show, Generic, Aeson.ToJSON)

data MatrixFileMessage = MatrixFileMessage
  { msgtype :: !Text
  , body :: !Text
  , filename :: !Text
  , url :: !Text
  , info :: !MatrixFileInfo
  }
  deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON MatrixFileMessage)

data MatrixFileMessageRequest = MatrixFileMessageRequest
  { message :: !MatrixFileMessage
  , replyRelation :: !(Maybe MatrixReplyTo)
  , streamMetadata :: !MatrixStreamMetadata
  }
  deriving (Show, Generic)

instance Aeson.ToJSON MatrixFileMessageRequest where
  toJSON MatrixFileMessageRequest{message, replyRelation, streamMetadata} =
    case Aeson.toJSON message of
      Aeson.Object fields ->
        Aeson.Object (fields <> AesonKeyMap.fromList relationFields)
      value ->
        value
    where
      relationFields =
        [("com.pfeiwu.ai.stream", Aeson.toJSON streamMetadata)]
          <> maybe [] (\(MatrixReplyTo eventId) -> [("m.relates_to", Aeson.toJSON (MatrixRelatesTo (Just eventId)))]) replyRelation

data MatrixEditMessageRequest = MatrixEditMessageRequest
  { body :: !Text
  , formattedBody :: !(Maybe Text)
  , mentions :: !MatrixMentions
  , replacesEventId :: !MatrixEventId
  , streamMetadata :: !MatrixStreamMetadata
  }
  deriving (Show, Generic)

completeMatrixEditMessageRequest :: MatrixEditMessageRequest -> MatrixEditMessageRequest
completeMatrixEditMessageRequest request =
  MatrixEditMessageRequest
    { body = request.body
    , formattedBody = request.formattedBody
    , mentions = request.mentions
    , replacesEventId = request.replacesEventId
    , streamMetadata = MatrixStreamMetadata True
    }

instance Aeson.ToJSON MatrixEditMessageRequest where
  toJSON MatrixEditMessageRequest{body, formattedBody, mentions, replacesEventId, streamMetadata} =
    Aeson.object $
      [ "msgtype" Aeson..= ("m.text" :: Text)
      , "body" Aeson..= ("* " <> body)
      , "m.new_content" Aeson..= Aeson.object
          ( [ "msgtype" Aeson..= ("m.text" :: Text)
            , "body" Aeson..= body
            , "m.mentions" Aeson..= mentions
            , matrixStreamMetadataPair streamMetadata
            ]
              <> matrixFormattedBodyFields formattedBody
          )
      , "m.mentions" Aeson..= mentions
      , matrixStreamMetadataPair streamMetadata
      , "m.relates_to" Aeson..= Aeson.object
          [ "rel_type" Aeson..= ("m.replace" :: Text)
          , "event_id" Aeson..= matrixEventIdText replacesEventId
          ]
      ]
        <> matrixFormattedBodyFields (("* " <>) <$> formattedBody)

matrixFormattedBodyFields :: Maybe Text -> [Aeson.Pair]
matrixFormattedBodyFields = \case
  Nothing ->
    []
  Just html ->
    [ "format" Aeson..= ("org.matrix.custom.html" :: Text)
    , "formatted_body" Aeson..= html
    ]

data RedactEventRequest = RedactEventRequest
  { reason :: Maybe Text
  }
  deriving (Show, Generic, Aeson.ToJSON)

data SetTypingRequest = SetTypingRequest
  { timeout :: Int
  }
  deriving (Show, Generic)

instance Aeson.ToJSON SetTypingRequest where
  toJSON SetTypingRequest{timeout} =
    Aeson.object
      [ "typing" Aeson..= True
      , "timeout" Aeson..= timeout
      ]

newtype SendMessageResponse = SendMessageResponse
  { eventId :: MatrixEventId
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON SendMessageResponse)

newtype RedactEventResponse = RedactEventResponse
  { redactionEventId :: Text
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (PrefixedSnakeJSON "redaction" RedactEventResponse)

matrixSyncTimeoutMilliseconds :: MatrixSyncMode -> Int
matrixSyncTimeoutMilliseconds = \case
  MatrixInitialSync ->
    0
  MatrixLongPollSync ->
    matrixLongPollSyncTimeoutMilliseconds

matrixLongPollSyncTimeoutMilliseconds :: Int
matrixLongPollSyncTimeoutMilliseconds = 30000

matrixSyncResponseTimeoutMicroseconds :: Int
matrixSyncResponseTimeoutMicroseconds = 40000000

matrixApiResponseTimeoutMicroseconds :: Int
matrixApiResponseTimeoutMicroseconds = 10000000

matrixMediaDownloadResponseTimeoutMicroseconds :: Int
matrixMediaDownloadResponseTimeoutMicroseconds = 60000000

matrixReloginAttempts :: Int
matrixReloginAttempts = 12

matrixReloginDelaySeconds :: Int
matrixReloginDelaySeconds = 180

matrixReloginDelayMicroseconds :: Int
matrixReloginDelayMicroseconds = matrixReloginDelaySeconds * 1000000

matrixRetryDelayMicroseconds :: Int
matrixRetryDelayMicroseconds = 5000000
