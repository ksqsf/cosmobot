{-|
Module      : Bot.Chat.Driver.Matrix
Description : Matrix Client-Server chat driver
Stability   : experimental
-}
{-# LANGUAGE OverloadedLabels #-}

module Bot.Chat.Driver.Matrix
  ( matrixDriver
  , Matrix
  , Config (..)
  , SyncResponse (..)
  , JoinedRoom (..)
  , Timeline (..)
  , Event (..)
  , EventContent (..)
  , SendMessageResponse (..)
  , RoomEvent (..)
  , runMatrix
  , incomingMessages
  , eventToIncomingMessage
  , eventToIncomingMessageWith
  , replyTo
  , replyAudio
  , uploadFile
  , deleteMessage
  )
where

import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Storage.Matrix as MatrixStorage
import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Util.HTTP as Http
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.Prim.IORef as IORef
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Req
import qualified Network.HTTP.Types.Status as HTTPStatus
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import System.FilePath ((</>), (<.>), takeExtension, takeFileName)
import System.IO.Error (ioError, userError)
import qualified Text.URI as URI

data Config = Config
  { homeserver :: !Text
  , loginUser :: !(Maybe Text)
  , loginPassword :: !(Maybe Text)
  , deviceId :: !(Maybe Text)
  , directRooms :: ![Text]
  , userId :: !(Maybe Text)
  , allowedRooms :: ![Text]
  , superusers :: ![Text]
  }
  deriving (Show)

newtype MatrixRoomId = MatrixRoomId Text
  deriving (Show, Eq, Ord)

newtype MatrixEventId = MatrixEventId Text
  deriving (Show, Eq, Ord)

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

matrixEventMessageId :: MatrixEventId -> MessageId
matrixEventMessageId =
  textMessageId . matrixEventIdText

instance IsString MatrixRoomId where
  fromString =
    matrixRoomId . Text.pack

instance IsString MatrixEventId where
  fromString =
    matrixEventId . Text.pack

matrixDriver
  :: (Matrix :> es, FileSystem :> es, IOE :> es)
  => Driver.ChatPlatformDriver es
matrixDriver = Driver.ChatPlatformDriver
  { Driver.platform = PlatformMatrix
  , Driver.replyTo = replyTo
  , Driver.replyAudio = replyAudio
  , Driver.uploadFile = uploadFile
  , Driver.editMessage = \_ _ _ -> pure False
  , Driver.deleteMessage = deleteMessage
  , Driver.replyStreamStyle = \_ -> pure (Chat.ChunkedReply matrixStreamingMessageLimit)
  , Driver.getMessageContent = \_ _ -> pure Nothing
  , Driver.getSenderMemberInfo = \_ -> pure Nothing
  , Driver.getMemberInfo = \_ _ -> pure Nothing
  , Driver.getUserAvatar = \_ _ -> pure Nothing
  , Driver.listGroupMembers = \_ -> pure Nothing
  , Driver.mentionUser = \_ _ _ -> pure Nothing
  , Driver.setMemberTitle = \_ _ _ -> pure False
  }

matrixStreamingMessageLimit :: Int
matrixStreamingMessageLimit = 4000

data Matrix :: Effect where
  MatrixConfig :: Matrix m Config
  LoadSyncToken :: Matrix m (Maybe Text)
  StoreSyncToken :: Text -> Matrix m ()
  Sync :: Maybe Text -> Matrix m (Maybe SyncResponse)
  DirectRooms :: Matrix m (Set MatrixRoomId)
  JoinedMemberCounts :: Matrix m (Map MatrixRoomId Int)
  JoinedMemberCount :: MatrixRoomId -> Matrix m (Maybe Int)
  SendText :: MatrixRoomId -> Maybe MatrixReplyTo -> Text -> Matrix m (Maybe SendMessageResponse)
  UploadMedia :: FilePath -> Text -> Text -> Matrix m MatrixUploadResponse
  SendFileMessage :: Text -> Maybe MatrixReplyTo -> MatrixFileMessage -> Matrix m (Maybe SendMessageResponse)
  DeleteEvent :: Text -> MessageId -> Maybe MatrixEventId -> Matrix m Bool

type instance DispatchOf Matrix = Dynamic

matrixConfig :: Matrix :> es => Eff es Config
matrixConfig = send MatrixConfig

loadSyncToken :: Matrix :> es => Eff es (Maybe Text)
loadSyncToken =
  send LoadSyncToken

storeSyncToken :: Matrix :> es => Text -> Eff es ()
storeSyncToken =
  send . StoreSyncToken

sync :: Matrix :> es => Maybe Text -> Eff es (Maybe SyncResponse)
sync =
  send . Sync

directRooms :: Matrix :> es => Eff es (Set MatrixRoomId)
directRooms =
  send DirectRooms

joinedMemberCounts :: Matrix :> es => Eff es (Map MatrixRoomId Int)
joinedMemberCounts =
  send JoinedMemberCounts

joinedMemberCount :: Matrix :> es => MatrixRoomId -> Eff es (Maybe Int)
joinedMemberCount =
  send . JoinedMemberCount

sendText :: Matrix :> es => MatrixRoomId -> Maybe MatrixReplyTo -> Text -> Eff es (Maybe SendMessageResponse)
sendText roomId replyToEventId body =
  send (SendText roomId replyToEventId body)

uploadMedia :: Matrix :> es => FilePath -> Text -> Text -> Eff es MatrixUploadResponse
uploadMedia path fileName mime =
  send (UploadMedia path fileName mime)

sendFileMessage :: Matrix :> es => Text -> Maybe MatrixReplyTo -> MatrixFileMessage -> Eff es (Maybe SendMessageResponse)
sendFileMessage roomId replyRelation message =
  send (SendFileMessage roomId replyRelation message)

deleteEvent :: Matrix :> es => Text -> MessageId -> Maybe MatrixEventId -> Eff es Bool
deleteEvent roomId messageId eventId =
  send (DeleteEvent roomId messageId eventId)

runMatrix
  :: (IOE :> es, Log :> es, Concurrent :> es, Prim :> es, Storage.Storage :> es)
  => Config
  -> Eff (Matrix : es) a
  -> Eff es a
runMatrix cfg inner = do
  manager <- liftIO Http.newTlsManager
  eventIds <- IORef.newIORef (Map.empty :: Map MessageId MatrixEventId)
  directRoomIdsRef <- IORef.newIORef (Set.fromList (matrixRoomId <$> cfg.directRooms))
  joinedMemberCountsRef <- IORef.newIORef (Map.empty :: Map MatrixRoomId Int)
  initialAuthState <- initialMatrixAuthState manager cfg
  authState <- IORef.newIORef initialAuthState
  refreshLock <- MVar.newMVar ()
  let auth = MatrixAuth manager cfg authState refreshLock
  interpret
    ( \_ -> \case
        MatrixConfig ->
          pure cfg
        LoadSyncToken ->
          MatrixStorage.loadSyncToken
        StoreSyncToken token ->
          MatrixStorage.saveSyncToken token
        Sync since -> do
          response <- withMaybeMatrixAccessToken auth \token ->
            syncCall manager cfg since token
          traverse_ (rememberMatrixRoomState directRoomIdsRef joinedMemberCountsRef) response
          pure response
        DirectRooms ->
          IORef.readIORef directRoomIdsRef
        JoinedMemberCounts ->
          IORef.readIORef joinedMemberCountsRef
        JoinedMemberCount roomId ->
          withMaybeMatrixAccessToken auth \token -> do
            count <- joinedMemberCountCall manager cfg token roomId
            rememberJoinedMemberCount directRoomIdsRef joinedMemberCountsRef roomId count
            pure count
        SendText roomId replyToEventId body -> do
          response <- withMaybeMatrixAccessToken auth \token ->
            sendMessageCall manager cfg token roomId replyToEventId body
          traverse_ (rememberMatrixEvent eventIds) response
          pure response
        UploadMedia path fileName mime ->
          withMatrixAccessToken auth \token ->
            uploadMediaCall manager cfg path fileName mime token
        SendFileMessage roomId replyRelation message -> do
          response <- withMaybeMatrixAccessToken auth \token ->
            sendFileMessageCall manager cfg token roomId replyRelation message
          traverse_ (rememberMatrixEvent eventIds) response
          pure response
        DeleteEvent roomId messageId knownEventId -> do
          stored <- IORef.readIORef eventIds
          case knownEventId <|> Map.lookup messageId stored of
            Nothing ->
              pure False
            Just eventId ->
              fromMaybe False <$> withMaybeMatrixAccessToken auth \token ->
                redactEventCall manager cfg token roomId (matrixEventIdText eventId) $> True
    )
    inner

rememberMatrixEvent :: Prim :> es => IORef.IORef (Map MessageId MatrixEventId) -> SendMessageResponse -> Eff es ()
rememberMatrixEvent eventIds response =
  IORef.modifyIORef' eventIds (Map.insert (matrixEventMessageId response.eventId) response.eventId)

rememberMatrixRoomState
  :: Prim :> es
  => IORef.IORef (Set MatrixRoomId)
  -> IORef.IORef (Map MatrixRoomId Int)
  -> SyncResponse
  -> Eff es ()
rememberMatrixRoomState directRoomIdsRef joinedMemberCountsRef response = do
  IORef.modifyIORef' directRoomIdsRef (<> syncDirectRoomIds response)
  for_ (syncJoinedMemberCounts response) \(roomId, count) ->
    rememberJoinedMemberCount directRoomIdsRef joinedMemberCountsRef roomId count

rememberJoinedMemberCount
  :: Prim :> es
  => IORef.IORef (Set MatrixRoomId)
  -> IORef.IORef (Map MatrixRoomId Int)
  -> MatrixRoomId
  -> Int
  -> Eff es ()
rememberJoinedMemberCount directRoomIdsRef joinedMemberCountsRef roomId count = do
  IORef.modifyIORef' joinedMemberCountsRef (Map.insert roomId count)
  IORef.modifyIORef' directRoomIdsRef \directRoomIds ->
    if count == 2
      then Set.insert roomId directRoomIds
      else Set.delete roomId directRoomIds

data MatrixAuth = MatrixAuth
  { authManager :: !Manager
  , authConfig :: !Config
  , authState :: !(IORef.IORef MatrixAuthState)
  , authRefreshLock :: !(MVar.MVar ())
  }

data MatrixAuthState = MatrixAuthState
  { authAccessToken :: !(Maybe Text)
  , authRefreshToken :: !(Maybe Text)
  }
  deriving (Show, Eq)

instance Aeson.FromJSON MatrixAuthState where
  parseJSON = Aeson.withObject "MatrixAuthState" \o ->
    MatrixAuthState
      <$> o Aeson..:? "access_token"
      <*> o Aeson..:? "refresh_token"

instance Aeson.ToJSON MatrixAuthState where
  toJSON MatrixAuthState{authAccessToken, authRefreshToken} =
    Aeson.object
      [ "access_token" Aeson..= authAccessToken
      , "refresh_token" Aeson..= authRefreshToken
      ]

initialMatrixAuthState :: (IOE :> es, Log :> es) => Manager -> Config -> Eff es MatrixAuthState
initialMatrixAuthState manager cfg =
  case (cfg.loginUser, cfg.loginPassword) of
    (Just user, Just password) -> do
      response <- loginCall manager cfg user password
      pure MatrixAuthState
        { authAccessToken = Just response.loginAccessToken
        , authRefreshToken = response.loginRefreshToken
        }
    _ ->
      pure MatrixAuthState{authAccessToken = Nothing, authRefreshToken = Nothing}

withMaybeMatrixAccessToken
  :: (IOE :> es, Log :> es, Concurrent :> es, Prim :> es)
  => MatrixAuth
  -> (Text -> Eff es a)
  -> Eff es (Maybe a)
withMaybeMatrixAccessToken auth action = do
  currentAuthState <- IORef.readIORef auth.authState
  case (currentAuthState.authAccessToken, currentAuthState.authRefreshToken) of
    (Nothing, Nothing) ->
      pure Nothing
    _ ->
      Just <$> withMatrixAccessToken auth action

withMatrixAccessToken
  :: (IOE :> es, Log :> es, Concurrent :> es, Prim :> es)
  => MatrixAuth
  -> (Text -> Eff es a)
  -> Eff es a
withMatrixAccessToken auth action = do
  currentAuthState <- IORef.readIORef auth.authState
  token <- case currentAuthState.authAccessToken of
    Just accessToken ->
      pure accessToken
    Nothing ->
      refreshMatrixAccessToken auth ""
  action token `catch` \(err :: MatrixApiException) ->
    if matrixAccessTokenExpired err
      then do
        refreshed <- refreshMatrixAccessToken auth token
        action refreshed `catch` \(retryErr :: MatrixApiException) ->
          throwIO (userError (Text.unpack (matrixApiExceptionMessage retryErr)))
      else
        throwIO (userError (Text.unpack (matrixApiExceptionMessage err)))

refreshMatrixAccessToken
  :: (IOE :> es, Log :> es, Concurrent :> es, Prim :> es)
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
            logInfo_ "Matrix access token expired; refreshing"
            response <- refreshAccessTokenCall auth.authManager auth.authConfig refreshToken
            let refreshedState = MatrixAuthState
                  { authAccessToken = Just response.refreshedAccessToken
                  , authRefreshToken = response.refreshedRefreshToken <|> currentAuthState.authRefreshToken
                  }
            IORef.writeIORef auth.authState refreshedState
            pure response.refreshedAccessToken

reloginMatrixAccessToken
  :: (IOE :> es, Log :> es, Prim :> es)
  => MatrixAuth
  -> Eff es Text
reloginMatrixAccessToken auth =
  case (auth.authConfig.loginUser, auth.authConfig.loginPassword) of
    (Just user, Just password) -> do
      logInfo_ "Matrix access token expired and no refresh token is available; logging in again"
      response <- loginCall auth.authManager auth.authConfig user password
      let refreshedState = MatrixAuthState
            { authAccessToken = Just response.loginAccessToken
            , authRefreshToken = response.loginRefreshToken
            }
      IORef.writeIORef auth.authState refreshedState
      pure response.loginAccessToken
    _ ->
      throwIO (userError "Matrix access token expired and no refresh token or login credentials are configured.")

matrixAccessTokenExpired :: MatrixApiException -> Bool
matrixAccessTokenExpired = \case
  MatrixApiException _ status err ->
    HTTPStatus.statusCode status == 401 && err.errcode == "M_UNKNOWN_TOKEN"
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

matrixErrorResponseText :: MatrixErrorResponse -> Text
matrixErrorResponseText err =
  Text.intercalate "; " $
    [ err.errcode <> maybe "" (": " <>) err.matrixError
    ]
      <> maybe [] (\retry -> [[i|retry_after_ms=#{retry}|]]) err.retryAfterMs
      <> if err.softLogout then ["soft_logout=true"] else []

matrixApiException :: Text -> HttpException -> MatrixApiException
matrixApiException method = \case
  VanillaHttpException (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException response body)) ->
    case Aeson.eitherDecodeStrict body of
      Right err ->
        MatrixApiException method (HTTP.responseStatus response) err
      Left parseErr ->
        MatrixTransportException method [i|HTTP #{HTTPStatus.statusCode (HTTP.responseStatus response)} with non-Matrix error body: #{parseErr}|]
  VanillaHttpException (HTTP.HttpExceptionRequest _ content) ->
    MatrixTransportException method [i|HTTP transport error: #{show content :: String}|]
  VanillaHttpException err ->
    MatrixTransportException method [i|HTTP error: #{show err :: String}|]
  JsonHttpException message ->
    MatrixTransportException method [i|JSON error: #{message}|]

matrixReq :: IOE :> es => Text -> IO a -> Eff es a
matrixReq method action =
  liftIO action `catch` \(err :: HttpException) ->
    throwIO (matrixApiException method err)

newtype MatrixRefreshRequest = MatrixRefreshRequest
  { requestRefreshToken :: Text
  }

instance Aeson.ToJSON MatrixRefreshRequest where
  toJSON MatrixRefreshRequest{requestRefreshToken} =
    Aeson.object
      [ "refresh_token" Aeson..= requestRefreshToken
      ]

data MatrixRefreshResponse = MatrixRefreshResponse
  { refreshedAccessToken :: !Text
  , refreshedRefreshToken :: !(Maybe Text)
  , refreshedExpiresInMs :: !(Maybe Integer)
  }
  deriving (Show, Eq)

instance Aeson.FromJSON MatrixRefreshResponse where
  parseJSON = Aeson.withObject "MatrixRefreshResponse" \o ->
    MatrixRefreshResponse
      <$> o Aeson..: "access_token"
      <*> o Aeson..:? "refresh_token"
      <*> o Aeson..:? "expires_in_ms"

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
  deriving (Show, Eq)

instance Aeson.FromJSON MatrixLoginResponse where
  parseJSON = Aeson.withObject "MatrixLoginResponse" \o ->
    MatrixLoginResponse
      <$> o Aeson..: "user_id"
      <*> o Aeson..:? "device_id"
      <*> o Aeson..: "access_token"
      <*> o Aeson..:? "refresh_token"
      <*> o Aeson..:? "expires_in_ms"

incomingMessages :: (Matrix :> es, Log :> es, IOE :> es, Concurrent :> es) => Stream (Of IncomingMessage) (Eff es) ()
incomingMessages = do
  cfg <- S.lift matrixConfig
  if matrixAuthConfigured cfg
    then do
      S.lift $ logInfo_ [i|Matrix sync starting: auth=#{matrixAuthMode cfg}|]
      storedSince <- S.lift loadSyncToken
      case storedSince of
        Just since ->
          syncLoop cfg (Just since)
        Nothing -> do
          S.lift $ logInfo_ "Matrix sync state is empty; initializing from current homeserver state"
          initializeSyncState cfg
    else S.lift $ logInfo_ "Matrix driver disabled: no access token, refresh token, or login credentials configured"
  where
    initializeSyncState cfg = do
      result <- S.lift $ sync Nothing `catchSync` \err -> do
        logInfo_ [i|Matrix sync initialization failed, retrying: #{show err :: String}|]
        threadDelay matrixRetryDelayMicroseconds
        pure Nothing
      case result of
        Nothing ->
          initializeSyncState cfg
        Just response -> do
          S.lift $ storeSyncToken response.nextBatch
          S.lift $ logInfo_ "Matrix sync state initialized; skipped initial timeline batch"
          syncLoop cfg (Just response.nextBatch)

    syncLoop cfg since = do
      result <- S.lift $ sync since `catchSync` \err -> do
        logInfo_ [i|Matrix sync failed, retrying: #{show err :: String}|]
        threadDelay matrixRetryDelayMicroseconds
        pure Nothing
      case result of
        Nothing ->
          syncLoop cfg since
        Just response -> do
          directRoomIds <- S.lift directRooms
          joinedCounts <- S.lift joinedMemberCounts
          probedDirectRoomIds <- S.lift (probeDirectRoomIds directRoomIds joinedCounts response)
          refreshedDirectRoomIds <- S.lift directRooms
          let effectiveDirectRoomIds = refreshedDirectRoomIds <> probedDirectRoomIds
              events = syncEvents effectiveDirectRoomIds response
              directCount = Set.size effectiveDirectRoomIds
          S.lift $ logInfo_ [i|Matrix sync batch: #{length events}; direct_rooms=#{directCount}|]
          for_ events \event ->
            case eventToIncomingMessageWith cfg event of
              Nothing -> do
                let reason = matrixEventIgnoreReason cfg event
                S.lift $ logTrace_ ("Ignoring Matrix event: " <> reason)
                S.lift $ logInfo_ ("Ignoring Matrix event: " <> reason)
              Just message -> do
                S.lift $ logTrace "incoming Matrix message" message
                S.lift $ logInfo_ [i|incoming Matrix message: #{matrixIncomingLogLine event} #{incomingMessageLogLine message}|]
                S.yield message
          S.lift $ storeSyncToken response.nextBatch
          syncLoop cfg (Just response.nextBatch)

syncEvents :: Set MatrixRoomId -> SyncResponse -> [RoomEvent]
syncEvents directRoomIds response =
  [ RoomEvent
      { roomId
      , roomIsDirect = roomId `Set.member` directRoomIds || roomLooksDirect room
      , event
      }
  | (roomIdText, room) <- Map.toList response.rooms.join
  , let roomId = matrixRoomId roomIdText
  , event <- room.timeline.events
  ]

syncDirectRoomIds :: SyncResponse -> Set MatrixRoomId
syncDirectRoomIds =
  Set.fromList . fmap matrixRoomId . (.directRooms) . (.accountData)

syncJoinedMemberCounts :: SyncResponse -> [(MatrixRoomId, Int)]
syncJoinedMemberCounts response =
  [ (roomId, count)
  | (roomIdText, room) <- Map.toList response.rooms.join
  , let roomId = matrixRoomId roomIdText
  , Just count <- [room.summary.joinedMemberCount]
  ]

probeDirectRoomIds :: Matrix :> es => Set MatrixRoomId -> Map MatrixRoomId Int -> SyncResponse -> Eff es (Set MatrixRoomId)
probeDirectRoomIds knownDirectRoomIds joinedCounts response =
  Set.fromList <$> filterM looksDirect roomIdsToProbe
  where
    roomIdsToProbe =
      [ roomId
      | (roomIdText, room) <- Map.toList response.rooms.join
      , let roomId = matrixRoomId roomIdText
      , not (roomId `Set.member` knownDirectRoomIds)
      , not (roomId `Map.member` joinedCounts)
      , isNothing room.summary.joinedMemberCount
      ]

    looksDirect roomId =
      (== Just 2) <$> joinedMemberCount roomId

roomLooksDirect :: JoinedRoom -> Bool
roomLooksDirect room =
  room.summary.joinedMemberCount == Just 2

matrixIncomingLogLine :: RoomEvent -> Text
matrixIncomingLogLine RoomEvent{roomId, roomIsDirect} =
  [i|room_id=#{roomId} direct=#{roomIsDirect}|]

matrixAuthConfigured :: Config -> Bool
matrixAuthConfigured cfg =
  isJust cfg.loginUser && isJust cfg.loginPassword

matrixAuthMode :: Config -> Text
matrixAuthMode cfg
  | isJust cfg.loginUser && isJust cfg.loginPassword = "login"
  | otherwise = "none"

replyTo :: (Matrix :> es, FileSystem :> es, IOE :> es) => IncomingMessage -> Text -> Eff es (Maybe MessageId)
replyTo message body =
  case (message.platform, viaNonEmpty head message.chatAliases) of
    (PlatformMatrix, Just roomId) -> do
      let matrixRoom = matrixRoomId roomId
          replyRelation = matrixReplyTo message
          text = Chat.renderReplyBody body
          imageRefs = Chat.replyImageUrls body
      textResponse <- if Text.null (Text.strip text)
        then pure Nothing
        else sendText matrixRoom replyRelation text
      imageResponses <- traverse (sendMatrixImage roomId replyRelation) imageRefs
      let imageMessageIds = map (matrixEventMessageId . (.eventId)) (catMaybes imageResponses)
      pure (matrixEventMessageId . (.eventId) <$> textResponse <|> viaNonEmpty head imageMessageIds)
    _ ->
      pure Nothing

matrixReplyTo :: IncomingMessage -> Maybe MatrixReplyTo
matrixReplyTo message =
  MatrixReplyTo <$> (matrixRawEventId message.raw <|> (matrixEventId . messageIdText <$> message.messageId))

uploadFile :: (Matrix :> es, FileSystem :> es, IOE :> es) => IncomingMessage -> FilePath -> Eff es (Either Text (Maybe MessageId))
uploadFile message path =
  case (message.platform, viaNonEmpty head message.chatAliases) of
    (PlatformMatrix, Just roomId) -> do
      let fileName = matrixUploadFileName path
      size <- FileSystem.getFileSize path
      uploaded <- uploadMedia path fileName "application/octet-stream"
      response <- sendFileMessage roomId (matrixReplyTo message) MatrixFileMessage
        { msgtype = "m.file"
        , body = fileName
        , filename = fileName
        , url = uploaded.contentUri
        , info = MatrixFileInfo
            { mimetype = "application/octet-stream"
            , size = size
            }
        }
      pure (Right (matrixEventMessageId . (.eventId) <$> response))
    _ ->
      pure (Left "Matrix file upload requires a Matrix room id.")

sendMatrixImage
  :: (Matrix :> es, FileSystem :> es, IOE :> es)
  => Text
  -> Maybe MatrixReplyTo
  -> Text
  -> Eff es (Maybe SendMessageResponse)
sendMatrixImage roomId replyRelation imageRef =
  case matrixMxcRef imageRef of
    Just contentUri ->
      sendMatrixImageMessage roomId replyRelation "image" contentUri "application/octet-stream" 0
    Nothing ->
      withMatrixImageFile imageRef \path fileName mime -> do
        size <- FileSystem.getFileSize path
        uploaded <- uploadMedia path fileName mime
        sendMatrixImageMessage roomId replyRelation fileName uploaded.contentUri mime size

sendMatrixImageMessage
  :: Matrix :> es
  => Text
  -> Maybe MatrixReplyTo
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Eff es (Maybe SendMessageResponse)
sendMatrixImageMessage roomId replyRelation fileName contentUri mime size =
  sendFileMessage roomId replyRelation MatrixFileMessage
    { msgtype = "m.image"
    , body = fileName
    , filename = fileName
    , url = contentUri
    , info = MatrixFileInfo
        { mimetype = mime
        , size = size
        }
    }

replyAudio :: (Matrix :> es, FileSystem :> es, IOE :> es) => IncomingMessage -> Text -> Maybe Text -> Eff es (Either Text (Maybe MessageId))
replyAudio message audioRef caption =
  case (message.platform, viaNonEmpty head message.chatAliases) of
    (PlatformMatrix, Just roomId) ->
      sendMatrixAudio roomId audioRef caption
    _ ->
      pure (Left "Matrix audio reply requires a Matrix room id.")

sendMatrixAudio :: (Matrix :> es, FileSystem :> es, IOE :> es) => Text -> Text -> Maybe Text -> Eff es (Either Text (Maybe MessageId))
sendMatrixAudio roomId audioRef caption =
  case matrixMxcRef audioRef of
    Just contentUri -> do
      let fileName = "audio"
      response <- sendFileMessage roomId Nothing (matrixAudioMessage caption fileName contentUri "application/octet-stream" 0)
      pure (Right (matrixEventMessageId . (.eventId) <$> response))
    Nothing ->
      withMatrixAudioFile audioRef \path fileName mime -> do
        size <- FileSystem.getFileSize path
        uploaded <- uploadMedia path fileName mime
        response <- sendFileMessage roomId Nothing (matrixAudioMessage caption fileName uploaded.contentUri mime size)
        pure (Right (matrixEventMessageId . (.eventId) <$> response))

matrixAudioMessage :: Maybe Text -> Text -> Text -> Text -> Integer -> MatrixFileMessage
matrixAudioMessage caption fileName contentUri mime size =
  MatrixFileMessage
    { msgtype = "m.audio"
    , body = fromMaybe fileName (caption >>= nonEmptyText)
    , filename = fileName
    , url = contentUri
    , info = MatrixFileInfo
        { mimetype = mime
        , size = size
        }
    }

matrixUploadFileName :: FilePath -> Text
matrixUploadFileName path =
  let name = Text.pack (takeFileName path)
  in if Text.null name then "file" else name

matrixMxcRef :: Text -> Maybe Text
matrixMxcRef ref =
  let stripped = Text.strip ref
  in stripped <$ guard ("mxc://" `Text.isPrefixOf` stripped)

withMatrixImageFile
  :: (FileSystem :> es, IOE :> es)
  => Text
  -> (FilePath -> Text -> Text -> Eff es a)
  -> Eff es a
withMatrixImageFile imageRef action =
  case matrixLocalPath imageRef of
    Just path ->
      action path (matrixUploadFileName path) (matrixImageMimeType path)
    Nothing ->
      case matrixDataImage imageRef of
        Just (mime, bytes) ->
          withTemporaryMatrixImage mime bytes \path ->
            action path (matrixUploadFileName path) mime
        Nothing ->
          throwIO (userError "Matrix image reply requires a file://, data:image/*, or mxc:// image reference.")

withMatrixAudioFile
  :: (FileSystem :> es, IOE :> es)
  => Text
  -> (FilePath -> Text -> Text -> Eff es (Either Text (Maybe MessageId)))
  -> Eff es (Either Text (Maybe MessageId))
withMatrixAudioFile audioRef action =
  case matrixLocalPath audioRef of
    Just path ->
      action path (matrixUploadFileName path) (matrixAudioMimeType path)
    Nothing ->
      case matrixDataAudio audioRef of
        Just (mime, bytes) ->
          withTemporaryMatrixAudio mime bytes \path ->
            action path (matrixUploadFileName path) mime
        Nothing ->
          pure (Left "Matrix audio reply requires a file://, data:audio/*, or mxc:// audio reference.")

matrixLocalPath :: Text -> Maybe FilePath
matrixLocalPath ref =
  let stripped = Text.strip ref
  in case Text.stripPrefix "file://" stripped of
    Just path ->
      Just (Text.unpack path)
    Nothing
      | isLocalPathRef stripped ->
          Just (Text.unpack stripped)
      | otherwise ->
          Nothing

isLocalPathRef :: Text -> Bool
isLocalPathRef ref =
  "/" `Text.isPrefixOf` ref || "./" `Text.isPrefixOf` ref || "../" `Text.isPrefixOf` ref

matrixDataAudio :: Text -> Maybe (Text, StrictByteString.ByteString)
matrixDataAudio ref = do
  rest <- Text.stripPrefix "data:audio/" (Text.strip ref)
  let (subtype, encodedWithMarker) = Text.breakOn ";base64," rest
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  bytes <- either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encoded))
  pure ("audio/" <> subtype, bytes)

matrixDataImage :: Text -> Maybe (Text, StrictByteString.ByteString)
matrixDataImage ref = do
  rest <- Text.stripPrefix "data:image/" (Text.strip ref)
  let (subtype, encodedWithMarker) = Text.breakOn ";base64," rest
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  bytes <- either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encoded))
  pure ("image/" <> subtype, bytes)

withTemporaryMatrixImage
  :: (FileSystem :> es, IOE :> es)
  => Text
  -> StrictByteString.ByteString
  -> (FilePath -> Eff es a)
  -> Eff es a
withTemporaryMatrixImage mime bytes action = do
  FileSystem.createDirectoryIfMissing True matrixTempDir
  nonce <- liftIO getMonotonicTimeNSec
  let path = matrixTempDir </> ("matrix-image-" <> show nonce <.> matrixImageExtension mime)
  FileSystemByteString.writeFile path bytes
  action path `finally` cleanup path
  where
    cleanup path =
      FileSystem.removeFile path `catchSync` \_ -> pure ()

withTemporaryMatrixAudio
  :: (FileSystem :> es, IOE :> es)
  => Text
  -> StrictByteString.ByteString
  -> (FilePath -> Eff es a)
  -> Eff es a
withTemporaryMatrixAudio mime bytes action = do
  FileSystem.createDirectoryIfMissing True matrixTempDir
  nonce <- liftIO getMonotonicTimeNSec
  let path = matrixTempDir </> ("matrix-audio-" <> show nonce <.> matrixAudioExtension mime)
  FileSystemByteString.writeFile path bytes
  action path `finally` cleanup path
  where
    cleanup path =
      FileSystem.removeFile path `catchSync` \_ -> pure ()

matrixAudioMimeType :: FilePath -> Text
matrixAudioMimeType path =
  case Text.toLower (Text.pack (takeExtension path)) of
    ".aac" -> "audio/aac"
    ".flac" -> "audio/flac"
    ".mp3" -> "audio/mpeg"
    ".oga" -> "audio/ogg"
    ".ogg" -> "audio/ogg"
    ".opus" -> "audio/ogg"
    ".wav" -> "audio/wav"
    ".webm" -> "audio/webm"
    _ -> "application/octet-stream"

matrixImageMimeType :: FilePath -> Text
matrixImageMimeType path =
  case Text.toLower (Text.pack (takeExtension path)) of
    ".apng" -> "image/apng"
    ".avif" -> "image/avif"
    ".gif" -> "image/gif"
    ".jpg" -> "image/jpeg"
    ".jpeg" -> "image/jpeg"
    ".png" -> "image/png"
    ".svg" -> "image/svg+xml"
    ".webp" -> "image/webp"
    _ -> "application/octet-stream"

matrixImageExtension :: Text -> String
matrixImageExtension mime =
  case Text.toLower mime of
    "image/apng" -> "apng"
    "image/avif" -> "avif"
    "image/gif" -> "gif"
    "image/jpeg" -> "jpg"
    "image/png" -> "png"
    "image/svg+xml" -> "svg"
    "image/webp" -> "webp"
    _ -> "bin"

matrixAudioExtension :: Text -> String
matrixAudioExtension mime =
  case Text.toLower mime of
    "audio/aac" -> "aac"
    "audio/flac" -> "flac"
    "audio/mpeg" -> "mp3"
    "audio/mp4" -> "m4a"
    "audio/ogg" -> "ogg"
    "audio/wav" -> "wav"
    "audio/webm" -> "webm"
    _ -> "bin"

matrixTempDir :: FilePath
matrixTempDir =
  "/tmp/cosmobot-matrix"

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped


deleteMessage :: Matrix :> es => IncomingMessage -> MessageId -> Eff es Bool
deleteMessage message messageId =
  case (message.platform, viaNonEmpty head message.chatAliases) of
    (PlatformMatrix, Just roomId) ->
      deleteEvent roomId messageId (currentRawEventId message messageId)
    _ ->
      pure False

currentRawEventId :: IncomingMessage -> MessageId -> Maybe MatrixEventId
currentRawEventId message messageId = do
  guard (message.messageId == Just messageId)
  matrixRawEventId message.raw

matrixRawEventId :: Aeson.Value -> Maybe MatrixEventId
matrixRawEventId =
  Aeson.parseMaybe (Aeson.withObject "Matrix event" \o -> matrixEventId <$> o Aeson..: "event_id")

eventToIncomingMessage :: RoomEvent -> Maybe IncomingMessage
eventToIncomingMessage =
  eventToIncomingMessageWith defaultConfig

eventToIncomingMessageWith :: Config -> RoomEvent -> Maybe IncomingMessage
eventToIncomingMessageWith cfg RoomEvent{roomId, roomIsDirect, event} = do
  guard (event.type_ == "m.room.message")
  guard (not (isOwnEvent cfg event))
  body <- event.content.body
  guard (not (Text.null (Text.strip body)))
  pure IncomingMessage
    { platform = PlatformMatrix
    , kind = if roomIsDirect then ChatPrivate else ChatGroup
    , chatId = Just (stableTextId (matrixRoomIdText roomId))
    , chatAliases = [matrixRoomIdText roomId]
    , digest = matrixMessageDigest cfg roomId event
    , senderId = Just event.sender
    , senderUsername = Just event.sender
    , messageId = matrixEventMessageId <$> event.eventId
    , replyToMessageId = matrixEventMessageId <$> event.content.replyToEventId
    , mentions = []
    , mentionUsernames = matrixMentions cfg event.content body
    , imageUrls = []
    , text = Text.strip body
    , raw = event.raw
    }

matrixEventIgnoreReason :: Config -> RoomEvent -> Text
matrixEventIgnoreReason cfg RoomEvent{roomId, event}
  | eventType /= "m.room.message" =
      [i|unsupported event type #{eventType}; #{context}|]
  | isOwnEvent cfg event =
      [i|own event; #{context}|]
  | isNothing event.content.body =
      [i|missing content.body; #{context}|]
  | Text.null (Text.strip (fromMaybe "" event.content.body)) =
      [i|blank content.body; #{context}|]
  | otherwise =
      [i|unknown reason; #{context}|]
  where
    eventType :: Text
    eventType = event.type_

    eventSender :: Text
    eventSender = event.sender

    eventIdText :: Text
    eventIdText = maybe "<none>" matrixEventIdText event.eventId

    eventMsgtype :: Text
    eventMsgtype = fromMaybe "<none>" event.content.msgtype

    context :: Text
    context =
      [i|room=#{roomId} sender=#{eventSender} event_id=#{eventIdText} msgtype=#{eventMsgtype}|]

matrixMessageDigest :: Config -> MatrixRoomId -> Event -> MessageDigest
matrixMessageDigest cfg roomId event =
  MessageDigest
    { chatIsAllowed = roomAllowed
    , senderIsAllowed = senderSuperuser
    , senderIsSuperuser = senderSuperuser
    , mentionsBot = maybe False (\botId -> botId `elem` event.content.mentions || botId `Text.isInfixOf` eventText) cfg.userId
    , botId = cfg.userId
    }
  where
    roomAllowed =
      matrixRoomIdText roomId `elem` cfg.allowedRooms
    senderSuperuser =
      event.sender `elem` cfg.superusers
    eventText =
      fromMaybe "" event.content.body

matrixMentions :: Config -> EventContent -> Text -> [Text]
matrixMentions cfg content body =
  case content.mentions of
    [] ->
      [ userId
      | Just userId <- [cfg.userId]
      , userId `Text.isInfixOf` body
      ]
    mentions ->
      mentions

isOwnEvent :: Config -> Event -> Bool
isOwnEvent cfg event =
  cfg.userId == Just event.sender

defaultConfig :: Config
defaultConfig = Config
  { homeserver = "https://matrix.org"
  , loginUser = Nothing
  , loginPassword = Nothing
  , deviceId = Nothing
  , directRooms = []
  , userId = Nothing
  , allowedRooms = []
  , superusers = []
  }

loginCall :: (IOE :> es, Log :> es) => Manager -> Config -> Text -> Text -> Eff es MatrixLoginResponse
loginCall manager cfg user password = do
  (baseUrl, baseOptions) <- liftIO (matrixBaseUrl cfg.homeserver)
  let options =
        baseOptions
          <> responseTimeout matrixApiResponseTimeoutMicroseconds
      request = MatrixLoginRequest
        { loginIdentifier = MatrixLoginIdentifier
            { loginIdentifierType = "m.id.user"
            , loginIdentifierUser = user
            }
        , loginPassword = password
        , loginDeviceId = cfg.deviceId
        , loginInitialDeviceDisplayName = Just "cosmobot"
        , loginRefreshToken = True
        }
  logInfo_ "Matrix API request: login"
  matrixReq "login"
    ( Http.runReqWithConfig (matrixHttpConfig manager) $
        req POST
          (baseUrl /: "_matrix" /: "client" /: "v3" /: "login")
          (ReqBodyJson request)
          jsonResponse
          options
    )
    <&> responseBody

refreshAccessTokenCall :: (IOE :> es, Log :> es) => Manager -> Config -> Text -> Eff es MatrixRefreshResponse
refreshAccessTokenCall manager cfg refreshToken = do
  (baseUrl, baseOptions) <- liftIO (matrixBaseUrl cfg.homeserver)
  let options =
        baseOptions
          <> responseTimeout matrixApiResponseTimeoutMicroseconds
      request = MatrixRefreshRequest refreshToken
  logInfo_ "Matrix API request: refresh access token"
  matrixReq "refresh"
    ( Http.runReqWithConfig (matrixHttpConfig manager) $
        req POST
          (baseUrl /: "_matrix" /: "client" /: "v3" /: "refresh")
          (ReqBodyJson request)
          jsonResponse
          options
    )
    <&> responseBody

syncCall :: (IOE :> es, Log :> es) => Manager -> Config -> Maybe Text -> Text -> Eff es SyncResponse
syncCall manager cfg since token = do
  (baseUrl, baseOptions) <- liftIO (matrixBaseUrl cfg.homeserver)
  let options =
        baseOptions
          <> matrixAuth token
          <> responseTimeout matrixSyncResponseTimeoutMicroseconds
          <> "timeout" =: matrixSyncTimeoutMilliseconds
          <> maybe mempty ("since" =:) since
  let sinceLabel :: Text
      sinceLabel = maybe "<initial>" (const "<next_batch>") since
  logInfo_ [i|Matrix API request: sync since=#{sinceLabel}|]
  matrixReq "sync"
    ( Http.runReqWithConfig (matrixHttpConfig manager) $
        req GET (baseUrl /: "_matrix" /: "client" /: "v3" /: "sync") NoReqBody jsonResponse options
    )
    <&> responseBody

joinedMemberCountCall :: (IOE :> es, Log :> es) => Manager -> Config -> Text -> MatrixRoomId -> Eff es Int
joinedMemberCountCall manager cfg token roomId = do
  (baseUrl, baseOptions) <- liftIO (matrixBaseUrl cfg.homeserver)
  let options =
        baseOptions
          <> matrixAuth token
          <> responseTimeout matrixApiResponseTimeoutMicroseconds
  logInfo_ [i|Matrix API request: joined_members room=#{roomId}|]
  response :: JoinedMembersResponse <- matrixReq "joined_members"
    ( Http.runReqWithConfig (matrixHttpConfig manager) $
        req GET
          (baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText roomId /: "joined_members")
          NoReqBody
          jsonResponse
          options
    )
    <&> responseBody
  pure (Map.size response.joinedMembers)

sendMessageCall :: (IOE :> es, Log :> es) => Manager -> Config -> Text -> MatrixRoomId -> Maybe MatrixReplyTo -> Text -> Eff es SendMessageResponse
sendMessageCall manager cfg token roomId replyRelation body = do
  (baseUrl, baseOptions) <- liftIO (matrixBaseUrl cfg.homeserver)
  txnId <- liftIO (show <$> getMonotonicTimeNSec)
  let options =
        baseOptions
          <> matrixAuth token
          <> responseTimeout matrixApiResponseTimeoutMicroseconds
      request = SendMessageRequest
        { msgtype = "m.text"
        , body = nonEmptyMatrixBody body
        , replyRelation
        }
  logInfo_ "Matrix API request: send m.room.message"
  matrixReq "send m.room.message" (Http.runReqWithConfig (matrixHttpConfig manager) $
    req PUT
      (baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText roomId /: "send" /: "m.room.message" /: txnId)
      (ReqBodyJson request)
      jsonResponse
      options)
    <&> responseBody

uploadMediaCall :: (IOE :> es, Log :> es) => Manager -> Config -> FilePath -> Text -> Text -> Text -> Eff es MatrixUploadResponse
uploadMediaCall manager cfg path fileName mime token = do
  (baseUrl, baseOptions) <- liftIO (matrixBaseUrl cfg.homeserver)
  let options =
        baseOptions
          <> matrixAuth token
          <> header "Content-Type" (TextEncoding.encodeUtf8 mime)
          <> responseTimeout matrixApiResponseTimeoutMicroseconds
          <> "filename" =: fileName
  logInfo_ "Matrix API request: upload media"
  matrixReq "upload media" (Http.runReqWithConfig (matrixHttpConfig manager) $
    req POST
      (baseUrl /: "_matrix" /: "media" /: "v3" /: "upload")
      (ReqBodyFile path)
      jsonResponse
      options)
    <&> responseBody

sendFileMessageCall :: (IOE :> es, Log :> es) => Manager -> Config -> Text -> Text -> Maybe MatrixReplyTo -> MatrixFileMessage -> Eff es SendMessageResponse
sendFileMessageCall manager cfg token roomId replyRelation message@MatrixFileMessage{msgtype = mediaMsgtype} = do
  (baseUrl, baseOptions) <- liftIO (matrixBaseUrl cfg.homeserver)
  txnId <- liftIO (show <$> getMonotonicTimeNSec)
  let options =
        baseOptions
          <> matrixAuth token
          <> responseTimeout matrixApiResponseTimeoutMicroseconds
      request = MatrixFileMessageRequest
        { message
        , replyRelation
        }
  logInfo_ [i|Matrix API request: send #{mediaMsgtype}|]
  matrixReq [i|send #{mediaMsgtype}|] (Http.runReqWithConfig (matrixHttpConfig manager) $
    req PUT
      (baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: roomId /: "send" /: "m.room.message" /: txnId)
      (ReqBodyJson request)
      jsonResponse
      options)
    <&> responseBody

redactEventCall :: (IOE :> es, Log :> es) => Manager -> Config -> Text -> Text -> Text -> Eff es RedactEventResponse
redactEventCall manager cfg token roomId eventId = do
  (baseUrl, baseOptions) <- liftIO (matrixBaseUrl cfg.homeserver)
  txnId <- liftIO (show <$> getMonotonicTimeNSec)
  let options =
        baseOptions
          <> matrixAuth token
          <> responseTimeout matrixApiResponseTimeoutMicroseconds
      request = RedactEventRequest{reason = Nothing}
  logInfo_ "Matrix API request: redact event"
  matrixReq "redact event" (Http.runReqWithConfig (matrixHttpConfig manager) $
    req PUT
      (baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: roomId /: "redact" /: eventId /: txnId)
      (ReqBodyJson request)
      jsonResponse
      options)
    <&> responseBody

matrixBaseUrl :: Text -> IO (Url 'Https, Option 'Https)
matrixBaseUrl homeserver = do
  uri <- URI.mkURI homeserver
  case useHttpsURI uri of
    Nothing ->
      ioError (userError [i|Unsupported Matrix homeserver URL: #{homeserver}. Use a full HTTPS base URL.|])
    Just parsed ->
      pure parsed

matrixAuth :: Text -> Option 'Https
matrixAuth token =
  header "Authorization" (ByteString.pack [i|Bearer #{token}|])

matrixHttpConfig :: Manager -> HttpConfig
matrixHttpConfig manager =
  (Http.httpConfig manager)
    { httpConfigRetryJudge = \_ _ -> False
    , httpConfigRetryJudgeException = \_ _ -> False
    }

stableTextId :: Text -> Integer
stableTextId =
  Text.foldl' step 14695981039346656037
  where
    step acc char =
      fromIntegral ((fromIntegral acc `xor` fromIntegral (fromEnum char)) * fnvPrime :: Word64)
    fnvPrime :: Word64
    fnvPrime = 1099511628211

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
      <*> o Aeson..:? "rooms" Aeson..!= Rooms Map.empty
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

newtype Rooms = Rooms
  { join :: Map Text JoinedRoom
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Rooms where
  parseJSON = Aeson.withObject "Rooms" \o ->
    Rooms <$> o Aeson..:? "join" Aeson..!= Map.empty

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
  { joinedMembers :: Map Text Aeson.Value
  }
  deriving (Show, Generic)

instance Aeson.FromJSON JoinedMembersResponse where
  parseJSON = Aeson.withObject "JoinedMembersResponse" \o ->
    JoinedMembersResponse <$> o Aeson..:? "joined" Aeson..!= Map.empty

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

instance Aeson.FromJSON MatrixInReplyTo where
  parseJSON = Aeson.withObject "MatrixInReplyTo" \o ->
    MatrixInReplyTo . fmap matrixEventId <$> o Aeson..:? "event_id"

instance Aeson.ToJSON MatrixInReplyTo where
  toJSON MatrixInReplyTo{replyEventId} =
    Aeson.object
      [ "event_id" Aeson..= fmap matrixEventIdText replyEventId
      ]

data SendMessageRequest = SendMessageRequest
  { msgtype :: !Text
  , body :: !Text
  , replyRelation :: !(Maybe MatrixReplyTo)
  }
  deriving (Show, Generic)

instance Aeson.ToJSON SendMessageRequest where
  toJSON SendMessageRequest{msgtype, body, replyRelation} =
    Aeson.object $
      [ "msgtype" Aeson..= msgtype
      , "body" Aeson..= body
      ]
        <> maybe [] (\(MatrixReplyTo eventId) -> ["m.relates_to" Aeson..= MatrixRelatesTo (Just eventId)]) replyRelation

newtype MatrixUploadResponse = MatrixUploadResponse
  { contentUri :: Text
  }
  deriving (Show, Generic)

instance Aeson.FromJSON MatrixUploadResponse where
  parseJSON = Aeson.withObject "MatrixUploadResponse" \o ->
    MatrixUploadResponse <$> o Aeson..: "content_uri"

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

instance Aeson.ToJSON MatrixFileMessage where
  toJSON MatrixFileMessage{msgtype, body, filename, url, info} =
    Aeson.object
      [ "msgtype" Aeson..= msgtype
      , "body" Aeson..= body
      , "filename" Aeson..= filename
      , "url" Aeson..= url
      , "info" Aeson..= info
      ]

data MatrixFileMessageRequest = MatrixFileMessageRequest
  { message :: !MatrixFileMessage
  , replyRelation :: !(Maybe MatrixReplyTo)
  }
  deriving (Show, Generic)

instance Aeson.ToJSON MatrixFileMessageRequest where
  toJSON MatrixFileMessageRequest{message, replyRelation} =
    case Aeson.toJSON message of
      Aeson.Object fields ->
        Aeson.Object (fields <> AesonKeyMap.fromList relationFields)
      value ->
        value
    where
      relationFields =
        maybe [] (\(MatrixReplyTo eventId) -> [("m.relates_to", Aeson.toJSON (MatrixRelatesTo (Just eventId)))]) replyRelation

data RedactEventRequest = RedactEventRequest
  { reason :: Maybe Text
  }
  deriving (Show, Generic, Aeson.ToJSON)

newtype SendMessageResponse = SendMessageResponse
  { eventId :: MatrixEventId
  }
  deriving (Show, Generic)

instance Aeson.FromJSON SendMessageResponse where
  parseJSON = Aeson.withObject "SendMessageResponse" \o ->
    SendMessageResponse . matrixEventId <$> o Aeson..: "event_id"

newtype RedactEventResponse = RedactEventResponse
  { redactionEventId :: Text
  }
  deriving (Show, Generic)

instance Aeson.FromJSON RedactEventResponse where
  parseJSON = Aeson.withObject "RedactEventResponse" \o ->
    RedactEventResponse <$> o Aeson..: "event_id"

matrixSyncTimeoutMilliseconds :: Int
matrixSyncTimeoutMilliseconds = 30000

matrixSyncResponseTimeoutMicroseconds :: Int
matrixSyncResponseTimeoutMicroseconds = 40000000

matrixApiResponseTimeoutMicroseconds :: Int
matrixApiResponseTimeoutMicroseconds = 10000000

matrixRetryDelayMicroseconds :: Int
matrixRetryDelayMicroseconds = 5000000
