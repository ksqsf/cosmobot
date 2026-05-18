{-|
Module      : Bot.Chat.Driver.Matrix
Description : Matrix Client-Server API effect
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
  , uploadFile
  , deleteMessage
  )
where

import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Util.HTTP as Http
import Control.Concurrent (threadDelay)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (Manager)
import Network.HTTP.Req
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.Directory (getFileSize)
import System.FilePath (takeFileName)
import System.IO.Error (ioError, userError)
import qualified Text.URI as URI

data Config = Config
  { homeserver :: !Text
  , accessToken :: !(Maybe Text)
  , userId :: !(Maybe Text)
  , allowedRooms :: ![Text]
  , superusers :: ![Text]
  }
  deriving (Show)

matrixDriver
  :: (Matrix :> es, IOE :> es)
  => Driver.ChatPlatformDriver es
matrixDriver = Driver.ChatPlatformDriver
  { Driver.platform = PlatformMatrix
  , Driver.replyTo = replyTo
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
  }

matrixStreamingMessageLimit :: Int
matrixStreamingMessageLimit = 4000

data Matrix :: Effect where
  MatrixConfig :: Matrix m Config
  Sync :: Maybe Text -> Matrix m (Maybe SyncResponse)
  SendText :: Text -> Text -> Matrix m (Maybe SendMessageResponse)
  UploadMedia :: FilePath -> Text -> Matrix m MatrixUploadResponse
  SendFileMessage :: Text -> MatrixFileMessage -> Matrix m (Maybe SendMessageResponse)
  DeleteEvent :: Text -> MessageId -> Maybe Text -> Matrix m Bool

type instance DispatchOf Matrix = Dynamic

matrixConfig :: Matrix :> es => Eff es Config
matrixConfig = send MatrixConfig

sync :: Matrix :> es => Maybe Text -> Eff es (Maybe SyncResponse)
sync =
  send . Sync

sendText :: Matrix :> es => Text -> Text -> Eff es (Maybe SendMessageResponse)
sendText roomId body =
  send (SendText roomId body)

uploadMedia :: Matrix :> es => FilePath -> Text -> Eff es MatrixUploadResponse
uploadMedia path fileName =
  send (UploadMedia path fileName)

sendFileMessage :: Matrix :> es => Text -> MatrixFileMessage -> Eff es (Maybe SendMessageResponse)
sendFileMessage roomId message =
  send (SendFileMessage roomId message)

deleteEvent :: Matrix :> es => Text -> MessageId -> Maybe Text -> Eff es Bool
deleteEvent roomId messageId eventId =
  send (DeleteEvent roomId messageId eventId)

runMatrix
  :: (IOE :> es, Log :> es)
  => Config
  -> Eff (Matrix : es) a
  -> Eff es a
runMatrix cfg inner = do
  manager <- liftIO Http.newTlsManager
  eventIds <- liftIO (IORef.newIORef (Map.empty :: Map MessageId Text))
  interpret
    ( \_ -> \case
        MatrixConfig ->
          pure cfg
        Sync since ->
          traverse (syncCall manager cfg since) cfg.accessToken
        SendText roomId body -> do
          response <- traverse (\token -> sendMessageCall manager cfg token roomId body) cfg.accessToken
          traverse_ (rememberMatrixEvent eventIds) response
          pure response
        UploadMedia path fileName ->
          maybe (throwIO (userError "Matrix access token is not configured.")) (uploadMediaCall manager cfg path fileName) cfg.accessToken
        SendFileMessage roomId message -> do
          response <- traverse (\token -> sendFileMessageCall manager cfg token roomId message) cfg.accessToken
          traverse_ (rememberMatrixEvent eventIds) response
          pure response
        DeleteEvent roomId messageId knownEventId -> do
          stored <- liftIO (IORef.readIORef eventIds)
          case knownEventId <|> Map.lookup messageId stored of
            Nothing ->
              pure False
            Just eventId ->
              maybe (pure False) (\token -> redactEventCall manager cfg token roomId eventId $> True) cfg.accessToken
    )
    inner

rememberMatrixEvent :: IOE :> es => IORef.IORef (Map MessageId Text) -> SendMessageResponse -> Eff es ()
rememberMatrixEvent eventIds response =
  liftIO $ IORef.modifyIORef' eventIds (Map.insert (textMessageId response.eventId) response.eventId)

incomingMessages :: (Matrix :> es, Log :> es, IOE :> es) => Stream (Of IncomingMessage) (Eff es) ()
incomingMessages = do
  cfg <- S.lift matrixConfig
  unless (isNothing cfg.accessToken) (syncLoop cfg Nothing)
  where
    syncLoop cfg since = do
      result <- S.lift $ sync since `catch` \(err :: SomeException) -> do
        logInfo_ [i|Matrix sync failed, retrying: #{show err :: String}|]
        liftIO $ threadDelay matrixRetryDelayMicroseconds
        pure Nothing
      case result of
        Nothing ->
          syncLoop cfg since
        Just response -> do
          let events = syncEvents response
          S.lift $ logInfo_ [i|Matrix sync batch: #{length events}|]
          for_ events \event ->
            case eventToIncomingMessageWith cfg event of
              Nothing -> do
                S.lift $ logTrace_ "Ignoring Matrix event"
                S.lift $ logInfo_ "Ignoring Matrix event"
              Just message -> do
                S.lift $ logTrace "incoming Matrix message" message
                S.lift $ logInfo_ [i|incoming Matrix message: #{incomingMessageLogLine message}|]
                S.yield message
          syncLoop cfg (Just response.nextBatch)

syncEvents :: SyncResponse -> [RoomEvent]
syncEvents response =
  [ RoomEvent roomId event
  | (roomId, room) <- Map.toList response.rooms.join
  , event <- room.timeline.events
  ]

replyTo :: Matrix :> es => IncomingMessage -> Text -> Eff es (Maybe MessageId)
replyTo message body =
  case (message.platform, viaNonEmpty head message.chatAliases) of
    (PlatformMatrix, Just roomId) -> do
      response <- sendText roomId (Chat.renderReplyBody body)
      pure (textMessageId . (.eventId) <$> response)
    _ ->
      pure Nothing

uploadFile :: (Matrix :> es, IOE :> es) => IncomingMessage -> FilePath -> Eff es (Either Text (Maybe MessageId))
uploadFile message path =
  case (message.platform, viaNonEmpty head message.chatAliases) of
    (PlatformMatrix, Just roomId) -> do
      let fileName = matrixUploadFileName path
      size <- liftIO (getFileSize path)
      uploaded <- uploadMedia path fileName
      response <- sendFileMessage roomId MatrixFileMessage
        { body = fileName
        , filename = fileName
        , url = uploaded.contentUri
        , info = MatrixFileInfo
            { mimetype = "application/octet-stream"
            , size = size
            }
        }
      pure (Right (textMessageId . (.eventId) <$> response))
    _ ->
      pure (Left "Matrix file upload requires a Matrix room id.")

matrixUploadFileName :: FilePath -> Text
matrixUploadFileName path =
  let name = Text.pack (takeFileName path)
  in if Text.null name then "file" else name

deleteMessage :: Matrix :> es => IncomingMessage -> MessageId -> Eff es Bool
deleteMessage message messageId =
  case (message.platform, viaNonEmpty head message.chatAliases) of
    (PlatformMatrix, Just roomId) ->
      deleteEvent roomId messageId (currentRawEventId message messageId)
    _ ->
      pure False

currentRawEventId :: IncomingMessage -> MessageId -> Maybe Text
currentRawEventId message messageId = do
  guard (message.messageId == Just messageId)
  matrixRawEventId message.raw

matrixRawEventId :: Aeson.Value -> Maybe Text
matrixRawEventId =
  Aeson.parseMaybe (Aeson.withObject "Matrix event" (Aeson..: "event_id"))

eventToIncomingMessage :: RoomEvent -> Maybe IncomingMessage
eventToIncomingMessage =
  eventToIncomingMessageWith defaultConfig

eventToIncomingMessageWith :: Config -> RoomEvent -> Maybe IncomingMessage
eventToIncomingMessageWith cfg RoomEvent{roomId, event} = do
  guard (event.type_ == "m.room.message")
  guard (not (isOwnEvent cfg event))
  body <- event.content.body
  guard (not (Text.null (Text.strip body)))
  pure IncomingMessage
    { platform = PlatformMatrix
    , kind = ChatGroup
    , chatId = Just (stableTextId roomId)
    , chatAliases = [roomId]
    , digest = matrixMessageDigest cfg roomId event
    , senderId = Just event.sender
    , senderUsername = Just event.sender
    , messageId = textMessageId <$> event.eventId
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = matrixMentions cfg event.content body
    , imageUrls = []
    , text = Text.strip body
    , raw = event.raw
    }

matrixMessageDigest :: Config -> Text -> Event -> MessageDigest
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
      roomId `elem` cfg.allowedRooms
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
  , accessToken = Nothing
  , userId = Nothing
  , allowedRooms = []
  , superusers = []
  }

syncCall :: (IOE :> es, Log :> es) => Manager -> Config -> Maybe Text -> Text -> Eff es SyncResponse
syncCall manager cfg since token = do
  (baseUrl, baseOptions) <- liftIO (matrixBaseUrl cfg.homeserver)
  let options =
        baseOptions
          <> matrixAuth token
          <> responseTimeout matrixSyncResponseTimeoutMicroseconds
          <> "timeout" =: matrixSyncTimeoutMilliseconds
          <> maybe mempty ("since" =:) since
  liftIO
    ( Http.runReqWithConfig (matrixHttpConfig manager) $
        req GET (baseUrl /: "_matrix" /: "client" /: "v3" /: "sync") NoReqBody jsonResponse options
    )
    <&> responseBody

sendMessageCall :: (IOE :> es, Log :> es) => Manager -> Config -> Text -> Text -> Text -> Eff es SendMessageResponse
sendMessageCall manager cfg token roomId body = do
  (baseUrl, baseOptions) <- liftIO (matrixBaseUrl cfg.homeserver)
  txnId <- liftIO (show <$> getMonotonicTimeNSec)
  let options =
        baseOptions
          <> matrixAuth token
          <> responseTimeout matrixApiResponseTimeoutMicroseconds
      request = SendMessageRequest
        { msgtype = "m.text"
        , body = nonEmptyMatrixBody body
        }
  logInfo_ "Matrix API request: send m.room.message"
  liftIO (Http.runReqWithConfig (matrixHttpConfig manager) $
    req PUT
      (baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: roomId /: "send" /: "m.room.message" /: txnId)
      (ReqBodyJson request)
      jsonResponse
      options)
    <&> responseBody

uploadMediaCall :: (IOE :> es, Log :> es) => Manager -> Config -> FilePath -> Text -> Text -> Eff es MatrixUploadResponse
uploadMediaCall manager cfg path fileName token = do
  (baseUrl, baseOptions) <- liftIO (matrixBaseUrl cfg.homeserver)
  let options =
        baseOptions
          <> matrixAuth token
          <> header "Content-Type" "application/octet-stream"
          <> responseTimeout matrixApiResponseTimeoutMicroseconds
          <> "filename" =: fileName
  logInfo_ "Matrix API request: upload media"
  liftIO (Http.runReqWithConfig (matrixHttpConfig manager) $
    req POST
      (baseUrl /: "_matrix" /: "media" /: "v3" /: "upload")
      (ReqBodyFile path)
      jsonResponse
      options)
    <&> responseBody

sendFileMessageCall :: (IOE :> es, Log :> es) => Manager -> Config -> Text -> Text -> MatrixFileMessage -> Eff es SendMessageResponse
sendFileMessageCall manager cfg token roomId message = do
  (baseUrl, baseOptions) <- liftIO (matrixBaseUrl cfg.homeserver)
  txnId <- liftIO (show <$> getMonotonicTimeNSec)
  let options =
        baseOptions
          <> matrixAuth token
          <> responseTimeout matrixApiResponseTimeoutMicroseconds
  logInfo_ "Matrix API request: send m.file"
  liftIO (Http.runReqWithConfig (matrixHttpConfig manager) $
    req PUT
      (baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: roomId /: "send" /: "m.room.message" /: txnId)
      (ReqBodyJson message)
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
  liftIO (Http.runReqWithConfig (matrixHttpConfig manager) $
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
  { roomId :: !Text
  , event :: !Event
  }
  deriving (Show)

data SyncResponse = SyncResponse
  { nextBatch :: !Text
  , rooms :: !Rooms
  }
  deriving (Show, Generic)

instance Aeson.FromJSON SyncResponse where
  parseJSON = Aeson.withObject "SyncResponse" \o ->
    SyncResponse
      <$> o Aeson..: "next_batch"
      <*> o Aeson..:? "rooms" Aeson..!= Rooms Map.empty

newtype Rooms = Rooms
  { join :: Map Text JoinedRoom
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Rooms where
  parseJSON = Aeson.withObject "Rooms" \o ->
    Rooms <$> o Aeson..:? "join" Aeson..!= Map.empty

newtype JoinedRoom = JoinedRoom
  { timeline :: Timeline
  }
  deriving (Show, Generic)

instance Aeson.FromJSON JoinedRoom where
  parseJSON = Aeson.withObject "JoinedRoom" \o ->
    JoinedRoom <$> o Aeson..:? "timeline" Aeson..!= Timeline []

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
  , eventId :: !(Maybe Text)
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
        eventId <- o Aeson..:? "event_id"
        content <- o Aeson..:? "content" Aeson..!= EventContent Nothing Nothing []
        pure Event{type_, sender, eventId, content, raw = value}

data EventContent = EventContent
  { msgtype :: !(Maybe Text)
  , body :: !(Maybe Text)
  , mentions :: ![Text]
  }
  deriving (Show, Generic)

instance Aeson.FromJSON EventContent where
  parseJSON = Aeson.withObject "EventContent" \o -> do
    msgtype <- o Aeson..:? "msgtype"
    body <- o Aeson..:? "body"
    mentions <- o Aeson..:? "m.mentions" Aeson..!= MatrixMentions []
    pure EventContent
      { msgtype
      , body
      , mentions = mentions.userIds
      }

newtype MatrixMentions = MatrixMentions
  { userIds :: [Text]
  }
  deriving (Show, Generic)

instance Aeson.FromJSON MatrixMentions where
  parseJSON = Aeson.withObject "MatrixMentions" \o ->
    MatrixMentions <$> o Aeson..:? "user_ids" Aeson..!= []

data SendMessageRequest = SendMessageRequest
  { msgtype :: !Text
  , body :: !Text
  }
  deriving (Show, Generic, Aeson.ToJSON)

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
  { body :: !Text
  , filename :: !Text
  , url :: !Text
  , info :: !MatrixFileInfo
  }
  deriving (Show, Generic)

instance Aeson.ToJSON MatrixFileMessage where
  toJSON MatrixFileMessage{body, filename, url, info} =
    Aeson.object
      [ "msgtype" Aeson..= ("m.file" :: Text)
      , "body" Aeson..= body
      , "filename" Aeson..= filename
      , "url" Aeson..= url
      , "info" Aeson..= info
      ]

data RedactEventRequest = RedactEventRequest
  { reason :: Maybe Text
  }
  deriving (Show, Generic, Aeson.ToJSON)

newtype SendMessageResponse = SendMessageResponse
  { eventId :: Text
  }
  deriving (Show, Generic)

instance Aeson.FromJSON SendMessageResponse where
  parseJSON = Aeson.withObject "SendMessageResponse" \o ->
    SendMessageResponse <$> o Aeson..: "event_id"

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
