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
  )
where

import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Prelude
import Control.Concurrent (threadDelay)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (Manager)
import Network.HTTP.Req
import qualified Streaming as S
import qualified Streaming.Prelude as S
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
  :: Matrix :> es
  => Driver.ChatPlatformDriver es
matrixDriver = Driver.ChatPlatformDriver
  { Driver.platform = PlatformMatrix
  , Driver.replyTo = replyTo
  , Driver.editMessage = \_ _ _ -> pure False
  , Driver.replyStreamStyle = \_ -> pure (Chat.ChunkedReply matrixStreamingMessageLimit)
  , Driver.getMessageContent = \_ _ -> pure Nothing
  , Driver.getSenderMemberInfo = \_ -> pure Nothing
  , Driver.getMemberInfo = \_ _ -> pure Nothing
  , Driver.listGroupMembers = \_ -> pure Nothing
  , Driver.mentionUser = \_ _ _ -> pure Nothing
  }

matrixStreamingMessageLimit :: Int
matrixStreamingMessageLimit = 4000

data Matrix :: Effect where
  MatrixConfig :: Matrix m Config
  Sync :: Maybe Text -> Matrix m (Maybe SyncResponse)
  SendText :: Text -> Text -> Matrix m (Maybe SendMessageResponse)

type instance DispatchOf Matrix = Dynamic

matrixConfig :: Matrix :> es => Eff es Config
matrixConfig = send MatrixConfig

sync :: Matrix :> es => Maybe Text -> Eff es (Maybe SyncResponse)
sync =
  send . Sync

sendText :: Matrix :> es => Text -> Text -> Eff es (Maybe SendMessageResponse)
sendText roomId body =
  send (SendText roomId body)

runMatrix
  :: (IOE :> es, Log :> es)
  => Config
  -> Eff (Matrix : es) a
  -> Eff es a
runMatrix cfg inner = withReqManager \manager ->
  interpret
    ( \_ -> \case
        MatrixConfig ->
          pure cfg
        Sync since ->
          traverse (syncCall manager cfg since) cfg.accessToken
        SendText roomId body ->
          traverse (\token -> sendMessageCall manager cfg token roomId body) cfg.accessToken
    )
    inner

incomingMessages :: (Matrix :> es, Log :> es, IOE :> es) => Stream (Of IncomingMessage) (Eff es) ()
incomingMessages = do
  cfg <- S.lift matrixConfig
  unless (isNothing cfg.accessToken) (syncLoop cfg Nothing)
  where
    syncLoop cfg since = do
      result <- S.lift $ sync since `catch` \(err :: SomeException) -> do
        logInfo "Matrix sync failed, retrying" (show err :: String)
        liftIO $ threadDelay matrixRetryDelayMicroseconds
        pure Nothing
      case result of
        Nothing ->
          syncLoop cfg since
        Just response -> do
          let events = syncEvents response
          S.lift $ logInfo "Matrix sync batch" (length events)
          for_ events \event ->
            case eventToIncomingMessageWith cfg event of
              Nothing -> do
                S.lift $ logTrace_ "Ignoring Matrix event"
                S.lift $ logInfo_ "Ignoring Matrix event"
              Just message -> do
                S.lift $ logTrace "incoming Matrix message" message
                S.lift $ logInfo "incoming Matrix message" (incomingMessageLogLine message)
                S.yield message
          syncLoop cfg (Just response.nextBatch)

syncEvents :: SyncResponse -> [RoomEvent]
syncEvents response =
  [ RoomEvent roomId event
  | (roomId, room) <- Map.toList response.rooms.join
  , event <- room.timeline.events
  ]

replyTo :: Matrix :> es => IncomingMessage -> Text -> Eff es (Maybe Integer)
replyTo message body =
  case (message.platform, viaNonEmpty head message.chatAliases) of
    (PlatformMatrix, Just roomId) -> do
      response <- sendText roomId (Chat.renderReplyBody body)
      pure (stableTextId . (.eventId) <$> response)
    _ ->
      pure Nothing

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
    , senderId = Just (stableTextId event.sender)
    , senderUsername = Just event.sender
    , messageId = stableTextId <$> event.eventId
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = matrixMentions cfg body
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
    , mentionsBot = maybe False (`Text.isInfixOf` eventText) cfg.userId
    }
  where
    roomAllowed =
      roomId `elem` cfg.allowedRooms
    senderSuperuser =
      event.sender `elem` cfg.superusers
    eventText =
      fromMaybe "" event.content.body

matrixMentions :: Config -> Text -> [Text]
matrixMentions cfg body =
  [ userId
  | Just userId <- [cfg.userId]
  , userId `Text.isInfixOf` body
  ]

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
  liftIO (runReq (matrixHttpConfig manager) (req GET (baseUrl /: "_matrix" /: "client" /: "v3" /: "sync") NoReqBody jsonResponse options))
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
  logInfo "Matrix API request" ("send m.room.message" :: Text)
  liftIO (runReq (matrixHttpConfig manager) $
    req PUT
      (baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: roomId /: "send" /: "m.room.message" /: txnId)
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
  defaultHttpConfig
    { httpConfigAltManager = Just manager
    , httpConfigRetryJudge = \_ _ -> False
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
        content <- o Aeson..:? "content" Aeson..!= EventContent Nothing Nothing
        pure Event{type_, sender, eventId, content, raw = value}

data EventContent = EventContent
  { msgtype :: !(Maybe Text)
  , body :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance Aeson.FromJSON EventContent where
  parseJSON = Aeson.withObject "EventContent" \o ->
    EventContent
      <$> o Aeson..:? "msgtype"
      <*> o Aeson..:? "body"

data SendMessageRequest = SendMessageRequest
  { msgtype :: !Text
  , body :: !Text
  }
  deriving (Show, Generic, Aeson.ToJSON)

newtype SendMessageResponse = SendMessageResponse
  { eventId :: Text
  }
  deriving (Show, Generic)

instance Aeson.FromJSON SendMessageResponse where
  parseJSON = Aeson.withObject "SendMessageResponse" \o ->
    SendMessageResponse <$> o Aeson..: "event_id"

matrixSyncTimeoutMilliseconds :: Int
matrixSyncTimeoutMilliseconds = 30000

matrixSyncResponseTimeoutMicroseconds :: Int
matrixSyncResponseTimeoutMicroseconds = 40000000

matrixApiResponseTimeoutMicroseconds :: Int
matrixApiResponseTimeoutMicroseconds = 10000000

matrixRetryDelayMicroseconds :: Int
matrixRetryDelayMicroseconds = 5000000
