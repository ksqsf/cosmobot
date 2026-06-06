{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.QQ
Description : QQ/NapCat OneBot v11 chat driver
Stability   : experimental
-}

module Bot.Chat.Driver.QQ
  ( QQDriver
  , newQQDriver
  , runQQDriver
  , Config (..)
  , Event (..)
  , ActionResponse (..)
  , incomingMessages
  , eventToIncomingMessage
  , eventToIncomingMessageWith
  , forwardedMessagesText
  , readActionResponse
  , getUserAvatar
  )
where

import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Media as Media
import Bot.Core.Message
import Bot.Prelude
import qualified Control.Concurrent.Chan as Chan
import qualified Data.IORef as IORef
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import qualified Network.WebSockets as WS
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.FilePath (takeFileName)
import qualified Effectful.Concurrent.MVar as MVar
import Effectful.Timeout

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

-- | Connection settings for a OneBot v11 websocket endpoint.
data Config = Config
  { host  :: !String
  , port  :: !Int
  , path  :: !String
  , token :: !(Maybe Text)
  , botQQ :: !(Maybe Integer)
  , allowedGroups :: ![Integer]
  , allowedUsers :: ![Integer]
  , superusers :: ![Integer]
  }
  deriving (Show)

data QQDriver = QQDriver
  { config :: !Config
  , eventChan :: !(Chan.Chan Event)
  , actionChan :: !(Chan.Chan ActionRequest)
  }

newQQDriver :: IOE :> es => Config -> Eff es QQDriver
newQQDriver config = do
  eventChan <- liftIO Chan.newChan
  actionChan <- liftIO Chan.newChan
  pure QQDriver{config, eventChan, actionChan}

instance Driver.ChatDriver QQDriver where
  type ChatDriverEffects QQDriver es = (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es, Concurrency.Concurrency :> es, Media.Media :> es)

  driverPlatform _ =
    PlatformQQ

  sendReplyMessage =
    replyToQQ

  replyAudio =
    replyAudioQQ

  uploadFile =
    uploadFileQQ

  deleteMessage =
    deleteMessageQQ

  messageOutPolicy _ _ =
    pure (Chat.ChunkedMessage qqStreamingMessageLimit)

  getMessageContent driver _ messageId =
    getMessageContentQQ driver messageId

  getSenderMemberInfo driver message =
    case (message.kind, message.chatId, message.senderId) of
      (ChatGroup, Just groupId, Just rawUserId)
        | Just userId <- parseIntegerUserId rawUserId ->
        getGroupMemberInfo driver groupId userId
      _ ->
        pure Nothing

  getMemberInfo driver message userId =
    case (message.kind, message.chatId) of
      (ChatGroup, Just groupId)
        | Just numericUserId <- parseIntegerUserId userId ->
          getGroupMemberInfo driver groupId numericUserId
      _ ->
        pure Nothing

  getUserAvatar _ _ userId =
    pure (getUserAvatar <$> parseIntegerUserId userId)

  normalizeMediaRef _ =
    qqPublicImageRef

  listGroupMembers driver message =
    case (message.kind, message.chatId) of
      (ChatGroup, Just groupId) ->
        getGroupMemberList driver groupId
      _ ->
        pure Nothing

  mentionUser =
    mentionUserQQ

  setMemberTitle =
    setGroupMemberTitleQQ

qqStreamingMessageLimit :: Int
qqStreamingMessageLimit = 4000

runQQDriver
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => QQDriver
  -> Eff es a
  -> Eff es a
runQQDriver driver inner = do
  Concurrency.withWorker "qq.connection" (qqConnectionLoop cfg eventChan actionChan) inner
  where
    cfg = driver.config
    eventChan = driver.eventChan
    actionChan = driver.actionChan

receiveEvent :: IOE :> es => QQDriver -> Eff es Event
receiveEvent driver =
  liftIO (Chan.readChan driver.eventChan)

sendAction
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es)
  => QQDriver
  -> Aeson.Value
  -> Eff es ActionResponse
sendAction driver value = do
  responseVar <- liftIO newEmptyMVar
  liftIO $ Chan.writeChan driver.actionChan (ActionRequest value responseVar)
  result <- timeout qqActionTimeoutMicroseconds (takeMVar responseVar)
  case result of
    Just response ->
      pure response
    Nothing -> do
      logInfo "QQ action timed out"
      pure failedActionResponse

data ActionRequest = ActionRequest !Aeson.Value !(MVar ActionResponse)

qqConnectionLoop
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Timeout :> es, Concurrency.Concurrency :> es)
  => Config
  -> Chan.Chan Event
  -> Chan.Chan ActionRequest
  -> Eff es ()
qqConnectionLoop cfg eventChan actionChan =
  forever do
    result <- runQQConnectionOnce cfg eventChan actionChan
    case result of
      Right () ->
        logInfo "QQ websocket disconnected; reconnecting"
      Left err ->
        logInfo [i|QQ websocket failed; reconnecting: #{err}|]
    threadDelay qqReconnectDelayMicroseconds

runQQConnectionOnce
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => Config
  -> Chan.Chan Event
  -> Chan.Chan ActionRequest
  -> Eff es (Either String ())
runQQConnectionOnce cfg eventChan actionChan =
  (Right <$> do
    withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
      liftIO $ WS.runClient cfg.host cfg.port (websocketPath cfg) \conn ->
        runInIO (runConnection eventChan actionChan conn)
  )
    `catch` \(connectionErr :: WS.ConnectionException) ->
      pure (Left (show connectionErr))
    `catch` \(handshakeErr :: WS.HandshakeException) ->
      pure (Left (show handshakeErr))
    `catch` \(ioErr :: IOException) ->
      pure (Left (show ioErr))
    `catchSync` \err ->
      pure (Left (displayException err))

runConnection
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => Chan.Chan Event
  -> Chan.Chan ActionRequest
  -> WS.Connection
  -> Eff es ()
runConnection eventChan actionChan conn = do
  pendingResponses <- liftIO (newMVar Map.empty)
  actionCounter <- liftIO (newMVar (1 :: Integer))
  done <- liftIO newEmptyMVar
  lastFrameAt <- liftIO (getCurrentTime >>= IORef.newIORef)
  frameReader <- forkConnectionThread "reader" done (readFrames eventChan pendingResponses lastFrameAt conn)
  sender <- forkConnectionThread "sender" done (sendActions actionChan pendingResponses actionCounter conn)
  monitor <- forkConnectionThread "heartbeat-monitor" done (monitorConnectionHeartbeat lastFrameAt)
  reason <- liftIO (takeMVar done)
  logInfo [i|QQ websocket connection ending: #{show reason :: String}|]
  closeWebSocketForReconnect conn
  stopConnectionThread "reader" frameReader
  stopConnectionThread "sender" sender
  stopConnectionThread "heartbeat monitor" monitor
  failPendingResponses pendingResponses
  logInfo "QQ websocket connection ended"

forkConnectionThread
  :: (Concurrency.Concurrency :> es, Concurrent :> es)
  => Text
  -> MVar SomeException
  -> Eff es ()
  -> Eff es Concurrency.ResourceHandle
forkConnectionThread label done action = Concurrency.spawnTopLevelTask [i|qq.websocket.#{label}|] do
  result <- try action
  case result of
    Left err ->
      void (MVar.tryPutMVar done err)
    Right () ->
      void (MVar.tryPutMVar done (toException ThreadKilled))

sendActions
  :: (IOE :> es, KatipE :> es, Concurrent :> es)
  => Chan.Chan ActionRequest
  -> PendingResponses
  -> MVar Integer
  -> WS.Connection
  -> Eff es ()
sendActions actionChan pendingResponses actionCounter conn =
  forever do
    ActionRequest value responseVar <- liftIO (Chan.readChan actionChan)
    echo <- nextActionEcho actionCounter
    let echoedValue = addActionEcho echo value
    MVar.modifyMVar_ pendingResponses \pending ->
      pure (Map.insert echo responseVar pending)
    (liftIO (WS.sendTextData conn (Aeson.encode echoedValue)) `catchSync` \err -> do
      MVar.modifyMVar_ pendingResponses \pending ->
        pure (Map.delete echo pending)
      void $ MVar.tryPutMVar responseVar failedActionResponse
      throwIO err)

failPendingResponses :: (Concurrent :> es) => PendingResponses -> Eff es ()
failPendingResponses pendingResponses = do
  pending <- MVar.modifyMVar pendingResponses \pending ->
    pure (Map.empty, pending)
  traverse_ (flip MVar.tryPutMVar failedActionResponse) pending

closeWebSocketForReconnect :: (IOE :> es, KatipE :> es, Timeout :> es) => WS.Connection -> Eff es ()
closeWebSocketForReconnect conn = do
  result <- timeout qqConnectionCloseTimeoutMicroseconds $
    trySync (liftIO $ WS.sendClose conn ("reconnect" :: Text))
  case result of
    Nothing ->
      logInfo "QQ websocket close timed out during reconnect"
    Just (Left err) ->
      logDebug [i|QQ websocket close during reconnect failed: #{show err :: String}|]
    Just (Right ()) ->
      pure ()

stopConnectionThread :: (Timeout :> es, KatipE :> es, Concurrency.Concurrency :> es) => Text -> Concurrency.ResourceHandle -> Eff es ()
stopConnectionThread label resourceHandle = do
  result <- timeout qqConnectionThreadStopTimeoutMicroseconds (Concurrency.cancelResource resourceHandle.resourceId)
  when (isNothing result) $
    logInfo [i|QQ websocket #{label} thread did not stop before reconnect; continuing|]

websocketPath :: Config -> String
websocketPath Config{path, token = Nothing} = path
websocketPath Config{path, token = Just t} =
  path <> separator <> "access_token=" <> Text.unpack t
  where
    separator
      | "?" `isInfixOf` path = "&"
      | otherwise            = "?"

-- ---------------------------------------------------------------------------
-- Streaming
-- ---------------------------------------------------------------------------

-- | Stream OneBot message events as platform-independent messages.
incomingMessages :: (IOE :> es, KatipE :> es) => QQDriver -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages driver = do
  event <- S.lift (receiveEvent driver)
  case eventToIncomingMessageWith driver.config event of
    Nothing
      | isHeartbeatEvent event ->
          S.lift $ logDebug "Ignoring QQ heartbeat event"
      | otherwise -> do
          let Event{postType} = event
          S.lift $ logDebug [i|Ignoring QQ event: #{postType}|]
          S.lift $ logInfo [i|Ignoring QQ event: #{postType}|]
    Just message -> do
      S.lift $ logDebug [i|incoming qq message: #{show message :: String}|]
      S.lift $ logInfo [i|incoming qq message: #{incomingMessageLogLine message}|]
      S.yield message
  incomingMessages driver

-- ---------------------------------------------------------------------------
-- OneBot v11 events
-- ---------------------------------------------------------------------------

readFrames
  :: (IOE :> es, KatipE :> es, Concurrent :> es)
  => Chan.Chan Event
  -> PendingResponses
  -> IORef.IORef UTCTime
  -> WS.Connection
  -> Eff es ()
readFrames eventChan pendingResponses lastFrameAt conn = forever do
  value <- readValue conn
  liftIO (getCurrentTime >>= IORef.writeIORef lastFrameAt)
  case Aeson.fromJSON value of
    Aeson.Success event -> do
      when (isHeartbeatEvent event) do
        logDebug "QQ websocket heartbeat received"
      liftIO $ Chan.writeChan eventChan event
    Aeson.Error _ ->
      case Aeson.fromJSON value of
        Aeson.Success response ->
          dispatchActionResponse pendingResponses response
        Aeson.Error err ->
          logInfo [i|Ignoring malformed QQ frame: #{Text.pack err}|]

-- | Read frames until an action response is found.
readActionResponse :: (IOE :> es, KatipE :> es) => WS.Connection -> Eff es ActionResponse
readActionResponse conn = do
  value <- readValue conn
  case Aeson.fromJSON value of
    Aeson.Success response -> pure response
    Aeson.Error _ ->
      case Aeson.fromJSON value of
        Aeson.Success (_event :: Event) ->
          readActionResponse conn
        Aeson.Error err -> do
          logInfo [i|Ignoring malformed QQ action response: #{Text.pack err}|]
          readActionResponse conn

readValue :: (IOE :> es, KatipE :> es) => WS.Connection -> Eff es Aeson.Value
readValue conn = do
  bytes <- liftIO (WS.receiveData conn :: IO ByteString)
  case Aeson.eitherDecodeStrict bytes of
    Right value -> pure value
    Left err -> do
      logInfo [i|Ignoring malformed QQ frame: #{Text.pack err}|]
      readValue conn

monitorConnectionHeartbeat :: (IOE :> es, KatipE :> es, Concurrent :> es) => IORef.IORef UTCTime -> Eff es ()
monitorConnectionHeartbeat lastFrameAt = forever do
  threadDelay qqHeartbeatCheckMicroseconds
  now <- liftIO getCurrentTime
  lastSeen <- liftIO (IORef.readIORef lastFrameAt)
  let silence = diffUTCTime now lastSeen
  when (silence > qqHeartbeatTimeout) do
    logInfo [i|QQ websocket heartbeat timed out after #{show silence :: String}; reconnecting|]
    throwIO (QQHeartbeatTimeout silence)

failedActionResponse :: ActionResponse
failedActionResponse =
  ActionResponse
    { status = Just "failed"
    , retcode = Nothing
    , data_ = Nothing
    , message = Just "action failed"
    , echo = Nothing
    }

qqActionTimeoutMicroseconds :: Int
qqActionTimeoutMicroseconds =
  40 * 1000000

qqReconnectDelayMicroseconds :: Int
qqReconnectDelayMicroseconds =
  5 * 1000000

qqConnectionCloseTimeoutMicroseconds :: Int
qqConnectionCloseTimeoutMicroseconds =
  2 * 1000000

qqConnectionThreadStopTimeoutMicroseconds :: Int
qqConnectionThreadStopTimeoutMicroseconds =
  2 * 1000000

qqHeartbeatCheckMicroseconds :: Int
qqHeartbeatCheckMicroseconds =
  15 * 1000000

qqHeartbeatTimeout :: NominalDiffTime
qqHeartbeatTimeout =
  90

newtype QQHeartbeatTimeout = QQHeartbeatTimeout NominalDiffTime
  deriving (Show)

instance Exception QQHeartbeatTimeout

-- | Raw OneBot action response.
data ActionResponse = ActionResponse
  { status  :: !(Maybe Text)
  , retcode :: !(Maybe Integer)
  , data_   :: !(Maybe Aeson.Value)
  , message :: !(Maybe Text)
  , echo    :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance Aeson.FromJSON ActionResponse where
  parseJSON = Aeson.withObject "ActionResponse" $ \o -> do
    status <- o Aeson..:? "status"
    retcode <- o Aeson..:? "retcode"
    data_ <- o Aeson..:? "data"
    message <- o Aeson..:? "message"
    echo <- parseEcho o
    pure ActionResponse{..}
    where
      parseEcho o =
        (o Aeson..:? "echo" :: Aeson.Parser (Maybe Text)) >>= \case
          Just value -> pure (Just value)
          Nothing -> do
            raw <- o Aeson..:? "echo"
            pure (TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode <$> (raw :: Maybe Aeson.Value))

type PendingResponses = MVar (Map Text (MVar ActionResponse))

nextActionEcho :: Concurrent :> es => MVar Integer -> Eff es Text
nextActionEcho counter =
  MVar.modifyMVar counter \value ->
    pure (value + 1, [i|cosmobot-#{value}|])

addActionEcho :: Text -> Aeson.Value -> Aeson.Value
addActionEcho echo value =
  case value of
    Aeson.Object obj ->
      Aeson.Object (KeyMap.insert "echo" (Aeson.String echo) obj)
    _ ->
      value

dispatchActionResponse
  :: (IOE :> es, KatipE :> es, Concurrent :> es)
  => PendingResponses
  -> ActionResponse
  -> Eff es ()
dispatchActionResponse pendingResponses response =
  case response.echo of
    Nothing ->
      logInfo "Ignoring QQ action response without echo"
    Just echo -> do
      waiter <- MVar.withMVar pendingResponses \pending ->
        pure (Map.lookup echo pending)
      case waiter of
        Nothing ->
          logInfo [i|Ignoring QQ action response with unknown echo: #{echo}|]
        Just responseVar ->
          void $ MVar.tryPutMVar responseVar response

-- | Reply to a QQ private or group message.
replyToQQ
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es, Media.Media :> es)
  => QQDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Either Text MessageId)
replyToQQ driver message body =
  case (message.kind, message.chatId, message.senderId) of
    (ChatGroup, Just groupId, _) -> do
      qqMessage <- replyMessage message body
      response <- sendAction driver (Aeson.object
        [ "action" Aeson..= Aeson.String "send_group_msg"
        , "params" Aeson..= Aeson.object
            [ "group_id" Aeson..= groupId
            , "message" Aeson..= qqMessage
            ]
        ])
      qqMessageIdResult "send_group_msg" response
    (ChatPrivate, _, Just rawUserId)
      | Just userId <- parseIntegerUserId rawUserId -> do
      qqMessage <- replyMessage message body
      response <- sendAction driver (Aeson.object
        [ "action" Aeson..= Aeson.String "send_private_msg"
        , "params" Aeson..= Aeson.object
            [ "user_id" Aeson..= userId
            , "message" Aeson..= qqMessage
            ]
        ])
      qqMessageIdResult "send_private_msg" response
    _ -> pure (Left "QQ reply requires a QQ group id or private sender id.")

-- | Send a reply that mentions a QQ user where the platform supports it.
mentionUserQQ
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es, Media.Media :> es)
  => QQDriver
  -> IncomingMessage
  -> Text
  -> Text
  -> Eff es (Either Text MessageId)
mentionUserQQ driver message userId body =
  case (message.kind, message.chatId, message.senderId) of
    (ChatGroup, Just groupId, _)
      | Just numericUserId <- parseIntegerUserId userId -> do
        qqMessage <- mentionMessage message numericUserId body
        maybe (Left "QQ group mention did not produce a message id.") (Right . integerMessageId) . responseMessageId <$> sendAction driver (Aeson.object
          [ "action" Aeson..= Aeson.String "send_group_msg"
          , "params" Aeson..= Aeson.object
              [ "group_id" Aeson..= groupId
              , "message" Aeson..= qqMessage
              ]
          ])
    (ChatPrivate, _, Just rawUserId)
      | Just userId_ <- parseIntegerUserId rawUserId -> do
      qqMessage <- replyMessage message body
      maybe (Left "QQ private mention reply did not produce a message id.") (Right . integerMessageId) . responseMessageId <$> sendAction driver (Aeson.object
        [ "action" Aeson..= Aeson.String "send_private_msg"
        , "params" Aeson..= Aeson.object
            [ "user_id" Aeson..= userId_
            , "message" Aeson..= qqMessage
            ]
        ])
    _ -> pure (Left "QQ mention reply requires a QQ group id or private sender id.")

-- | Send a file segment through OneBot. The path is interpreted by NapCat, so
-- when NapCat runs in Docker it must be visible inside that container.
uploadFileQQ
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es)
  => QQDriver
  -> IncomingMessage
  -> FilePath
  -> Eff es (Either Text MessageId)
uploadFileQQ driver message path =
  case (message.kind, message.chatId, message.senderId) of
    (ChatGroup, Just groupId, _) -> do
      response <- sendFileMessage driver "send_group_msg"
        [ "group_id" Aeson..= groupId
        , "message" Aeson..= fileMessage path
        ]
      qqMessageIdResult "send_group_msg" response
    (ChatPrivate, _, Just rawUserId)
      | Just userId <- parseIntegerUserId rawUserId -> do
      response <- sendFileMessage driver "send_private_msg"
        [ "user_id" Aeson..= userId
        , "message" Aeson..= fileMessage path
        ]
      qqMessageIdResult "send_private_msg" response
    _ ->
      pure (Left "QQ file upload requires a QQ group or private chat with a known target.")

-- | Send an audio record segment through OneBot, falling back to NapCat file
-- upload for local files if the adapter rejects record sending.
replyAudioQQ
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es)
  => QQDriver
  -> IncomingMessage
  -> Text
  -> Maybe Text
  -> Eff es (Either Text MessageId)
replyAudioQQ driver message audioRef caption =
  case (message.kind, message.chatId, message.senderId) of
    (ChatGroup, Just groupId, _) -> do
      qqMessage <- audioMessage audioRef caption
      response <- sendAudioMessage driver "send_group_msg"
        [ "group_id" Aeson..= groupId
        , "message" Aeson..= qqMessage
        ]
      audioResponseOrFallback response "send_group_msg"
    (ChatPrivate, _, Just rawUserId)
      | Just userId <- parseIntegerUserId rawUserId -> do
      qqMessage <- audioMessage audioRef caption
      response <- sendAudioMessage driver "send_private_msg"
        [ "user_id" Aeson..= userId
        , "message" Aeson..= qqMessage
        ]
      audioResponseOrFallback response "send_private_msg"
    _ ->
      pure (Left "QQ audio reply requires a QQ group or private chat with a known target.")
  where
    audioResponseOrFallback response action
      | actionSucceeded response =
          qqMessageIdResult action response
      | Just path <- localAudioPath audioRef =
          uploadFileQQ driver message path
      | otherwise =
          pure (Left (qqUploadFailureText action response))

sendFileMessage
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es)
  => QQDriver
  -> Text
  -> [Aeson.Pair]
  -> Eff es ActionResponse
sendFileMessage driver action params =
  sendAction driver (Aeson.object
    [ "action" Aeson..= Aeson.String action
    , "params" Aeson..= Aeson.object params
    ])

sendAudioMessage
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es)
  => QQDriver
  -> Text
  -> [Aeson.Pair]
  -> Eff es ActionResponse
sendAudioMessage driver action params =
  sendAction driver (Aeson.object
    [ "action" Aeson..= Aeson.String action
    , "params" Aeson..= Aeson.object params
    ])

qqMessageIdResult :: Applicative f => Text -> ActionResponse -> f (Either Text MessageId)
qqMessageIdResult action response =
  if actionSucceeded response
    then pure (maybe (Left (qqMalformedMessageResponseText action)) (Right . integerMessageId) (responseMessageId response))
    else pure (Left (qqUploadFailureText action response))

fileMessage :: FilePath -> Aeson.Value
fileMessage path =
  Aeson.toJSON
    [ Aeson.object
        [ "type" Aeson..= Aeson.String "file"
        , "data" Aeson..= Aeson.object
            [ "file" Aeson..= path
            , "name" Aeson..= qqUploadFileName path
            ]
        ]
    ]

qqUploadFileName :: FilePath -> Text
qqUploadFileName path =
  let name = Text.pack (takeFileName path)
  in if Text.null name then "file" else name

qqUploadFailureText :: Text -> ActionResponse -> Text
qqUploadFailureText action response =
  [i|QQ #{action} failed: status=#{statusText}, retcode=#{retcodeText}, message=#{responseMessage}. Make sure the file path is accessible from the NapCat container.|]
  where
    statusText = show response.status :: String
    retcodeText = show response.retcode :: String
    responseMessage = fromMaybe "" response.message

qqMalformedMessageResponseText :: Text -> Text
qqMalformedMessageResponseText action =
  [i|QQ #{action} returned a successful response without data.message_id.|]

responseMessageId :: ActionResponse -> Maybe Integer
responseMessageId response =
  response.data_ >>= \case
    Aeson.Object obj -> case KeyMap.lookup "message_id" obj of
      Just value -> Aeson.parseMaybe Aeson.parseJSON value
      Nothing    -> Nothing
    _ -> Nothing

-- | Delete a QQ message by OneBot message id.
deleteMessageQQ
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es)
  => QQDriver
  -> IncomingMessage
  -> MessageId
  -> Eff es Bool
deleteMessageQQ driver _ messageId =
  case messageIdInteger messageId of
    Just rawMessageId -> do
      response <- sendAction driver (Aeson.object
        [ "action" Aeson..= Aeson.String "delete_msg"
        , "params" Aeson..= Aeson.object
            [ "message_id" Aeson..= rawMessageId
            ]
        ])
      pure (actionSucceeded response)
    _ ->
      pure False

actionSucceeded :: ActionResponse -> Bool
actionSucceeded response =
  response.status == Just "ok" || response.retcode == Just 0

-- | Fetch message text and image references by QQ message id.
getMessageContentQQ
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es)
  => QQDriver
  -> MessageId
  -> Eff es (Maybe ReferencedMessage)
getMessageContentQQ driver messageId = do
  case messageIdInteger messageId of
    Nothing ->
      pure Nothing
    Just rawMessageId -> do
      response <- sendAction driver (Aeson.object
        [ "action" Aeson..= Aeson.String "get_msg"
        , "params" Aeson..= Aeson.object
            [ "message_id" Aeson..= rawMessageId
            ]
        ])
      case response.data_ of
        Nothing ->
          pure Nothing
        Just value ->
          traverse (appendForwardedMessageText driver (referencedMessageForwardIds value)) (referencedMessageFromValue value)

appendForwardedMessageText
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es)
  => QQDriver
  -> [Text]
  -> ReferencedMessage
  -> Eff es ReferencedMessage
appendForwardedMessageText driver forwardIds referenced = do
  forwardedTexts <- traverse (getForwardedMessageText driver) forwardIds
  pure (referencedWithText referenced (joinMessageTexts (referenced.text : forwardedTexts)))

getForwardedMessageText
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es)
  => QQDriver
  -> Text
  -> Eff es Text
getForwardedMessageText driver forwardId = do
  response <- sendAction driver (Aeson.object
    [ "action" Aeson..= Aeson.String "get_forward_msg"
    , "params" Aeson..= Aeson.object
        [ "id" Aeson..= forwardId
        ]
    ])
  pure (maybe "" forwardedMessagesText response.data_)

-- | Fetch platform-provided QQ group member information.
getGroupMemberInfo
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es)
  => QQDriver
  -> Integer
  -> Integer
  -> Eff es (Maybe Aeson.Value)
getGroupMemberInfo driver groupId userId =
  (.data_) <$> sendAction driver (Aeson.object
    [ "action" Aeson..= Aeson.String "get_group_member_info"
    , "params" Aeson..= Aeson.object
        [ "group_id" Aeson..= groupId
        , "user_id" Aeson..= userId
        , "no_cache" Aeson..= False
        ]
    ])

-- | Fetch platform-provided QQ group member list.
getGroupMemberList
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es)
  => QQDriver
  -> Integer
  -> Eff es (Maybe Aeson.Value)
getGroupMemberList driver groupId =
  (.data_) <$> sendAction driver (Aeson.object
    [ "action" Aeson..= Aeson.String "get_group_member_list"
    , "params" Aeson..= Aeson.object
        [ "group_id" Aeson..= groupId
        , "no_cache" Aeson..= False
        ]
    ])

-- | Set a QQ group member's special title through OneBot/NapCat.
setGroupMemberTitleQQ
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es)
  => QQDriver
  -> IncomingMessage
  -> Text
  -> Text
  -> Eff es Bool
setGroupMemberTitleQQ driver message userId title =
  case (message.kind, message.chatId) of
    (ChatGroup, Just groupId)
      | Just numericUserId <- parseIntegerUserId userId -> do
      response <- sendAction driver (Aeson.object
        [ "action" Aeson..= Aeson.String "set_group_special_title"
        , "params" Aeson..= Aeson.object
            [ "group_id" Aeson..= groupId
            , "user_id" Aeson..= numericUserId
            , "special_title" Aeson..= title
            , "duration" Aeson..= (-1 :: Integer)
            ]
        ])
      pure (actionSucceeded response)
    _ ->
      pure False

-- | Return a stable QQ avatar URL for a user id.
getUserAvatar :: Integer -> Aeson.Value
getUserAvatar userId =
  Aeson.object
    [ "platform" Aeson..= ("qq" :: Text)
    , "user_id" Aeson..= userId
    , "avatar_url" Aeson..= qqAvatarUrl userId
    ]

qqAvatarUrl :: Integer -> Text
qqAvatarUrl userId =
  [i|https://q.qlogo.cn/g?b=qq&nk=#{userId}&s=640|]

parseIntegerUserId :: Text -> Maybe Integer
parseIntegerUserId raw =
  case reads (Text.unpack (Text.strip raw)) of
    [(userId, "")] ->
      Just userId
    _ ->
      Nothing

referencedWithText :: ReferencedMessage -> Text -> ReferencedMessage
referencedWithText ReferencedMessage{messageId, senderDisplayName, senderIdentifier, imageUrls} text =
  ReferencedMessage{messageId, senderDisplayName, senderIdentifier, text, imageUrls}

referencedMessageFromValue :: Aeson.Value -> Maybe ReferencedMessage
referencedMessageFromValue = Aeson.parseMaybe $
  Aeson.withObject "ReferencedMessage" $ \o -> do
    rawMessageId <- o Aeson..:? "message_id"
    message <- o Aeson..:? "message"
    rawMessage <- o Aeson..:? "raw_message"
    sender <- o Aeson..:? "sender"
    let messageId = integerMessageId <$> (rawMessageId :: Maybe Integer)
    let text = fromMaybe "" ((message >>= messageText) <|> rawMessage)
    let imageUrls = maybe [] messageImageUrls message
    let senderDisplayName = sender >>= qqSenderDisplayName
    let senderIdentifier = sender >>= qqSenderIdentifier
    pure ReferencedMessage{..}

qqSenderDisplayName :: Aeson.Value -> Maybe Text
qqSenderDisplayName = Aeson.parseMaybe $
  Aeson.withObject "QQSender" $ \o -> do
    nickname <- o Aeson..:? "nickname"
    card <- o Aeson..:? "card"
    maybe (fail "QQ sender has no display name") pure (nonEmptyText nickname <|> nonEmptyText card)

qqSenderIdentifier :: Aeson.Value -> Maybe Text
qqSenderIdentifier = Aeson.parseMaybe $
  Aeson.withObject "QQSender" $ \o -> do
    userId <- o Aeson..: "user_id"
    pure (show (userId :: Integer))

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText value =
  Text.strip <$> value >>= \text ->
    text <$ guard (not (Text.null text))

replyMessage :: (IOE :> es, Media.Media :> es) => IncomingMessage -> Text -> Eff es Aeson.Value
replyMessage message body =
  Aeson.toJSON <$> maybe textOnly withReply message.messageId
  where
    text = Chat.renderReplyBody body
    imageUrls = Chat.replyImageUrls body
    textOnly =
      replyContent text imageUrls
    withReply messageId =
      ( [ Aeson.object
            [ "type" Aeson..= Aeson.String "reply"
            , "data" Aeson..= Aeson.object
                [ "id" Aeson..= messageIdText messageId
                ]
            ]
        ] <>
      ) <$> replyContent text imageUrls

mentionMessage :: (IOE :> es, Media.Media :> es) => IncomingMessage -> Integer -> Text -> Eff es Aeson.Value
mentionMessage message userId body =
  Aeson.toJSON <$> maybe mentionOnly withReply message.messageId
  where
    text = Chat.renderReplyBody body
    mentionOnly =
      pure (mentionContent userId text)
    withReply messageId =
      pure
        ( [ Aeson.object
              [ "type" Aeson..= Aeson.String "reply"
              , "data" Aeson..= Aeson.object
                  [ "id" Aeson..= messageIdText messageId
                  ]
              ]
          ] <> mentionContent userId text
        )

mentionContent :: Integer -> Text -> [Aeson.Value]
mentionContent userId body =
  [ Aeson.object
      [ "type" Aeson..= Aeson.String "at"
      , "data" Aeson..= Aeson.object
          [ "qq" Aeson..= userId
          ]
      ]
  , Aeson.object
      [ "type" Aeson..= Aeson.String "text"
      , "data" Aeson..= Aeson.object
          [ "text" Aeson..= (" " <> body)
          ]
      ]
  ]

replyContent :: (IOE :> es, Media.Media :> es) => Text -> [Text] -> Eff es [Aeson.Value]
replyContent body imageUrls = do
  imageSegments <- traverse imageSegment imageUrls
  pure $
    [ textSegment body | not (Text.null body) ]
      <> imageSegments

audioMessage :: IOE :> es => Text -> Maybe Text -> Eff es Aeson.Value
audioMessage audioRef caption =
  Aeson.toJSON <$> audioContent audioRef caption

audioContent :: IOE :> es => Text -> Maybe Text -> Eff es [Aeson.Value]
audioContent audioRef caption = do
  record <- recordSegment audioRef
  pure $
    maybe [] (\text -> [textSegment text | not (Text.null (Text.strip text))]) caption
      <> [record]

recordSegment :: IOE :> es => Text -> Eff es Aeson.Value
recordSegment ref =
  recordSegmentValue <$> qqAudioFile ref

recordSegmentValue :: Text -> Aeson.Value
recordSegmentValue file =
  Aeson.object
    [ "type" Aeson..= Aeson.String "record"
    , "data" Aeson..= Aeson.object
        [ "file" Aeson..= file
        ]
    ]

textSegment :: Text -> Aeson.Value
textSegment body =
  Aeson.object
    [ "type" Aeson..= Aeson.String "text"
    , "data" Aeson..= Aeson.object
        [ "text" Aeson..= body
        ]
    ]

imageSegment :: (IOE :> es, Media.Media :> es) => Text -> Eff es Aeson.Value
imageSegment url =
  imageSegmentValue <$> qqImageFile url

imageSegmentValue :: Text -> Aeson.Value
imageSegmentValue file =
  Aeson.object
    [ "type" Aeson..= Aeson.String "image"
    , "data" Aeson..= Aeson.object
        [ "file" Aeson..= file
        ]
    ]

qqImageFile :: (IOE :> es, Media.Media :> es) => Text -> Eff es Text
qqImageFile url =
  qqPublicImageRef url

qqPublicImageRef :: Media.Media :> es => Text -> Eff es Text
qqPublicImageRef url =
  maybe (pure url) Media.publicMediaRef (mediaRef url)

mediaRef :: Text -> Maybe Text
mediaRef ref =
  let stripped = Text.strip ref
  in stripped <$ guard ("media:" `Text.isPrefixOf` stripped)

qqAudioFile :: IOE :> es => Text -> Eff es Text
qqAudioFile ref =
  case localAudioPath ref of
    Just path ->
      pure ("file://" <> Text.pack path)
    Nothing ->
      pure (fromMaybe ref (dataAudioBase64 ref))

localAudioPath :: Text -> Maybe FilePath
localAudioPath ref =
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

dataAudioBase64 :: Text -> Maybe Text
dataAudioBase64 ref = do
  rest <- Text.stripPrefix "data:audio/" (Text.strip ref)
  let (_, encodedWithMarker) = Text.breakOn ";base64," rest
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  pure ("base64://" <> encoded)

-- | Raw OneBot event fields needed by the message parser.
data Event = Event
  { time        :: !(Maybe Integer)
  , selfId      :: !(Maybe Integer)
  , postType    :: !Text
  , messageType :: !(Maybe Text)
  , subType     :: !(Maybe Text)
  , messageId   :: !(Maybe Integer)
  , userId      :: !(Maybe Integer)
  , groupId     :: !(Maybe Integer)
  , message     :: !(Maybe Aeson.Value)
  , rawMessage  :: !(Maybe Text)
  , sender      :: !(Maybe Aeson.Value)
  , rawEvent    :: !Aeson.Value
  }
  deriving (Show)

instance Aeson.FromJSON Event where
  parseJSON rawEvent = Aeson.withObject "OneBotEvent" parse rawEvent
    where
      parse o = do
        time <- o Aeson..:? "time"
        selfId <- o Aeson..:? "self_id"
        postType <- o Aeson..: "post_type"
        messageType <- o Aeson..:? "message_type"
        subType <- o Aeson..:? "sub_type"
        messageId <- o Aeson..:? "message_id"
        userId <- o Aeson..:? "user_id"
        groupId <- o Aeson..:? "group_id"
        message <- o Aeson..:? "message"
        rawMessage <- o Aeson..:? "raw_message"
        sender <- o Aeson..:? "sender"
        pure Event{..}

eventToIncomingMessage :: Event -> Maybe IncomingMessage
eventToIncomingMessage =
  eventToIncomingMessageWith defaultMessageConfig

isHeartbeatEvent :: Event -> Bool
isHeartbeatEvent event =
  event.postType == "meta_event"
    && case event.rawEvent of
      Aeson.Object obj ->
        KeyMap.lookup "meta_event_type" obj == Just (Aeson.String "heartbeat")
      _ ->
        False

eventToIncomingMessageWith :: Config -> Event -> Maybe IncomingMessage
eventToIncomingMessageWith cfg event
  | event.postType /= "message" = Nothing
  | isSelfMessage event = Nothing
  | otherwise = Just IncomingMessage
      { platform  = PlatformQQ
      , kind      = oneBotChatKind event.messageType
      , chatId    = event.groupId <|> event.userId
      , chatAliases = []
      , digest = qqMessageDigest cfg event
      , senderId  = Text.pack . show <$> event.userId
      , senderUsername = Nothing
      , messageId = integerMessageId <$> event.messageId
      , replyToMessageId = integerMessageId <$> (event.message >>= replySegmentMessageId)
      , mentions  = eventMentionIds event
      , mentionUsernames = []
      , imageUrls = maybe [] messageImageUrls event.message
      , text      = fromMaybe "" ((event.message >>= messageText) <|> event.rawMessage)
      , raw       = event.rawEvent
      }

defaultMessageConfig :: Config
defaultMessageConfig =
  Config
    { host = ""
    , port = 0
    , path = ""
    , token = Nothing
    , botQQ = Nothing
    , allowedGroups = []
    , allowedUsers = []
    , superusers = []
    }

qqMessageDigest :: Config -> Event -> MessageDigest
qqMessageDigest cfg event =
  MessageDigest
    { chatIsAllowed = maybe False (`elem` cfg.allowedGroups) event.groupId
    , senderIsAllowed = senderAllowed
    , senderIsSuperuser = senderSuperuser
    , mentionsBot = maybe False ((`elem` eventMentionIds event) . show) cfg.botQQ
    , botId = Text.pack . show <$> (event.selfId <|> cfg.botQQ)
    }
  where
    senderAllowed =
      maybe False (\userId -> userId `elem` cfg.allowedUsers || userId `elem` cfg.superusers) event.userId
    senderSuperuser =
      maybe False (`elem` cfg.superusers) event.userId

isSelfMessage :: Event -> Bool
isSelfMessage event =
  isJust do
    selfId <- event.selfId
    userId <- event.userId
    guard (selfId == userId)

oneBotChatKind :: Maybe Text -> ChatKind
oneBotChatKind = \case
  Just "private" -> ChatPrivate
  Just "group"   -> ChatGroup
  Just other     -> ChatUnknown other
  Nothing        -> ChatUnknown "unknown"

messageText :: Aeson.Value -> Maybe Text
messageText = \case
  Aeson.String text -> Just (Text.strip (rawMessageText text))
  Aeson.Array segments -> Just (Text.strip (foldMap segmentText segments))
  _ -> Nothing

messageImageUrls :: Aeson.Value -> [Text]
messageImageUrls = \case
  Aeson.Array segments -> mapMaybe imageSegmentUrl (toList segments)
  _ -> []

referencedMessageForwardIds :: Aeson.Value -> [Text]
referencedMessageForwardIds =
  Aeson.parseMaybe (Aeson.withObject "ReferencedMessageForwardIds" (Aeson..:? "message")) >>> \case
    Just (Just message) -> messageForwardIds message
    _ -> []

messageForwardIds :: Aeson.Value -> [Text]
messageForwardIds = \case
  Aeson.Array segments -> mapMaybe forwardSegmentId (toList segments)
  _ -> []

forwardSegmentId :: Aeson.Value -> Maybe Text
forwardSegmentId = \case
  Aeson.Object obj
    | Just (Aeson.String "forward") <- KeyMap.lookup "type" obj
    , Just (Aeson.Object data_) <- KeyMap.lookup "data" obj
    , Just (Aeson.String id_) <- KeyMap.lookup "id" data_
    , not (Text.null id_) ->
        Just id_
  _ -> Nothing

forwardedMessagesText :: Aeson.Value -> Text
forwardedMessagesText =
  joinMessageTexts . forwardedMessageTexts

forwardedMessageTexts :: Aeson.Value -> [Text]
forwardedMessageTexts = \case
  Aeson.Object obj
    | Just (Aeson.Array messages) <- KeyMap.lookup "messages" obj ->
        mapMaybe forwardedMessageNodeText (toList messages)
    | Just (Aeson.Array messages) <- KeyMap.lookup "content" obj ->
        mapMaybe forwardedMessageNodeText (toList messages)
  Aeson.Array messages ->
    mapMaybe forwardedMessageNodeText (toList messages)
  message ->
    maybeToList (messageText message)

forwardedMessageNodeText :: Aeson.Value -> Maybe Text
forwardedMessageNodeText value =
  nonEmptyText
    (forwardedNodeContent value >>= messageText)
    <|> nonEmptyText (forwardedNodeRawMessage value)
    <|> nonEmptyText (messageText value)

forwardedNodeContent :: Aeson.Value -> Maybe Aeson.Value
forwardedNodeContent =
  join . Aeson.parseMaybe parser
  where
    parser =
      Aeson.withObject "ForwardedMessageNode" $ \o -> do
        type_ <- o Aeson..:? "type"
        data_ <- o Aeson..:? "data"
        content <- o Aeson..:? "content"
        message <- o Aeson..:? "message"
        case (type_ :: Maybe Text, data_ :: Maybe Aeson.Value, content, message) of
          (Just "node", Just (Aeson.Object nodeData), _, _) ->
            pure (KeyMap.lookup "content" nodeData)
          (_, _, Just value, _) ->
            pure (Just value)
          (_, _, _, Just value) ->
            pure (Just value)
          _ ->
            pure Nothing

forwardedNodeRawMessage :: Aeson.Value -> Maybe Text
forwardedNodeRawMessage =
  join . Aeson.parseMaybe (Aeson.withObject "ForwardedMessageNode" (Aeson..:? "raw_message"))

joinMessageTexts :: [Text] -> Text
joinMessageTexts =
  Text.intercalate "\n" . filter (not . Text.null) . map Text.strip

imageSegmentUrl :: Aeson.Value -> Maybe Text
imageSegmentUrl = \case
  Aeson.Object obj
    | Just (Aeson.String "image") <- KeyMap.lookup "type" obj
    , Just (Aeson.Object data_) <- KeyMap.lookup "data" obj ->
        case KeyMap.lookup "url" data_ of
          Just (Aeson.String url) | not (Text.null url) -> Just url
          _ -> Nothing
  _ -> Nothing

segmentText :: Aeson.Value -> Text
segmentText = \case
  Aeson.Object obj
    | Just (Aeson.String "text") <- KeyMap.lookup "type" obj
    , Just (Aeson.Object data_) <- KeyMap.lookup "data" obj
    , Just (Aeson.String text) <- KeyMap.lookup "text" data_ ->
        text
    | Just (Aeson.String "at") <- KeyMap.lookup "type" obj
    , Just (Aeson.Object data_) <- KeyMap.lookup "data" obj
    , Just value <- KeyMap.lookup "qq" data_
    , Just qq <- mentionValueText value ->
        "@" <> qq
  _ -> ""

rawMessageText :: Text -> Text
rawMessageText text =
  case Text.breakOn "[CQ:at" text of
    (before, "") ->
      before
    (before, rest) ->
      let afterStart = Text.drop (Text.length "[CQ:at") rest
          (segment, afterSegment) = Text.breakOn "]" afterStart
          renderedMention = maybe "" ("@" <>) (rawMentionValue segment)
          next = Text.drop 1 afterSegment
      in before <> renderedMention <> rawMessageText next

mentionIds :: Aeson.Value -> [Text]
mentionIds = \case
  Aeson.Array segments -> mapMaybe mentionSegmentId (toList segments)
  _ -> []

eventMentionIds :: Event -> [Text]
eventMentionIds event =
  case maybe [] mentionIds event.message of
    [] -> maybe [] rawMentionIds event.rawMessage
    ids -> ids

rawMentionIds :: Text -> [Text]
rawMentionIds text =
  case Text.breakOn "[CQ:at" text of
    (_, "") ->
      []
    (_, rest) ->
      let afterStart = Text.drop (Text.length "[CQ:at") rest
          (segment, afterSegment) = Text.breakOn "]" afterStart
          next = Text.drop 1 afterSegment
      in maybeToList (rawMentionId segment) <> rawMentionIds next

rawMentionId :: Text -> Maybe Text
rawMentionId segment = do
  rawMentionValue segment

rawMentionValue :: Text -> Maybe Text
rawMentionValue segment = do
  field <- find (Text.isPrefixOf "qq=") (Text.splitOn "," segment)
  pure (Text.drop (Text.length "qq=") field)

mentionSegmentId :: Aeson.Value -> Maybe Text
mentionSegmentId = \case
  Aeson.Object obj
    | Just (Aeson.String "at") <- KeyMap.lookup "type" obj
    , Just (Aeson.Object data_) <- KeyMap.lookup "data" obj
    , Just value <- KeyMap.lookup "qq" data_ ->
        mentionValueText value
  _ -> Nothing

mentionValueText :: Aeson.Value -> Maybe Text
mentionValueText = \case
  Aeson.String "all" -> Nothing
  Aeson.String qq -> Just qq
  value -> Text.pack . show <$> parseReplyId value

replySegmentMessageId :: Aeson.Value -> Maybe Integer
replySegmentMessageId = \case
  Aeson.Array segments -> asum (map replySegmentId (toList segments))
  _ -> Nothing

replySegmentId :: Aeson.Value -> Maybe Integer
replySegmentId = \case
  Aeson.Object obj
    | Just (Aeson.String "reply") <- KeyMap.lookup "type" obj
    , Just (Aeson.Object data_) <- KeyMap.lookup "data" obj
    , Just value <- KeyMap.lookup "id" data_ ->
        parseReplyId value
  _ -> Nothing

parseReplyId :: Aeson.Value -> Maybe Integer
parseReplyId value =
  Aeson.parseMaybe Aeson.parseJSON value <|>
    case value of
      Aeson.String text -> readMaybe (toString text)
      _ -> Nothing
