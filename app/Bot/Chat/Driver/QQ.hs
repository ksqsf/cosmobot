{-|
Module      : Bot.Chat.Driver.QQ
Description : QQ/NapCat OneBot v11 websocket effect
Stability   : experimental
-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Chat.Driver.QQ
  ( qqDriver
  , QQ
  , Config (..)
  , Event (..)
  , ActionResponse (..)
  , receiveEvent
  , sendAction
  , runQQ
  , eventsStream
  , incomingMessages
  , eventToIncomingMessage
  , eventToIncomingMessageWith
  , forwardedMessagesText
  , readActionResponse
  , replyTo
  , getMessageContent
  , getUserAvatar
  , getGroupMemberInfo
  , getGroupMemberList
  , mentionUser
  )
where

import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Prelude
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyByteString
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text as Text
import qualified Network.WebSockets as WS
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified System.Timeout as Timeout

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

qqDriver
  :: (QQ :> es, IOE :> es)
  => Driver.ChatPlatformDriver es
qqDriver = Driver.ChatPlatformDriver
  { Driver.platform = PlatformQQ
  , Driver.replyTo = replyTo
  , Driver.editMessage = \_ _ _ -> pure False
  , Driver.replyStreamStyle = \_ -> pure (Chat.ChunkedReply qqStreamingMessageLimit)
  , Driver.getMessageContent = \_ messageId -> getMessageContent messageId
  , Driver.getSenderMemberInfo = \message ->
      case (message.kind, message.chatId, message.senderId) of
        (ChatGroup, Just groupId, Just rawUserId)
          | Just userId <- parseIntegerUserId rawUserId ->
          getGroupMemberInfo groupId userId
        _ ->
          pure Nothing
  , Driver.getMemberInfo = \message userId ->
      case (message.kind, message.chatId) of
        (ChatGroup, Just groupId) ->
          getGroupMemberInfo groupId userId
        _ ->
          pure Nothing
  , Driver.getUserAvatar = \message userId ->
      case message.platform of
        PlatformQQ ->
          pure (getUserAvatar <$> parseIntegerUserId userId)
        _ ->
          pure Nothing
  , Driver.listGroupMembers = \message ->
      case (message.kind, message.chatId) of
        (ChatGroup, Just groupId) ->
          getGroupMemberList groupId
        _ ->
          pure Nothing
  , Driver.mentionUser = mentionUser
  }

qqStreamingMessageLimit :: Int
qqStreamingMessageLimit = 4000

-- ---------------------------------------------------------------------------
-- Effect
-- ---------------------------------------------------------------------------

-- | Low-level OneBot transport effect.
data QQ :: Effect where
  QQConfig :: QQ m Config
  ReceiveEvent :: QQ m Event
  SendAction :: Aeson.Value -> QQ m ActionResponse

type instance DispatchOf QQ = Dynamic

qqConfig :: QQ :> es => Eff es Config
qqConfig = send QQConfig

-- | Receive one raw OneBot event from the websocket reader.
receiveEvent :: QQ :> es => Eff es Event
receiveEvent = send ReceiveEvent

-- | Send one raw OneBot action payload.
sendAction :: QQ :> es => Aeson.Value -> Eff es ActionResponse
sendAction = send . SendAction

-- | Connect to OneBot and interpret QQ operations over the websocket.
runQQ
  :: IOE :> es
  => Log :> es
  => Config
  -> Eff (QQ : es) a
  -> Eff es a
runQQ cfg inner = withEffToIO (ConcUnlift Persistent Unlimited) $ \runInIO -> do
  eventChan <- liftIO Chan.newChan
  actionChan <- liftIO Chan.newChan
  liftIO $ Async.withAsync (runInIO $ qqConnectionLoop cfg eventChan actionChan) \_ ->
    runInIO $
      interpret
          (\_ -> \case
            QQConfig ->
              pure cfg
            ReceiveEvent ->
              liftIO (Chan.readChan eventChan)
            SendAction value -> do
              responseVar <- liftIO MVar.newEmptyMVar
              liftIO $ Chan.writeChan actionChan (ActionRequest value responseVar)
              result <- liftIO $ Timeout.timeout qqActionTimeoutMicroseconds (MVar.takeMVar responseVar)
              case result of
                Just response ->
                  pure response
                Nothing -> do
                  logInfo_ "QQ action timed out"
                  pure failedActionResponse)
          inner

data ActionRequest = ActionRequest !Aeson.Value !(MVar.MVar ActionResponse)

qqConnectionLoop
  :: (IOE :> es, Log :> es)
  => Config
  -> Chan.Chan Event
  -> Chan.Chan ActionRequest
  -> Eff es ()
qqConnectionLoop cfg eventChan actionChan =
  forever do
    result <- runQQConnectionOnce cfg eventChan actionChan
    case result of
      Right () ->
        logInfo_ "QQ websocket disconnected; reconnecting"
      Left err ->
        logInfo_ [i|QQ websocket failed; reconnecting: #{err}|]
    liftIO $ threadDelay qqReconnectDelayMicroseconds

runQQConnectionOnce
  :: (IOE :> es, Log :> es)
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

runConnection
  :: (IOE :> es, Log :> es)
  => Chan.Chan Event
  -> Chan.Chan ActionRequest
  -> WS.Connection
  -> Eff es ()
runConnection eventChan actionChan conn = do
  pendingResponses <- liftIO (MVar.newMVar Map.empty)
  actionCounter <- liftIO (MVar.newMVar (1 :: Integer))
  done <- liftIO MVar.newEmptyMVar
  readerThread <- forkConnectionThread done (readFrames eventChan pendingResponses conn)
  sender <- forkConnectionThread done (sendActions actionChan pendingResponses actionCounter conn)
  reason <- liftIO (MVar.takeMVar done)
  liftIO (killThread readerThread)
  liftIO (killThread sender)
  failPendingResponses pendingResponses
  logInfo_ [i|QQ websocket connection ended: #{show reason :: String}|]

forkConnectionThread
  :: (IOE :> es, Log :> es)
  => MVar.MVar SomeException
  -> Eff es ()
  -> Eff es ThreadId
forkConnectionThread done action =
  withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
    liftIO $ forkIO do
      result <- Exception.try (runInIO action)
      case result of
        Left err ->
          void (MVar.tryPutMVar done err)
        Right () ->
          void (MVar.tryPutMVar done (toException ThreadKilled))

sendActions
  :: (IOE :> es, Log :> es)
  => Chan.Chan ActionRequest
  -> PendingResponses
  -> MVar.MVar Integer
  -> WS.Connection
  -> Eff es ()
sendActions actionChan pendingResponses actionCounter conn =
  forever do
    ActionRequest value responseVar <- liftIO (Chan.readChan actionChan)
    echo <- liftIO (nextActionEcho actionCounter)
    let echoedValue = addActionEcho echo value
    liftIO $ MVar.modifyMVar_ pendingResponses \pending ->
      pure (Map.insert echo responseVar pending)
    (liftIO (WS.sendTextData conn (Aeson.encode echoedValue)) `catch` \(err :: SomeException) -> do
      liftIO $ MVar.modifyMVar_ pendingResponses \pending ->
        pure (Map.delete echo pending)
      void $ liftIO (MVar.tryPutMVar responseVar failedActionResponse)
      throwIO err)

failPendingResponses :: IOE :> es => PendingResponses -> Eff es ()
failPendingResponses pendingResponses = do
  pending <- liftIO $ MVar.modifyMVar pendingResponses \pending ->
    pure (Map.empty, pending)
  traverse_ (liftIO . flip MVar.tryPutMVar failedActionResponse) pending

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

-- | Stream raw OneBot events.
eventsStream :: QQ :> es => Stream (Of Event) (Eff es) ()
eventsStream = do
  event <- S.lift receiveEvent
  S.yield event
  eventsStream

-- | Stream OneBot message events as platform-independent messages.
incomingMessages :: (QQ :> es, Log :> es) => Stream (Of IncomingMessage) (Eff es) ()
incomingMessages = do
  cfg <- S.lift qqConfig
  event <- S.lift receiveEvent
  case eventToIncomingMessageWith cfg event of
    Nothing -> do
      let Event{postType} = event
      S.lift $ logTrace_ [i|Ignoring QQ event: #{postType}|]
      S.lift $ logInfo_ [i|Ignoring QQ event: #{postType}|]
    Just message -> do
      S.lift $ logTrace "incoming qq message" message
      S.lift $ logInfo_ [i|incoming qq message: #{incomingMessageLogLine message}|]
      S.yield message
  incomingMessages

-- ---------------------------------------------------------------------------
-- OneBot v11 events
-- ---------------------------------------------------------------------------

readFrames
  :: (IOE :> es, Log :> es)
  => Chan.Chan Event
  -> PendingResponses
  -> WS.Connection
  -> Eff es ()
readFrames eventChan pendingResponses conn = forever do
  value <- readValue conn
  case Aeson.fromJSON value of
    Aeson.Success event ->
      liftIO $ Chan.writeChan eventChan event
    Aeson.Error _ ->
      case Aeson.fromJSON value of
        Aeson.Success response ->
          dispatchActionResponse pendingResponses response
        Aeson.Error err ->
          logInfo_ [i|Ignoring malformed QQ frame: #{Text.pack err}|]

-- | Read frames until an action response is found.
readActionResponse :: (IOE :> es, Log :> es) => WS.Connection -> Eff es ActionResponse
readActionResponse conn = do
  value <- readValue conn
  case Aeson.fromJSON value of
    Aeson.Success response -> pure response
    Aeson.Error _ ->
      case Aeson.fromJSON value of
        Aeson.Success (_event :: Event) ->
          readActionResponse conn
        Aeson.Error err -> do
          logInfo_ [i|Ignoring malformed QQ action response: #{Text.pack err}|]
          readActionResponse conn

readValue :: (IOE :> es, Log :> es) => WS.Connection -> Eff es Aeson.Value
readValue conn = do
  bytes <- liftIO (WS.receiveData conn :: IO ByteString)
  case Aeson.eitherDecodeStrict bytes of
    Right value -> pure value
    Left err -> do
      logInfo_ [i|Ignoring malformed QQ frame: #{Text.pack err}|]
      readValue conn

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

type PendingResponses = MVar.MVar (Map Text (MVar.MVar ActionResponse))

nextActionEcho :: MVar.MVar Integer -> IO Text
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
  :: (IOE :> es, Log :> es)
  => PendingResponses
  -> ActionResponse
  -> Eff es ()
dispatchActionResponse pendingResponses response =
  case response.echo of
    Nothing ->
      logInfo_ "Ignoring QQ action response without echo"
    Just echo -> do
      waiter <- liftIO $ MVar.withMVar pendingResponses \pending ->
        pure (Map.lookup echo pending)
      case waiter of
        Nothing ->
          logInfo_ [i|Ignoring QQ action response with unknown echo: #{echo}|]
        Just responseVar ->
          void $ liftIO (MVar.tryPutMVar responseVar response)

-- | Reply to a QQ private or group message.
replyTo :: (QQ :> es, IOE :> es) => IncomingMessage -> Text -> Eff es (Maybe Integer)
replyTo message body =
  case (message.platform, message.kind, message.chatId, message.senderId) of
    (PlatformQQ, ChatGroup, Just groupId, _) -> do
      qqMessage <- replyMessage message body
      responseMessageId <$> sendAction (Aeson.object
        [ "action" Aeson..= Aeson.String "send_group_msg"
        , "params" Aeson..= Aeson.object
            [ "group_id" Aeson..= groupId
            , "message" Aeson..= qqMessage
            ]
        ])
    (PlatformQQ, ChatPrivate, _, Just rawUserId)
      | Just userId <- parseIntegerUserId rawUserId -> do
      qqMessage <- replyMessage message body
      responseMessageId <$> sendAction (Aeson.object
        [ "action" Aeson..= Aeson.String "send_private_msg"
        , "params" Aeson..= Aeson.object
            [ "user_id" Aeson..= userId
            , "message" Aeson..= qqMessage
            ]
        ])
    _ -> pure Nothing

-- | Send a reply that mentions a QQ user where the platform supports it.
mentionUser :: (QQ :> es, IOE :> es) => IncomingMessage -> Integer -> Text -> Eff es (Maybe Integer)
mentionUser message userId body =
  case (message.platform, message.kind, message.chatId, message.senderId) of
    (PlatformQQ, ChatGroup, Just groupId, _) -> do
      qqMessage <- mentionMessage message userId body
      responseMessageId <$> sendAction (Aeson.object
        [ "action" Aeson..= Aeson.String "send_group_msg"
        , "params" Aeson..= Aeson.object
            [ "group_id" Aeson..= groupId
            , "message" Aeson..= qqMessage
            ]
        ])
    (PlatformQQ, ChatPrivate, _, Just rawUserId)
      | Just userId_ <- parseIntegerUserId rawUserId -> do
      qqMessage <- replyMessage message body
      responseMessageId <$> sendAction (Aeson.object
        [ "action" Aeson..= Aeson.String "send_private_msg"
        , "params" Aeson..= Aeson.object
            [ "user_id" Aeson..= userId_
            , "message" Aeson..= qqMessage
            ]
        ])
    _ -> pure Nothing

responseMessageId :: ActionResponse -> Maybe Integer
responseMessageId response =
  response.data_ >>= \case
    Aeson.Object obj -> case KeyMap.lookup "message_id" obj of
      Just value -> Aeson.parseMaybe Aeson.parseJSON value
      Nothing    -> Nothing
    _ -> Nothing

-- | Fetch message text and image references by QQ message id.
getMessageContent :: QQ :> es => Integer -> Eff es (Maybe ReferencedMessage)
getMessageContent messageId = do
  response <- sendAction (Aeson.object
    [ "action" Aeson..= Aeson.String "get_msg"
    , "params" Aeson..= Aeson.object
        [ "message_id" Aeson..= messageId
        ]
    ])
  case response.data_ of
    Nothing ->
      pure Nothing
    Just value ->
      traverse (appendForwardedMessageText (referencedMessageForwardIds value)) (referencedMessageFromValue value)

appendForwardedMessageText :: QQ :> es => [Text] -> ReferencedMessage -> Eff es ReferencedMessage
appendForwardedMessageText forwardIds referenced = do
  forwardedTexts <- traverse getForwardedMessageText forwardIds
  pure (referencedWithText referenced (joinMessageTexts (referenced.text : forwardedTexts)))

getForwardedMessageText :: QQ :> es => Text -> Eff es Text
getForwardedMessageText forwardId = do
  response <- sendAction (Aeson.object
    [ "action" Aeson..= Aeson.String "get_forward_msg"
    , "params" Aeson..= Aeson.object
        [ "id" Aeson..= forwardId
        ]
    ])
  pure (maybe "" forwardedMessagesText response.data_)

-- | Fetch platform-provided QQ group member information.
getGroupMemberInfo :: QQ :> es => Integer -> Integer -> Eff es (Maybe Aeson.Value)
getGroupMemberInfo groupId userId =
  (.data_) <$> sendAction (Aeson.object
    [ "action" Aeson..= Aeson.String "get_group_member_info"
    , "params" Aeson..= Aeson.object
        [ "group_id" Aeson..= groupId
        , "user_id" Aeson..= userId
        , "no_cache" Aeson..= False
        ]
    ])

-- | Fetch platform-provided QQ group member list.
getGroupMemberList :: QQ :> es => Integer -> Eff es (Maybe Aeson.Value)
getGroupMemberList groupId =
  (.data_) <$> sendAction (Aeson.object
    [ "action" Aeson..= Aeson.String "get_group_member_list"
    , "params" Aeson..= Aeson.object
        [ "group_id" Aeson..= groupId
        , "no_cache" Aeson..= False
        ]
    ])

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
  [i|https://q1.qlogo.cn/g?b=qq&nk=#{userId}&s=640|]

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
    messageId <- o Aeson..:? "message_id"
    message <- o Aeson..:? "message"
    rawMessage <- o Aeson..:? "raw_message"
    sender <- o Aeson..:? "sender"
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

replyMessage :: IOE :> es => IncomingMessage -> Text -> Eff es Aeson.Value
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
                [ "id" Aeson..= (show messageId :: Text)
                ]
            ]
        ] <>
      ) <$> replyContent text imageUrls

mentionMessage :: IOE :> es => IncomingMessage -> Integer -> Text -> Eff es Aeson.Value
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
                  [ "id" Aeson..= messageId
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

replyContent :: IOE :> es => Text -> [Text] -> Eff es [Aeson.Value]
replyContent body imageUrls = do
  imageSegments <- traverse imageSegment imageUrls
  pure $
    [ textSegment body | not (Text.null body) ]
      <> imageSegments

textSegment :: Text -> Aeson.Value
textSegment body =
  Aeson.object
    [ "type" Aeson..= Aeson.String "text"
    , "data" Aeson..= Aeson.object
        [ "text" Aeson..= body
        ]
    ]

imageSegment :: IOE :> es => Text -> Eff es Aeson.Value
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

qqImageFile :: IOE :> es => Text -> Eff es Text
qqImageFile url =
  case Text.stripPrefix "file://" url of
    Nothing ->
      pure url
    Just path -> do
      bytes <- liftIO (ByteString.readFile (Text.unpack path))
      pure ("base64://" <> TextEncoding.decodeUtf8 (Base64.encode bytes))

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
      , messageId = event.messageId
      , replyToMessageId = event.message >>= replySegmentMessageId
      , mentions  = maybe [] mentionIds event.message
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
    , mentionsBot = maybe False (`elem` maybe [] mentionIds event.message) cfg.botQQ
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
  Aeson.String text -> Just (Text.strip text)
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
  _ -> ""

mentionIds :: Aeson.Value -> [Integer]
mentionIds = \case
  Aeson.Array segments -> mapMaybe mentionSegmentId (toList segments)
  _ -> []

mentionSegmentId :: Aeson.Value -> Maybe Integer
mentionSegmentId = \case
  Aeson.Object obj
    | Just (Aeson.String "at") <- KeyMap.lookup "type" obj
    , Just (Aeson.Object data_) <- KeyMap.lookup "data" obj
    , Just value <- KeyMap.lookup "qq" data_ ->
        parseQQ value
  _ -> Nothing

parseQQ :: Aeson.Value -> Maybe Integer
parseQQ = \case
  Aeson.String "all" -> Nothing
  value -> parseReplyId value

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
