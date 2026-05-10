{-|
Module      : Bot.Effect.Chat.QQ
Description : QQ/NapCat OneBot v11 websocket effect
Stability   : experimental
-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Effect.Chat.QQ
  ( QQ
  , Config (..)
  , Event (..)
  , ActionResponse (..)
  , receiveEvent
  , sendAction
  , runQQ
  , eventsStream
  , incomingMessages
  , eventToIncomingMessage
  , readActionResponse
  , replyTo
  , getMessageContent
  , getGroupMemberInfo
  , getGroupMemberList
  , mentionUser
  )
where

import qualified Bot.Effect.Chat as Chat
import Bot.Message
import Bot.Prelude
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import Data.List (isInfixOf)
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
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Effect
-- ---------------------------------------------------------------------------

-- | Low-level OneBot transport effect.
data QQ :: Effect where
  ReceiveEvent :: QQ m Event
  SendAction :: Aeson.Value -> QQ m ActionResponse

type instance DispatchOf QQ = Dynamic

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
  responseChan <- liftIO Chan.newChan
  liftIO $ WS.runClient cfg.host cfg.port (websocketPath cfg) $ \conn ->
    runInIO $ do
      _ <- liftIO $ forkIO $ runInIO $
        readFrames eventChan responseChan conn
      interpret
        (\_ -> \case
          ReceiveEvent -> liftIO (Chan.readChan eventChan)
          SendAction value -> do
            result <- (Right <$> liftIO do
                WS.sendTextData conn (Aeson.encode value)
                Timeout.timeout qqActionTimeoutMicroseconds (Chan.readChan responseChan))
              `catch` \(err :: SomeException) -> do
                logInfo "QQ action failed" (show err :: String)
                pure (Left ())
            case result of
              Right (Just response) ->
                pure response
              Right Nothing -> do
                logInfo_ "QQ action timed out"
                pure failedActionResponse
              Left () ->
                pure failedActionResponse)
        inner

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
  event <- S.lift receiveEvent
  case eventToIncomingMessage event of
    Nothing -> do
      let Event{postType} = event
      S.lift $ logTrace_ [i|Ignoring QQ event: #{postType}|]
      S.lift $ logInfo "Ignoring QQ event" postType
    Just message -> do
      S.lift $ logTrace "incoming qq message" message
      S.lift $ logInfo "incoming qq message" (incomingMessageLogLine message)
      S.yield message
  incomingMessages

-- ---------------------------------------------------------------------------
-- OneBot v11 events
-- ---------------------------------------------------------------------------

readFrames
  :: (IOE :> es, Log :> es)
  => Chan.Chan Event
  -> Chan.Chan ActionResponse
  -> WS.Connection
  -> Eff es ()
readFrames eventChan responseChan conn = forever do
  value <- readValue conn
  case Aeson.fromJSON value of
    Aeson.Success event ->
      liftIO $ Chan.writeChan eventChan event
    Aeson.Error _ ->
      case Aeson.fromJSON value of
        Aeson.Success response ->
          liftIO $ Chan.writeChan responseChan response
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
    }

qqActionTimeoutMicroseconds :: Int
qqActionTimeoutMicroseconds =
  40 * 1000000

-- | Raw OneBot action response.
data ActionResponse = ActionResponse
  { status  :: !(Maybe Text)
  , retcode :: !(Maybe Integer)
  , data_   :: !(Maybe Aeson.Value)
  , message :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance Aeson.FromJSON ActionResponse where
  parseJSON = Aeson.withObject "ActionResponse" $ \o -> do
    status <- o Aeson..:? "status"
    retcode <- o Aeson..:? "retcode"
    data_ <- o Aeson..:? "data"
    message <- o Aeson..:? "message"
    pure ActionResponse{..}

-- | Reply to a QQ private or group message.
replyTo :: (QQ :> es, IOE :> es) => IncomingMessage -> Text -> Eff es (Maybe Integer)
replyTo message body =
  case (message.platform, message.kind, message.chatId <|> message.senderId) of
    (PlatformQQ, ChatGroup, Just groupId) -> do
      qqMessage <- replyMessage message body
      responseMessageId <$> sendAction (Aeson.object
        [ "action" Aeson..= Aeson.String "send_group_msg"
        , "params" Aeson..= Aeson.object
            [ "group_id" Aeson..= groupId
            , "message" Aeson..= qqMessage
            ]
        ])
    (PlatformQQ, ChatPrivate, Just userId) -> do
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
  case (message.platform, message.kind, message.chatId <|> message.senderId) of
    (PlatformQQ, ChatGroup, Just groupId) -> do
      qqMessage <- mentionMessage message userId body
      responseMessageId <$> sendAction (Aeson.object
        [ "action" Aeson..= Aeson.String "send_group_msg"
        , "params" Aeson..= Aeson.object
            [ "group_id" Aeson..= groupId
            , "message" Aeson..= qqMessage
            ]
        ])
    (PlatformQQ, ChatPrivate, Just userId_) -> do
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
getMessageContent messageId =
  actionDataMessage <$> sendAction (Aeson.object
    [ "action" Aeson..= Aeson.String "get_msg"
    , "params" Aeson..= Aeson.object
        [ "message_id" Aeson..= messageId
        ]
    ])

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

actionDataMessage :: ActionResponse -> Maybe ReferencedMessage
actionDataMessage response =
  response.data_ >>= referencedMessageFromValue

referencedMessageFromValue :: Aeson.Value -> Maybe ReferencedMessage
referencedMessageFromValue = Aeson.parseMaybe $
  Aeson.withObject "ReferencedMessage" $ \o -> do
    messageId <- o Aeson..:? "message_id"
    message <- o Aeson..:? "message"
    rawMessage <- o Aeson..:? "raw_message"
    let text = fromMaybe "" ((message >>= messageText) <|> rawMessage)
    let imageUrls = maybe [] messageImageUrls message
    pure ReferencedMessage{..}

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
eventToIncomingMessage event
  | event.postType /= "message" = Nothing
  | isSelfMessage event = Nothing
  | otherwise = Just IncomingMessage
      { platform  = PlatformQQ
      , kind      = oneBotChatKind event.messageType
      , chatId    = event.groupId <|> event.userId
      , senderId  = event.userId
      , senderUsername = Nothing
      , messageId = event.messageId
      , replyToMessageId = event.message >>= replySegmentMessageId
      , mentions  = maybe [] mentionIds event.message
      , mentionUsernames = []
      , imageUrls = maybe [] messageImageUrls event.message
      , text      = fromMaybe "" ((event.message >>= messageText) <|> event.rawMessage)
      , raw       = event.rawEvent
      }

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
