{-|
Module      : Bot.Chat.Driver.Discord
Description : Discord Gateway and REST chat driver
Stability   : experimental
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Chat.Driver.Discord
  ( discordDriver
  , Discord
  , Config (..)
  , GatewayEnvelope (..)
  , GatewayHello (..)
  , Message (..)
  , User (..)
  , Attachment (..)
  , Member (..)
  , Reference (..)
  , CreateMessageRequest (..)
  , runDiscord
  , incomingMessages
  , eventToIncomingMessage
  , eventToIncomingMessageWith
  , replyTo
  , replyAudio
  , uploadFile
  , editMessage
  , deleteMessage
  , getMessageContent
  , mentionUser
  )
where

import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Util.HTTP as Http
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.MVar as MVar
import Effectful.FileSystem (FileSystem)
import qualified Network.Connection as Connection
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import Network.HTTP.Req
import qualified Network.TLS as TLS
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WSStream
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.FilePath (takeFileName)
import System.IO.Error (userError)

data Config = Config
  { botToken :: !Text
  , botId :: !(Maybe Text)
  , applicationId :: !(Maybe Text)
  , allowedGuilds :: ![Integer]
  , allowedChannels :: ![Integer]
  , allowedUsers :: ![Text]
  , superusers :: ![Text]
  , gatewayHost :: !String
  , gatewayPath :: !String
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig = Config
  { botToken = ""
  , botId = Nothing
  , applicationId = Nothing
  , allowedGuilds = []
  , allowedChannels = []
  , allowedUsers = []
  , superusers = []
  , gatewayHost = "gateway.discord.gg"
  , gatewayPath = "/?v=10&encoding=json"
  }

discordDriver
  :: (Discord :> es, FileSystem :> es, IOE :> es)
  => Driver.ChatPlatformDriver es
discordDriver = Driver.ChatPlatformDriver
  { Driver.platform = PlatformDiscord
  , Driver.replyTo = replyTo
  , Driver.replyAudio = replyAudio
  , Driver.uploadFile = uploadFile
  , Driver.editMessage = editMessage
  , Driver.deleteMessage = deleteMessage
  , Driver.replyStreamStyle = \_ -> pure (Chat.EditableReply discordEditChunkChars discordMessageTextLimit)
  , Driver.getMessageContent = getMessageContent
  , Driver.getSenderMemberInfo = \message ->
      case (message.platform, discordMessageGuildId message.raw, message.senderId) of
        (PlatformDiscord, Just guildId, Just userId) ->
          Just <$> getGuildMember guildId userId
        _ ->
          pure Nothing
  , Driver.getMemberInfo = \message userId ->
      case (message.platform, discordMessageGuildId message.raw) of
        (PlatformDiscord, Just guildId) ->
          Just <$> getGuildMember guildId (show userId)
        _ ->
          pure Nothing
  , Driver.getUserAvatar = \message userId ->
      case message.platform of
        PlatformDiscord ->
          Just <$> getUser userId
        _ ->
          pure Nothing
  , Driver.listGroupMembers = \message ->
      case (message.platform, discordMessageGuildId message.raw) of
        (PlatformDiscord, Just guildId) ->
          Just <$> listGuildMembers guildId
        _ ->
          pure Nothing
  , Driver.mentionUser = mentionUser
  , Driver.setMemberTitle = \_ _ _ -> pure False
  }

discordEditChunkChars :: Int
discordEditChunkChars = 512

discordMessageTextLimit :: Int
discordMessageTextLimit = 2000

data Discord :: Effect where
  DiscordConfig :: Discord m Config
  ReceiveEvent :: Discord m Message
  CreateMessage :: Text -> CreateMessageRequest -> Discord m Message
  EditMessage :: Text -> Text -> CreateMessageRequest -> Discord m Message
  DeleteMessage :: Text -> Text -> Discord m ()
  FetchMessage :: Text -> Text -> Discord m Message
  FetchUser :: Text -> Discord m Aeson.Value
  FetchGuildMember :: Text -> Text -> Discord m Aeson.Value
  FetchGuildMembers :: Text -> Discord m Aeson.Value
  UploadDiscordFile :: Text -> Maybe Text -> FilePath -> Discord m Message

type instance DispatchOf Discord = Dynamic

discordConfig :: Discord :> es => Eff es Config
discordConfig = send DiscordConfig

receiveEvent :: Discord :> es => Eff es Message
receiveEvent = send ReceiveEvent

createMessage :: Discord :> es => Text -> CreateMessageRequest -> Eff es Message
createMessage channelId request =
  send (CreateMessage channelId request)

editDiscordMessage :: Discord :> es => Text -> Text -> CreateMessageRequest -> Eff es Message
editDiscordMessage channelId messageId request =
  send (EditMessage channelId messageId request)

deleteDiscordMessage :: Discord :> es => Text -> Text -> Eff es ()
deleteDiscordMessage channelId messageId =
  send (DeleteMessage channelId messageId)

fetchMessage :: Discord :> es => Text -> Text -> Eff es Message
fetchMessage channelId messageId =
  send (FetchMessage channelId messageId)

getUser :: Discord :> es => Text -> Eff es Aeson.Value
getUser =
  send . FetchUser

getGuildMember :: Discord :> es => Text -> Text -> Eff es Aeson.Value
getGuildMember guildId userId =
  send (FetchGuildMember guildId userId)

listGuildMembers :: Discord :> es => Text -> Eff es Aeson.Value
listGuildMembers =
  send . FetchGuildMembers

uploadDiscordFile :: Discord :> es => Text -> Maybe Text -> FilePath -> Eff es Message
uploadDiscordFile channelId content path =
  send (UploadDiscordFile channelId content path)

runDiscord
  :: (IOE :> es, Log :> es, Concurrent :> es)
  => Config
  -> Eff (Discord : es) a
  -> Eff es a
runDiscord cfg inner = withEffToIO (ConcUnlift Persistent Unlimited) $ \runInIO -> do
  eventChan <- liftIO Chan.newChan
  manager <- liftIO Http.newTlsManager
  let runInner =
        runInIO $
          interpret
            ( \_ -> \case
                DiscordConfig ->
                  pure cfg
                ReceiveEvent ->
                  liftIO (Chan.readChan eventChan)
                CreateMessage channelId request ->
                  discordJsonRequest manager cfg POST ["channels", channelId, "messages"] request
                EditMessage channelId messageId request ->
                  discordJsonRequest manager cfg PATCH ["channels", channelId, "messages", messageId] request
                DeleteMessage channelId messageId ->
                  discordNoResponseRequest manager cfg DELETE ["channels", channelId, "messages", messageId]
                FetchMessage channelId messageId ->
                  discordGetRequest manager cfg ["channels", channelId, "messages", messageId]
                FetchUser userId ->
                  discordGetRequest manager cfg ["users", userId]
                FetchGuildMember guildId userId ->
                  discordGetRequest manager cfg ["guilds", guildId, "members", userId]
                FetchGuildMembers guildId ->
                  discordGetRequest manager cfg ["guilds", guildId, "members"]
                UploadDiscordFile channelId content path ->
                  discordUploadFile manager cfg channelId content path
            )
            inner
  if discordEnabled cfg
    then liftIO $ Async.withAsync (runInIO $ discordConnectionLoop cfg eventChan) \_ -> runInner
    else runInner

discordEnabled :: Config -> Bool
discordEnabled cfg =
  not (Text.null (Text.strip cfg.botToken))

discordConnectionLoop
  :: (IOE :> es, Log :> es, Concurrent :> es)
  => Config
  -> Chan.Chan Message
  -> Eff es ()
discordConnectionLoop cfg eventChan =
  forever do
    result <- runDiscordConnectionOnce cfg eventChan
    case result of
      Right () ->
        logInfo_ "Discord gateway disconnected; reconnecting"
      Left err ->
        logInfo_ [i|Discord gateway failed; reconnecting: #{err}|]
    threadDelay discordReconnectDelayMicroseconds

runDiscordConnectionOnce
  :: (IOE :> es, Log :> es, Concurrent :> es)
  => Config
  -> Chan.Chan Message
  -> Eff es (Either String ())
runDiscordConnectionOnce cfg eventChan =
  (Right <$> withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
    liftIO $ runSecureWebSocketClient cfg.gatewayHost cfg.gatewayPath \conn ->
      runInIO (runGatewayConnection cfg eventChan conn))
    `catch` \(connectionErr :: WS.ConnectionException) ->
      pure (Left (show connectionErr))
    `catch` \(handshakeErr :: WS.HandshakeException) ->
      pure (Left (show handshakeErr))
    `catch` \(ioErr :: IOException) ->
      pure (Left (show ioErr))

runSecureWebSocketClient :: String -> String -> WS.ClientApp a -> IO a
runSecureWebSocketClient host path app = do
  context <- Connection.initConnectionContext
  conn <- Connection.connectTo context Connection.ConnectionParams
    { Connection.connectionHostname = host
    , Connection.connectionPort = 443
    , Connection.connectionUseSecure = Just discordTlsSettings
    , Connection.connectionUseSocks = Nothing
    }
  stream <- WSStream.makeStream
    (Just <$> Connection.connectionGetChunk conn)
    (maybe (Connection.connectionClose conn) (Connection.connectionPut conn . LazyByteString.toStrict))
  result <- WS.runClientWithStream stream host path WS.defaultConnectionOptions [] app
  Connection.connectionClose conn
  pure result

discordTlsSettings :: Connection.TLSSettings
discordTlsSettings =
  Connection.TLSSettingsSimple
    { Connection.settingDisableCertificateValidation = False
    , Connection.settingDisableSession = False
    , Connection.settingUseServerName = True
    , Connection.settingClientSupported =
        TLS.defaultSupported
          { TLS.supportedExtendedMainSecret = TLS.AllowEMS
          }
    }

runGatewayConnection
  :: (IOE :> es, Log :> es, Concurrent :> es)
  => Config
  -> Chan.Chan Message
  -> WS.Connection
  -> Eff es ()
runGatewayConnection cfg eventChan conn = do
  firstEnvelope <- readGatewayEnvelope conn
  case firstEnvelope.op of
    10 -> do
      hello :: GatewayHello <- parseGatewayData "Discord hello" firstEnvelope.d
      logInfo_ "Discord gateway connected"
      lastSequence <- MVar.newMVar firstEnvelope.s
      heartbeatThread <- forkIO (heartbeatLoop conn lastSequence hello.heartbeatInterval)
      identifyGateway cfg conn
      readGatewayEvents eventChan lastSequence conn `finally` killThread heartbeatThread
    op ->
      logInfo_ [i|Discord gateway expected HELLO, got op=#{op}|]

heartbeatLoop
  :: (IOE :> es, Concurrent :> es)
  => WS.Connection
  -> MVar.MVar (Maybe Int)
  -> Int
  -> Eff es ()
heartbeatLoop conn lastSequence intervalMs = forever do
  threadDelay (intervalMs * 1000)
  sequenceNumber <- MVar.readMVar lastSequence
  liftIO $ WS.sendTextData conn (Aeson.encode (heartbeatPayload sequenceNumber))

readGatewayEvents
  :: (IOE :> es, Log :> es, Concurrent :> es)
  => Chan.Chan Message
  -> MVar.MVar (Maybe Int)
  -> WS.Connection
  -> Eff es ()
readGatewayEvents eventChan lastSequence conn = forever do
  envelope <- readGatewayEnvelope conn
  void $ MVar.swapMVar lastSequence envelope.s
  case (envelope.op, envelope.t) of
    (0, Just "MESSAGE_CREATE") -> do
      message :: Message <- parseGatewayData "Discord message create" envelope.d
      liftIO $ Chan.writeChan eventChan message
    (7, _) ->
      throwIO (userError "Discord gateway requested reconnect")
    (9, _) ->
      throwIO (userError "Discord gateway invalid session")
    _ ->
      pure ()

readGatewayEnvelope :: (IOE :> es, Log :> es) => WS.Connection -> Eff es GatewayEnvelope
readGatewayEnvelope conn = do
  bytes <- liftIO (WS.receiveData conn :: IO ByteString.ByteString)
  case Aeson.eitherDecodeStrict bytes of
    Right envelope ->
      pure envelope
    Left err -> do
      logInfo_ [i|Ignoring malformed Discord gateway frame: #{Text.pack err}|]
      readGatewayEnvelope conn

identifyGateway :: IOE :> es => Config -> WS.Connection -> Eff es ()
identifyGateway cfg conn =
  liftIO $ WS.sendTextData conn (Aeson.encode (identifyPayload cfg))

heartbeatPayload :: Maybe Int -> Aeson.Value
heartbeatPayload sequenceNumber =
  Aeson.object
    [ "op" Aeson..= (1 :: Int)
    , "d" Aeson..= sequenceNumber
    ]

identifyPayload :: Config -> Aeson.Value
identifyPayload cfg =
  Aeson.object
    [ "op" Aeson..= (2 :: Int)
    , "d" Aeson..= Aeson.object
        [ "token" Aeson..= cfg.botToken
        , "intents" Aeson..= discordGatewayIntents
        , "properties" Aeson..= Aeson.object
            [ "os" Aeson..= ("linux" :: Text)
            , "browser" Aeson..= ("cosmobot" :: Text)
            , "device" Aeson..= ("cosmobot" :: Text)
            ]
        ]
    ]

discordGatewayIntents :: Int
discordGatewayIntents =
  512 + 4096 + 32768

discordReconnectDelayMicroseconds :: Int
discordReconnectDelayMicroseconds =
  5 * 1000000

incomingMessages :: (Discord :> es, Log :> es) => Stream (Of IncomingMessage) (Eff es) ()
incomingMessages = do
  cfg <- S.lift discordConfig
  if discordEnabled cfg
    then incomingMessagesLoop cfg
    else S.lift $ logInfo_ "Discord driver disabled: no bot token configured"

incomingMessagesLoop :: (Discord :> es, Log :> es) => Config -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessagesLoop cfg = do
  event <- S.lift receiveEvent
  case eventToIncomingMessageWith cfg event of
    Nothing -> do
      S.lift $ logTrace_ "Ignoring Discord event"
      S.lift $ logInfo_ "Ignoring Discord event"
    Just message -> do
      S.lift $ logTrace "incoming Discord message" message
      S.lift $ logInfo_ [i|incoming Discord message: #{incomingMessageLogLine message}|]
      S.yield message
  incomingMessagesLoop cfg

eventToIncomingMessage :: Message -> Maybe IncomingMessage
eventToIncomingMessage =
  eventToIncomingMessageWith defaultConfig

eventToIncomingMessageWith :: Config -> Message -> Maybe IncomingMessage
eventToIncomingMessageWith cfg message = do
  guard (not (isOwnMessage cfg message))
  guard (not message.author.bot)
  guard (not (Text.null (Text.strip message.content)) || not (null message.attachments))
  pure IncomingMessage
    { platform = PlatformDiscord
    , kind = if isJust message.guildId then ChatGroup else ChatPrivate
    , chatId = Just (discordSnowflakeNumber message.channelId)
    , chatAliases = catMaybes [Just message.channelId, message.guildId]
    , digest = discordMessageDigest cfg message
    , senderId = Just message.author.id
    , senderUsername = message.author.globalName <|> message.author.username
    , messageId = Just (textMessageId message.id)
    , replyToMessageId = message.referencedMessage <&> (.id) <&> textMessageId
    , mentions = mapMaybe (parseIntegerUserId . (.id)) message.mentions
    , mentionUsernames = mapMaybe (.username) message.mentions
    , imageUrls = messageImageUrls message
    , text = Text.strip message.content
    , raw = message.raw
    }

discordMessageDigest :: Config -> Message -> MessageDigest
discordMessageDigest cfg message =
  MessageDigest
    { chatIsAllowed = chatAllowed
    , senderIsAllowed = if isNothing message.guildId then (chatAllowed || senderAllowed || senderSuperuser) else senderSuperuser
    , senderIsSuperuser = senderSuperuser
    , mentionsBot = maybe False (`elem` map (.id) message.mentions) cfg.botId
    , botId = cfg.botId
    }
  where
    chatAllowed =
      discordSnowflakeNumber message.channelId `elem` cfg.allowedChannels ||
        maybe False ((`elem` cfg.allowedGuilds) . discordSnowflakeNumber) message.guildId
    senderAllowed =
      message.author.id `elem` cfg.allowedUsers
    senderSuperuser =
      message.author.id `elem` cfg.superusers

replyTo :: (Discord :> es, FileSystem :> es, IOE :> es) => IncomingMessage -> Text -> Eff es (Maybe MessageId)
replyTo message body =
  case (message.platform, discordChannelId message) of
    (PlatformDiscord, Just channelId) -> do
      let text = Chat.renderReplyBody body
          imageRefs = Chat.replyImageUrls body
          request = createMessageRequest text (discordReplyReference message)
      sentText <- if Text.null (Text.strip text)
        then pure Nothing
        else Just <$> createMessage channelId request
      sentImages <- traverse (sendDiscordImage channelId (discordReplyReference message)) imageRefs
      pure (textMessageId . (.id) <$> sentText <|> (textMessageId . (.id) <$> viaNonEmpty head sentImages))
    _ ->
      pure Nothing

sendDiscordImage :: (Discord :> es, FileSystem :> es, IOE :> es) => Text -> Maybe Reference -> Text -> Eff es Message
sendDiscordImage channelId replyReference imageRef =
  case discordRemoteImageRef imageRef of
    Just url ->
      createMessage channelId (createMessageRequest url replyReference)
    Nothing ->
      withDiscordImageFile imageRef \path ->
        uploadDiscordFile channelId Nothing path

editMessage :: Discord :> es => IncomingMessage -> MessageId -> Text -> Eff es Bool
editMessage message messageId body =
  case (message.platform, discordChannelId message) of
    (PlatformDiscord, Just channelId) -> do
      void $ editDiscordMessage channelId (messageIdText messageId) (createMessageRequest (Chat.renderReplyBody body) Nothing)
      pure True
    _ ->
      pure False

deleteMessage :: Discord :> es => IncomingMessage -> MessageId -> Eff es Bool
deleteMessage message messageId =
  case (message.platform, discordChannelId message) of
    (PlatformDiscord, Just channelId) -> do
      deleteDiscordMessage channelId (messageIdText messageId)
      pure True
    _ ->
      pure False

getMessageContent :: Discord :> es => IncomingMessage -> MessageId -> Eff es (Maybe ReferencedMessage)
getMessageContent message messageId =
  case (message.platform, discordChannelId message) of
    (PlatformDiscord, Just channelId) -> do
      fetched <- fetchMessage channelId (messageIdText messageId)
      pure (Just ReferencedMessage
        { messageId = Just (textMessageId fetched.id)
        , senderDisplayName = fetched.author.globalName <|> fetched.author.username
        , senderIdentifier = Just fetched.author.id
        , text = fetched.content
        , imageUrls = messageImageUrls fetched
        })
    _ ->
      pure Nothing

mentionUser :: Discord :> es => IncomingMessage -> Integer -> Text -> Eff es (Maybe MessageId)
mentionUser message userId body =
  case (message.platform, discordChannelId message) of
    (PlatformDiscord, Just channelId) -> do
      sent <- createMessage channelId (createMessageRequest ([i|<@#{userId}> #{body}|]) (discordReplyReference message))
      pure (Just (textMessageId sent.id))
    _ ->
      pure Nothing

replyAudio :: (Discord :> es, FileSystem :> es, IOE :> es) => IncomingMessage -> Text -> Maybe Text -> Eff es (Either Text (Maybe MessageId))
replyAudio message audioRef caption =
  case (message.platform, discordChannelId message) of
    (PlatformDiscord, Just channelId) -> do
      sent <- withDiscordImageFile audioRef \path ->
        uploadDiscordFile channelId (Chat.renderReplyBody <$> caption) path
      pure (Right (Just (textMessageId sent.id)))
    _ ->
      pure (Left "Discord audio reply requires a Discord channel id.")

uploadFile :: (Discord :> es, FileSystem :> es, IOE :> es) => IncomingMessage -> FilePath -> Eff es (Either Text (Maybe MessageId))
uploadFile message path =
  case (message.platform, discordChannelId message) of
    (PlatformDiscord, Just channelId) -> do
      sent <- uploadDiscordFile channelId Nothing path
      pure (Right (Just (textMessageId sent.id)))
    _ ->
      pure (Left "Discord file upload requires a Discord channel id.")

discordChannelId :: IncomingMessage -> Maybe Text
discordChannelId message =
  viaNonEmpty head message.chatAliases

discordReplyReference :: IncomingMessage -> Maybe Reference
discordReplyReference message =
  Reference <$> message.messageId <*> discordChannelId message <*> Just (discordMessageGuildId message.raw)

createMessageRequest :: Text -> Maybe Reference -> CreateMessageRequest
createMessageRequest content messageReference =
  CreateMessageRequest
    { content = nonEmptyDiscordContent content
    , messageReference
    , allowedMentions = AllowedMentions
        { parse = []
        , users = []
        , repliedUser = Just False
        }
    }

nonEmptyDiscordContent :: Text -> Text
nonEmptyDiscordContent content
  | Text.null (Text.strip content) = " "
  | otherwise = Text.take discordMessageTextLimit content

discordRemoteImageRef :: Text -> Maybe Text
discordRemoteImageRef ref
  | "http://" `Text.isPrefixOf` ref || "https://" `Text.isPrefixOf` ref = Just ref
  | otherwise = Nothing

withDiscordImageFile :: (FileSystem :> es, IOE :> es) => Text -> (FilePath -> Eff es a) -> Eff es a
withDiscordImageFile ref action =
  case Text.stripPrefix "file://" ref of
    Just path -> action (Text.unpack path)
    Nothing -> action (Text.unpack ref)

messageImageUrls :: Message -> [Text]
messageImageUrls message =
  [ attachment.url
  | attachment <- message.attachments
  , maybe False ("image/" `Text.isPrefixOf`) attachment.contentType
  ]

isOwnMessage :: Config -> Message -> Bool
isOwnMessage cfg message =
  Just message.author.id == cfg.botId

discordMessageGuildId :: Aeson.Value -> Maybe Text
discordMessageGuildId =
  join . Aeson.parseMaybe (Aeson.withObject "Discord message" \o -> o Aeson..:? "guild_id")

discordSnowflakeNumber :: Text -> Integer
discordSnowflakeNumber raw =
  fromMaybe (stableTextId raw) (parseIntegerUserId raw)

parseIntegerUserId :: Text -> Maybe Integer
parseIntegerUserId raw =
  readMaybe (Text.unpack (Text.strip raw))

stableTextId :: Text -> Integer
stableTextId =
  Text.foldl' step 14695981039346656037
  where
    step acc char =
      fromIntegral ((fromIntegral acc `xor` fromIntegral (fromEnum char)) * fnvPrime :: Word64)
    fnvPrime :: Word64
    fnvPrime = 1099511628211

discordJsonRequest
  :: (IOE :> es, Log :> es, Aeson.ToJSON body, Aeson.FromJSON result, HttpMethod method, HttpBodyAllowed (AllowsBody method) 'CanHaveBody)
  => Manager
  -> Config
  -> method
  -> [Text]
  -> body
  -> Eff es result
discordJsonRequest manager cfg method path body = do
  logTrace_ [i|Discord REST request: #{Text.intercalate "/" path}|]
  liftIO $ Http.runReqWithConfig (Http.httpConfig manager) do
    req method (discordApiUrl path) (ReqBodyJson body) jsonResponse (discordRequestOptions cfg)
      <&> responseBody

discordNoResponseRequest
  :: (IOE :> es, Log :> es, HttpMethod method, HttpBodyAllowed (AllowsBody method) 'NoBody)
  => Manager
  -> Config
  -> method
  -> [Text]
  -> Eff es ()
discordNoResponseRequest manager cfg method path = do
  logTrace_ [i|Discord REST request: #{Text.intercalate "/" path}|]
  void $ liftIO $ Http.runReqWithConfig (Http.httpConfig manager) do
    req method (discordApiUrl path) NoReqBody ignoreResponse (discordRequestOptions cfg)

discordGetRequest
  :: (IOE :> es, Log :> es, Aeson.FromJSON result)
  => Manager
  -> Config
  -> [Text]
  -> Eff es result
discordGetRequest manager cfg path = do
  logTrace_ [i|Discord REST request: #{Text.intercalate "/" path}|]
  liftIO $ Http.runReqWithConfig (Http.httpConfig manager) do
    req GET (discordApiUrl path) NoReqBody jsonResponse (discordRequestOptions cfg)
      <&> responseBody

discordUploadFile
  :: (IOE :> es, Log :> es)
  => Manager
  -> Config
  -> Text
  -> Maybe Text
  -> FilePath
  -> Eff es Message
discordUploadFile manager cfg channelId content path = do
  base <- liftIO $ HTTP.parseRequest [i|https://discord.com/api/v10/channels/#{channelId}/messages|]
  let payload = Aeson.object
        [ "content" Aeson..= fromMaybe "" content
        ]
      request =
        base
          { HTTP.method = "POST"
          , HTTP.requestHeaders =
              [ ("Authorization", TextEncoding.encodeUtf8 ("Bot " <> cfg.botToken))
              , ("User-Agent", "cosmobot")
              ]
          }
      parts =
        [ Multipart.partLBS "payload_json" (Aeson.encode payload)
        , Multipart.partFileRequestBodyM "files[0]" (takeFileName path) (HTTP.streamFile path)
        ]
  multipartRequest <- liftIO $ Multipart.formDataBody parts request
  response <- liftIO $ HTTP.httpLbs multipartRequest manager
  case Aeson.eitherDecode (HTTP.responseBody response) of
    Right message ->
      pure message
    Left err ->
      throwIO (userError [i|Discord upload response decode failed: #{err}|])

discordApiUrl :: [Text] -> Url 'Https
discordApiUrl path =
  foldl' (/:) (https "discord.com" /: "api" /: "v10") path

discordRequestOptions :: Config -> Option 'Https
discordRequestOptions cfg =
  header "Authorization" (TextEncoding.encodeUtf8 ("Bot " <> cfg.botToken))
    <> header "User-Agent" "cosmobot"

data GatewayEnvelope = GatewayEnvelope
  { op :: !Int
  , d :: !Aeson.Value
  , s :: !(Maybe Int)
  , t :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance Aeson.FromJSON GatewayEnvelope where
  parseJSON = Aeson.withObject "GatewayEnvelope" \o ->
    GatewayEnvelope
      <$> o Aeson..: "op"
      <*> o Aeson..: "d"
      <*> o Aeson..:? "s"
      <*> o Aeson..:? "t"

data GatewayHello = GatewayHello
  { heartbeatInterval :: !Int
  }
  deriving (Show, Generic)

instance Aeson.FromJSON GatewayHello where
  parseJSON = Aeson.withObject "GatewayHello" \o ->
    GatewayHello <$> o Aeson..: "heartbeat_interval"

parseGatewayData :: (IOE :> es, Aeson.FromJSON a) => String -> Aeson.Value -> Eff es a
parseGatewayData label value =
  case Aeson.parseEither Aeson.parseJSON value of
    Right parsed ->
      pure parsed
    Left err ->
      throwIO (userError [i|#{label} parse failed: #{err}|])

data Message = Message
  { id :: !Text
  , channelId :: !Text
  , guildId :: !(Maybe Text)
  , author :: !User
  , member :: !(Maybe Member)
  , content :: !Text
  , attachments :: ![Attachment]
  , mentions :: ![User]
  , referencedMessage :: !(Maybe Message)
  , messageReference :: !(Maybe Reference)
  , raw :: !Aeson.Value
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Message where
  parseJSON value = Aeson.withObject "Message" (\o ->
    Message
      <$> o Aeson..: "id"
      <*> o Aeson..: "channel_id"
      <*> o Aeson..:? "guild_id"
      <*> o Aeson..: "author"
      <*> o Aeson..:? "member"
      <*> fmap (fromMaybe "") (o Aeson..:? "content")
      <*> fmap (fromMaybe []) (o Aeson..:? "attachments")
      <*> fmap (fromMaybe []) (o Aeson..:? "mentions")
      <*> o Aeson..:? "referenced_message"
      <*> o Aeson..:? "message_reference"
      <*> pure value
    ) value

data User = User
  { id :: !Text
  , username :: !(Maybe Text)
  , globalName :: !(Maybe Text)
  , bot :: !Bool
  , avatar :: !(Maybe Text)
  }
  deriving (Show, Generic, Aeson.ToJSON)

instance Aeson.FromJSON User where
  parseJSON = Aeson.withObject "User" \o ->
    User
      <$> o Aeson..: "id"
      <*> o Aeson..:? "username"
      <*> o Aeson..:? "global_name"
      <*> fmap (fromMaybe False) (o Aeson..:? "bot")
      <*> o Aeson..:? "avatar"

data Member = Member
  { nick :: !(Maybe Text)
  , user :: !(Maybe User)
  }
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data Attachment = Attachment
  { id :: !Text
  , filename :: !Text
  , url :: !Text
  , contentType :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Attachment where
  parseJSON = Aeson.withObject "Attachment" \o ->
    Attachment
      <$> o Aeson..: "id"
      <*> o Aeson..: "filename"
      <*> o Aeson..: "url"
      <*> o Aeson..:? "content_type"

data Reference = Reference
  { messageId :: !MessageId
  , channelId :: !Text
  , guildId :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Reference where
  parseJSON = Aeson.withObject "Reference" \o ->
    Reference
      <$> (textMessageId <$> o Aeson..: "message_id")
      <*> o Aeson..: "channel_id"
      <*> o Aeson..:? "guild_id"

instance Aeson.ToJSON Reference where
  toJSON Reference{..} = Aeson.object $
    [ "message_id" Aeson..= messageIdText messageId
    , "channel_id" Aeson..= channelId
    ]
    <> maybeField "guild_id" guildId

data AllowedMentions = AllowedMentions
  { parse :: ![Text]
  , users :: ![Text]
  , repliedUser :: !(Maybe Bool)
  }
  deriving (Show, Generic)

instance Aeson.ToJSON AllowedMentions where
  toJSON AllowedMentions{..} = Aeson.object $
    [ "parse" Aeson..= parse
    , "users" Aeson..= users
    ]
    <> maybeField "replied_user" repliedUser

data CreateMessageRequest = CreateMessageRequest
  { content :: !Text
  , messageReference :: !(Maybe Reference)
  , allowedMentions :: !AllowedMentions
  }
  deriving (Show, Generic)

instance Aeson.ToJSON CreateMessageRequest where
  toJSON CreateMessageRequest{..} = Aeson.object $
    [ "content" Aeson..= content
    , "allowed_mentions" Aeson..= allowedMentions
    ]
    <> maybeField "message_reference" messageReference

maybeField :: Aeson.ToJSON value => Aeson.Key -> Maybe value -> [Aeson.Pair]
maybeField key =
  maybe [] \value -> [key Aeson..= value]
