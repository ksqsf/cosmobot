{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.Discord.Protocol
Description : Discord Gateway and REST chat driver
Stability   : experimental
-}

module Bot.Chat.Driver.Discord.Protocol where

import Bot.Chat.Driver.Discord.Types (Config (..))
import qualified Bot.Chat.Driver.Types as Driver
import Bot.Core.Message
import Bot.Prelude
import Bot.Util.Aeson
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.MVar as MVar
import qualified Network.Connection as Connection
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import Network.HTTP.Req
import qualified Network.TLS as TLS
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WSStream

data DiscordDriver = DiscordDriver
  { config :: !Config
  , eventChan :: !(Chan.Chan GatewayEvent)
  }

data GatewayEvent
  = GatewayMessageCreated !Message
  | GatewayMessageDeleted !DeletedMessage

data DiscordException
  = DiscordReconnectRequest
  | DiscordInvalidSession
  | DiscordHeartbeatAckTimeout
  | DiscordUploadResponseDecodeFailed !Text
  | DiscordGatewayDataParseFailed !Text !Text
  deriving (Eq, Show)

instance Exception DiscordException where
  displayException = Text.unpack . \case
    DiscordReconnectRequest -> "Discord gateway requested reconnect"
    DiscordInvalidSession -> "Discord gateway invalid session"
    DiscordHeartbeatAckTimeout -> "Discord gateway heartbeat ACK timed out"
    DiscordUploadResponseDecodeFailed err -> [i|Discord upload response decode failed: #{err}|]
    DiscordGatewayDataParseFailed label err -> [i|#{label} parse failed: #{err}|]

data ConnectionOutcome
  = Disconnected
  | ReconnectRequested
  | ConnectionFailed !Text
  deriving (Eq, Show)

newDiscordDriver :: IOE :> es => Config -> Eff es DiscordDriver
newDiscordDriver config = do
  eventChan <- liftIO Chan.newChan
  pure DiscordDriver{config, eventChan}

receiveEvent :: IOE :> es => DiscordDriver -> Eff es GatewayEvent
receiveEvent driver =
  liftIO (Chan.readChan driver.eventChan)

createMessage :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> CreateMessageRequest -> Eff es Message
createMessage driver channelId request =
  discordJsonRequest driver.config "POST" POST ["channels", channelId, "messages"] request

editDiscordMessage :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> Text -> CreateMessageRequest -> Eff es Message
editDiscordMessage driver channelId messageId request =
  discordJsonRequest driver.config "PATCH" PATCH ["channels", channelId, "messages", messageId] request

deleteDiscordMessage :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> Text -> Eff es ()
deleteDiscordMessage driver channelId messageId =
  discordNoResponseRequest driver.config "DELETE" DELETE ["channels", channelId, "messages", messageId]

fetchMessage :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> Text -> Eff es Message
fetchMessage driver channelId messageId =
  discordGetRequest driver.config ["channels", channelId, "messages", messageId]

getUser :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> Eff es Aeson.Value
getUser driver userId =
  discordGetRequest driver.config ["users", userId]

getGuildMember :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> Text -> Eff es Aeson.Value
getGuildMember driver guildId userId =
  discordGetRequest driver.config ["guilds", guildId, "members", userId]

listGuildMembers :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> Eff es Aeson.Value
listGuildMembers driver guildId =
  discordGetRequest driver.config ["guilds", guildId, "members"]

uploadDiscordFile :: (HTTP.HTTP :> es, KatipE :> es, IOE :> es) => DiscordDriver -> Text -> Maybe Text -> FilePath -> Maybe Text -> Eff es Message
uploadDiscordFile driver channelId content path fileName =
  discordUploadFile driver.config channelId content path fileName

triggerTyping :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> Eff es ()
triggerTyping driver channelId =
  discordNoResponseRequest driver.config "POST" POST ["channels", channelId, "typing"]

runDiscordDriver
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => DiscordDriver
  -> Eff es a
  -> Eff es a
runDiscordDriver driver inner = do
  let cfg = driver.config
      eventChan = driver.eventChan
  if discordEnabled cfg
    then do
      Concurrency.withWorker "discord.gateway" (discordConnectionLoop cfg eventChan) inner
    else inner

discordEnabled :: Config -> Bool
discordEnabled cfg =
  not (Text.null (Text.strip cfg.botToken))

discordConnectionLoop
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => Config
  -> Chan.Chan GatewayEvent
  -> Eff es ()
discordConnectionLoop cfg eventChan =
  forever do
    runDiscordConnectionOnce cfg eventChan >>= \case
      Disconnected -> do
        $(logDebug) "Discord gateway disconnected; reconnecting"
        threadDelay discordReconnectDelayMicroseconds
      ReconnectRequested ->
        pure ()
      ConnectionFailed err -> do
        $(logWarning) [i|Discord gateway failed; reconnecting: #{err}|]
        threadDelay discordReconnectDelayMicroseconds

runDiscordConnectionOnce
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => Config
  -> Chan.Chan GatewayEvent
  -> Eff es ConnectionOutcome
runDiscordConnectionOnce cfg eventChan = do
  trySync (runSecureWebSocketClient cfg.gatewayHost cfg.gatewayPath (runGatewayConnection cfg eventChan)) >>= \case
    Right () -> pure Disconnected
    Left err -> pure (classifyConnectionException err)

classifyConnectionException :: SomeException -> ConnectionOutcome
classifyConnectionException err =
  case fromException err of
    Just DiscordReconnectRequest -> ReconnectRequested
    _ -> ConnectionFailed (exceptionSummary err)

exceptionSummary :: Exception e => e -> Text
exceptionSummary =
  Text.takeWhile (/= '\n') . Text.pack . displayException

runSecureWebSocketClient
  :: IOE :> es
  => String
  -> String
  -> (WS.Connection -> Eff es a)
  -> Eff es a
runSecureWebSocketClient host path app = do
  context <- liftIO Connection.initConnectionContext
  bracket
    ( liftIO $ Connection.connectTo context Connection.ConnectionParams
        { Connection.connectionHostname = host
        , Connection.connectionPort = 443
        , Connection.connectionUseSecure = Just discordTlsSettings
        , Connection.connectionUseSocks = Nothing
        }
    )
    (liftIO . Connection.connectionClose)
    \conn -> do
      stream <- liftIO $ WSStream.makeStream
        (Just <$> Connection.connectionGetChunk conn)
        (maybe (Connection.connectionClose conn) (Connection.connectionPut conn . LazyByteString.toStrict))
      websocket <- liftIO $ WS.newClientConnection stream host path WS.defaultConnectionOptions []
      app websocket

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
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => Config
  -> Chan.Chan GatewayEvent
  -> WS.Connection
  -> Eff es ()
runGatewayConnection cfg eventChan conn = do
  firstEnvelope <- readGatewayEnvelope conn
  case firstEnvelope.op of
    10 -> do
      hello :: GatewayHello <- parseGatewayData "Discord hello" firstEnvelope.d
      $(logDebug) "Discord gateway connected"
      lastSequence <- MVar.newMVar firstEnvelope.s
      heartbeatAck <- MVar.newMVar True
      identifyGateway cfg conn
      runDiscordGatewaySession eventChan lastSequence heartbeatAck hello.heartbeatInterval conn
    op ->
      $(logError) [i|Discord gateway expected HELLO, got op=#{op}|]

runDiscordGatewaySession
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => Chan.Chan GatewayEvent
  -> MVar.MVar (Maybe Int)
  -> MVar.MVar Bool
  -> Int
  -> WS.Connection
  -> Eff es ()
runDiscordGatewaySession eventChan lastSequence heartbeatAck heartbeatInterval conn = do
  Concurrency.raceTasks_
    "discord.gateway.heartbeat"
    (heartbeatLoop conn lastSequence heartbeatAck heartbeatInterval)
    "discord.gateway.reader"
    (readGatewayEvents eventChan lastSequence heartbeatAck conn)

heartbeatLoop
  :: (IOE :> es, KatipE :> es, Concurrent :> es)
  => WS.Connection
  -> MVar.MVar (Maybe Int)
  -> MVar.MVar Bool
  -> Int
  -> Eff es ()
heartbeatLoop conn lastSequence heartbeatAck intervalMs = forever do
  threadDelay (intervalMs * 1000)
  acked <- MVar.swapMVar heartbeatAck False
  if acked
    then do
      sequenceNumber <- MVar.readMVar lastSequence
      liftIO $ WS.sendTextData conn (Aeson.encode (heartbeatPayload sequenceNumber))
    else do
      $(logError) "Discord gateway heartbeat ACK timed out; closing connection"
      liftIO $ WS.sendClose conn ("heartbeat ACK timeout" :: Text)
      throwIO DiscordHeartbeatAckTimeout

readGatewayEvents
  :: (IOE :> es, KatipE :> es, Concurrent :> es)
  => Chan.Chan GatewayEvent
  -> MVar.MVar (Maybe Int)
  -> MVar.MVar Bool
  -> WS.Connection
  -> Eff es ()
readGatewayEvents eventChan lastSequence heartbeatAck conn = forever do
  envelope <- readGatewayEnvelope conn
  updateLastSequence lastSequence envelope.s
  case (envelope.op, envelope.t) of
    (0, Just "MESSAGE_CREATE") -> do
      message :: Message <- parseGatewayData "Discord message create" envelope.d
      liftIO $ Chan.writeChan eventChan (GatewayMessageCreated message)
    (0, Just "MESSAGE_DELETE") -> do
      deleted :: DeletedMessage <- parseGatewayData "Discord message delete" envelope.d
      liftIO $ Chan.writeChan eventChan (GatewayMessageDeleted deleted)
    (1, _) -> do
      sequenceNumber <- MVar.readMVar lastSequence
      liftIO $ WS.sendTextData conn (Aeson.encode (heartbeatPayload sequenceNumber))
    (7, _) ->
      throwIO DiscordReconnectRequest
    (9, _) ->
      throwIO DiscordInvalidSession
    (11, _) ->
      void $ MVar.swapMVar heartbeatAck True
    _ ->
      pure ()

updateLastSequence :: Concurrent :> es => MVar.MVar (Maybe Int) -> Maybe Int -> Eff es ()
updateLastSequence lastSequence =
  traverse_ \sequenceNumber ->
    void $ MVar.swapMVar lastSequence (Just sequenceNumber)

readGatewayEnvelope :: (IOE :> es, KatipE :> es) => WS.Connection -> Eff es GatewayEnvelope
readGatewayEnvelope conn = do
  bytes <- liftIO (WS.receiveData conn :: IO ByteString.ByteString)
  case Aeson.eitherDecodeStrict bytes of
    Right envelope ->
      pure envelope
    Left err -> do
      $(logWarning) [i|Ignoring malformed Discord gateway frame: #{Text.pack err}|]
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

discordJsonRequest
  :: (HTTP.HTTP :> es, KatipE :> es, Aeson.ToJSON body, Aeson.FromJSON result, HttpMethod method, HttpBodyAllowed (AllowsBody method) 'CanHaveBody)
  => Config
  -> Text
  -> method
  -> [Text]
  -> body
  -> Eff es result
discordJsonRequest cfg methodName method path body = discordRequestContext methodName path do
  $(logDebug) [i|Discord REST request: #{Text.intercalate "/" path}|]
  HTTP.runReq do
    req method (discordApiUrl path) (ReqBodyJson body) jsonResponse (discordRequestOptions cfg)
      <&> responseBody

discordNoResponseRequest
  :: (HTTP.HTTP :> es, KatipE :> es, HttpMethod method, HttpBodyAllowed (AllowsBody method) 'NoBody)
  => Config
  -> Text
  -> method
  -> [Text]
  -> Eff es ()
discordNoResponseRequest cfg methodName method path = discordRequestContext methodName path do
  $(logDebug) [i|Discord REST request: #{Text.intercalate "/" path}|]
  void $ HTTP.runReq do
    req method (discordApiUrl path) NoReqBody ignoreResponse (discordRequestOptions cfg)

discordGetRequest
  :: (HTTP.HTTP :> es, KatipE :> es, Aeson.FromJSON result)
  => Config
  -> [Text]
  -> Eff es result
discordGetRequest cfg path = discordRequestContext "GET" path do
  $(logDebug) [i|Discord REST request: #{Text.intercalate "/" path}|]
  HTTP.runReq do
    req GET (discordApiUrl path) NoReqBody jsonResponse (discordRequestOptions cfg)
      <&> responseBody

discordUploadFile
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => Config
  -> Text
  -> Maybe Text
  -> FilePath
  -> Maybe Text
  -> Eff es Message
discordUploadFile cfg channelId content path fileName = discordRequestContext "POST" ["channels", channelId, "messages"] do
  manager <- HTTP.manager
  base <- liftIO $ Client.parseRequest [i|https://discord.com/api/v10/channels/#{channelId}/messages|]
  let payload = Aeson.object
        [ "content" Aeson..= fromMaybe "" content
        ]
      request =
        base
          { Client.method = "POST"
          , Client.requestHeaders =
              [ ("Authorization", TextEncoding.encodeUtf8 ("Bot " <> cfg.botToken))
              , ("User-Agent", "cosmobot")
              ]
          }
      parts =
        [ Multipart.partLBS "payload_json" (Aeson.encode payload)
        , Multipart.partFileRequestBodyM "files[0]" (Text.unpack (Driver.uploadFileName path fileName)) (Client.streamFile path)
        ]
  multipartRequest <- liftIO $ Multipart.formDataBody parts request
  response <- liftIO $ Client.httpLbs multipartRequest manager
  case Aeson.eitherDecode (Client.responseBody response) of
    Right message ->
      pure message
    Left err ->
      throwIO (DiscordUploadResponseDecodeFailed (Text.pack err))

discordRequestContext :: KatipE :> es => Text -> [Text] -> Eff es a -> Eff es a
discordRequestContext method path =
  katipAddContext
    ( sl "discord_method" method
        <> sl "discord_path" ("/" <> Text.intercalate "/" path)
    )

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
    deriving Aeson.FromJSON via (SnakeJSON GatewayEnvelope)

data GatewayHello = GatewayHello
  { heartbeatInterval :: !Int
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON GatewayHello)

parseGatewayData :: (IOE :> es, Aeson.FromJSON a) => Text -> Aeson.Value -> Eff es a
parseGatewayData label value =
  case Aeson.parseEither Aeson.parseJSON value of
    Right parsed ->
      pure parsed
    Left err ->
      throwIO (DiscordGatewayDataParseFailed label (Text.pack err))

data Message = Message
  { id :: !Text
  , channelId :: !Text
  , guildId :: !(Maybe Text)
  , author :: !User
  , member :: !(Maybe Member)
  , content :: !Text
  , attachments :: ![Attachment]
  , embeds :: ![Embed]
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
      <*> fmap (fromMaybe []) (o Aeson..:? "embeds")
      <*> fmap (fromMaybe []) (o Aeson..:? "mentions")
      <*> o Aeson..:? "referenced_message"
      <*> o Aeson..:? "message_reference"
      <*> pure value
    ) value

data DeletedMessage = DeletedMessage
  { id :: !Text
  , channelId :: !Text
  , guildId :: !(Maybe Text)
  , raw :: !Aeson.Value
  }
  deriving (Show, Generic)

instance Aeson.FromJSON DeletedMessage where
  parseJSON value = Aeson.withObject "DeletedMessage" (\o ->
    DeletedMessage
      <$> o Aeson..: "id"
      <*> o Aeson..: "channel_id"
      <*> o Aeson..:? "guild_id"
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
    deriving Aeson.FromJSON via (SnakeJSON Attachment)

data Embed = Embed
  { image :: !(Maybe EmbedImage)
  , thumbnail :: !(Maybe EmbedImage)
  }
  deriving (Show, Generic, Aeson.FromJSON)

data EmbedImage = EmbedImage
  { imageUrl :: !Text
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (PrefixedSnakeJSON "image" EmbedImage)

data Reference = Reference
  { messageId :: !MessageId
  , channelId :: !Text
  , guildId :: !(Maybe Text)
  }
  deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing Reference)

data AllowedMentions = AllowedMentions
  { parse :: ![Text]
  , users :: ![Text]
  , repliedUser :: !(Maybe Bool)
  }
  deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing AllowedMentions)

data CreateMessageRequest = CreateMessageRequest
  { content :: !Text
  , messageReference :: !(Maybe Reference)
  , allowedMentions :: !AllowedMentions
  }
  deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing CreateMessageRequest)
