{-|
Module      : Bot.Plugin.Protocol
Description : Newline-delimited JSON-RPC protocol for external plugins
Stability   : experimental
-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Bot.Plugin.Protocol
  ( protocolVersion
  , maxFrameBytes
  , ProtocolError (..)
  , RpcId (..)
  , RpcRequest (..)
  , RpcResponse (..)
  , RpcError (..)
  , PluginMethod (..)
  , pluginMethodName
  , parsePluginMethod
  , RouteInvokeParams (..)
  , ToolInvokeParams (..)
  , encodeFrame
  , decodeFrame
  , parseInitializationResult
  )
where

import Bot.Core.Message
  ( ChatKind (..)
  , ChatPlatform (..)
  , IncomingMessage (..)
  , IncomingMessageEventKind (..)
  )
import Bot.Plugin.Types
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Set as Set

protocolVersion :: Text
protocolVersion = "1.0.0"

maxFrameBytes :: Int
maxFrameBytes = 1024 * 1024

data ProtocolError
  = FrameTooLarge
  | InvalidFrame !Text
  | InvalidManifest !Text
  deriving (Eq, Show)

newtype RpcId = RpcId { unRpcId :: Aeson.Value }
  deriving (Eq, Show)

instance Aeson.ToJSON RpcId where
  toJSON = (.unRpcId)

instance Aeson.FromJSON RpcId where
  parseJSON value = case value of
    Aeson.String _ -> pure (RpcId value)
    Aeson.Number _ -> pure (RpcId value)
    _ -> fail "JSON-RPC id must be a string or number"

data RpcRequest = RpcRequest
  { requestId :: !(Maybe RpcId)
  , method :: !Text
  , params :: !Aeson.Value
  }
  deriving (Eq, Show)

instance Aeson.ToJSON RpcRequest where
  toJSON request = Aeson.object $
    [ "jsonrpc" Aeson..= Aeson.String "2.0"
    , "method" Aeson..= request.method
    , "params" Aeson..= request.params
    ] <> maybe [] (pure . ("id" Aeson..=)) request.requestId

instance Aeson.FromJSON RpcRequest where
  parseJSON = Aeson.withObject "JSON-RPC request" \object -> do
    requireJsonRpcVersion object
    RpcRequest
      <$> object Aeson..:? "id"
      <*> object Aeson..: "method"
      <*> object Aeson..:? "params" Aeson..!= Aeson.Null

data RpcResponse
  = RpcSuccess !RpcId !Aeson.Value
  | RpcFailure !(Maybe RpcId) !RpcError
  deriving (Eq, Show)

instance Aeson.ToJSON RpcResponse where
  toJSON = \case
    RpcSuccess responseId result -> Aeson.object
      [ "jsonrpc" Aeson..= Aeson.String "2.0"
      , "id" Aeson..= responseId
      , "result" Aeson..= result
      ]
    RpcFailure responseId err -> Aeson.object
      [ "jsonrpc" Aeson..= Aeson.String "2.0"
      , "id" Aeson..= maybe Aeson.Null Aeson.toJSON responseId
      , "error" Aeson..= err
      ]

instance Aeson.FromJSON RpcResponse where
  parseJSON = Aeson.withObject "JSON-RPC response" \object -> do
    requireJsonRpcVersion object
    case (KeyMap.lookup "result" object, KeyMap.lookup "error" object) of
      (Just result, Nothing) -> RpcSuccess <$> object Aeson..: "id" <*> pure result
      (Nothing, Just errorValue) -> RpcFailure
        <$> parseNullableId (KeyMap.lookup "id" object)
        <*> Aeson.parseJSON errorValue
      _ -> fail "JSON-RPC response must contain exactly one of result or error"

data RpcError = RpcError
  { code :: !Int
  , message :: !Text
  , details :: !(Maybe Aeson.Value)
  }
  deriving (Eq, Show)

instance Aeson.ToJSON RpcError where
  toJSON err = Aeson.object $
    [ "code" Aeson..= err.code
    , "message" Aeson..= err.message
    ] <> maybe [] (pure . ("data" Aeson..=)) err.details

instance Aeson.FromJSON RpcError where
  parseJSON = Aeson.withObject "JSON-RPC error" \object -> RpcError
    <$> object Aeson..: "code"
    <*> object Aeson..: "message"
    <*> object Aeson..:? "data"

data PluginMethod
  = PluginInitialize
  | PluginRouteInvoke
  | PluginToolInvoke
  | PluginShutdown
  deriving (Eq, Show)

pluginMethodName :: PluginMethod -> Text
pluginMethodName = \case
  PluginInitialize -> "plugin.initialize"
  PluginRouteInvoke -> "plugin.route.invoke"
  PluginToolInvoke -> "plugin.tool.invoke"
  PluginShutdown -> "plugin.shutdown"

parsePluginMethod :: Text -> Maybe PluginMethod
parsePluginMethod = \case
  "plugin.initialize" -> Just PluginInitialize
  "plugin.route.invoke" -> Just PluginRouteInvoke
  "plugin.tool.invoke" -> Just PluginToolInvoke
  "plugin.shutdown" -> Just PluginShutdown
  _ -> Nothing

data RouteInvokeParams = RouteInvokeParams
  { invocationId :: !Text
  , routeId :: !Text
  , message :: !IncomingMessage
  , arguments :: !Text
  , timeoutSeconds :: !Int
  }
  deriving (Show)

instance Aeson.ToJSON RouteInvokeParams where
  toJSON params = Aeson.object
    [ "invocationId" Aeson..= params.invocationId
    , "routeId" Aeson..= params.routeId
    , "message" Aeson..= incomingMessageToJson params.message
    , "arguments" Aeson..= params.arguments
    , "timeoutSeconds" Aeson..= params.timeoutSeconds
    ]

instance Aeson.FromJSON RouteInvokeParams where
  parseJSON = Aeson.withObject "route invocation parameters" \object -> RouteInvokeParams
    <$> object Aeson..: "invocationId"
    <*> object Aeson..: "routeId"
    <*> (object Aeson..: "message" >>= parseIncomingMessage)
    <*> object Aeson..:? "arguments" Aeson..!= ""
    <*> object Aeson..: "timeoutSeconds"

data ToolInvokeParams = ToolInvokeParams
  { invocationId :: !Text
  , tool :: !Text
  , message :: !IncomingMessage
  , arguments :: !Aeson.Value
  , timeoutSeconds :: !Int
  }
  deriving (Show)

instance Aeson.ToJSON ToolInvokeParams where
  toJSON params = Aeson.object
    [ "invocationId" Aeson..= params.invocationId
    , "tool" Aeson..= params.tool
    , "message" Aeson..= incomingMessageToJson params.message
    , "arguments" Aeson..= params.arguments
    , "timeoutSeconds" Aeson..= params.timeoutSeconds
    ]

instance Aeson.FromJSON ToolInvokeParams where
  parseJSON = Aeson.withObject "tool invocation parameters" \object -> ToolInvokeParams
    <$> object Aeson..: "invocationId"
    <*> object Aeson..: "tool"
    <*> (object Aeson..: "message" >>= parseIncomingMessage)
    <*> object Aeson..:? "arguments" Aeson..!= Aeson.Object mempty
    <*> object Aeson..: "timeoutSeconds"

encodeFrame :: Aeson.ToJSON value => value -> Either ProtocolError ByteString
encodeFrame value =
  let bytes = LazyByteString.toStrict (Aeson.encode value) <> "\n"
  in if ByteString.length bytes > maxFrameBytes
      then Left FrameTooLarge
      else Right bytes

decodeFrame :: Aeson.FromJSON value => ByteString -> Either ProtocolError value
decodeFrame raw
  | ByteString.length raw > maxFrameBytes = Left FrameTooLarge
  | otherwise = do
      payload <- stripLineEnding raw
      when (ByteString.any (== 10) payload || ByteString.any (== 13) payload) $
        Left (InvalidFrame "frame contains an embedded newline")
      first (InvalidFrame . toText) (Aeson.eitherDecodeStrict' payload)

parseInitializationResult :: Aeson.Value -> Either ProtocolError PluginManifest
parseInitializationResult =
  first (InvalidManifest . toText) . AesonTypes.parseEither Aeson.parseJSON

instance Aeson.ToJSON PluginManifest where
  toJSON manifest = Aeson.object
    [ "protocolVersion" Aeson..= manifest.protocolVersion
    , "pluginVersion" Aeson..= manifest.pluginVersion
    , "routes" Aeson..= manifest.routes
    , "filters" Aeson..= manifest.filters
    , "tools" Aeson..= manifest.tools
    , "requestedCapabilities" Aeson..= Set.toList manifest.requestedCapabilities
    ]

instance Aeson.FromJSON PluginManifest where
  parseJSON = Aeson.withObject "plugin initialization result" \object -> do
    manifest <- PluginManifest
      <$> object Aeson..: "protocolVersion"
      <*> object Aeson..: "pluginVersion"
      <*> object Aeson..: "routes"
      <*> object Aeson..: "filters"
      <*> object Aeson..: "tools"
      <*> (Set.fromList <$> object Aeson..: "requestedCapabilities")
    either (fail . toString) pure (validateManifest manifest)

instance Aeson.ToJSON RouteDeclaration where
  toJSON route = Aeson.object
    [ "id" Aeson..= route.routeId
    , "help" Aeson..= Aeson.object
        [ "label" Aeson..= route.helpLabel
        , "description" Aeson..= route.helpDescription
        ]
    , "filter" Aeson..= route.filter
    , "disposition" Aeson..= route.disposition
    , "access" Aeson..= route.access
    ]

instance Aeson.FromJSON RouteDeclaration where
  parseJSON = Aeson.withObject "plugin route" \object -> do
    (helpLabel, helpDescription) <- object Aeson..: "help" >>= Aeson.withObject "route help" (\help -> (,)
      <$> help Aeson..: "label"
      <*> help Aeson..: "description")
    RouteDeclaration
      <$> object Aeson..: "id"
      <*> pure helpLabel
      <*> pure helpDescription
      <*> object Aeson..: "filter"
      <*> object Aeson..:? "disposition" Aeson..!= StopRouting
      <*> object Aeson..:? "access" Aeson..!= AllowedAccess

instance Aeson.ToJSON RouteDisposition where
  toJSON = Aeson.String . \case
    ContinueRouting -> "continue"
    StopRouting -> "stop"

instance Aeson.FromJSON RouteDisposition where
  parseJSON = Aeson.withText "route disposition" \case
    "continue" -> pure ContinueRouting
    "stop" -> pure StopRouting
    value -> fail ("unknown route disposition: " <> toString value)

instance Aeson.ToJSON RouteAccess where
  toJSON = Aeson.String . \case
    PublicAccess -> "public"
    AllowedAccess -> "allowed"
    SuperuserAccess -> "superuser"

instance Aeson.FromJSON RouteAccess where
  parseJSON = Aeson.withText "route access" \case
    "public" -> pure PublicAccess
    "allowed" -> pure AllowedAccess
    "superuser" -> pure SuperuserAccess
    value -> fail ("unknown route access: " <> toString value)

instance Aeson.ToJSON ToolDeclaration where
  toJSON tool = Aeson.object
    [ "name" Aeson..= tool.name
    , "description" Aeson..= tool.description
    , "schema" Aeson..= tool.schema
    ]

instance Aeson.FromJSON ToolDeclaration where
  parseJSON = Aeson.withObject "plugin tool" \object -> ToolDeclaration
    <$> object Aeson..: "name"
    <*> object Aeson..: "description"
    <*> object Aeson..: "schema"

instance Aeson.ToJSON RouteFilter where
  toJSON = \case
    FilterAll filters -> singleton "all" filters
    FilterAny filters -> singleton "any" filters
    FilterNot filter_ -> singleton "not" filter_
    FilterPredicate predicate -> Aeson.toJSON predicate

instance Aeson.FromJSON RouteFilter where
  parseJSON = Aeson.withObject "route filter" \object -> case KeyMap.toList object of
    [("all", value)] -> FilterAll <$> Aeson.parseJSON value
    [("any", value)] -> FilterAny <$> Aeson.parseJSON value
    [("not", value)] -> FilterNot <$> Aeson.parseJSON value
    [_] -> FilterPredicate <$> Aeson.parseJSON (Aeson.Object object)
    _ -> fail "route filter must contain exactly one operator"

instance Aeson.ToJSON RoutePredicate where
  toJSON = \case
    CommandIs command -> singleton "command" command
    HasPrefix prefix -> singleton "prefix" prefix
    PlatformIs platform -> singleton "platform" (platformName platform)
    EventIs event -> singleton "event" (eventName event)
    ChatKindIs kind -> singleton "chatKind" (chatKindName kind)
    IsReply value -> singleton "reply" value
    MentionsBot value -> singleton "mention" value
    HasAccess access -> singleton "access" access

instance Aeson.FromJSON RoutePredicate where
  parseJSON = Aeson.withObject "route predicate" \object -> case KeyMap.toList object of
    [("command", value)] -> CommandIs <$> Aeson.parseJSON value
    [("prefix", value)] -> HasPrefix <$> Aeson.parseJSON value
    [("platform", value)] -> PlatformIs <$> (Aeson.parseJSON value >>= parsePlatform)
    [("event", value)] -> EventIs <$> (Aeson.parseJSON value >>= parseEvent)
    [("chatKind", value)] -> ChatKindIs <$> (Aeson.parseJSON value >>= parseChatKind)
    [("reply", value)] -> IsReply <$> Aeson.parseJSON value
    [("mention", value)] -> MentionsBot <$> Aeson.parseJSON value
    [("access", value)] -> HasAccess <$> Aeson.parseJSON value
    _ -> fail "unknown route predicate"

instance Aeson.ToJSON AccessPredicate where
  toJSON = Aeson.String . \case
    ChatAllowed -> "chatAllowed"
    SenderAllowed -> "senderAllowed"
    SenderSuperuser -> "superuser"

instance Aeson.FromJSON AccessPredicate where
  parseJSON = Aeson.withText "access predicate" \case
    "chatAllowed" -> pure ChatAllowed
    "senderAllowed" -> pure SenderAllowed
    "superuser" -> pure SenderSuperuser
    value -> fail ("unknown access predicate: " <> toString value)

singleton :: Aeson.ToJSON value => Text -> value -> Aeson.Value
singleton key value = Aeson.object [Key.fromText key Aeson..= value]

requireJsonRpcVersion :: AesonTypes.Object -> AesonTypes.Parser ()
requireJsonRpcVersion object = do
  version <- object Aeson..: "jsonrpc"
  unless (version == ("2.0" :: Text)) (fail "jsonrpc must be 2.0")

parseNullableId :: Maybe Aeson.Value -> AesonTypes.Parser (Maybe RpcId)
parseNullableId Nothing = pure Nothing
parseNullableId (Just Aeson.Null) = pure Nothing
parseNullableId (Just value) = Just <$> Aeson.parseJSON value

stripLineEnding :: ByteString -> Either ProtocolError ByteString
stripLineEnding bytes = case ByteString.unsnoc bytes of
  Just (withoutNewline, 10) ->
    Right (fromMaybe withoutNewline (ByteString.stripSuffix "\r" withoutNewline))
  _ -> Left (InvalidFrame "frame is missing its newline delimiter")

incomingMessageToJson :: IncomingMessage -> Aeson.Value
incomingMessageToJson message = Aeson.object
  [ "eventKind" Aeson..= eventName message.eventKind
  , "platform" Aeson..= platformName message.platform
  , "kind" Aeson..= chatKindName message.kind
  , "chatId" Aeson..= message.chatId
  , "chatAliases" Aeson..= message.chatAliases
  , "chatDisplayName" Aeson..= message.chatDisplayName
  , "digest" Aeson..= message.digest
  , "senderId" Aeson..= message.senderId
  , "senderUsername" Aeson..= message.senderUsername
  , "senderDisplayName" Aeson..= message.senderDisplayName
  , "senderGlobalDisplayName" Aeson..= message.senderGlobalDisplayName
  , "messageId" Aeson..= message.messageId
  , "replyToMessageId" Aeson..= message.replyToMessageId
  , "mentions" Aeson..= message.mentions
  , "mentionUsernames" Aeson..= message.mentionUsernames
  , "imageUrls" Aeson..= message.imageUrls
  , "files" Aeson..= message.files
  , "text" Aeson..= message.text
  , "raw" Aeson..= message.raw
  ]

parseIncomingMessage :: Aeson.Value -> AesonTypes.Parser IncomingMessage
parseIncomingMessage = Aeson.withObject "plugin incoming message" \object -> IncomingMessage
  <$> (object Aeson..:? "eventKind" Aeson..!= "created" >>= parseEvent)
  <*> (object Aeson..: "platform" >>= parsePlatform)
  <*> (object Aeson..: "kind" >>= parseChatKind)
  <*> object Aeson..:? "chatId"
  <*> object Aeson..:? "chatAliases" Aeson..!= []
  <*> object Aeson..:? "chatDisplayName"
  <*> object Aeson..: "digest"
  <*> object Aeson..:? "senderId"
  <*> object Aeson..:? "senderUsername"
  <*> object Aeson..:? "senderDisplayName"
  <*> object Aeson..:? "senderGlobalDisplayName"
  <*> object Aeson..:? "messageId"
  <*> object Aeson..:? "replyToMessageId"
  <*> object Aeson..:? "mentions" Aeson..!= []
  <*> object Aeson..:? "mentionUsernames" Aeson..!= []
  <*> object Aeson..:? "imageUrls" Aeson..!= []
  <*> object Aeson..:? "files" Aeson..!= []
  <*> object Aeson..:? "text" Aeson..!= ""
  <*> object Aeson..:? "raw" Aeson..!= Aeson.Null

platformName :: ChatPlatform -> Text
platformName = \case
  PlatformQQ -> "qq"
  PlatformTelegram -> "telegram"
  PlatformMatrix -> "matrix"
  PlatformDiscord -> "discord"
  PlatformRPC -> "rpc"
  PlatformACP -> "acp"

parsePlatform :: Text -> AesonTypes.Parser ChatPlatform
parsePlatform = \case
  "qq" -> pure PlatformQQ
  "telegram" -> pure PlatformTelegram
  "matrix" -> pure PlatformMatrix
  "discord" -> pure PlatformDiscord
  "rpc" -> pure PlatformRPC
  "acp" -> pure PlatformACP
  value -> fail ("unknown platform: " <> toString value)

eventName :: IncomingMessageEventKind -> Text
eventName = \case
  IncomingMessageCreated -> "created"
  IncomingMessageDeleted -> "deleted"

parseEvent :: Text -> AesonTypes.Parser IncomingMessageEventKind
parseEvent = \case
  "created" -> pure IncomingMessageCreated
  "deleted" -> pure IncomingMessageDeleted
  value -> fail ("unknown message event: " <> toString value)

chatKindName :: ChatKind -> Text
chatKindName = \case
  ChatPrivate -> "private"
  ChatGroup -> "group"
  ChatChannel -> "channel"
  ChatUnknown value -> value

parseChatKind :: Text -> AesonTypes.Parser ChatKind
parseChatKind = \case
  "private" -> pure ChatPrivate
  "group" -> pure ChatGroup
  "channel" -> pure ChatChannel
  value -> pure (ChatUnknown value)
