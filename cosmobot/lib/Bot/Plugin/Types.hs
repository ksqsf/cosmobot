{-|
Module      : Bot.Plugin.Types
Description : External plugin bundle and manifest domain types
Stability   : experimental
-}
module Bot.Plugin.Types
  ( PluginId (..)
  , validatePluginId
  , PluginBundle (..)
  , PluginLifecycleConfig (..)
  , defaultPluginLifecycleConfig
  , PluginStatus (..)
  , Capability (..)
  , PluginManifest (..)
  , RouteDeclaration (..)
  , RouteDisposition (..)
  , RouteAccess (..)
  , ToolDeclaration (..)
  , ToolInvocationResult (..)
  , ToolFailureKind (..)
  , RouteFilter (..)
  , RoutePredicate (..)
  , AccessPredicate (..)
  , maxRouteFilterDepth
  , maxRouteFilterNodes
  , validateManifest
  , validateToolNamespace
  , validateRouteFilter
  , matchesRouteFilter
  , matchesRouteAccess
  , matchesRouteDeclaration
  )
where

import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

newtype PluginId = PluginId { unPluginId :: Text }
  deriving stock (Eq, Ord, Show, Generic)

instance Aeson.ToJSON PluginId where
  toJSON = Aeson.String . (.unPluginId)

instance Aeson.FromJSON PluginId where
  parseJSON = Aeson.withText "plugin id" $
    either (fail . toString) pure . validatePluginId

validatePluginId :: Text -> Either Text PluginId
validatePluginId value =
  PluginId value <$ requireProviderIdentifier "plugin id" value

data PluginLifecycleConfig = PluginLifecycleConfig
  { required :: !Bool
  , sandboxed :: !Bool
  , routeTimeoutSeconds :: !Int
  , toolTimeoutSeconds :: !Int
  , restartLimit :: !Int
  }
  deriving (Eq, Show)

defaultPluginLifecycleConfig :: PluginLifecycleConfig
defaultPluginLifecycleConfig = PluginLifecycleConfig
  { required = False
  , sandboxed = True
  , routeTimeoutSeconds = 10
  , toolTimeoutSeconds = 300
  , restartLimit = 3
  }

data PluginBundle = PluginBundle
  { pluginId :: !PluginId
  , bundleDir :: !FilePath
  , executablePath :: !FilePath
  , configPath :: !FilePath
  , lifecycle :: !PluginLifecycleConfig
  }
  deriving (Eq, Show)

data PluginStatus = PluginStatus
  { pluginId :: !PluginId
  , generation :: !Int
  , pluginVersion :: !Text
  , required :: !Bool
  , sandboxed :: !Bool
  , routeCount :: !Int
  , toolCount :: !Int
  }
  deriving (Eq, Show, Generic)

data Capability = Chat | LLM | Agent | Media
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Aeson.ToJSON Capability where
  toJSON = Aeson.String . capabilityName

instance Aeson.FromJSON Capability where
  parseJSON = Aeson.withText "plugin capability" \case
    "chat" -> pure Chat
    "llm" -> pure LLM
    "agent" -> pure Agent
    "media" -> pure Media
    name -> fail ("unknown plugin capability: " <> toString name)

capabilityName :: Capability -> Text
capabilityName = \case
  Chat -> "chat"
  LLM -> "llm"
  Agent -> "agent"
  Media -> "media"

data PluginManifest = PluginManifest
  { protocolVersion :: !Text
  , pluginVersion :: !Text
  , routes :: ![RouteDeclaration]
  , filters :: !(Map.Map Text RouteFilter)
  , tools :: ![ToolDeclaration]
  , requestedCapabilities :: !(Set.Set Capability)
  }
  deriving (Eq, Show)

data RouteDeclaration = RouteDeclaration
  { routeId :: !Text
  , helpLabel :: !Text
  , helpDescription :: !Text
  , filter :: !Text
  , disposition :: !RouteDisposition
  , access :: !RouteAccess
  }
  deriving (Eq, Show, Generic)

data RouteDisposition = ContinueRouting | StopRouting
  deriving (Eq, Show)

data RouteAccess = PublicAccess | AllowedAccess | SuperuserAccess
  deriving (Eq, Show)

data ToolDeclaration = ToolDeclaration
  { name :: !Text
  , description :: !Text
  , schema :: !Aeson.Value
  }
  deriving (Eq, Show, Generic)

data ToolFailureKind = PermanentArguments | TransientInvocation
  deriving (Eq, Show)

data ToolInvocationResult
  = ToolInvocationSuccess !Text ![Text]
  | ToolInvocationFailure !ToolFailureKind !Text !Text
  deriving (Eq, Show)

data RouteFilter
  = FilterAll ![RouteFilter]
  | FilterAny ![RouteFilter]
  | FilterNot !RouteFilter
  | FilterPredicate !RoutePredicate
  deriving (Eq, Show)

data RoutePredicate
  = CommandIs !Text
  | HasPrefix !Text
  | PlatformIs !ChatPlatform
  | EventIs !IncomingMessageEventKind
  | ChatKindIs !ChatKind
  | IsReply !Bool
  | MentionsBot !Bool
  | HasAccess !AccessPredicate
  deriving (Eq, Show)

data AccessPredicate = ChatAllowed | SenderAllowed | SenderSuperuser
  deriving (Eq, Show)

maxRouteFilterDepth, maxRouteFilterNodes :: Int
maxRouteFilterDepth = 8
maxRouteFilterNodes = 64

validateManifest :: PluginManifest -> Either Text PluginManifest
validateManifest manifest = do
  require (manifest.protocolVersion == "1.0.0") "unsupported protocolVersion"
  requireNonEmpty "pluginVersion" manifest.pluginVersion
  traverse_ validateFilterEntry (Map.toList manifest.filters)
  traverse_ validateRoute manifest.routes
  traverse_ validateTool manifest.tools
  requireUnique "route id" (map (.routeId) manifest.routes)
  requireUnique "tool name" (map (.name) manifest.tools)
  pure manifest
  where
    validateFilterEntry (filterId, routeFilter) = do
      requireNonEmpty "filter id" filterId
      validateRouteFilter routeFilter

    validateRoute route = do
      requireNonEmpty "route id" route.routeId
      requireNonEmpty "route help label" route.helpLabel
      requireNonEmpty "route help description" route.helpDescription
      require (Map.member route.filter manifest.filters)
        ("route references unknown filter: " <> route.filter)

    validateTool tool = do
      requireProviderIdentifier "tool name" tool.name
      requireNonEmpty "tool description" tool.description
      validateJsonSchema tool.schema
      case tool.schema of
        Aeson.Object object -> require
          (KeyMap.lookup (AesonKey.fromText "type") object == Just (Aeson.String "object"))
          ("tool schema must be an object schema: " <> tool.name)
        _ -> Left ("tool schema must be a JSON object: " <> tool.name)

validateToolNamespace :: PluginId -> PluginManifest -> Either Text ()
validateToolNamespace pluginId manifest =
  traverse_ validate manifest.tools
  where
    validate tool = require
      (Text.length pluginId.unPluginId + 2 + Text.length tool.name <= 64)
      ("model tool name exceeds 64 characters: " <> pluginId.unPluginId <> "__" <> tool.name)

validateJsonSchema :: Aeson.Value -> Either Text ()
validateJsonSchema (Aeson.Object object) = do
  traverse_ validateAlternatives (KeyMap.lookup "anyOf" object)
  schemaType <- traverse parseSchemaType (KeyMap.lookup "type" object)
  case schemaType of
    Just "object" -> validateObjectSchema object
    Just "array" -> traverse_ validateJsonSchema (KeyMap.lookup "items" object)
    Just _ -> rejectStructuralKeywords object
    Nothing -> do
      require (not (KeyMap.member "properties" object || KeyMap.member "required" object || KeyMap.member "items" object))
        "schema structural fields require a type"
  where
    validateAlternatives (Aeson.Array alternatives) = do
      require (not (Vector.null alternatives)) "schema anyOf must not be empty"
      traverse_ validateJsonSchema alternatives
    validateAlternatives _ = Left "schema anyOf must be an array"
validateJsonSchema _ = Left "schema must be an object"

parseSchemaType :: Aeson.Value -> Either Text Text
parseSchemaType (Aeson.String schemaType) = do
  require (schemaType `elem` ["object", "array", "string", "integer", "number", "boolean", "null"])
    ("unsupported schema type: " <> schemaType)
  pure schemaType
parseSchemaType _ = Left "schema type must be a string"

validateObjectSchema :: KeyMap.KeyMap Aeson.Value -> Either Text ()
validateObjectSchema object = do
  properties <- case KeyMap.lookup "properties" object of
    Nothing -> Right mempty
    Just (Aeson.Object value) -> traverse_ validateJsonSchema value $> value
    Just _ -> Left "schema properties must be an object"
  required <- case KeyMap.lookup "required" object of
    Nothing -> Right []
    Just (Aeson.Array values) -> traverse parseRequiredName (Vector.toList values)
    Just _ -> Left "schema required must be an array"
  require (all (`KeyMap.member` properties) (map AesonKey.fromText required))
    "schema required names must exist in properties"
  case KeyMap.lookup "additionalProperties" object of
    Nothing -> Right ()
    Just (Aeson.Bool _) -> Right ()
    Just schema -> validateJsonSchema schema
  where
    parseRequiredName (Aeson.String value) = Right value
    parseRequiredName _ = Left "schema required names must be strings"

rejectStructuralKeywords :: KeyMap.KeyMap Aeson.Value -> Either Text ()
rejectStructuralKeywords object =
  require (not (any (`KeyMap.member` object) ["properties", "required", "items", "additionalProperties"]))
    "schema structural fields do not match its type"

validateRouteFilter :: RouteFilter -> Either Text ()
validateRouteFilter routeFilter = do
  require (filterDepth routeFilter <= maxRouteFilterDepth) "route filter exceeds maximum depth"
  require (filterNodes routeFilter <= maxRouteFilterNodes) "route filter exceeds maximum node count"
  validate routeFilter
  where
    validate = \case
      FilterAll filters -> require (not (null filters)) "all filter must not be empty" >> traverse_ validate filters
      FilterAny filters -> require (not (null filters)) "any filter must not be empty" >> traverse_ validate filters
      FilterNot filter_ -> validate filter_
      FilterPredicate predicate -> validatePredicate predicate

    validatePredicate = \case
      CommandIs value -> requireNonEmpty "command predicate" value
      HasPrefix value -> requireNonEmpty "prefix predicate" value
      _ -> Right ()

matchesRouteFilter :: RouteFilter -> IncomingMessage -> Bool
matchesRouteFilter routeFilter message = case routeFilter of
  FilterAll filters -> all (`matchesRouteFilter` message) filters
  FilterAny filters -> any (`matchesRouteFilter` message) filters
  FilterNot filter_ -> not (matchesRouteFilter filter_ message)
  FilterPredicate predicate -> matchesPredicate predicate message

matchesRouteDeclaration
  :: Map.Map Text RouteFilter
  -> RouteDeclaration
  -> IncomingMessage
  -> Bool
matchesRouteDeclaration filters route message =
  matchesRouteAccess route.access message
    && maybe False (`matchesRouteFilter` message) (Map.lookup route.filter filters)

matchesPredicate :: RoutePredicate -> IncomingMessage -> Bool
matchesPredicate predicate message = case predicate of
  CommandIs command -> firstWord message.text == command
  HasPrefix prefix -> prefix `Text.isPrefixOf` message.text
  PlatformIs platform -> message.platform == platform
  EventIs event -> message.eventKind == event
  ChatKindIs kind -> message.kind == kind
  IsReply expected -> isJust message.replyToMessageId == expected
  MentionsBot expected -> message.digest.mentionsBot == expected
  HasAccess access -> matchesAccessPredicate access message

matchesRouteAccess :: RouteAccess -> IncomingMessage -> Bool
matchesRouteAccess = \case
  PublicAccess -> const True
  AllowedAccess -> \message -> message.digest.chatIsAllowed || message.digest.senderIsAllowed
  SuperuserAccess -> (.digest.senderIsSuperuser)

matchesAccessPredicate :: AccessPredicate -> IncomingMessage -> Bool
matchesAccessPredicate = \case
  ChatAllowed -> (.digest.chatIsAllowed)
  SenderAllowed -> (.digest.senderIsAllowed)
  SenderSuperuser -> (.digest.senderIsSuperuser)

firstWord :: Text -> Text
firstWord = fromMaybe "" . viaNonEmpty head . Text.words

filterDepth :: RouteFilter -> Int
filterDepth = \case
  FilterAll filters -> 1 + foldl' max 0 (map filterDepth filters)
  FilterAny filters -> 1 + foldl' max 0 (map filterDepth filters)
  FilterNot filter_ -> 1 + filterDepth filter_
  FilterPredicate _ -> 1

filterNodes :: RouteFilter -> Int
filterNodes = \case
  FilterAll filters -> 1 + sum (map filterNodes filters)
  FilterAny filters -> 1 + sum (map filterNodes filters)
  FilterNot filter_ -> 1 + filterNodes filter_
  FilterPredicate _ -> 1

require :: Bool -> Text -> Either Text ()
require True _ = Right ()
require False message = Left message

requireNonEmpty :: Text -> Text -> Either Text ()
requireNonEmpty label value =
  require (not (Text.null (Text.strip value))) (label <> " must not be empty")

requireIdentifier :: Text -> Text -> Either Text ()
requireIdentifier label value = do
  requireNonEmpty label value
  require (Text.all validCharacter value) (label <> " contains invalid characters: " <> value)
  where
    validCharacter character = Char.isAscii character && Char.isAlphaNum character
      || character == '_' || character == '-'

requireProviderIdentifier :: Text -> Text -> Either Text ()
requireProviderIdentifier label value = do
  requireIdentifier label value
  require (not ("__" `Text.isInfixOf` value)) (label <> " must not contain '__'")

requireUnique :: Text -> [Text] -> Either Text ()
requireUnique label values =
  require (Set.size (Set.fromList values) == length values) ("duplicate " <> label)
