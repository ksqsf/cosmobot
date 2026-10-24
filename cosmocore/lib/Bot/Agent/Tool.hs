{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module      : Bot.Agent.Tool
Description : Dynamic agent tools, schemas, and argument decoding
Stability   : experimental
-}
module Bot.Agent.Tool
  ( Tool
  , ToolCallContext
  , NamedTag (..)
  , ToolTag (..)
  , ToolRunner
  , ToolArgument
  , ParsedArguments
  , NoArguments
  , noArguments
  , parsedArguments
  , requiredArgument
  , optionalArgument
  , mapArgument
  , validateArgument
  , withDefault
  , askToolContext
  , askToolCallMetadata
  , askToolTurn
  , askToolTranscript
  , allowWhen
  , hideUnlessM
  , hoistTool
  , mapSchemaM
  , noisy
  , resolveToolSchema
  , startTool
  , startToolWithTranscript
  , tagged
  , tool
  , toolEnableName
  , toolEnableTagRequests
  , toolAllowed
  , toolIsNoisy
  , toolName
  , toolTags
  , toolWithRunState
  , withDescription
  , withDescriptionBy
  )
where

import Bot.Agent.Result
import Bot.Core.Transcript (Transcript (..))
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Foldable as Foldable
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Reader.Static as Reader

type ToolRunner metadata m =
  metadata -> Aeson.Value -> m ToolResult

type ContextualToolRunner metadata m =
  metadata -> Int -> Transcript -> Aeson.Value -> m ToolResult

data ToolCallContext host metadata = ToolCallContext
  { agentContext :: !host
  , metadata :: !metadata
  , turn :: !Int
  , transcript :: !Transcript
  }

type ToolAction host metadata es =
  Eff (Reader.Reader (ToolCallContext host metadata) : es) ToolResult

askToolContext
  :: forall host metadata es. Reader.Reader (ToolCallContext host metadata) :> es
  => Eff es host
askToolContext =
  Reader.ask @(ToolCallContext host metadata) <&> (.agentContext)

askToolCallMetadata
  :: forall host metadata es. Reader.Reader (ToolCallContext host metadata) :> es
  => Eff es metadata
askToolCallMetadata =
  Reader.ask @(ToolCallContext host metadata) <&> (.metadata)

askToolTurn
  :: forall host metadata es. Reader.Reader (ToolCallContext host metadata) :> es
  => Eff es Int
askToolTurn =
  Reader.ask @(ToolCallContext host metadata) <&> (.turn)

askToolTranscript
  :: forall host metadata es. Reader.Reader (ToolCallContext host metadata) :> es
  => Eff es Transcript
askToolTranscript =
  Reader.ask @(ToolCallContext host metadata) <&> (.transcript)

data NamedTag = NamedTag
  { tagName :: !Text
  , tagDescription :: !Text
  }
  deriving (Eq, Ord, Show)

data ToolTag
  = Named !NamedTag
  | Essential
  deriving (Eq, Ord, Show)

toolEnableName :: Text
toolEnableName =
  "tool_enable"

toolEnableTagRequests :: Transcript -> [[Text]]
toolEnableTagRequests (Transcript messages) =
  [ tags
    | message <- Foldable.toList messages
    , message.role == "assistant"
    , call <- message.toolCalls
    , call.name == toolEnableName
    , Just tags <- [decodeTags call.arguments]
    ]
  where
    decodeTags =
      Aeson.decodeStrict' . TextEncoding.encodeUtf8
        >=> AesonTypes.parseMaybe
          (Aeson.withObject "tool_enable arguments" (Aeson..: Key.fromText "tags"))

data ToolArgument a = ToolArgument
  { argumentName :: !Text
  , argumentSchema :: !Aeson.Value
  , argumentRequired :: !Bool
  , argumentParser :: AesonTypes.Object -> AesonTypes.Parser a
  }

data ParsedArguments a = ParsedArguments
  { parsedSchema :: !Aeson.Value
  , parsedParser :: Aeson.Value -> AesonTypes.Parser a
  }

data NoArguments = NoArguments

noArguments :: NoArguments
noArguments =
  NoArguments

requiredArgument :: Aeson.FromJSON a => (Text, Aeson.Value) -> ToolArgument a
requiredArgument (name, schema) =
  ToolArgument
    { argumentName = name
    , argumentSchema = schema
    , argumentRequired = True
    , argumentParser = (Aeson..: Key.fromText name)
    }

optionalArgument :: Aeson.FromJSON a => (Text, Aeson.Value) -> ToolArgument (Maybe a)
optionalArgument (name, schema) =
  ToolArgument
    { argumentName = name
    , argumentSchema = schema
    , argumentRequired = False
    , argumentParser = (Aeson..:? Key.fromText name)
    }

mapArgument
  :: (a -> AesonTypes.Parser b)
  -> ToolArgument a
  -> ToolArgument b
mapArgument transform argument =
  ToolArgument
    { argumentName = argument.argumentName
    , argumentSchema = argument.argumentSchema
    , argumentRequired = argument.argumentRequired
    , argumentParser = argument.argumentParser >=> transform
    }

validateArgument
  :: (a -> Either Text b)
  -> ToolArgument a
  -> ToolArgument b
validateArgument validate =
  mapArgument (either (fail . toString) pure . validate)

withDefault :: a -> ToolArgument (Maybe a) -> ToolArgument a
withDefault defaultValue =
  mapArgument (pure . fromMaybe defaultValue)

parsedArguments
  :: Aeson.Value
  -> (Aeson.Value -> AesonTypes.Parser a)
  -> ParsedArguments a
parsedArguments schema parser =
  ParsedArguments{parsedSchema = schema, parsedParser = parser}

class ToolArguments arguments where
  type ToolHandler arguments host metadata (es :: [Effect])

  toolArgumentsSchema :: arguments -> Aeson.Value

  parseToolHandler
    :: arguments
    -> ToolHandler arguments host metadata es
    -> Aeson.Value
    -> AesonTypes.Parser (ToolAction host metadata es)

instance ToolArguments NoArguments where
  type ToolHandler NoArguments host metadata es = ToolAction host metadata es

  toolArgumentsSchema _ =
    emptyObjectSchema

  parseToolHandler _ handler =
    Aeson.withObject "tool arguments" (const (pure handler))

instance ToolArguments (ToolArgument a) where
  type ToolHandler (ToolArgument a) host metadata es = a -> ToolAction host metadata es

  toolArgumentsSchema argument =
    argumentsObjectSchema [argumentField argument] (maybeToList (requiredName argument))

  parseToolHandler argument handler =
    Aeson.withObject "tool arguments" \object ->
      handler <$> argument.argumentParser object

instance ToolArguments (ToolArgument a, ToolArgument b) where
  type ToolHandler (ToolArgument a, ToolArgument b) host metadata es = a -> b -> ToolAction host metadata es

  toolArgumentsSchema (a, b) =
    argumentsObjectSchema
      [argumentField a, argumentField b]
      (catMaybes [requiredName a, requiredName b])

  parseToolHandler (a, b) handler =
    Aeson.withObject "tool arguments" \object ->
      handler <$> a.argumentParser object <*> b.argumentParser object

instance ToolArguments (ToolArgument a, ToolArgument b, ToolArgument c) where
  type ToolHandler (ToolArgument a, ToolArgument b, ToolArgument c) host metadata es =
    a -> b -> c -> ToolAction host metadata es

  toolArgumentsSchema (a, b, c) =
    argumentsObjectSchema
      [argumentField a, argumentField b, argumentField c]
      (catMaybes [requiredName a, requiredName b, requiredName c])

  parseToolHandler (a, b, c) handler =
    Aeson.withObject "tool arguments" \object ->
      handler
        <$> a.argumentParser object
        <*> b.argumentParser object
        <*> c.argumentParser object

instance ToolArguments (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d) where
  type ToolHandler (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d) host metadata es =
    a -> b -> c -> d -> ToolAction host metadata es

  toolArgumentsSchema (a, b, c, d) =
    argumentsObjectSchema
      [argumentField a, argumentField b, argumentField c, argumentField d]
      (catMaybes [requiredName a, requiredName b, requiredName c, requiredName d])

  parseToolHandler (a, b, c, d) handler =
    Aeson.withObject "tool arguments" \object ->
      handler
        <$> a.argumentParser object
        <*> b.argumentParser object
        <*> c.argumentParser object
        <*> d.argumentParser object

instance ToolArguments (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d, ToolArgument e) where
  type ToolHandler (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d, ToolArgument e) host metadata es =
    a -> b -> c -> d -> e -> ToolAction host metadata es

  toolArgumentsSchema (a, b, c, d, e) =
    argumentsObjectSchema
      [argumentField a, argumentField b, argumentField c, argumentField d, argumentField e]
      (catMaybes [requiredName a, requiredName b, requiredName c, requiredName d, requiredName e])

  parseToolHandler (a, b, c, d, e) handler =
    Aeson.withObject "tool arguments" \object ->
      handler
        <$> a.argumentParser object
        <*> b.argumentParser object
        <*> c.argumentParser object
        <*> d.argumentParser object
        <*> e.argumentParser object

instance ToolArguments (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d, ToolArgument e, ToolArgument f) where
  type ToolHandler (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d, ToolArgument e, ToolArgument f) host metadata es =
    a -> b -> c -> d -> e -> f -> ToolAction host metadata es

  toolArgumentsSchema (a, b, c, d, e, f) =
    argumentsObjectSchema
      [argumentField a, argumentField b, argumentField c, argumentField d, argumentField e, argumentField f]
      (catMaybes [requiredName a, requiredName b, requiredName c, requiredName d, requiredName e, requiredName f])

  parseToolHandler (a, b, c, d, e, f) handler =
    Aeson.withObject "tool arguments" \object ->
      handler
        <$> a.argumentParser object
        <*> b.argumentParser object
        <*> c.argumentParser object
        <*> d.argumentParser object
        <*> e.argumentParser object
        <*> f.argumentParser object

instance ToolArguments (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d, ToolArgument e, ToolArgument f, ToolArgument g) where
  type ToolHandler (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d, ToolArgument e, ToolArgument f, ToolArgument g) host metadata es =
    a -> b -> c -> d -> e -> f -> g -> ToolAction host metadata es

  toolArgumentsSchema (a, b, c, d, e, f, g) =
    argumentsObjectSchema
      [argumentField a, argumentField b, argumentField c, argumentField d, argumentField e, argumentField f, argumentField g]
      (catMaybes [requiredName a, requiredName b, requiredName c, requiredName d, requiredName e, requiredName f, requiredName g])

  parseToolHandler (a, b, c, d, e, f, g) handler =
    Aeson.withObject "tool arguments" \object ->
      handler
        <$> a.argumentParser object
        <*> b.argumentParser object
        <*> c.argumentParser object
        <*> d.argumentParser object
        <*> e.argumentParser object
        <*> f.argumentParser object
        <*> g.argumentParser object

instance ToolArguments (ParsedArguments a) where
  type ToolHandler (ParsedArguments a) host metadata es = a -> ToolAction host metadata es

  toolArgumentsSchema =
    (.parsedSchema)

  parseToolHandler arguments handler value =
    handler <$> arguments.parsedParser value

argumentField :: ToolArgument a -> (Text, Aeson.Value)
argumentField argument =
  (argument.argumentName, argument.argumentSchema)

requiredName :: ToolArgument a -> Maybe Text
requiredName argument =
  if argument.argumentRequired
    then Just argument.argumentName
    else Nothing

emptyObjectSchema :: Aeson.Value
emptyObjectSchema =
  argumentsObjectSchema [] []

argumentsObjectSchema :: [(Text, Aeson.Value)] -> [Text] -> Aeson.Value
argumentsObjectSchema fields required =
  Aeson.object
    [ "type" Aeson..= Aeson.String "object"
    , "properties" Aeson..= Aeson.object
        [ Key.fromText fieldName Aeson..= schema
        | (fieldName, schema) <- fields
        ]
    , "required" Aeson..= required
    , "additionalProperties" Aeson..= False
    ]

-- | A tool whose model-visible schema may vary between model turns.
--
-- Use the smart constructor and combinators rather than constructing this
-- value directly. A tool's name is its stable dispatch identity; schema
-- resolution may hide the tool or change its description and parameters.
data Tool host metadata m = Tool
  { name :: !Text
  , tags :: ![ToolTag]
  , schemaResolver :: host -> Transcript -> Int -> m (Maybe LLM.FunctionTool)
  , noisyFlag :: !Bool
  , allowedPredicate :: host -> Bool
  , runnerFactory :: host -> m (ContextualToolRunner metadata m)
  }

hoistTool :: Monad n => (forall x. m x -> n x) -> Tool host metadata m -> Tool host metadata n
hoistTool liftTool definition =
  Tool
    { name = definition.name
    , tags = definition.tags
    , schemaResolver = \context transcript turn ->
        liftTool (definition.schemaResolver context transcript turn)
    , noisyFlag = definition.noisyFlag
    , allowedPredicate = definition.allowedPredicate
    , runnerFactory = \context -> do
        run <- liftTool (definition.runnerFactory context)
        pure \metadata turn transcript arguments ->
          liftTool (run metadata turn transcript arguments)
    }

tool
  :: ToolArguments arguments
  => Text
  -> arguments
  -> ToolHandler arguments host metadata es
  -> Tool host metadata (Eff es)
tool name arguments handler =
  toolWithRunState name arguments (const (pure ())) (const handler)

toolWithRunState
  :: ToolArguments arguments
  => Text
  -> arguments
  -> (host -> Eff es state)
  -> (state -> ToolHandler arguments host metadata es)
  -> Tool host metadata (Eff es)
toolWithRunState name arguments initialize handler =
  Tool
    { name
    , tags = [Essential]
    , schemaResolver = \_ _ _ -> pure (Just LLM.FunctionTool
        { name
        , description = ""
        , parameters = toolArgumentsSchema arguments
        })
    , noisyFlag = False
    , allowedPredicate = const True
    , runnerFactory = \context -> do
        initialized <- initialize context
        pure \metadata turn transcript rawArguments ->
          case AesonTypes.parseEither
            (parseToolHandler arguments (handler initialized))
            rawArguments of
              Left err ->
                pure (argumentFailure (toText err))
              Right action ->
                Reader.runReader ToolCallContext
                  { agentContext = context
                  , metadata
                  , turn
                  , transcript
                  }
                  action
    }
  where
    argumentFailure err =
      toolFailure (permanentArgumentFailure err err)

allowWhen :: (host -> Bool) -> Tool host metadata m -> Tool host metadata m
allowWhen predicate definition =
  definition
    { allowedPredicate = \context ->
        definition.allowedPredicate context && predicate context
    }

tagged :: [NamedTag] -> Tool host metadata m -> Tool host metadata m
tagged tags definition =
  definition{tags = map Named tags}

noisy :: Tool host metadata m -> Tool host metadata m
noisy definition =
  definition{noisyFlag = True}

mapSchemaM
  :: Monad m
  => (host -> Transcript -> Int -> LLM.FunctionTool -> m (Maybe LLM.FunctionTool))
  -> Tool host metadata m
  -> Tool host metadata m
mapSchemaM transform definition =
  definition
    { schemaResolver = \context transcript turn -> do
        definition.schemaResolver context transcript turn >>= \case
          Nothing ->
            pure Nothing
          Just schema ->
            transform context transcript turn schema
    }

hideUnlessM
  :: Monad m
  => (host -> Transcript -> Int -> m Bool)
  -> Tool host metadata m
  -> Tool host metadata m
hideUnlessM predicate =
  mapSchemaM \context transcript turn schema ->
    predicate context transcript turn <&> \visible ->
      if visible then Just schema else Nothing

withDescription :: Monad m => Text -> Tool host metadata m -> Tool host metadata m
withDescription =
  withDescriptionBy . const

withDescriptionBy :: Monad m => (host -> Text) -> Tool host metadata m -> Tool host metadata m
withDescriptionBy description =
  mapSchemaM \context _ _ schema ->
    pure (Just schema{LLM.description = description context})

toolName :: Tool host metadata m -> Text
toolName Tool{name} =
  name

toolTags :: Tool host metadata m -> [ToolTag]
toolTags Tool{tags} =
  tags

resolveToolSchema
  :: Functor m
  => Tool host metadata m
  -> host
  -> Transcript
  -> Int
  -> m (Maybe LLM.FunctionTool)
resolveToolSchema Tool{name, schemaResolver} context transcript turn =
  fmap (fmap \schema -> LLM.FunctionTool
    { name
    , description = schema.description
    , parameters = schema.parameters
    })
    (schemaResolver context transcript turn)

toolIsNoisy :: Tool host metadata m -> Bool
toolIsNoisy Tool{noisyFlag} =
  noisyFlag

toolAllowed :: Tool host metadata m -> host -> Bool
toolAllowed Tool{allowedPredicate} =
  allowedPredicate

startTool :: Functor m => Tool host metadata m -> host -> m (ToolRunner metadata m)
startTool definition context =
  (\run metadata arguments -> run metadata 0 (Transcript mempty) arguments)
    <$> startToolWithTranscript definition context

startToolWithTranscript :: Tool host metadata m -> host -> m (ContextualToolRunner metadata m)
startToolWithTranscript Tool{runnerFactory} =
  runnerFactory
