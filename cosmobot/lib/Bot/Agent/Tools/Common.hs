{-|
Module      : Bot.Agent.Tools.Common
Description : Shared helpers for built-in agent tools
Stability   : experimental
-}

module Bot.Agent.Tools.Common
  ( everyone
  , superuserOnly
  , withParsedToolArgs
  , withTextArg
  , withIntegerArg
  , fieldText
  , fieldTextArray
  , fieldTextArrayArray
  , fieldInteger
  , fieldIntegerMax
  , fieldBoolean
  , objectSchema
  , jsonText
  , renderResourceError
  , resourceToolFailure
  , UseLimit (..)
  , newUseLimiter
  )
where

import Bot.Agent.Types
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

everyone :: AgentContext es -> Bool
everyone _ =
  True

superuserOnly :: AgentContext es -> Bool
superuserOnly =
  (.superuser)

withParsedToolArgs
  :: (Aeson.Value -> AesonTypes.Parser a)
  -> Aeson.Value
  -> (a -> Eff es ToolResult)
  -> Eff es ToolResult
withParsedToolArgs parser args action =
  either (pure . argumentFailure . Text.pack) action (AesonTypes.parseEither parser args)
  where
    argumentFailure err =
      toolFailure (permanentArgumentFailure err err).failure

withTextArg :: Text -> (Text -> Eff es ToolResult) -> Aeson.Value -> Eff es ToolResult
withTextArg key action args =
  withParsedToolArgs parser args action
  where
    parser = Aeson.withObject "tool arguments" (Aeson..: Key.fromText key)

withIntegerArg :: Text -> (Integer -> Eff es ToolResult) -> Aeson.Value -> Eff es ToolResult
withIntegerArg key action args =
  withParsedToolArgs parser args action
  where
    parser = Aeson.withObject "tool arguments" (Aeson..: Key.fromText key)

fieldText :: Text -> Text -> (Text, Aeson.Value)
fieldText name description =
  schemaField name description [("type", Aeson.String "string")]

fieldTextArray :: Text -> Text -> (Text, Aeson.Value)
fieldTextArray name description =
  schemaField name description [("type", Aeson.String "array"), ("items", textSchema)]

fieldTextArrayArray :: Text -> Text -> (Text, Aeson.Value)
fieldTextArrayArray name description =
  schemaField name description [("type", Aeson.String "array"), ("items", Aeson.object [("type", Aeson.String "array"), ("items", textSchema)])]

fieldInteger :: Text -> Text -> (Text, Aeson.Value)
fieldInteger name description =
  schemaField name description [("type", Aeson.String "integer"), ("minimum", Aeson.Number 0)]

fieldIntegerMax :: Text -> Int -> Text -> (Text, Aeson.Value)
fieldIntegerMax name maximum description =
  schemaField name description [("type", Aeson.String "integer"), ("minimum", Aeson.Number 0), ("maximum", Aeson.Number (fromIntegral maximum))]

fieldBoolean :: Text -> Text -> (Text, Aeson.Value)
fieldBoolean name description =
  schemaField name description [("type", Aeson.String "boolean")]

schemaField :: Text -> Text -> [(Key.Key, Aeson.Value)] -> (Text, Aeson.Value)
schemaField name description schema =
  (name, Aeson.Object (KeyMap.insert "description" (Aeson.String description) (KeyMap.fromList schema)))

textSchema :: Aeson.Value
textSchema =
  Aeson.object [("type", Aeson.String "string")]

objectSchema :: [(Text, Aeson.Value)] -> [Text] -> Aeson.Value
objectSchema fields required =
  Aeson.object
    [ "type" Aeson..= Aeson.String "object"
    , "properties" Aeson..= Aeson.object
        [ Key.fromText name Aeson..= schema
        | (name, schema) <- fields
        ]
    , "required" Aeson..= required
    , "additionalProperties" Aeson..= False
    ]

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

renderResourceError :: Resource.ResourceError -> Text
renderResourceError = \case
  Resource.MissingResourceIdentity -> "Resource operations require chat and sender identity."
  Resource.ResourceNotFoundOrNotOwned -> "Resource not found or not owned."
  Resource.ResourceTypeMismatch -> "Resource has the wrong type."
  Resource.ResourceUnavailable -> "Resource is currently unavailable."
  Resource.InvalidResourceName -> "Resource names must be 1-64 characters using only letters, digits, dot, underscore, or hyphen."
  Resource.ResourceNameAlreadyExists -> "Resource name already exists."
  Resource.ResourceCreationFailed err -> err
  Resource.ResourceRenameFailed err -> err
  Resource.ResourceCleanupFailed err -> err

resourceToolFailure :: Resource.ResourceError -> ToolResult
resourceToolFailure err =
  let message = renderResourceError err
  in toolFailure (permanentArgumentFailure message message).failure

data UseLimit
  = UseAllowed
  | UseLimitReached !Int

newUseLimiter :: IOE :> es => Maybe Int -> Eff es (Eff es UseLimit)
newUseLimiter maxUses = do
  uses <- liftIO (IORef.newIORef 0)
  pure do
    liftIO $
      IORef.atomicModifyIORef' uses \currentUses ->
        case maxUses of
          Just limit | currentUses >= limit ->
            (currentUses, UseLimitReached currentUses)
          _ ->
            (currentUses + 1, UseAllowed)
