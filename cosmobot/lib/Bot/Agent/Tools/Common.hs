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
  , UseLimit (..)
  , newUseLimiter
  )
where

import Bot.Agent.Types
import Bot.Prelude
import Autodocodec (Bounds (..), boolCodec, integerWithBoundsCodec, listCodec, textCodec)
import Autodocodec.Schema (JSONSchema, jsonSchemaVia)
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
  schemaField name description (jsonSchemaVia textCodec)

fieldTextArray :: Text -> Text -> (Text, Aeson.Value)
fieldTextArray name description =
  schemaField name description (jsonSchemaVia (listCodec textCodec))

fieldTextArrayArray :: Text -> Text -> (Text, Aeson.Value)
fieldTextArrayArray name description =
  schemaField name description (jsonSchemaVia (listCodec (listCodec textCodec)))

fieldInteger :: Text -> Text -> (Text, Aeson.Value)
fieldInteger name description =
  fieldIntegerWithBounds name Bounds{boundsLower = Just 0, boundsUpper = Nothing} description

fieldIntegerMax :: Text -> Int -> Text -> (Text, Aeson.Value)
fieldIntegerMax name maximum description =
  fieldIntegerWithBounds name Bounds{boundsLower = Just 0, boundsUpper = Just (fromIntegral maximum)} description

fieldIntegerWithBounds :: Text -> Bounds Integer -> Text -> (Text, Aeson.Value)
fieldIntegerWithBounds name bounds description =
  schemaField name description (jsonSchemaVia (integerWithBoundsCodec bounds))

fieldBoolean :: Text -> Text -> (Text, Aeson.Value)
fieldBoolean name description =
  schemaField name description (jsonSchemaVia boolCodec)

schemaField :: Text -> Text -> JSONSchema -> (Text, Aeson.Value)
schemaField name description schema =
  (name, withDescription description (Aeson.toJSON schema))

withDescription :: Text -> Aeson.Value -> Aeson.Value
withDescription description = \case
  Aeson.Object object ->
    Aeson.Object (KeyMap.insert "description" (Aeson.String description) object)
  value ->
    value

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

data UseLimit
  = UseAllowed
  | UseLimitReached !Int

newUseLimiter :: IOE :> es => Maybe Int -> Eff es (Eff es UseLimit)
newUseLimiter maxUses = do
  uses <- liftIO (IORef.newIORef 0)
  pure do
    currentUses <- liftIO (IORef.readIORef uses)
    case maxUses of
      Just limit | currentUses >= limit ->
        pure (UseLimitReached currentUses)
      _ -> do
        liftIO (IORef.modifyIORef' uses (+ 1))
        pure UseAllowed
