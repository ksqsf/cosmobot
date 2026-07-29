{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Agent.Tools.Common
Description : Shared helpers for built-in agent tools
Stability   : experimental
-}

module Bot.Agent.Tools.Common
  ( chatTag
  , imageTag
  , sandboxTag
  , subagentTag
  , workTag
  , workspaceTag
  , superuserOnly
  , requiredText
  , optionalText
  , optionalInteger
  , requiredInt
  , optionalInt
  , optionalBoolean
  , optionalTextArray
  , fieldText
  , fieldTextArray
  , fieldTextArrayArray
  , fieldDateTime
  , fieldInteger
  , fieldIntegerMax
  , fieldBoolean
  , objectSchema
  , ttlMinutesArgument
  , jsonText
  , listResourceNames
  , renderResourceError
  , resourceToolFailure
  , UseLimit (..)
  , newUseLimiter
  )
where

import Bot.Agent.Tool
import Bot.Agent.Types
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.IORef as IORef
import qualified Data.Text.Encoding as TextEncoding

chatTag :: NamedTag
chatTag =
  NamedTag "chat" "Interact with the current chat, messages, members, files, and media."

imageTag :: NamedTag
imageTag =
  NamedTag "image" "View, generate, and edit images."

sandboxTag :: NamedTag
sandboxTag =
  NamedTag "sandbox" "Create and manage isolated persistent container sandboxes, run commands, and exchange files with the media cache."

subagentTag :: NamedTag
subagentTag =
  NamedTag "subagent" "Create and manage background agents scoped to the current chat."

workTag :: NamedTag
workTag =
  NamedTag "work" "Handle files and cached media, generate audio or Typst documents, schedule actions, and use permitted shells, terminals, or Emacs."

workspaceTag :: NamedTag
workspaceTag =
  NamedTag "workspace" "Manage privileged /work workspaces for multi-step repository, scripting, research, and operations tasks."

superuserOnly :: AgentContext -> Bool
superuserOnly =
  (.superuser)

requiredText :: Text -> Text -> ToolArgument Text
requiredText name description =
  requiredArgument (fieldText name description)

optionalText :: Text -> Text -> ToolArgument (Maybe Text)
optionalText name description =
  optionalArgument (fieldText name description)

optionalInteger :: Text -> Text -> ToolArgument (Maybe Integer)
optionalInteger name description =
  optionalArgument (fieldInteger name description)

requiredInt :: Text -> Text -> ToolArgument Int
requiredInt name description =
  requiredArgument (fieldInteger name description)

optionalInt :: Text -> Text -> ToolArgument (Maybe Int)
optionalInt name description =
  optionalArgument (fieldInteger name description)

optionalBoolean :: Text -> Text -> ToolArgument (Maybe Bool)
optionalBoolean name description =
  optionalArgument (fieldBoolean name description)

optionalTextArray :: Text -> Text -> ToolArgument (Maybe [Text])
optionalTextArray name description =
  optionalArgument (fieldTextArray name description)

fieldText :: Text -> Text -> (Text, Aeson.Value)
fieldText name description =
  schemaField name description [("type", Aeson.String "string")]

fieldTextArray :: Text -> Text -> (Text, Aeson.Value)
fieldTextArray name description =
  schemaField name description [("type", Aeson.String "array"), ("items", textSchema)]

fieldTextArrayArray :: Text -> Text -> (Text, Aeson.Value)
fieldTextArrayArray name description =
  schemaField name description [("type", Aeson.String "array"), ("items", Aeson.object [("type", Aeson.String "array"), ("items", textSchema)])]

fieldDateTime :: Text -> Text -> (Text, Aeson.Value)
fieldDateTime name description =
  schemaField name description [("type", Aeson.String "string"), ("format", Aeson.String "date-time")]

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

ttlMinutesArgument :: ToolArgument Int
ttlMinutesArgument =
  validateArgument validTTLMinutes
    (requiredInt "ttl_minutes" "Resource inactivity lifetime in minutes. Minimum 5.")

validTTLMinutes :: Int -> Either Text Int
validTTLMinutes ttlMinutes
  | ttlMinutes < 5 = Left "ttl_minutes must be at least 5."
  | ttlMinutes > maxBound `div` 60 = Left "ttl_minutes is too large."
  | otherwise = Right ttlMinutes

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

listResourceNames
  :: forall es a. (Resource.Resource :> es, Resource.ResourceObject (Eff es) a)
  => Proxy a
  -> Resource.ResourceAccess
  -> Eff es ToolResult
listResourceNames resourceType access = do
  resources <- Resource.list access
  pure . toolText . jsonText $
    [ resource.resourceId
    | resource <- resources
    , resource.resourceType == Resource.resourceTypeName @(Eff es) resourceType
    ]

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
  Resource.ResourceLifetimeUpdateFailed err -> err
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
