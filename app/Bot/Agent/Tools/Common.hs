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
  , fieldInteger
  , fieldBoolean
  , objectSchema
  , jsonText
  )
where

import Bot.Agent.Types
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LazyByteString
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
  either (pure . toolText . Text.pack) action (AesonTypes.parseEither parser args)

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
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "string"
      , "description" Aeson..= description
      ]
  )

fieldTextArray :: Text -> Text -> (Text, Aeson.Value)
fieldTextArray name description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "array"
      , "items" Aeson..= Aeson.object
          [ "type" Aeson..= Aeson.String "string"
          ]
      , "description" Aeson..= description
      ]
  )

fieldInteger :: Text -> Text -> (Text, Aeson.Value)
fieldInteger name description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "integer"
      , "minimum" Aeson..= (0 :: Int)
      , "description" Aeson..= description
      ]
  )

fieldBoolean :: Text -> Text -> (Text, Aeson.Value)
fieldBoolean name description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "boolean"
      , "description" Aeson..= description
      ]
  )

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
