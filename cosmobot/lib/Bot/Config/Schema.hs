{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : Bot.Config.Schema
Description : Typed, owner-defined configuration inspection metadata
Stability   : experimental
-}

module Bot.Config.Schema
  ( ConfigSchema (..)
  , ConfigOption
  , OptionType
  , Secret (..)
  , boolean
  , integer
  , number
  , text
  , enum
  , secret
  , list
  , identity
  , identityList
  , option
  , optionalOption
  , schemaFromValue
  , prefixOptions
  , mapOptions
  , mapMaybeOptions
  , inspectOptions
  , optionPath
  , optionIsSecret
  , optionIsRequired
  , optionToml
  , decodeOptionValue
  )
where

import Bot.Prelude hiding (identity)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Toml.Schema.Matcher as Matcher
import qualified Toml.Semantics.Types as TomlValue
import qualified Prelude

-- | Secret wrapper whose only public representation is its presence state.
newtype Secret = Secret { unSecret :: Text }
  deriving stock (Eq)

instance Prelude.Show Secret where
  showsPrec _ (Secret value) =
    Prelude.showString $ if Text.null value then "<secret:unset>" else "<secret:configured>"

data OptionType a = OptionType
  { kind :: !Text
  , choices :: ![Text]
  , encode :: a -> Aeson.Value
  , decode :: Aeson.Value -> AesonTypes.Parser a
  , render :: a -> Text
  , configured :: a -> Bool
  , isSecret :: !Bool
  }

data ConfigOption source runtime = forall a. ConfigOption
  { path :: ![Text]
  , label :: !Text
  , description :: !Text
  , owner :: !Text
  , optionType :: !(OptionType a)
  , required :: !Bool
  , defaultValue :: !(Maybe a)
  , constraints :: !Aeson.Value
  , sourceValue :: source -> Maybe a
  , effectiveValue :: runtime -> Maybe a
  }

-- | An owner parser and the inspection contract for the values it owns.
data ConfigSchema source runtime = ConfigSchema
  { parser :: forall l. TomlValue.Value' l -> Matcher.Matcher l source
  , options :: ![ConfigOption source runtime]
  }

boolean :: OptionType Bool
boolean = jsonType "boolean" []

integer :: (Aeson.FromJSON a, Aeson.ToJSON a, Integral a) => OptionType a
integer = jsonType "integer" []

number :: (Aeson.FromJSON a, Aeson.ToJSON a, RealFloat a) => OptionType a
number = jsonType "number" []

text :: OptionType Text
text = jsonType "text" []

enum :: [Text] -> OptionType Text
enum values = (jsonType "enum" values)
  { decode = Aeson.withText "enum" \value ->
      if value `elem` values then pure value else fail "value is not an allowed enum member"
  }

secret :: OptionType Secret
secret = OptionType
  { kind = "secret"
  , choices = []
  , encode = secretState
  , decode = Aeson.withText "secret" (pure . Secret)
  , render = TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode . (.unSecret)
  , configured = not . Text.null . (.unSecret)
  , isSecret = True
  }

list :: (Aeson.FromJSON a, Aeson.ToJSON a) => Text -> OptionType [a]
list itemKind = (jsonType "list" [])
  { choices = [itemKind]
  }

identity :: OptionType Aeson.Value
identity = (jsonType "identity" [])
  { decode = parseIdentity
  }

identityList :: OptionType [Aeson.Value]
identityList = (jsonType "identity_list" [])
  { decode = Aeson.withArray "identity list" (traverse parseIdentity . toList)
  }

parseIdentity :: Aeson.Value -> AesonTypes.Parser Aeson.Value
parseIdentity value@Aeson.String{} = pure value
parseIdentity value@Aeson.Number{} = (Aeson.parseJSON value :: AesonTypes.Parser Integer) $> value
parseIdentity _ = fail "identity must be a string or integer"

jsonType :: (Aeson.FromJSON a, Aeson.ToJSON a) => Text -> [Text] -> OptionType a
jsonType kind choices = OptionType
  { kind
  , choices
  , encode = Aeson.toJSON
  , decode = Aeson.parseJSON
  , render = TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode
  , configured = const True
  , isSecret = False
  }

secretState :: Secret -> Aeson.Value
secretState (Secret value) = Aeson.String $ if Text.null value then "unset" else "configured"

option
  :: [Text]
  -> Text
  -> Text
  -> Text
  -> OptionType a
  -> a
  -> Aeson.Value
  -> (source -> a)
  -> (runtime -> a)
  -> ConfigOption source runtime
option path label description owner optionType defaultValue constraints sourceValue effectiveValue =
  ConfigOption
    { path
    , label
    , description
    , owner
    , optionType
    , required = False
    , defaultValue = Just defaultValue
    , constraints
    , sourceValue = Just . sourceValue
    , effectiveValue = Just . effectiveValue
    }

optionalOption
  :: [Text]
  -> Text
  -> Text
  -> Text
  -> OptionType a
  -> Bool
  -> Aeson.Value
  -> (source -> Maybe a)
  -> (runtime -> Maybe a)
  -> ConfigOption source runtime
optionalOption path label description owner optionType required constraints sourceValue effectiveValue =
  ConfigOption
    { path
    , label
    , description
    , owner
    , optionType
    , required
    , defaultValue = Nothing
    , constraints
    , sourceValue
    , effectiveValue
    }

schemaFromValue :: ConfigSchema source runtime -> TomlValue.Value' l -> Matcher.Matcher l source
schemaFromValue ConfigSchema{parser} = parser

prefixOptions :: [Text] -> [ConfigOption source runtime] -> [ConfigOption source runtime]
prefixOptions prefix = map \ConfigOption{path, label, description, owner, optionType, required, defaultValue, constraints, sourceValue, effectiveValue} ->
  ConfigOption
    { path = prefix <> path
    , label
    , description
    , owner
    , optionType
    , required
    , defaultValue
    , constraints
    , sourceValue
    , effectiveValue
    }

mapOptions
  :: (outerSource -> source)
  -> (outerRuntime -> runtime)
  -> [ConfigOption source runtime]
  -> [ConfigOption outerSource outerRuntime]
mapOptions sourceMap runtimeMap = map \ConfigOption{path, label, description, owner, optionType, required, defaultValue, constraints, sourceValue, effectiveValue} ->
  ConfigOption
    { path
    , label
    , description
    , owner
    , optionType
    , required
    , defaultValue
    , constraints
    , sourceValue = sourceValue . sourceMap
    , effectiveValue = effectiveValue . runtimeMap
    }

mapMaybeOptions
  :: (outerSource -> Maybe source)
  -> (outerRuntime -> Maybe runtime)
  -> [ConfigOption source runtime]
  -> [ConfigOption outerSource outerRuntime]
mapMaybeOptions sourceMap runtimeMap = map \ConfigOption{path, label, description, owner, optionType, required, defaultValue, constraints, sourceValue, effectiveValue} ->
  ConfigOption
    { path
    , label
    , description
    , owner
    , optionType
    , required
    , defaultValue
    , constraints
    , sourceValue = sourceMap >=> sourceValue
    , effectiveValue = runtimeMap >=> effectiveValue
    }

optionPath :: ConfigOption source runtime -> [Text]
optionPath ConfigOption{path} = path

optionIsSecret :: ConfigOption source runtime -> Bool
optionIsSecret ConfigOption{optionType} = optionType.isSecret

optionIsRequired :: ConfigOption source runtime -> Bool
optionIsRequired ConfigOption{required} = required

optionToml :: ConfigOption source runtime -> Aeson.Value -> Either String Text
optionToml ConfigOption{optionType} value =
  AesonTypes.parseEither optionType.decode value <&> optionType.render

decodeOptionValue :: ConfigOption source runtime -> Aeson.Value -> Either String Aeson.Value
decodeOptionValue ConfigOption{optionType} value =
  AesonTypes.parseEither optionType.decode value <&> optionType.encode

inspectOptions
  :: ([Text] -> Bool)
  -> source
  -> runtime
  -> [ConfigOption source runtime]
  -> [Aeson.Value]
inspectOptions present source runtime = map inspect
  where
    inspect ConfigOption{path, label, description, owner, optionType, required, defaultValue, constraints, sourceValue, effectiveValue} =
      let sourcePresent = present path
          public value = if optionType.isSecret
            then Aeson.String (if optionType.configured value then "configured" else "unset")
            else optionType.encode value
          sourcePublic = sourceValue source <&> public
          effectivePublic = effectiveValue runtime <&> public
          defaultPublic = defaultValue <&> public
      in Aeson.object
        [ "path" Aeson..= path
        , "label" Aeson..= label
        , "description" Aeson..= description
        , "owner" Aeson..= owner
        , "type" Aeson..= Aeson.object
            (["kind" Aeson..= optionType.kind] <> ["values" Aeson..= optionType.choices | not (null optionType.choices)])
        , "required" Aeson..= required
        , "default" Aeson..= defaultPublic
        , "constraints" Aeson..= constraints
        , "activation" Aeson..= ("restart" :: Text)
        , "source" Aeson..= Aeson.object
            [ "present" Aeson..= sourcePresent
            , "value" Aeson..= if sourcePresent then sourcePublic else Nothing
            ]
        , "effective" Aeson..= (effectivePublic <|> defaultPublic)
        ]
