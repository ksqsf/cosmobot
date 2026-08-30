{-|
Module      : Bot.Handler.ShutUp.Config
Description : Shut-up handler configuration
Stability   : experimental
-}

module Bot.Handler.ShutUp.Config
  ( ShutUpConfig (..)
  , DeletePattern (..)
  , defaultShutUpConfig
  , schema
  )
where

import Bot.Prelude
import qualified Bot.Config.Schema as Schema
import qualified Data.Aeson as Aeson
import Prelude (Show (..), showString)
import Toml.Schema
import Text.Regex.TDFA
  ( Regex
  , defaultCompOpt
  , defaultExecOpt
  , makeRegexOptsM
  )

-- | Compiled message deletion rule.
data DeletePattern = DeletePattern
  { source :: !Text
  , regex :: !Regex
  }

instance Show DeletePattern where
  showsPrec _ DeletePattern{source} =
    showString [i|DeletePattern #{source}|]

newtype ShutUpConfig = ShutUpConfig
  { deletePatterns :: [DeletePattern]
  }
  deriving (Show)

defaultShutUpConfig :: ShutUpConfig
defaultShutUpConfig = ShutUpConfig
  { deletePatterns = []
  }

schema :: Schema.ConfigSchema ShutUpConfig ShutUpConfig
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue do
    patterns <- fromMaybe [] <$> optKey "delete_patterns"
    deletePatterns <- traverse compileDeletePattern patterns
    pure ShutUpConfig{deletePatterns}
  , Schema.options =
      [ Schema.option ["delete_patterns"] "Delete patterns" "Regular expressions for messages to delete." "Bot.Handler.ShutUp.Config" (Schema.list "text") [] Aeson.Null
          (map (.source) . (.deletePatterns))
          (map (.source) . (.deletePatterns))
      ]
  }

instance FromValue ShutUpConfig where
  fromValue = Schema.schemaFromValue schema

compileDeletePattern :: Text -> ParseTable l DeletePattern
compileDeletePattern source =
  case makeRegexOptsM defaultCompOpt defaultExecOpt source :: Either String Regex of
    Left err ->
      fail [i|Invalid handler.shutup.delete_patterns regex #{source}: #{err}|]
    Right regex ->
      pure DeletePattern{source, regex}
