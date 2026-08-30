{-|
Module      : Bot.Skills.Config
Description : Skills file configuration
Stability   : experimental
-}

module Bot.Skills.Config
  ( FileConfig (..)
  , defaultFileConfig
  , toSkillsConfig
  , schema
  )
where

import Bot.Prelude
import qualified Bot.Config.Schema as Schema
import qualified Bot.Skills as Skills
import qualified Data.Aeson as Aeson
import Toml.Schema

newtype FileConfig = FileConfig
  { dir :: FilePath
  }
  deriving (Show)

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { dir = "skills"
  }

schema :: Schema.ConfigSchema FileConfig Skills.SkillsConfig
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue do
    dir <- fromMaybe defaultFileConfig.dir <$> optKey "dir"
    pure FileConfig{dir}
  , Schema.options =
      [ Schema.option ["dir"] "Directory" "Skills directory." "Bot.Skills.Config" Schema.text (toText defaultFileConfig.dir) Aeson.Null (toText . (.dir)) (toText . (.dir))
      ]
  , Schema.sections = [Schema.section [] "Skills" ["data"] "Data"]
  , Schema.repeatableSections = []
  }

instance FromValue FileConfig where
  fromValue = Schema.schemaFromValue schema

toSkillsConfig :: FileConfig -> Skills.SkillsConfig
toSkillsConfig cfg =
  Skills.SkillsConfig
    { dir = cfg.dir
    }
