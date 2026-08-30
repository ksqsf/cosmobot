{-|
Module      : Bot.Memory.Config
Description : Memory file configuration
Stability   : experimental
-}

module Bot.Memory.Config
  ( FileConfig (..)
  , defaultFileConfig
  , toMemoryConfig
  , schema
  )
where

import qualified Bot.Memory as Memory
import qualified Bot.Config.Schema as Schema
import Bot.Prelude
import qualified Data.Aeson as Aeson
import Toml.Schema

newtype FileConfig = FileConfig
  { dir :: FilePath
  }
  deriving (Show)

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { dir = "memory"
  }

schema :: Schema.ConfigSchema FileConfig Memory.MemoryConfig
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue do
    dir <- fromMaybe defaultFileConfig.dir <$> optKey "dir"
    pure FileConfig{dir}
  , Schema.options =
      [ Schema.option ["dir"] "Directory" "Persistent memory directory." "Bot.Memory.Config" Schema.text (toText defaultFileConfig.dir) Aeson.Null (toText . (.dir)) (toText . (.dir))
      ]
  , Schema.sections = [Schema.section [] "Memory" ["data"] "Data"]
  , Schema.repeatableSections = []
  }

instance FromValue FileConfig where
  fromValue = Schema.schemaFromValue schema

toMemoryConfig :: FileConfig -> Memory.MemoryConfig
toMemoryConfig cfg =
  Memory.MemoryConfig
    { dir = cfg.dir
    }
