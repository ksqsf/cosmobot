{-|
Module      : Bot.Handler.Ask.Config
Description : Ask handler configuration
Stability   : experimental
-}

module Bot.Handler.Ask.Config
  ( AskHandlerConfig (..)
  )
where

import Bot.Prelude
import Bot.Core.Message (ChatPlatform)
import Toml.Schema

-- | Identity, command, and prompt settings for the ask handler.
data AskHandlerConfig = AskHandlerConfig
  { name             :: !(Maybe Text)
  , command          :: !Text
  , drawCommand      :: !Text
  , systemPrompt     :: !Text
  , agentMaxTurns    :: !Int
  , botIds           :: ![(ChatPlatform, Text)]
  }
  deriving (Show)

instance FromValue AskHandlerConfig where
  fromValue = parseTableFromValue do
    name <- optKey "name"
    command <- reqKey "command"
    drawCommand <- fromMaybe "!draw" <$> optKey "draw_command"
    systemPrompt <- reqKey "system_prompt"
    agentMaxTurns <- fromMaybe 4 <$> optKey "agent_max_turns"
    pure AskHandlerConfig
      { name = name
      , command = command
      , drawCommand = drawCommand
      , systemPrompt = systemPrompt
      , agentMaxTurns = agentMaxTurns
      , botIds = []
      }
