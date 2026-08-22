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
import Bot.Agent.Types (ContextStrategy (..))
import Bot.Core.Message (ChatPlatform)
import Toml.Schema

-- | Identity, command, and prompt settings for the ask handler.
data AskHandlerConfig = AskHandlerConfig
  { name             :: !(Maybe Text)
  , command          :: !Text
  , drawCommand      :: !Text
  , systemPrompt     :: !Text
  , agentMaxTurns    :: !Int
  , contextStrategy  :: !ContextStrategy
  , contextCompactionThresholdKTokens :: !Int
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
    contextStrategyText <- fromMaybe ("compaction" :: Text) <$> optKey "context_strategy"
    contextStrategy <- case contextStrategyText of
      "compaction" -> pure ContextCompaction
      "recursive_transcript" -> pure RecursiveTranscript
      _ -> fail "handler.ask.context_strategy must be compaction or recursive_transcript"
    contextCompactionThresholdKTokens <- fromMaybe 1000 <$> optKey "context_compaction_threshold_ktokens"
    when (contextCompactionThresholdKTokens <= 0) do
      fail "handler.ask.context_compaction_threshold_ktokens must be positive"
    pure AskHandlerConfig
      { name = name
      , command = command
      , drawCommand = drawCommand
      , systemPrompt = systemPrompt
      , agentMaxTurns = agentMaxTurns
      , contextStrategy = contextStrategy
      , contextCompactionThresholdKTokens = contextCompactionThresholdKTokens
      , botIds = []
      }
