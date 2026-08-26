{-|
Module      : Bot.Handler.Console.Config
Description : Console session handler configuration
Stability   : experimental
-}

module Bot.Handler.Console.Config
  ( ConsoleHandlerConfig (..)
  )
where

import Bot.Agent.Types (ContextStrategy (..))
import Bot.Prelude
import Toml.Schema

data ConsoleHandlerConfig = ConsoleHandlerConfig
  { systemPrompt :: !Text
  , agentMaxTurns :: !Int
  , contextStrategy :: !ContextStrategy
  , contextCompactionThresholdKTokens :: !Int
  }
  deriving (Show)

instance FromValue ConsoleHandlerConfig where
  fromValue = parseTableFromValue do
    systemPrompt <- reqKey "system_prompt"
    agentMaxTurns <- fromMaybe 4 <$> optKey "agent_max_turns"
    contextStrategyText <- fromMaybe ("compaction" :: Text) <$> optKey "context_strategy"
    contextStrategy <- case contextStrategyText of
      "compaction" -> pure ContextCompaction
      "recursive_transcript" -> pure RecursiveTranscript
      _ -> fail "handler.console.context_strategy must be compaction or recursive_transcript"
    contextCompactionThresholdKTokens <- fromMaybe 1000 <$> optKey "context_compaction_threshold_ktokens"
    when (contextCompactionThresholdKTokens <= 0) do
      fail "handler.console.context_compaction_threshold_ktokens must be positive"
    pure ConsoleHandlerConfig
      { systemPrompt
      , agentMaxTurns
      , contextStrategy
      , contextCompactionThresholdKTokens
      }
