{-|
Module      : Bot.Handler.Console.Config
Description : Console session handler configuration
Stability   : experimental
-}

module Bot.Handler.Console.Config
  ( ConsoleHandlerConfig (..)
  , schema
  )
where

import Bot.Agent.Types (ContextStrategy (..))
import qualified Bot.Config.Schema as Schema
import Bot.Prelude
import qualified Data.Aeson as Aeson
import Toml.Schema

data ConsoleHandlerConfig = ConsoleHandlerConfig
  { systemPrompt :: !Text
  , agentMaxTurns :: !Int
  , contextStrategy :: !ContextStrategy
  , contextCompactionThresholdKTokens :: !Int
  }
  deriving (Show)

schema :: Schema.ConfigSchema ConsoleHandlerConfig ConsoleHandlerConfig
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue do
    systemPrompt <- reqKey "system_prompt"
    agentMaxTurns <- fromMaybe 4 <$> optKey "agent_max_turns"
    when (agentMaxTurns <= 0) $
      fail "handler.console.agent_max_turns must be positive"
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
  , Schema.options =
      [ Schema.optionalOption ["system_prompt"] "System prompt" "System instructions for RPC console sessions." owner Schema.text True Aeson.Null (Just . (.systemPrompt)) (Just . (.systemPrompt))
      , Schema.option ["agent_max_turns"] "Maximum turns" "Maximum model/tool turns per run." owner Schema.integer (4 :: Int) (Aeson.object ["minimum" Aeson..= (1 :: Int)]) (.agentMaxTurns) (.agentMaxTurns)
      , Schema.option ["context_strategy"] "Context strategy" "Transcript context strategy." owner (Schema.enum ["compaction", "recursive_transcript"]) "compaction" Aeson.Null (renderContextStrategy . (.contextStrategy)) (renderContextStrategy . (.contextStrategy))
      , Schema.option ["context_compaction_threshold_ktokens"] "Compaction threshold" "Context threshold in thousands of tokens." owner Schema.integer (1000 :: Int) (Aeson.object ["minimum" Aeson..= (1 :: Int)]) (.contextCompactionThresholdKTokens) (.contextCompactionThresholdKTokens)
      ]
  , Schema.sections = [Schema.section [] "Console" ["handlers"] "Handlers"]
  , Schema.repeatableSections = []
  }
  where owner = "Bot.Handler.Console.Config"

instance FromValue ConsoleHandlerConfig where
  fromValue = Schema.schemaFromValue schema

renderContextStrategy :: ContextStrategy -> Text
renderContextStrategy = \case
  ContextCompaction -> "compaction"
  RecursiveTranscript -> "recursive_transcript"
