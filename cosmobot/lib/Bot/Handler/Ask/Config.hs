{-|
Module      : Bot.Handler.Ask.Config
Description : Ask handler configuration
Stability   : experimental
-}

module Bot.Handler.Ask.Config
  ( AskHandlerConfig (..)
  , schema
  )
where

import Bot.Prelude
import Bot.Agent.Types (ContextStrategy (..))
import qualified Bot.Config.Schema as Schema
import Bot.Core.Message (ChatPlatform)
import qualified Data.Aeson as Aeson
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

schema :: Schema.ConfigSchema AskHandlerConfig AskHandlerConfig
schema = Schema.ConfigSchema
  { Schema.parser = parseTableFromValue do
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
  , Schema.options =
      [ Schema.optionalOption ["name"] "Name" "Optional displayed bot name." owner Schema.text False Aeson.Null (.name) (.name)
      , Schema.optionalOption ["command"] "Command" "Command that starts an ask conversation." owner Schema.text True Aeson.Null (Just . (.command)) (Just . (.command))
      , Schema.option ["draw_command"] "Draw command" "Command that starts image generation." owner Schema.text "!draw" Aeson.Null (.drawCommand) (.drawCommand)
      , Schema.optionalOption ["system_prompt"] "System prompt" "System instructions for ask conversations." owner Schema.text True Aeson.Null (Just . (.systemPrompt)) (Just . (.systemPrompt))
      , Schema.option ["agent_max_turns"] "Maximum turns" "Maximum model/tool turns per run." owner Schema.integer (4 :: Int) (Aeson.object ["minimum" Aeson..= (1 :: Int)]) (.agentMaxTurns) (.agentMaxTurns)
      , Schema.option ["context_strategy"] "Context strategy" "Transcript context strategy." owner (Schema.enum ["compaction", "recursive_transcript"]) "compaction" Aeson.Null (renderContextStrategy . (.contextStrategy)) (renderContextStrategy . (.contextStrategy))
      , Schema.option ["context_compaction_threshold_ktokens"] "Compaction threshold" "Context threshold in thousands of tokens." owner Schema.integer (1000 :: Int) (Aeson.object ["minimum" Aeson..= (1 :: Int)]) (.contextCompactionThresholdKTokens) (.contextCompactionThresholdKTokens)
      ]
  }
  where owner = "Bot.Handler.Ask.Config"

instance FromValue AskHandlerConfig where
  fromValue = Schema.schemaFromValue schema

renderContextStrategy :: ContextStrategy -> Text
renderContextStrategy = \case
  ContextCompaction -> "compaction"
  RecursiveTranscript -> "recursive_transcript"
