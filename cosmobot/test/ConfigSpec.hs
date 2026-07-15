module Main (main) where

import qualified Bot.Config as Config
import qualified Bot.Agent.Types as Agent
import qualified Bot.ACP.Config as ACPConfig
import Bot.Chat.Driver.Telegram (Config (..))
import Bot.Core.Message (ChatPlatform (..))
import qualified Bot.RPC.Config as RPCConfig
import Bot.Prelude
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "config"
      [ testCase "drivers table may be omitted" testDriversTableMayBeOmitted
      , testCase "configured telegram driver is enabled alone" testConfiguredTelegramDriverEnabledAlone
      , testCase "incomplete matrix and discord driver tables are disabled" testIncompleteMatrixAndDiscordDisabled
      , testCase "ask context compaction threshold uses ktokens" testAskCompactionThresholdUsesKTokens
      , testCase "sandbox image is configurable" testSandboxImage
      ]

testDriversTableMayBeOmitted :: IO ()
testDriversTableMayBeOmitted = do
  cfg <- loadConfigText minimalConfig
  assertBool "expected QQ driver to be disabled" (isNothing cfg.qq)
  assertBool "expected Telegram driver to be disabled" (isNothing cfg.telegram)
  assertBool "expected Matrix driver to be disabled" (isNothing cfg.matrix)
  assertBool "expected Discord driver to be disabled" (isNothing cfg.discord)
  let RPCConfig.Config{enabled = rpcEnabled} = cfg.rpc
  rpcEnabled @?= False
  let ACPConfig.Config{enabled = acpEnabled} = cfg.acp
  acpEnabled @?= False
  let Agent.ToolConfig{sandboxImage} = cfg.tool
  sandboxImage @?= "localhost/cosmobox:latest"

testConfiguredTelegramDriverEnabledAlone :: IO ()
testConfiguredTelegramDriverEnabledAlone = do
  cfg <- loadConfigText $
    minimalConfig
      <> Text.unlines
        [ ""
        , "[driver.telegram]"
        , "bot_token = \"telegram-token\""
        , "bot_id = \"cosmobot\""
        ]
  assertBool "expected QQ driver to be disabled" (isNothing cfg.qq)
  case cfg.telegram of
    Just telegram ->
      telegram.botToken @?= "telegram-token"
    Nothing ->
      assertFailure "expected Telegram driver config"
  assertBool "expected Matrix driver to be disabled" (isNothing cfg.matrix)
  assertBool "expected Discord driver to be disabled" (isNothing cfg.discord)
  cfg.handlers.ask.botIds @?= [(PlatformTelegram, "cosmobot")]

testIncompleteMatrixAndDiscordDisabled :: IO ()
testIncompleteMatrixAndDiscordDisabled = do
  cfg <- loadConfigText $
    minimalConfig
      <> Text.unlines
        [ ""
        , "[driver.matrix]"
        , "homeserver = \"https://matrix.example.test\""
        , "bot_id = \"@bot:matrix.example.test\""
        , ""
        , "[driver.discord]"
        , "bot_id = 424242"
        ]
  assertBool "expected Matrix driver to be disabled" (isNothing cfg.matrix)
  assertBool "expected Discord driver to be disabled" (isNothing cfg.discord)
  cfg.handlers.ask.botIds @?=
    [ (PlatformMatrix, "@bot:matrix.example.test")
    , (PlatformDiscord, "424242")
    ]

testAskCompactionThresholdUsesKTokens :: IO ()
testAskCompactionThresholdUsesKTokens = do
  cfg <- loadConfigText $
    minimalConfig
      <> Text.unlines
        [ "context_compaction_threshold_ktokens = 123"
        ]
  cfg.handlers.ask.contextCompactionThresholdKTokens @?= 123

testSandboxImage :: IO ()
testSandboxImage = do
  cfg <- loadConfigText $
    minimalConfig
      <> Text.unlines
        [ ""
        , "[resource.sandbox]"
        , "image = \"registry.example.test/custom:latest\""
        ]
  let Agent.ToolConfig{sandboxImage} = cfg.tool
  sandboxImage @?= "registry.example.test/custom:latest"

minimalConfig :: Text
minimalConfig =
  Text.unlines
    [ "[llm]"
    , ""
    , "[handler.ask]"
    , "command = \"!ask\""
    , "system_prompt = \"You are cosmobot.\""
    ]

loadConfigText :: Text -> IO Config.BotConfig
loadConfigText source =
  withSystemTempDirectory "cosmobot-config-spec-" \dir -> do
    let path = dir <> "/config.toml"
    TextIO.writeFile path source
    runEff . runFailIO $ Config.loadConfig path
