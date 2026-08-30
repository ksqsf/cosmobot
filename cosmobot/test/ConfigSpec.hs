module Main (main) where

import qualified Bot.Config as Config
import qualified Bot.Config.Edit as ConfigEdit
import qualified Bot.Agent.Types as Agent
import qualified Bot.ACP.Config as ACPConfig
import Bot.Chat.Driver.Telegram (Config (..))
import Bot.Core.Message (ChatPlatform (..))
import qualified Bot.RPC.Config as RPCConfig
import Bot.Prelude
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TextEncoding
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Toml.Syntax.Parser as TomlParser
import qualified Toml.Syntax.Types as TomlSyntax

main :: IO ()
main =
  defaultMain $
    testGroup "config"
      [ testCase "drivers table may be omitted" testDriversTableMayBeOmitted
      , testCase "configured telegram driver is enabled alone" testConfiguredTelegramDriverEnabledAlone
      , testCase "incomplete matrix and discord driver tables are disabled" testIncompleteMatrixAndDiscordDisabled
      , testCase "ask context compaction threshold uses ktokens" testAskCompactionThresholdUsesKTokens
      , testCase "ask recursive transcript strategy is configurable" testAskRecursiveTranscriptStrategy
      , testCase "sandbox image is configurable" testSandboxImage
      , testCase "Exa web search is configurable" testExaWebSearch
      , testCase "Python tool defaults are safe and enabled" testPythonDefaults
      , testCase "Python tool limits are configurable" testPythonConfig
      , testCase "Python tool bounds must be positive" testPythonPositiveBounds
      , testCase "Python tool converted bounds reject overflow" testPythonOverflow
      , testCase "Python wall timeout is at most one hour" testPythonWallTimeoutBound
      , testCase "Python tool rejects unknown keys" testPythonUnknownKey
      , testCase "plugin directory resolves beside config" testPluginDirectory
      , testCase "inspection and Show redact secrets" testSecretRedaction
      , testCase "invalid secret diagnostics redact source values" testSecretDiagnosticRedaction
      , testCase "scalar edits preserve comments and surrounding bytes" testScalarEditPreservesComments
      , testCase "insertions preserve CRLF newlines" testEditPreservesCrLf
      , testCase "named providers quote arbitrary names" testNamedProviderInsertion
      , testCase "provider removal preserves following sections" testProviderRemoval
      , testCase "inline tables are rejected without reformatting" testInlineTableRejected
      , testCase "every example assignment has one typed option" testExampleOptionCoverage
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

testAskRecursiveTranscriptStrategy :: IO ()
testAskRecursiveTranscriptStrategy = do
  cfg <- loadConfigText $
    minimalConfig
      <> Text.unlines
        [ "context_strategy = \"recursive_transcript\""
        ]
  cfg.handlers.ask.contextStrategy @?= Agent.RecursiveTranscript

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

testExaWebSearch :: IO ()
testExaWebSearch = do
  cfg <- loadConfigText $
    minimalConfig
      <> Text.unlines
        [ ""
        , "[tool.web_search]"
        , "api = \"exa\""
        , "exa_api_key = \"exa-test-key\""
        ]
  let Agent.ToolConfig{webSearchApi, exaApiKey} = cfg.tool
  webSearchApi @?= Agent.WebSearchExa
  exaApiKey @?= Just "exa-test-key"

testPythonDefaults :: IO ()
testPythonDefaults = do
  cfg <- loadConfigText minimalConfig
  cfg.tool.python @?= Agent.defaultPythonConfig

testPythonConfig :: IO ()
testPythonConfig = do
  cfg <- loadConfigText $
    minimalConfig
      <> Text.unlines
        [ ""
        , "[tool.python]"
        , "enable = false"
        , "wall_timeout_seconds = 45"
        , "cpu_seconds = 25"
        , "memory_mib = 768"
        , "max_tool_calls = 80"
        ]
  cfg.tool.python @?= Agent.PythonConfig
    { enabled = False
    , wallTimeoutSeconds = 45
    , cpuSeconds = 25
    , memoryMiB = 768
    , maxToolCalls = 80
    }

testPythonPositiveBounds :: IO ()
testPythonPositiveBounds =
  for_ ["wall_timeout_seconds", "cpu_seconds", "memory_mib", "max_tool_calls"] \key ->
    assertConfigFailureContains [i|tool.python.#{key} must be positive|] $
      pythonConfig [key <> " = 0"]

testPythonOverflow :: IO ()
testPythonOverflow =
  assertConfigFailureContains "tool.python.memory_mib is too large" $
    pythonConfig [[i|memory_mib = #{maxBound `div` (1024 * 1024) + 1 :: Int}|]]

testPythonWallTimeoutBound :: IO ()
testPythonWallTimeoutBound = do
  cfg <- loadConfigText (pythonConfig ["wall_timeout_seconds = 3600"])
  cfg.tool.python.wallTimeoutSeconds @?= 3600
  assertConfigFailureContains "tool.python.wall_timeout_seconds must not exceed 3600" $
    pythonConfig ["wall_timeout_seconds = 3601"]

testPythonUnknownKey :: IO ()
testPythonUnknownKey =
  assertConfigFailureContains "unknown tool.python keys: timeout" $
    pythonConfig ["timeout = 30"]

testPluginDirectory :: IO ()
testPluginDirectory =
  withSystemTempDirectory "cosmobot-config-spec-" \dir -> do
    let path = dir </> "config.toml"
        source = minimalConfig <> "\n[plugins]\nplugin_dir = \"extensions\"\n"
    TextIO.writeFile path source
    cfg <- runEff . runFailIO $ Config.loadConfig path
    cfg.plugins.pluginDir @?= dir </> "extensions"

testSecretRedaction :: IO ()
testSecretRedaction = do
  let sentinel = "sentinel-config-secret"
      source = "[rpc]\ntoken = \"" <> sentinel <> "\"\n\n" <> minimalConfig
  document <- parseDocument source
  let inspected = TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode $ Config.configDocumentInspection document
  assertBool "inspection leaked a credential" (not (sentinel `Text.isInfixOf` inspected))
  assertBool "BotConfig Show leaked a credential" (not (sentinel `Text.isInfixOf` toText (show (Config.configDocumentRuntime document) :: String)))
  for_ [ ConfigEdit.ReplaceSecret ["rpc", "token"] sentinel
       , ConfigEdit.SetOption ["rpc", "token"] (Aeson.String sentinel)
       ] \change ->
    assertBool "ConfigChange Show leaked a credential" (not (sentinel `Text.isInfixOf` toText (show change :: String)))

testSecretDiagnosticRedaction :: IO ()
testSecretDiagnosticRedaction = do
  let sentinel = "sentinel-invalid-secret"
      source = "[rpc]\ntoken = [\"" <> sentinel <> "\"]\n\n" <> minimalConfig
  case Config.parseConfigDocument "private/config.toml" source of
    Left diagnostics -> do
      let encoded = TextEncoding.decodeUtf8 . LazyByteString.toStrict $ Aeson.encode diagnostics
      assertBool "diagnostic leaked a credential" (not (sentinel `Text.isInfixOf` encoded))
    Right _ -> assertFailure "expected invalid secret type"

testScalarEditPreservesComments :: IO ()
testScalarEditPreservesComments = do
  let source = "# Unicode before edited span: 配置\n" <> Text.replace "command = \"!ask\"" "command = \"!ask\"  # keep this comment" minimalConfig
  document <- parseDocument source
  changed <- applyChanges document [ConfigEdit.SetOption ["handler", "ask", "command"] (Aeson.String "!chat")]
  assertBool "replacement lost the comment" ("command = \"!chat\"  # keep this comment" `Text.isInfixOf` changed)
  Text.replace "command = \"!chat\"" "command = \"!ask\"" changed @?= source

testEditPreservesCrLf :: IO ()
testEditPreservesCrLf = do
  let source = Text.replace "\n" "\r\n" minimalConfig
  document <- parseDocument source
  changed <- applyChanges document [ConfigEdit.SetOption ["storage", "sqlite_path"] (Aeson.String "state.sqlite3")]
  assertBool "inserted table did not use CRLF" ("\r\n[storage]\r\nsqlite_path = \"state.sqlite3\"\r\n" `Text.isInfixOf` changed)

testNamedProviderInsertion :: IO ()
testNamedProviderInsertion = do
  document <- parseDocument minimalConfig
  changed <- applyChanges document
    [ ConfigEdit.AddSection ["llm", "chat_provider", "provider with spaces"]
    , ConfigEdit.SetOption ["llm", "chat_provider", "provider with spaces", "model"] (Aeson.String "example/model")
    ]
  assertBool "provider table name was not quoted" ("[llm.chat_provider.\"provider with spaces\"]" `Text.isInfixOf` changed)
  assertBool "provider option was not inserted" ("model = \"example/model\"" `Text.isInfixOf` changed)
  void (parseDocument changed)

testProviderRemoval :: IO ()
testProviderRemoval = do
  let source = minimalConfig <> Text.unlines
        [ ""
        , "[llm.chat_provider.temporary]"
        , "model = \"temporary/model\""
        , ""
        , "[plugins]"
        , "plugin_dir = \"extensions\""
        ]
  document <- parseDocument source
  changed <- applyChanges document [ConfigEdit.RemoveSection ["llm", "chat_provider", "temporary"]]
  assertBool "provider table was not removed" (not ("temporary/model" `Text.isInfixOf` changed))
  assertBool "following section was removed" ("[plugins]\nplugin_dir = \"extensions\"" `Text.isInfixOf` changed)
  void (parseDocument changed)

testInlineTableRejected :: IO ()
testInlineTableRejected = do
  document <- parseDocument ("storage = { sqlite_path = \"state.sqlite3\" }\n" <> minimalConfig)
  case ConfigEdit.applyConfigChanges document [ConfigEdit.SetOption ["storage", "sqlite_path"] (Aeson.String "new.sqlite3")] of
    Left err -> err.code @?= "unsupported_source_shape"
    Right _ -> assertFailure "expected inline-table edit to be rejected"

testExampleOptionCoverage :: IO ()
testExampleOptionCoverage = do
  source <- TextIO.readFile "config.example.toml"
  document <- parseDocument source
  expressions <- either (assertFailure . show) pure (TomlParser.parseRawToml source)
  let values = Config.configDocumentOptionValues document
      assigned = exampleAssignmentPaths expressions
      missing = filter (`Map.notMember` values) assigned
  assertEqual "example assignments missing from the typed schema" [] missing
  assertEqual "typed option paths must be unique" (length (Config.configDocumentOptions document)) (Map.size values)

exampleAssignmentPaths :: [TomlSyntax.Expr annotation] -> [[Text]]
exampleAssignmentPaths = reverse . snd . foldl' step ([], [])
  where
    step (section, paths) = \case
      TomlSyntax.KeyValExpr key _ -> (section, (section <> map snd (toList key)) : paths)
      TomlSyntax.TableExpr key -> (map snd (toList key), paths)
      TomlSyntax.ArrayTableExpr key -> (map snd (toList key), paths)

parseDocument :: Text -> IO Config.ConfigDocument
parseDocument source =
  either (assertFailure . toString . Text.unlines . map (.message)) pure (Config.parseConfigDocument "config.toml" source)

applyChanges :: Config.ConfigDocument -> [ConfigEdit.ConfigChange] -> IO Text
applyChanges document = either (assertFailure . show) pure . ConfigEdit.applyConfigChanges document

pythonConfig :: [Text] -> Text
pythonConfig fields =
  minimalConfig <> Text.unlines ("" : "[tool.python]" : fields)

assertConfigFailureContains :: Text -> Text -> IO ()
assertConfigFailureContains expected source = do
  outcome <- runEff $ trySync (liftIO (loadConfigText source))
  case outcome of
    Left err ->
      assertBool
        [i|expected config error containing #{expected}, got #{displayException err}|]
        (expected `Text.isInfixOf` Text.pack (displayException err))
    Right cfg ->
      assertFailure [i|expected config failure, got #{show cfg :: String}|]

minimalConfig :: Text
minimalConfig =
  Text.unlines
    [ "[llm]"
    , ""
    , "[handler.console]"
    , "system_prompt = \"You are a coding agent.\""
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
