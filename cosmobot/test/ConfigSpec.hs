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
import qualified Data.Aeson.Types as AesonTypes
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
      , testCase "advertised numeric constraints are enforced by owner parsers" testAdvertisedNumericConstraints
      , testCase "plugin directory resolves beside config" testPluginDirectory
      , testCase "inspection and Show redact secrets" testSecretRedaction
      , testCase "inspection uses owner-defined section metadata" testInspectionSectionMetadata
      , testCase "inspection separates current source from active runtime" testInspectionActiveRuntime
      , testCase "invalid secret diagnostics redact source values" testSecretDiagnosticRedaction
      , testCase "scalar edits preserve comments and surrounding bytes" testScalarEditPreservesComments
      , testCase "multiline edits preserve bytes outside the changed value" testMultilineEditPreservesOutsideBytes
      , testCase "multiline closing quote runs preserve following bytes" testMultilineClosingQuoteRuns
      , testCase "insertions preserve CRLF newlines" testEditPreservesCrLf
      , testCase "dotted and quoted keys resolve by path segments" testDottedAndQuotedKeys
      , testCase "mixed identity lists preserve integer and text identities" testMixedIdentityList
      , testCase "named providers quote arbitrary names" testNamedProviderInsertion
      , testCase "provider removal preserves following sections" testProviderRemoval
      , testCase "conflicting edits are rejected atomically" testConflictingEdits
      , testCase "inline tables are rejected without reformatting" testInlineTableRejected
      , testCase "unsupported section source shapes are rejected" testUnsupportedSectionShapes
      , testCase "semantic diffs contain values and redact replaced secrets" testSemanticDiff
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

testAdvertisedNumericConstraints :: IO ()
testAdvertisedNumericConstraints =
  for_ cases \(expected, source) -> assertConfigFailureContains expected source
  where
    cases =
      [ ("rpc.port must be between 1 and 65535", minimalConfig <> "\n[rpc]\nport = 0\n")
      , ("acp.port must be between 1 and 65535", minimalConfig <> "\n[acp]\nport = 65536\n")
      , ("media.compression_level must be between 0 and 100", minimalConfig <> "\n[media]\ncompression_level = 101\n")
      , ("media.gc.older_than_days must not be negative", minimalConfig <> "\n[media.gc]\nolder_than_days = -1\n")
      , ("media.gc.interval_hours must be positive", minimalConfig <> "\n[media.gc]\ninterval_hours = 0\n")
      , ("handler.ask.agent_max_turns must be positive", Text.replace "command = \"!ask\"" "command = \"!ask\"\nagent_max_turns = 0" minimalConfig)
      , ("handler.console.agent_max_turns must be positive", Text.replace "system_prompt = \"You are a coding agent.\"" "system_prompt = \"You are a coding agent.\"\nagent_max_turns = 0" minimalConfig)
      , ("tool.web_fetch.max_uses must be positive", minimalConfig <> "\n[tool.web_fetch]\nmax_uses = 0\n")
      , ("tool.web_fetch.max_content_tokens must be positive", minimalConfig <> "\n[tool.web_fetch]\nmax_content_tokens = 0\n")
      , ("tool.web_search.max_results must be positive", minimalConfig <> "\n[tool.web_search]\nmax_results = 0\n")
      ]

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
  let inspected = TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode $ Config.configDocumentInspection document document
  assertBool "inspection leaked a credential" (not (sentinel `Text.isInfixOf` inspected))
  assertBool "BotConfig Show leaked a credential" (not (sentinel `Text.isInfixOf` toText (show (Config.configDocumentRuntime document) :: String)))
  for_ [ ConfigEdit.ReplaceSecret ["rpc", "token"] sentinel
       , ConfigEdit.SetOption ["rpc", "token"] (Aeson.String sentinel)
       ] \change ->
    assertBool "ConfigChange Show leaked a credential" (not (sentinel `Text.isInfixOf` toText (show change :: String)))

testInspectionSectionMetadata :: IO ()
testInspectionSectionMetadata = do
  document <- parseDocument minimalConfig
  sections <- either assertFailure pure (AesonTypes.parseEither parseSectionMetadata (Config.configDocumentInspection document document))
  let expected =
        [ (["acp"], "ACP", ["interfaces"], "Interfaces")
        , (["rpc"], "RPC", ["interfaces"], "Interfaces")
        , (["driver", "qq"], "QQ", ["drivers"], "Chat drivers")
        , (["llm"], "General", ["llm"], "LLM")
        , (["media", "gc"], "GC", ["media"], "Media")
        , (["media", "s3"], "S3", ["media"], "Media")
        , (["handler", "saucenao"], "SauceNAO", ["handlers"], "Handlers")
        ]
  for_ expected \entry@(path, _, _, _) ->
    assertBool [i|missing exact section metadata for #{path}|] (entry `elem` sections)
  repeatables <- either assertFailure pure (AesonTypes.parseEither parseRepeatableMetadata (Config.configDocumentInspection document document))
  repeatables @?=
    [ (["llm", "chat_provider"], "Chat providers", ["llm"], "LLM")
    , (["llm", "image_provider"], "Image providers", ["llm"], "LLM")
    , (["llm", "audio_provider"], "Audio providers", ["llm"], "LLM")
    ]

testInspectionActiveRuntime :: IO ()
testInspectionActiveRuntime = do
  active <- parseDocument (providerConfig [("existing", "active-model"), ("removed", "removed-model")])
  current <- parseDocument (providerConfig [("existing", "source-model"), ("added", "added-model")])
  let inspection = Config.configDocumentInspection current active
  optionSnapshot ["llm", "chat_provider", "existing", "model"] inspection
    @?= (True, Just (Aeson.String "source-model"), Aeson.String "active-model")
  optionSnapshot ["llm", "chat_provider", "added", "model"] inspection
    @?= (True, Just (Aeson.String "added-model"), Aeson.Null)
  optionSnapshot ["llm", "chat_provider", "removed", "model"] inspection
    @?= (False, Nothing, Aeson.String "removed-model")
  optionSnapshot ["driver", "discord", "gateway_host"] inspection
    @?= (False, Nothing, Aeson.Null)
  sectionSnapshot ["driver", "discord"] inspection @?= (True, False)

parseSectionMetadata :: Aeson.Value -> AesonTypes.Parser [([Text], Text, [Text], Text)]
parseSectionMetadata = Aeson.withObject "configuration inspection" \root -> do
  sections <- root Aeson..: "sections"
  traverse parseMetadata sections
  where
    parseMetadata = Aeson.withObject "configuration section" \sectionObject -> do
      path <- sectionObject Aeson..: "path"
      label <- sectionObject Aeson..: "label"
      (groupPath, groupLabel) <- sectionObject Aeson..: "group" >>= parseGroup
      pure (path, label, groupPath, groupLabel)

parseRepeatableMetadata :: Aeson.Value -> AesonTypes.Parser [([Text], Text, [Text], Text)]
parseRepeatableMetadata = Aeson.withObject "configuration inspection" \root -> do
  sections <- root Aeson..: "repeatableSections"
  traverse parseMetadata sections
  where
    parseMetadata = Aeson.withObject "repeatable configuration section" \sectionObject -> do
      path <- sectionObject Aeson..: "path"
      label <- sectionObject Aeson..: "label"
      (groupPath, groupLabel) <- sectionObject Aeson..: "group" >>= parseGroup
      pure (path, label, groupPath, groupLabel)

parseGroup :: Aeson.Value -> AesonTypes.Parser ([Text], Text)
parseGroup = Aeson.withObject "configuration group" \groupObject ->
  (,) <$> groupObject Aeson..: "path" <*> groupObject Aeson..: "label"

sectionSnapshot :: [Text] -> Aeson.Value -> (Bool, Bool)
sectionSnapshot target inspection =
  either (error . toText) id (AesonTypes.parseEither parse inspection)
  where
    parse = Aeson.withObject "configuration inspection" \root -> do
      sections <- (root Aeson..: "sections" :: AesonTypes.Parser [Aeson.Value])
      parsed <- traverse (Aeson.withObject "configuration section" \sectionObject ->
        (,,) <$> sectionObject Aeson..: "path" <*> sectionObject Aeson..: "optional" <*> sectionObject Aeson..: "present") sections
      maybe (fail [i|missing configuration section #{target}|]) (pure . \(_, sectionOptional, present) -> (sectionOptional, present)) $
        find (\(path, _, _) -> path == target) parsed

optionSnapshot :: [Text] -> Aeson.Value -> (Bool, Maybe Aeson.Value, Aeson.Value)
optionSnapshot target inspection =
  either (error . toText) id (AesonTypes.parseEither parse inspection)
  where
    parse = Aeson.withObject "configuration inspection" \root -> do
      sections <- (root Aeson..: "sections" :: AesonTypes.Parser [Aeson.Value])
      options <- concat <$> traverse (Aeson.withObject "configuration section" (Aeson..: "options")) sections
      parsed <- traverse parseOption options
      maybe (fail [i|missing configuration option #{target}|]) pure (find (\(path, _, _, _) -> path == target) parsed)
        <&> \(_, present, sourceValue, effective) -> (present, sourceValue, effective)
    parseOption = Aeson.withObject "configuration option" \optionObject -> do
      path <- optionObject Aeson..: "path"
      (present, sourceValue) <- optionObject Aeson..: "source" >>= Aeson.withObject "configuration source" \sourceObject ->
        (,) <$> sourceObject Aeson..: "present" <*> sourceObject Aeson..:? "value"
      effective <- optionObject Aeson..: "effective"
      pure (path, present, sourceValue, effective)

providerConfig :: [(Text, Text)] -> Text
providerConfig providers =
  Text.replace "\n[handler.console]" (providerTables <> "\n[handler.console]") minimalConfig
  where
    providerTables = foldMap (\(name, model) ->
      [i|\n[llm.chat_provider.#{name}]\nmodel = "#{model}"\n|]) providers

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

testMultilineEditPreservesOutsideBytes :: IO ()
testMultilineEditPreservesOutsideBytes = do
  let multiline = "system_prompt = \"\"\"\nYou are\ncosmobot.\n\"\"\"  # preserved after value"
      source = "# preserved before\n" <> Text.replace "system_prompt = \"You are cosmobot.\"" multiline minimalConfig <> "# preserved after\n"
  document <- parseDocument source
  changed <- applyChanges document [ConfigEdit.SetOption ["handler", "ask", "system_prompt"] (Aeson.String "Replacement")]
  assertBool "prefix outside multiline value changed" ("# preserved before\n" `Text.isPrefixOf` changed)
  assertBool "suffix outside multiline value changed" ("  # preserved after value\n# preserved after\n" `Text.isSuffixOf` changed)
  assertBool "multiline value was not replaced" ("system_prompt = \"Replacement\"" `Text.isInfixOf` changed)

testMultilineClosingQuoteRuns :: IO ()
testMultilineClosingQuoteRuns =
  for_ values \value -> do
    let source = Text.replace
          "system_prompt = \"You are cosmobot.\""
          ("system_prompt = " <> value)
          minimalConfig
          <> "\n[plugins]\nplugin_dir = \"extensions\"\n"
    document <- parseDocument source
    changed <- applyChanges document [ConfigEdit.SetOption ["handler", "ask", "system_prompt"] (Aeson.String "Replacement")]
    assertBool "replacement consumed following TOML" ("[plugins]\nplugin_dir = \"extensions\"\n" `Text.isSuffixOf` changed)
    void (parseDocument changed)
  where
    values =
      [ "\"\"\"\nends in one quote\"\"\"\""
      , "\"\"\"\nends in two quotes\"\"\"\"\""
      , "'''\nends in one quote''''"
      , "'''\nends in two quotes'''''"
      ]

testDottedAndQuotedKeys :: IO ()
testDottedAndQuotedKeys = do
  let source = Text.unlines
        [ "[llm]"
        , ""
        , "[llm.chat_provider.\"provider.with.dot\"]"
        , "model = \"old/model\""
        , ""
        , "[handler.console]"
        , "system_prompt = \"You are a coding agent.\""
        , ""
        , "[handler]"
        , "ask.command = \"!ask\"  # dotted key"
        , "ask.system_prompt = \"You are cosmobot.\""
        ]
  document <- parseDocument source
  changed <- applyChanges document
    [ ConfigEdit.SetOption ["handler", "ask", "command"] (Aeson.String "!chat")
    , ConfigEdit.SetOption ["llm", "chat_provider", "provider.with.dot", "model"] (Aeson.String "new/model")
    ]
  assertBool "dotted assignment was not replaced" ("ask.command = \"!chat\"  # dotted key" `Text.isInfixOf` changed)
  assertBool "quoted provider was not replaced" ("model = \"new/model\"" `Text.isInfixOf` changed)
  void (parseDocument changed)

testMixedIdentityList :: IO ()
testMixedIdentityList = do
  let source = minimalConfig <> "\n[driver.telegram]\nbot_token = \"token\"\nallowed_chats = [1, \"room\"]\n"
      identities = Aeson.Array (fromList [Aeson.Number 2, Aeson.String "other"])
  document <- parseDocument source
  changed <- applyChanges document [ConfigEdit.SetOption ["driver", "telegram", "allowed_chats"] identities]
  assertBool "mixed identity list was not rendered" ("allowed_chats = [2,\"other\"]" `Text.isInfixOf` changed)
  void (parseDocument changed)

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

testConflictingEdits :: IO ()
testConflictingEdits = do
  document <- parseDocument minimalConfig
  let path = ["handler", "ask", "command"]
  for_ [ [ConfigEdit.SetOption path (Aeson.String "!chat"), ConfigEdit.RemoveOption path]
       , [ConfigEdit.RemoveSection ["llm", "chat_provider", "new"], ConfigEdit.SetOption ["llm", "chat_provider", "new", "model"] (Aeson.String "model")]
       ] \changes ->
    case ConfigEdit.applyConfigChanges document changes of
      Left err -> err.code @?= "invalid_change"
      Right _ -> assertFailure "expected conflicting changes to be rejected"

testInlineTableRejected :: IO ()
testInlineTableRejected = do
  document <- parseDocument ("storage = { sqlite_path = \"state.sqlite3\" }\n" <> minimalConfig)
  case ConfigEdit.applyConfigChanges document [ConfigEdit.SetOption ["storage", "sqlite_path"] (Aeson.String "new.sqlite3")] of
    Left err -> err.code @?= "unsupported_source_shape"
    Right _ -> assertFailure "expected inline-table edit to be rejected"

testUnsupportedSectionShapes :: IO ()
testUnsupportedSectionShapes = do
  dotted <- parseDocument (Text.replace "[llm]\n" "[llm]\nchat_provider.temporary.model = \"model\"\n" minimalConfig)
  for_ [ ConfigEdit.AddSection ["llm", "chat_provider", "temporary"]
       , ConfigEdit.RemoveSection ["llm", "chat_provider", "temporary"]
       ] (assertUnsupported dotted)
  separated <- parseDocument $ minimalConfig <> Text.unlines
    [ ""
    , "[media]"
    , "cache_dir = \"cache\""
    , ""
    , "[plugins]"
    , "plugin_dir = \"plugins\""
    , ""
    , "[media.gc]"
    , "enabled = true"
    ]
  assertUnsupported separated (ConfigEdit.RemoveSection ["media"])
  where
    assertUnsupported document change =
      case ConfigEdit.applyConfigChanges document [change] of
        Left err -> err.code @?= "unsupported_source_shape"
        Right _ -> assertFailure "expected unsupported section source shape"

testSemanticDiff :: IO ()
testSemanticDiff = do
  let sentinel = "semantic-diff-secret"
      source = "[rpc]\ntoken = \"old-secret\"\n\n" <> minimalConfig
      changes =
        [ ConfigEdit.SetOption ["handler", "ask", "command"] (Aeson.String "!chat")
        , ConfigEdit.ReplaceSecret ["rpc", "token"] sentinel
        ]
  before <- parseDocument source
  changedSource <- applyChanges before changes
  afterDocument <- parseDocument changedSource
  let encoded = TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode $ ConfigEdit.semanticDiff changes before afterDocument
  assertBool "semantic diff omitted old value" ("\"before\":\"!ask\"" `Text.isInfixOf` encoded)
  assertBool "semantic diff omitted new value" ("\"after\":\"!chat\"" `Text.isInfixOf` encoded)
  assertBool "semantic diff returned option metadata" (not ("\"label\"" `Text.isInfixOf` encoded))
  assertBool "semantic diff omitted secret replacement" ("\"rpc\",\"token\"" `Text.isInfixOf` encoded)
  assertBool "semantic diff leaked a secret" (not (sentinel `Text.isInfixOf` encoded))

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
