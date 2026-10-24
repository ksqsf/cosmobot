module Main (main) where

import qualified Bot.LLM.Config as LLMConfig
import Bot.Prelude
import qualified Data.Text as Text
import qualified Toml
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "llm-openai-config"
      [ testCase "native API options and validation" testNativeOptions
      , testCase "selects named chat provider" testSelectsNamedChatProvider
      , testCase "selects named image provider" testSelectsNamedImageProvider
      , testCase "selects named audio provider" testSelectsNamedAudioProvider
      , testCase "blank selectors disable providers" testBlankSelectorsDisableProviders
      , testCase "missing selected provider fails clearly" testMissingSelectedProviderFailsClearly
      , testCase "provider defaults are applied" testProviderDefaultsAreApplied
      , testCase "non-positive chat timeout fails" testNonPositiveChatTimeoutFails
      , testCase "non-positive image timeout fails" testNonPositiveImageTimeoutFails
      , testCase "non-positive audio timeout fails" testNonPositiveAudioTimeoutFails
      ]

testSelectsNamedChatProvider :: IO ()
testSelectsNamedChatProvider = do
  cfg <- parseRuntimeConfig $
    Text.unlines
      [ "chat = \"openrouter\""
      , ""
      , "[chat_provider.openrouter]"
      , "base_url = \"https://openrouter.ai/api/v1\""
      , "api_key = \"chat-key\""
      , "model = \"openai/gpt-5.4-mini\""
      , "reasoning_effort = \"medium\""
      , "timeout = 42"
      ]
  case cfg.chatProvider of
    Just provider -> do
      provider.baseUrl @?= "https://openrouter.ai/api/v1"
      provider.apiKey @?= Just "chat-key"
      provider.model @?= "openai/gpt-5.4-mini"
      provider.reasoningEffort @?= "medium"
      provider.requestTimeout @?= 42
    Nothing ->
      assertFailure "expected selected chat provider"
  cfg.imageProvider @?= Nothing
  cfg.audioProvider @?= Nothing

testSelectsNamedImageProvider :: IO ()
testSelectsNamedImageProvider = do
  cfg <- parseRuntimeConfig $
    Text.unlines
      [ "image = \"openai\""
      , ""
      , "[image_provider.openai]"
      , "base_url = \"https://api.openai.com/v1\""
      , "api_key = \"image-key\""
      , "model = \"gpt-image-1.5\""
      , "can_generate = false"
      , "can_edit = true"
      , "timeout = 180"
      , "quality = \"high\""
      , "size = \"1024x1536\""
      , "aspect_ratio = \"portrait\""
      , "background = \"transparent\""
      , "moderation = \"low\""
      , "output_format = \"webp\""
      ]
  cfg.chatProvider @?= Nothing
  case cfg.imageProvider of
    Just provider -> do
      provider.baseUrl @?= "https://api.openai.com/v1"
      provider.apiKey @?= Just "image-key"
      provider.model @?= "gpt-image-1.5"
      provider.canGenerate @?= False
      provider.canEdit @?= True
      provider.requestTimeout @?= 180
      provider.quality @?= Just "high"
      provider.size @?= Just "1024x1536"
      provider.aspectRatio @?= Just "portrait"
      provider.background @?= Just "transparent"
      provider.moderation @?= Just "low"
      provider.outputFormat @?= Just "webp"
    Nothing ->
      assertFailure "expected selected image provider"
  cfg.audioProvider @?= Nothing

testSelectsNamedAudioProvider :: IO ()
testSelectsNamedAudioProvider = do
  cfg <- parseRuntimeConfig $
    Text.unlines
      [ "audio = \"openai\""
      , ""
      , "[audio_provider.openai]"
      , "base_url = \"https://api.openai.com/v1\""
      , "api_key = \"audio-key\""
      , "model = \"gpt-4o-mini-tts\""
      , "timeout = 120"
      , "voice = \"verse\""
      , "response_format = \"opus\""
      , "speed = 1.25"
      , "instructions = \"Speak warmly.\""
      ]
  cfg.chatProvider @?= Nothing
  cfg.imageProvider @?= Nothing
  case cfg.audioProvider of
    Just provider -> do
      provider.baseUrl @?= "https://api.openai.com/v1"
      provider.apiKey @?= Just "audio-key"
      provider.model @?= "gpt-4o-mini-tts"
      provider.voice @?= "verse"
      provider.responseFormat @?= "opus"
      provider.requestTimeout @?= 120
      provider.speed @?= Just 1.25
      provider.instructions @?= Just "Speak warmly."
    Nothing ->
      assertFailure "expected selected audio provider"

testBlankSelectorsDisableProviders :: IO ()
testBlankSelectorsDisableProviders = do
  cfg <- parseRuntimeConfig $
    Text.unlines
      [ "chat = \"  \""
      , "image = \"\""
      , "audio = \" \""
      , ""
      , "[chat_provider.openrouter]"
      , "api_key = \"chat-key\""
      , ""
      , "[image_provider.openai]"
      , "api_key = \"image-key\""
      , ""
      , "[audio_provider.openai]"
      , "api_key = \"audio-key\""
      ]
  cfg.chatProvider @?= Nothing
  cfg.imageProvider @?= Nothing
  cfg.audioProvider @?= Nothing

testMissingSelectedProviderFailsClearly :: IO ()
testMissingSelectedProviderFailsClearly = do
  case parseRuntimeConfigEither "chat = \"missing\"" of
    Left err ->
      assertBool
        [i|unexpected error: #{err}|]
        ("llm.chat selects missing, but llm.chat_provider.missing is not defined" `Text.isInfixOf` Text.pack err)
    Right cfg ->
      assertFailure [i|expected parse failure, got #{show cfg :: String}|]
  case parseRuntimeConfigEither "audio = \"missing\"" of
    Left err ->
      assertBool
        [i|unexpected error: #{err}|]
        ("llm.audio selects missing, but llm.audio_provider.missing is not defined" `Text.isInfixOf` Text.pack err)
    Right cfg ->
      assertFailure [i|expected parse failure, got #{show cfg :: String}|]

testProviderDefaultsAreApplied :: IO ()
testProviderDefaultsAreApplied = do
  cfg <- parseRuntimeConfig $
    Text.unlines
      [ "chat = \"default_chat\""
      , "image = \"default_image\""
      , "audio = \"default_audio\""
      , ""
      , "[chat_provider.default_chat]"
      , ""
      , "[image_provider.default_image]"
      , ""
      , "[audio_provider.default_audio]"
      ]
  cfg.chatProvider @?= Just LLMConfig.defaultChatProviderConfig
  fmap (.api) cfg.chatProvider @?= Just "chat_completions"
  cfg.imageProvider @?= Just LLMConfig.defaultImageProviderConfig
  cfg.audioProvider @?= Just LLMConfig.defaultAudioProviderConfig

testNonPositiveChatTimeoutFails :: IO ()
testNonPositiveChatTimeoutFails =
  assertParseFailureContains "llm.chat_provider.<name>.timeout must be positive" $
    Text.unlines
      [ "chat = \"bad\""
      , "[chat_provider.bad]"
      , "timeout = 0"
      ]

testNonPositiveImageTimeoutFails :: IO ()
testNonPositiveImageTimeoutFails =
  assertParseFailureContains "llm.image_provider.<name>.timeout must be positive" $
    Text.unlines
      [ "image = \"bad\""
      , "[image_provider.bad]"
      , "timeout = 0"
      ]

testNonPositiveAudioTimeoutFails :: IO ()
testNonPositiveAudioTimeoutFails =
  assertParseFailureContains "llm.audio_provider.<name>.timeout must be positive" $
    Text.unlines
      [ "audio = \"bad\""
      , "[audio_provider.bad]"
      , "timeout = 0"
      ]

parseRuntimeConfig :: Text -> IO LLMConfig.Config
parseRuntimeConfig source =
  either assertFailure pure (parseRuntimeConfigEither source)

parseRuntimeConfigEither :: Text -> Either String LLMConfig.Config
parseRuntimeConfigEither source =
  case Toml.decode source of
    Toml.Failure errors ->
      Left (Text.unpack (Text.unlines (map toText errors)))
    Toml.Success _warnings fileConfig ->
      Right (LLMConfig.toRuntimeConfig fileConfig)

assertParseFailureContains :: Text -> Text -> IO ()
assertParseFailureContains expected source =
  case parseRuntimeConfigEither source of
    Left err ->
      assertBool [i|expected #{expected} in #{err}|] (expected `Text.isInfixOf` Text.pack err)
    Right cfg ->
      assertFailure [i|expected parse failure, got #{show cfg :: String}|]

testNativeOptions :: Assertion
testNativeOptions = do
  let native = "chat = \"claude\"\n[chat_provider.claude]\napi = \"anthropic\"\nbase_url = \"https://api.anthropic.com/v1\"\nmodel = \"claude-opus-5-5\"\n"
  cfg <- parseRuntimeConfig (native <> "thinking = \"adaptive\"\nmax_tokens = 42\nreasoning_effort = \"high\"\n")
  case cfg.chatProvider of
    Just p -> do
      p.api @?= "anthropic"
      p.thinking @?= Just "adaptive"
      p.maxTokens @?= 42
      p.reasoningEffort @?= "high"
    Nothing -> assertFailure "missing native provider"
  proxy <- parseRuntimeConfig (native <> "anthropic_auth = \"bearer\"")
  fmap (.anthropicAuth) proxy.chatProvider @?= Just "bearer"
  assertParseFailureContains "anthropic_auth must be" (native <> "anthropic_auth = \"invalid\"")
  assertParseFailureContains "requires api = anthropic" "[chat_provider.x]\nanthropic_auth = \"bearer\""
  assertParseFailureContains "max_tokens must be positive" (native <> "max_tokens = 0")
  assertParseFailureContains "thinking must be adaptive" (native <> "thinking = \"enabled\"")
  assertParseFailureContains "api must be" "[chat_provider.x]\napi = \"unknown\""
  assertParseFailureContains "thinking must be adaptive" "[chat_provider.x]\nthinking = \"adaptive\""
  assertBool "native base URL and model are required" (isLeft (parseRuntimeConfigEither "[chat_provider.x]\napi = \"anthropic\""))

  let google = Text.replace "anthropic" "gemini" native
  googleCfg <- parseRuntimeConfig (google <> "reasoning_effort = \"\"\n")
  fmap (.api) googleCfg.chatProvider @?= Just "gemini"
  fmap (.reasoningEffort) googleCfg.chatProvider @?= Just ""
  assertParseFailureContains "thinking must be adaptive" (google <> "thinking = \"adaptive\"")
  assertParseFailureContains "requires api = anthropic" (google <> "anthropic_auth = \"bearer\"")

  let responses = Text.replace "anthropic" "responses" native
  responsesCfg <- parseRuntimeConfig (responses <> "reasoning_effort = \"\"\nmax_tokens = 2048")
  case responsesCfg.chatProvider of
    Just p -> do
      p.api @?= "responses"
      p.maxTokens @?= 2048
      p.reasoningEffort @?= ""
    Nothing -> assertFailure "missing Responses provider"
  assertParseFailureContains "thinking must be adaptive" (responses <> "thinking = \"adaptive\"")
  assertParseFailureContains "requires api = anthropic" (responses <> "anthropic_auth = \"bearer\"")
  assertBool "Responses requires explicit endpoint and model"
    (isLeft (parseRuntimeConfigEither "[chat_provider.x]\napi = \"responses\""))

  explicitChat <- parseRuntimeConfig "chat = \"x\"\n[chat_provider.x]\napi = \"chat_completions\""
  fmap (.api) explicitChat.chatProvider @?= Just "chat_completions"
  assertParseFailureContains "api must be" "[chat_provider.x]\napi = \"openai\""
