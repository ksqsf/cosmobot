module Main (main) where

import qualified Bot.LLM.OpenAI.Config as LLMConfig
import Bot.Prelude
import qualified Data.Text as Text
import qualified Toml
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "llm-openai-config"
      [ testCase "selects named chat provider" testSelectsNamedChatProvider
      , testCase "selects named image provider" testSelectsNamedImageProvider
      , testCase "blank selectors disable providers" testBlankSelectorsDisableProviders
      , testCase "missing selected provider fails clearly" testMissingSelectedProviderFailsClearly
      , testCase "provider defaults are applied" testProviderDefaultsAreApplied
      , testCase "non-positive chat timeout fails" testNonPositiveChatTimeoutFails
      , testCase "non-positive image timeout fails" testNonPositiveImageTimeoutFails
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
      , "output_compression = 80"
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
      provider.outputCompression @?= Just 80
    Nothing ->
      assertFailure "expected selected image provider"

testBlankSelectorsDisableProviders :: IO ()
testBlankSelectorsDisableProviders = do
  cfg <- parseRuntimeConfig $
    Text.unlines
      [ "chat = \"  \""
      , "image = \"\""
      , ""
      , "[chat_provider.openrouter]"
      , "api_key = \"chat-key\""
      , ""
      , "[image_provider.openai]"
      , "api_key = \"image-key\""
      ]
  cfg.chatProvider @?= Nothing
  cfg.imageProvider @?= Nothing

testMissingSelectedProviderFailsClearly :: IO ()
testMissingSelectedProviderFailsClearly =
  case parseRuntimeConfigEither "chat = \"missing\"" of
    Left err ->
      assertBool
        [i|unexpected error: #{err}|]
        ("llm.chat selects missing, but llm.chat_provider.missing is not defined" `Text.isInfixOf` Text.pack err)
    Right cfg ->
      assertFailure [i|expected parse failure, got #{show cfg :: String}|]

testProviderDefaultsAreApplied :: IO ()
testProviderDefaultsAreApplied = do
  cfg <- parseRuntimeConfig $
    Text.unlines
      [ "chat = \"default_chat\""
      , "image = \"default_image\""
      , ""
      , "[chat_provider.default_chat]"
      , ""
      , "[image_provider.default_image]"
      ]
  cfg.chatProvider @?= Just LLMConfig.defaultChatProviderConfig
  cfg.imageProvider @?= Just LLMConfig.defaultImageProviderConfig

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
