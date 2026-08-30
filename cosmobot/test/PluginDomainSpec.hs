module Main (main) where

import Bot.Core.Message
import Bot.Plugin.Config
import Bot.Plugin.Protocol
import Bot.Plugin.Types
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Effectful.FileSystem as FileSystem
import qualified System.Directory as Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "plugin domain"
  [ testGroup "bundle config"
      [ testCase "defaults and plugin-owned tables" testConfigDefaults
      , testCase "strict reserved table" testConfigStrictReservedTable
      , testCase "malformed configuration" testMalformedConfig
      , testCase "timeout cannot overflow microseconds" testTimeoutOverflow
      , testCase "missing bundle configuration" testMissingBundleConfig
      , testCase "unsafe plugin id" testUnsafePluginId
      , testCase "sorted immediate bundle discovery" testDiscovery
      ]
  , testGroup "protocol"
      [ testCase "initialization manifest round trip" testManifestRoundTrip
      , testCase "complete manifest is required" testManifestRequiredFields
      , testCase "manifest rejects missing filter" testManifestMissingFilter
      , testCase "manifest rejects malformed nested tool schema" testMalformedToolSchema
      , testCase "plugin message uses stable wire enums" testMessageWireEnums
      , testCase "shared route invocation fixture" testSharedRouteFixture
      , testCase "one MiB newline framing" testFraming
      ]
  , testGroup "filters"
      [ testCase "matches normalized message predicates" testFilterMatch
      , testCase "rejects filters outside bounds" testFilterBounds
      ]
  ]

testConfigDefaults :: Assertion
testConfigDefaults =
  parseBundleConfig "[greeting]\ndefault_name = \"friend\"\n"
    @?= Right defaultPluginLifecycleConfig

testConfigStrictReservedTable :: Assertion
testConfigStrictReservedTable = do
  let parsed = parseBundleConfig "[plugin]\nsandboxed = false\nfuture_flag = true\n"
  assertBool "unknown lifecycle setting rejected"
    (either (Text.isInfixOf "future_flag") (const False) parsed)

testMalformedConfig :: Assertion
testMalformedConfig =
  assertBool "malformed TOML rejected" (isLeft (parseBundleConfig "[plugin\n"))

testTimeoutOverflow :: Assertion
testTimeoutOverflow = do
  let seconds = maxBound `div` 1_000_000 + 1 :: Int
      source = "[plugin]\nroute_timeout_seconds = " <> show seconds <> "\n"
  assertBool "overflowing timeout rejected" (isLeft (parseBundleConfig source))

testMissingBundleConfig :: Assertion
testMissingBundleConfig = withSystemTempDirectory "plugin-domain" \root -> do
  let pluginId = PluginId "missing-config"
      executable = root </> "missing-config"
  writeFile executable "#!/bin/sh\n"
  permissions <- Directory.getPermissions executable
  Directory.setPermissions executable (Directory.setOwnerExecutable True permissions)
  result <- runEff $ FileSystem.runFileSystem $ loadPluginBundle root pluginId
  case result of
    Left err -> err.path @?= root </> pluginConfigFileName
    Right _ -> assertFailure "bundle without config was accepted"

testUnsafePluginId :: Assertion
testUnsafePluginId = do
  assertBool "path traversal rejected" (isLeft (validatePluginId "../escape"))
  assertBool "model-unsafe namespace rejected" (isLeft (validatePluginId "has space"))

testDiscovery :: Assertion
testDiscovery = withSystemTempDirectory "plugin-domain" \root -> do
  traverse_ (makeBundle root) ["zeta", "alpha"]
  writeFile (root </> "ignored-file") "not a bundle"
  result <- runEff $ FileSystem.runFileSystem $ discoverPluginBundles root
  bundles <- either (assertFailure . show) pure result
  map (.pluginId.unPluginId) bundles @?= ["alpha", "zeta"]
  map (.bundleDir) bundles @?= [root </> "alpha", root </> "zeta"]
  where
    makeBundle root pluginId = do
      let directory = root </> pluginId
          executable = directory </> pluginId
      Directory.createDirectory directory
      writeFile executable "#!/bin/sh\n"
      permissions <- Directory.getPermissions executable
      Directory.setPermissions executable (Directory.setOwnerExecutable True permissions)
      writeFile (directory </> "config.toml") "[plugin]\n"

testManifestRoundTrip :: Assertion
testManifestRoundTrip = do
  let encoded = Aeson.toJSON validManifest
  parseInitializationResult encoded @?= Right validManifest

testManifestRequiredFields :: Assertion
testManifestRequiredFields = do
  let incomplete = Aeson.object
        [ "protocolVersion" Aeson..= protocolVersion
        , "pluginVersion" Aeson..= Aeson.String "1.2.3"
        ]
  assertBool "missing registration fields rejected" (isLeft (parseInitializationResult incomplete))

testManifestMissingFilter :: Assertion
testManifestMissingFilter = do
  let invalid = validManifest{filters = Map.empty}
  assertBool "dangling route filter rejected" (isLeft (validateManifest invalid))

testMalformedToolSchema :: Assertion
testMalformedToolSchema = do
  let badSchema = Aeson.object
        [ "type" Aeson..= Aeson.String "object"
        , "properties" Aeson..= Aeson.object
            [ "items" Aeson..= Aeson.object
                [ "type" Aeson..= Aeson.String "array"
                , "items" Aeson..= Aeson.String "not-a-schema"
                ]
            ]
        ]
      badTool = (fromMaybe (error "missing valid tool") (viaNonEmpty head validManifest.tools)){schema = badSchema}
      invalid = validManifest{tools = [badTool]}
  assertBool "malformed nested schema accepted" (isLeft (validateManifest invalid))

testMessageWireEnums :: Assertion
testMessageWireEnums = do
  let params = RouteInvokeParams "inv" "hello" matchingMessage "friend" 10
  case Aeson.toJSON params of
    Aeson.Object object -> case Map.lookup "message" (Map.fromList [(AesonKey.toText key, value) | (key, value) <- KeyMap.toList object]) of
      Just (Aeson.Object messageObject) -> do
        KeyMap.lookup "platform" messageObject @?= Just (Aeson.String "matrix")
        KeyMap.lookup "eventKind" messageObject @?= Just (Aeson.String "created")
        KeyMap.lookup "kind" messageObject @?= Just (Aeson.String "group")
      _ -> assertFailure "missing wire message"
    _ -> assertFailure "route params were not an object"

testSharedRouteFixture :: Assertion
testSharedRouteFixture = do
  frame <- ByteString.readFile "protocol-fixtures/route-invoke.json"
  request <- either (assertFailure . show) pure (decodeFrame frame :: Either ProtocolError RpcRequest)
  request.method @?= "plugin.route.invoke"
  params <- either assertFailure pure (AesonTypes.parseEither Aeson.parseJSON request.params :: Either String RouteInvokeParams)
  Aeson.toJSON params @?= request.params

testFraming :: Assertion
testFraming = do
  let request = RpcRequest (Just (RpcId (Aeson.Number 1))) "plugin.initialize" Aeson.Null
  frame <- either (assertFailure . show) pure (encodeFrame request)
  ByteString.last frame @?= 10
  decodeFrame frame @?= Right request
  (decodeFrame (ByteString.replicate (maxFrameBytes + 1) 32) :: Either ProtocolError RpcRequest)
    @?= Left FrameTooLarge
  assertBool "embedded line rejected" (isLeft (decodeFrame "{}\n{}\n" :: Either ProtocolError Aeson.Value))
  assertBool "missing delimiter rejected" (isLeft (decodeFrame "{}" :: Either ProtocolError Aeson.Value))

testFilterMatch :: Assertion
testFilterMatch = do
  let routeFilter = FilterAll
        [ FilterPredicate (CommandIs "!hello")
        , FilterPredicate (PlatformIs PlatformMatrix)
        , FilterPredicate (ChatKindIs ChatGroup)
        , FilterPredicate (IsReply True)
        , FilterPredicate (MentionsBot True)
        , FilterPredicate (HasAccess SenderAllowed)
        ]
  assertBool "matching message accepted" (matchesRouteFilter routeFilter matchingMessage)
  assertBool "different command rejected"
    (not (matchesRouteFilter (FilterPredicate (CommandIs "!other")) matchingMessage))

testFilterBounds :: Assertion
testFilterBounds = do
  let tooDeep = foldr (const FilterNot) (FilterPredicate (IsReply False))
        [1 .. maxRouteFilterDepth]
  validateRouteFilter tooDeep @?= Left "route filter exceeds maximum depth"

validManifest :: PluginManifest
validManifest = PluginManifest
  { protocolVersion
  , pluginVersion = "1.2.3"
  , routes =
      [ RouteDeclaration
          { routeId = "hello"
          , helpLabel = "!hello"
          , helpDescription = "Say hello."
          , filter = "hello"
          , disposition = StopRouting
          , access = AllowedAccess
          }
      ]
  , filters = Map.singleton "hello" (FilterPredicate (CommandIs "!hello"))
  , tools =
      [ ToolDeclaration
          { name = "greet"
          , description = "Generate a greeting."
          , schema = Aeson.object
              [ "type" Aeson..= Aeson.String "object"
              , "properties" Aeson..= Aeson.object []
              ]
          }
      ]
  , requestedCapabilities = fromList [Chat, LLM]
  }

matchingMessage :: IncomingMessage
matchingMessage = IncomingMessage
  { eventKind = IncomingMessageCreated
  , platform = PlatformMatrix
  , kind = ChatGroup
  , chatId = Just "1"
  , chatAliases = []
  , chatDisplayName = Nothing
  , digest = emptyMessageDigest
      { chatIsAllowed = True
      , senderIsAllowed = True
      , mentionsBot = True
      }
  , senderId = Just "person"
  , senderUsername = Nothing
  , senderDisplayName = Nothing
  , senderGlobalDisplayName = Nothing
  , messageId = Just "message"
  , replyToMessageId = Just "parent"
  , mentions = []
  , mentionUsernames = []
  , imageUrls = []
  , files = []
  , text = "!hello friend"
  , raw = Aeson.Null
  }
