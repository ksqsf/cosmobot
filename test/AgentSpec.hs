module Main (main) where

import qualified Bot.Agent as Agent
import qualified Bot.Agent.Tools.Audio as AudioTools
import qualified Bot.Agent.Tools.Chat as ChatTools
import qualified Bot.Agent.Tools.Image as ImageTools
import qualified Bot.Agent.Tools.Media as MediaTools
import qualified Bot.Agent.Types as AgentTypes
import Bot.Agent.Tools.Common (UseLimit (..), newUseLimiter)
import Bot.Core.Conversation
import qualified Bot.Core.ReplyBody as ReplyBody
import Bot.Core.Route (runHandlers)
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import qualified Bot.Media.Config as MediaConfig
import qualified Bot.Media.Interpreter as MediaInterpreter
import qualified Bot.LLM.OpenAI.Config as LLMConfig
import qualified Bot.LLM.OpenAI.Transport as LLMTransport
import qualified Bot.LLM.Test as LLMTest
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Skills as Skills
import qualified Bot.Effect.Storage as StorageEffect
import qualified Bot.Effect.Typst as Typst
import qualified Bot.Memory as MemoryStore
import qualified Bot.Skills as SkillsStore
import Bot.Core.Message
import Bot.Handler.Ask (askHandlers)
import Bot.Handler.Ask.Config (AskHandlerConfig (..))
import qualified Bot.Log as Log
import Bot.Storage.Conversation
import qualified Bot.Storage.SQLite as StorageSQLite
import qualified Bot.System.Typst.Test as TypstTest
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import Data.Unique
import Effectful.FileSystem (FileSystem, runFileSystem)
import qualified Effectful.FileSystem as FS
import Effectful.Process (Process, runProcess)
import Effectful.Timeout (Timeout, runTimeout)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.Internal as HTTPInternal
import qualified Network.HTTP.Req as Req
import qualified Network.HTTP.Types.Status as HTTPStatus
import qualified Network.HTTP.Types.Version as HTTPVersion
import qualified Streaming.Prelude as S
import System.Directory
import System.FilePath
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit

type AgentStack =
  '[ Chat.Chat
   , AgentAudit.AgentAudit
   , ChatLog.ChatLog
   , LLM.LLM
   , Media.Media
   , Skills.Skills
   , Memory.Memory
   , Scheduler.Scheduler
   , Typst.Typst
   , StorageEffect.Storage
   , KatipE
   , Prim
   , Fail
   , Concurrent
   , Timeout
   , Process
   , FileSystem
   , IOE
   ]

data ChatMock = ChatMock
  { replies :: !(Maybe (IORef.IORef [Text]))
  , replyId :: !(Maybe MessageId)
  , userAvatar :: !(Maybe Aeson.Value)
  }

data StreamingAnswer = StreamingAnswer
  { chunks :: ![Text]
  , answer :: !LLM.ChatAnswer
  }

data ImageGenerateCall = ImageGenerateCall
  { prompt :: !Text
  , imageRefs :: ![Text]
  , options :: !LLM.ImageRequestOptions
  }
  deriving (Eq, Show)

data ImageEditCall = ImageEditCall
  { prompt :: !Text
  , imageRefs :: ![Text]
  , maskRef :: !(Maybe Text)
  , options :: !LLM.ImageRequestOptions
  }
  deriving (Eq, Show)

data AudioGenerateCall = AudioGenerateCall
  { prompt :: !Text
  , options :: !LLM.AudioRequestOptions
  }
  deriving (Eq, Show)

main :: IO ()
main =
  defaultMain $
    testGroup "agent"
      [ testCase "schedule tool creates a queryable pending schedule" testScheduleToolCreatesQueryableSchedule
      , testCase "send reply tool uses chat effect and records bot message" testSendReplyToolUsesChatEffect
      , testCase "send file tool uploads via chat effect" testSendFileToolUploadsViaChatEffect
      , testCase "send file tool reports upload failure" testSendFileToolReportsUploadFailure
      , testCase "send file tool is noisy and superuser-only" testSendFileToolIsNoisyAndSuperuserOnly
      , testCase "current sender chatlog tool queries matching sender messages" testCurrentSenderChatLogToolQueriesChatLog
      , testCase "user avatar tool queries chat effect" testUserAvatarToolQueriesChatEffect
      , testCase "user avatar tool requires user id" testUserAvatarToolRequiresUserId
      , testCase "user avatar tool rejects zero user id" testUserAvatarToolRejectsZeroUserId
      , testCase "typst_render tool renders and sends an image" testTypstToImageToolRendersAndSendsImage
      , testCase "image_edit tool edits current message image and sends result" testEditImageToolEditsCurrentMessageImageAndSendsResult
      , testCase "ask handler passes referenced images to image_edit tool" testAskHandlerPassesReferencedImagesToEditImageTool
      , testCase "image_generate tool passes image request options" testGenerateImageToolPassesImageRequestOptions
      , testCase "image_cache tool caches image for current context" testViewImageToolCachesImageForContext
      , testCase "media_text reads cached media text slices" testReadMediaTextToolReadsCachedSlices
      , testCase "audio_generate tool uses configured audio options and sends audio" testGenerateAudioToolUsesConfiguredAudioOptions
      , testCase "image_edit tool passes image request options" testEditImageToolPassesImageRequestOptions
      , testCase "agent request merges current message context into system prompt" testAgentRequestMergesCurrentMessageContextIntoSystemPrompt
      , testCase "agent compacts old conversation context before model turn" testAgentCompactsOldConversationContextBeforeModelTurn
      , testCase "agent announces context compaction" testAgentAnnouncesContextCompaction
      , testCase "ask handler system context includes configured bot and sender ids" testAskHandlerSystemContextIncludesConfiguredBotAndSenderIds
      , testCase "ask handler system context uses message bot id" testAskHandlerSystemContextUsesMessageBotId
      , testCase "ask handler injects startup skill metadata" testAskHandlerInjectsStartupSkillMetadata
      , testCase "ask handler announces noisy tool calls with audit id" testAskHandlerAnnouncesNoisyToolCallsWithAuditId
      , testCase "ask handler flushes streamed content before tool calls" testAskHandlerFlushesStreamedContentBeforeToolCalls
      , testCase "agent streams tool request content before tool notification" testAgentStreamsToolRequestContentBeforeToolNotification
      , testCase "agent audit records tool events" testAgentAuditRecordsToolEvents
      , testCase "agent audit storage omits large tool results" testAgentAuditStorageOmitsLargeToolResults
      , testCase "agent omits large tool results only after one model turn consumes them" testAgentOmitsLargeToolResultAfterOneModelTurnConsumesIt
      , testCase "agent audit records structured tool failure category" testAgentAuditRecordsStructuredToolFailureCategory
      , testCase "chat answer JSON remains object compatible" testChatAnswerJsonRemainsObjectCompatible
      , testCase "reply body parses structured content" testReplyBodyParsesStructuredContent
      , testCase "reply segment adapter folds deltas into messages" testReplySegmentAdapterFoldsDeltasIntoMessages
      , testCase "LLM tool request content streams immediately when enabled" testLLMToolRequestContentStreamsImmediatelyWhenEnabled
      , testCase "LLM image stream request asks only for final image" testLLMImageStreamRequestAsksOnlyForFinalImage
      , testCase "LLM audio speech request includes provider options" testLLMAudioSpeechRequestIncludesProviderOptions
      , testCase "LLM image stream completed event yields final image" testLLMImageStreamCompletedEventYieldsFinalImage
      , testCase "LLM image edit stream completed event yields final image" testLLMImageEditStreamCompletedEventYieldsFinalImage
      , testCase "LLM image stream ignores partial event without final image" testLLMImageStreamIgnoresPartialEventWithoutFinalImage
      , testCase "LLM log JSON truncates base64 image payloads" testLLMLogJsonTruncatesBase64ImagePayloads
      , testCase "LLM streaming effect preserves yielded chunks" testLLMStreamingEffectPreservesYieldedChunks
      , testCase "chat streaming chunks replies and yields updates" testChatStreamingChunksRepliesAndYieldsUpdates
      , testCase "editable segmented replies open a new tail after tool messages" testEditableSegmentedRepliesOpenNewTail
      , testCase "segmented replies flush final open segment" testSegmentedRepliesFlushFinalOpenSegment
      , testCase "editable chat streaming splits long replies and yields aliases" testEditableChatStreamingSplitsLongReplies
      , testCase "chunked active conversation aliases every sent reply" testChunkedActiveConversationAliasesEverySentReply
      , testCase "fetch_url max_uses limits fetch calls" testWebFetchMaxUsesLimitsCalls
      , testCase "conversation replies keep parent and child snapshots" testConversationRepliesKeepSnapshots
      , testCase "conversation branches do not overwrite siblings" testConversationBranchesDoNotOverwriteSiblings
      , testCase "conversation lookup is scoped by chat" testConversationLookupIsScopedByChat
      , testCase "conversation branches persist through SQLite reload" testConversationBranchesPersistThroughSQLiteReload
      , testCase "conversation cache miss loads evicted parent from SQLite" testConversationCacheMissLoadsEvictedParent
      , testCase "conversation storage omits large tool results" testConversationStorageOmitsLargeToolResults
      , testCase "conversation omits base64 generated image context" testConversationOmitsBase64GeneratedImageContext
      , testCase "LLM request omits base64 generated image context" testLLMRequestOmitsBase64GeneratedImageContext
      , testCase "conversation JSON remains list compatible" testConversationJsonRemainsListCompatible
      , testCase "memory tool manages current sender memory" testMemoryToolManagesCurrentSenderMemory
      , testCase "memory tool manages current chat memory" testMemoryToolManagesCurrentChatMemory
      , testCase "memory tool enforces non-superuser length limit" testMemoryToolEnforcesLengthLimit
      , testCase "run_bash captures stdout and stderr" testRunBashCapturesStdoutAndStderr
      , testCase "run_bash kills timed out process" testRunBashKillsTimedOutProcess
      , testCase "LLM response timeout summary is concise" testLLMResponseTimeoutSummaryIsConcise
      , testCase "LLM exception summary describes LLM errors" testLLMExceptionSummaryDescribesLLMErrors
      , testCase "LLM status error summary is concise" testLLMStatusErrorSummaryIsConcise
      , testCase "agent failure summarizes Req HTTP errors" testAgentFailureSummarizesReqHttpErrors
      ]

testScheduleToolCreatesQueryableSchedule :: IO ()
testScheduleToolCreatesQueryableSchedule = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "schedule" (Aeson.object ["delay_seconds" Aeson..= (60 :: Int), "prompt" Aeson..= ("check oven" :: Text)])]
    , chatAnswer "scheduled" []
    ]
  (answer, schedules) <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    result <- Agent.runAgent 4 agentContext Agent.defaultTools (startWithUser "remind me")
    pending <- Scheduler.listScheduledMessages testMessage
    pure (fst result, pending)
  answer @?= "scheduled"
  length schedules @?= 1
  let scheduled = fromMaybe (error "expected a schedule") (viaNonEmpty head schedules)
  scheduled.message.text @?= "!ask check oven"
  scheduled.message.replyToMessageId @?= Nothing

testSendReplyToolUsesChatEffect :: IO ()
testSendReplyToolUsesChatEffect = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "send_reply" (Aeson.object ["text" Aeson..= ("hello" :: Text), "image_urls" Aeson..= ["https://example.test/image.png" :: Text]])]
    , chatAnswer "sent" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [Text])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, _) <- runAgentWith answers (ChatMock (Just replies) (Just "42") Nothing) do
    runAgentWithToolMessageCapture 4 agentContext Agent.defaultTools (startWithUser "send it") recorded remembered
  answer @?= "sent"
  IORef.readIORef replies >>= (@?= ["hello\n[image] https://example.test/image.png"])
  IORef.readIORef recorded >>= (@?= ["hello\n[image] https://example.test/image.png"])
  IORef.readIORef remembered >>= (@?= [Just "42"])

testSendFileToolUploadsViaChatEffect :: IO ()
testSendFileToolUploadsViaChatEffect = do
  uploads <- IORef.newIORef ([] :: [FilePath])
  replies <- IORef.newIORef ([] :: [Text])
  result <- runSendFileTool replies \_ path -> do
    liftIO $ IORef.modifyIORef' uploads (<> [path])
    pure (Right (Just "900"))
  case result of
    Agent.ToolSucceeded{content} ->
      assertBool "tool result should describe sent file" ("Sent file /tmp/report.txt" `Text.isInfixOf` content)
    Agent.ToolFailed{failure} ->
      assertFailure [i|expected file upload success, got #{show failure :: String}|]
  IORef.readIORef uploads >>= (@?= ["/tmp/report.txt"])
  IORef.readIORef replies >>= (@?= [])

testSendFileToolReportsUploadFailure :: IO ()
testSendFileToolReportsUploadFailure = do
  replies <- IORef.newIORef ([] :: [Text])
  result <- runSendFileTool replies \_ _ ->
    pure (Left "upload failed")
  case result of
    Agent.ToolFailed{failure} -> do
      failure.userMessage @?= "发送文件失败：upload failed"
      failure.category @?= Agent.ExternalServiceUnavailable
    Agent.ToolSucceeded{} ->
      assertFailure "expected file upload failure"
  IORef.readIORef replies >>= (@?= ["发送文件失败：upload failed"])

testSendFileToolIsNoisyAndSuperuserOnly :: IO ()
testSendFileToolIsNoisyAndSuperuserOnly = do
  let tool = ChatTools.sendFileTool :: Agent.Tool '[Chat.Chat, IOE]
  tool.noisy @?= True
  tool.allowed agentContext @?= False
  tool.allowed superuserContext @?= True

runSendFileTool
  :: IORef.IORef [Text]
  -> (IncomingMessage -> FilePath -> Eff '[IOE] (Either Text (Maybe MessageId)))
  -> IO Agent.ToolResult
runSendFileTool replies upload =
  runEff $
    Chat.runChatWith
      Chat.ChatHandlers
        { handleReplyTo = \_ body -> do
            liftIO $ IORef.modifyIORef' replies (<> [body])
            pure (Just "901")
        , handleReplyAudio = noopReplyAudio
        , handleUploadFile = upload
        , handleEditMessage = noopEdit
        , handleDeleteMessage = noopDelete
        , handleReplyStreamStyle = noopReplyStreamStyle
        , handleGetMessageContent = noopFetch
        , handleGetSenderMemberInfo = noopSenderMember
        , handleGetMemberInfo = noopMember
        , handleGetUserAvatar = noopUserAvatar
        , handleListGroupMembers = noopMembers
        , handleMentionUser = noopMention
        , handleSetMemberTitle = noopSetMemberTitle
        } do
        runner <- ChatTools.sendFileTool.start superuserContext
        runner (Aeson.object ["path" Aeson..= ("file:///tmp/report.txt" :: Text)])

testCurrentSenderChatLogToolQueriesChatLog :: IO ()
testCurrentSenderChatLogToolQueriesChatLog = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "sender_chat_log" (Aeson.object ["keywords" Aeson..= ([["needle"] :: [Text]] :: [[Text]]), "limit" Aeson..= (10 :: Int)])]
    , chatAnswer "found" []
    ]
  (answer, conversation) <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    ChatLog.recordMessage (chatLogMessage 301 "200" 100 "older needle")
    ChatLog.recordMessage (chatLogMessage 302 "201" 100 "other sender needle")
    ChatLog.recordMessage (chatLogMessage 303 "200" 101 "other chat needle")
    ChatLog.recordMessage (chatLogMessage 304 "200" 100 "newer needle")
    Agent.runAgent 4 agentContext Agent.defaultTools (startWithUser "search my history")
  answer @?= "found"
  entries <- decodeSingleChatLogToolOutput conversation
  map (.text) entries @?= ["newer needle", "older needle"]

testUserAvatarToolQueriesChatEffect :: IO ()
testUserAvatarToolQueriesChatEffect = do
  let avatar = Aeson.object
        [ "platform" Aeson..= ("telegram" :: Text)
        , "user_id" Aeson..= (200 :: Integer)
        , "avatar_url" Aeson..= ("https://example.test/avatar.jpg" :: Text)
        ]
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "user_avatar" (Aeson.object ["user_id" Aeson..= ("200" :: Text)])]
    , chatAnswer "found" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [Text])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, conversation) <- runAgentWith answers (ChatMock (Just replies) (Just "44") (Just avatar)) do
    runAgentWithToolMessageCapture 4 agentContext Agent.defaultTools (startWithUser "avatar?") recorded remembered
  answer @?= "found"
  Text.unlines (toolOutputs conversation) @?= jsonText avatar <> "\n"
  imageContextUrls conversation @?= ["https://example.test/avatar.jpg"]
  -- The avatar tool should emit the avatar as a chat image, not only return JSON to the model.
  IORef.readIORef replies >>= (@?= ["[image] https://example.test/avatar.jpg"])
  IORef.readIORef recorded >>= (@?= ["[image] https://example.test/avatar.jpg"])
  IORef.readIORef remembered >>= (@?= [Just "44"])

testUserAvatarToolRequiresUserId :: IO ()
testUserAvatarToolRequiresUserId = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "user_avatar" (Aeson.object [])]
    , chatAnswer "rejected" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  (answer, conversation) <- runAgentWith answers (ChatMock (Just replies) (Just "44") Nothing) do
    Agent.runAgent 4 agentContext Agent.defaultTools (startWithUser "avatar?")
  answer @?= "rejected"
  Text.unlines (toolOutputs conversation) @?= "Error in $: key \"user_id\" not found\n"
  IORef.readIORef replies >>= (@?= [])

testUserAvatarToolRejectsZeroUserId :: IO ()
testUserAvatarToolRejectsZeroUserId = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "user_avatar" (Aeson.object ["user_id" Aeson..= (0 :: Integer)])]
    , chatAnswer "rejected" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  (answer, conversation) <- runAgentWith answers (ChatMock (Just replies) (Just "44") Nothing) do
    Agent.runAgent 4 agentContext Agent.defaultTools (startWithUser "avatar?")
  answer @?= "rejected"
  Text.unlines (toolOutputs conversation) @?= "Error in $: user_id must not be 0.\n"
  IORef.readIORef replies >>= (@?= [])

testTypstToImageToolRendersAndSendsImage :: IO ()
testTypstToImageToolRendersAndSendsImage = do
  let source = "#set page(width: auto, height: auto)\nHello from Typst"
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "typst_render" (Aeson.object ["source" Aeson..= source, "caption" Aeson..= ("demo" :: Text)])]
    , chatAnswer "sent" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  rendered <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [Text])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, _) <- runAgentWithTypst rendered answers (ChatMock (Just replies) (Just "43") Nothing) do
    runAgentWithToolMessageCapture 4 agentContext Agent.defaultTools (startWithUser "render typst") recorded remembered
  answer @?= "sent"
  IORef.readIORef rendered >>= (@?= [source])
  IORef.readIORef replies >>= (@?= ["[image] file:///tmp/cosmobot-agent-spec-typst.png"])
  IORef.readIORef recorded >>= (@?= ["[image] file:///tmp/cosmobot-agent-spec-typst.png"])
  IORef.readIORef remembered >>= (@?= [Just "43"])

testEditImageToolEditsCurrentMessageImageAndSendsResult :: IO ()
testEditImageToolEditsCurrentMessageImageAndSendsResult = do
  let inputImage = "https://example.test/input.png"
      maskImage = "https://example.test/mask.png"
      editedImage = "[image] data:image/png;base64,edited"
      message = testMessageWithImages [inputImage]
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "image_edit" (Aeson.object ["prompt" Aeson..= ("make it brighter" :: Text), "mask_image_url" Aeson..= maskImage])]
    , chatAnswer "done" []
    ]
  editCalls <- IORef.newIORef ([] :: [ImageEditCall])
  replies <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [Text])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, _) <- runAgentWithImageEdit answers editCalls editedImage (ChatMock (Just replies) (Just "47") Nothing) do
    runAgentWithToolMessageCapture 4 (agentContext{Agent.message = message, Agent.input = inputWithImages message.text message.imageUrls}) Agent.defaultTools (startWithUser "edit this") recorded remembered
  answer @?= "done"
  IORef.readIORef editCalls >>= (@?= [ImageEditCall "make it brighter" [inputImage] (Just maskImage) LLM.defaultImageRequestOptions])
  IORef.readIORef replies >>= assertElem editedImage
  IORef.readIORef recorded >>= assertElem editedImage

testAskHandlerPassesReferencedImagesToEditImageTool :: IO ()
testAskHandlerPassesReferencedImagesToEditImageTool = do
  let referencedImage = "https://example.test/replied.png"
      editedImage = "[image] data:image/png;base64,edited"
      prompt = "make the replied image brighter"
      referenced = ReferencedMessage
        { messageId = Just "70001"
        , senderDisplayName = Just "Bob"
        , senderIdentifier = Just "10001"
        , text = ""
        , imageUrls = [referencedImage]
        }
      message = askHandlerMessage
        { replyToMessageId = Just "70001"
        , imageUrls = []
        , text = "krkr 把回复里的图调亮"
        }
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "image_edit" (Aeson.object ["prompt" Aeson..= prompt])]
    , chatAnswer "done" []
    ]
  editCalls <- IORef.newIORef ([] :: [ImageEditCall])
  replies <- IORef.newIORef ([] :: [Text])
  _ <- runAgentWithImageEditAndReferencedMessage answers editCalls editedImage (Just referenced) (ChatMock (Just replies) (Just "47") Nothing) do
    conversations <- newConversationStore
    runHandlers (askHandlers Agent.defaultToolConfig askHandlerConfig conversations) message
    waitUntil (liftIO $ (>= 3) . length <$> IORef.readIORef replies)
    waitUntil do
      toolUses <- AgentAudit.queryRecentToolUses 10
      pure (any finishedEditImageUse toolUses)
  IORef.readIORef editCalls >>= (@?= [ImageEditCall prompt [referencedImage] Nothing LLM.defaultImageRequestOptions])

testGenerateImageToolPassesImageRequestOptions :: IO ()
testGenerateImageToolPassesImageRequestOptions = do
  let generatedImage = "[image] data:image/png;base64,generated"
      expectedOptions = imageOptions "high" "1024x1536" "transparent" "low"
      args =
        Aeson.object
          [ "prompt" Aeson..= ("draw a glass tower" :: Text)
          , "quality" Aeson..= (" high " :: Text)
          , "size" Aeson..= ("1024x1536" :: Text)
          , "background" Aeson..= ("transparent" :: Text)
          , "moderation" Aeson..= ("low" :: Text)
          ]
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "image_generate" args]
    , chatAnswer "done" []
    ]
  generateCalls <- IORef.newIORef ([] :: [ImageGenerateCall])
  replies <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [Text])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, _) <- runAgentWithImageGenerate answers generateCalls generatedImage (ChatMock (Just replies) (Just "48") Nothing) do
    runAgentWithToolMessageCapture 4 agentContext Agent.defaultTools (startWithUser "draw this") recorded remembered
  answer @?= "done"
  IORef.readIORef generateCalls >>= (@?= [ImageGenerateCall "draw a glass tower" [] expectedOptions])
  IORef.readIORef replies >>= assertElem generatedImage
  IORef.readIORef recorded >>= assertElem generatedImage

testViewImageToolCachesImageForContext :: IO ()
testViewImageToolCachesImageForContext =
  withSQLiteTempPath "view-image" \dbPath ->
    withTempDir "view-image-media" \dir -> do
      let cacheDir = dir </> "cache"
          cfg = MediaConfig.defaultConfig{MediaConfig.cacheDir = cacheDir}
          imageUrl = "data:image/png;base64,iVBORw0KGgpmYWtl" :: Text
      runResult <- runEff $
        runFileSystem $
          runProcess $
            runFail $
              runConcurrent $
                runTestLog $
                  StorageSQLite.runStorageSQLitePath dbPath $
                    MediaInterpreter.runMedia cfg do
                      runner <- ImageTools.viewImageTool.start agentContext
                      runner (Aeson.object ["url" Aeson..= imageUrl])
      toolResult <- either assertFailure pure runResult
      case toolResult of
        Agent.ToolSucceeded{content, imageUrls} -> do
          assertBool "tool result should mention media ref" ("media:" `Text.isInfixOf` content)
          case imageUrls of
            [mediaRef] ->
              assertBool "expected cached media ref" ("media:" `Text.isPrefixOf` mediaRef)
            other ->
              assertFailure [i|expected one image context ref, got #{show other :: String}|]
        Agent.ToolFailed{failure} ->
          assertFailure [i|image_cache failed: #{show failure :: String}|]

testReadMediaTextToolReadsCachedSlices :: IO ()
testReadMediaTextToolReadsCachedSlices =
  withSQLiteTempPath "read-media-text" \dbPath ->
    withTempDir "read-media-text-cache" \dir -> do
      let cfg = MediaConfig.defaultConfig{MediaConfig.cacheDir = dir </> "cache"}
          content = "abcdefg" :: Text
      runResult <- runEff $
        runFileSystem $
          runProcess $
            runFail $
              runConcurrent $
                runTestLog $
                  StorageSQLite.runStorageSQLitePath dbPath $
                    MediaInterpreter.runMedia cfg do
                      mediaRef <- Media.storeMediaObject Media.MediaObject
                        { bytes = TextEncoding.encodeUtf8 content
                        , mimeType = "text/plain; charset=utf-8"
                        , sourceName = Just "sample.txt"
                        }
                      let mediaId = maybe "" (\ref -> fromMaybe ref (Text.stripPrefix "media:" ref)) mediaRef
                      runner <- MediaTools.readMediaTextTool.start agentContext
                      result <- runner (Aeson.object
                        [ "media_id" Aeson..= mediaId
                        , "offset" Aeson..= (2 :: Int)
                        , "size" Aeson..= (3 :: Int)
                        ])
                      pure (mediaRef, result)
      (mediaRef, result) <- either assertFailure pure runResult
      let tool = MediaTools.readMediaTextTool :: Agent.Tool AgentStack
      assertBool "expected stored media ref" (maybe False ("media:mf_" `Text.isPrefixOf`) mediaRef)
      tool.noisy @?= False
      assertBool "media_text should be available to everyone" (tool.allowed agentContext)
      case result of
        Agent.ToolSucceeded{content = output} -> do
          assertBool "tool output should include requested slice" ("\"content\":\"cde\"" `Text.isInfixOf` output)
          assertBool "tool output should include returned count" ("\"returned_chars\":3" `Text.isInfixOf` output)
          assertBool "tool output should include total chars" ("\"total_chars\":7" `Text.isInfixOf` output)
        Agent.ToolFailed{failure} ->
          assertFailure [i|media_text failed: #{show failure :: String}|]

testGenerateAudioToolUsesConfiguredAudioOptions :: IO ()
testGenerateAudioToolUsesConfiguredAudioOptions = do
  let generatedAudio = "data:audio/mp3;base64,generated"
      expectedOptions = LLM.defaultAudioRequestOptions
      args =
        Aeson.object
          [ "prompt" Aeson..= ("say hello" :: Text)
          , "voice" Aeson..= (" verse " :: Text)
          , "format" Aeson..= ("mp3" :: Text)
          , "speed" Aeson..= (1.25 :: Double)
          , "instructions" Aeson..= (" speak warmly " :: Text)
          ]
  generateCalls <- IORef.newIORef ([] :: [AudioGenerateCall])
  audioReplies <- IORef.newIORef ([] :: [(Text, Maybe Text)])
  result <- runEff $
    LLMTest.runLLMWith
      (\_ -> S.yield "unused text stream answer" $> "unused text stream answer")
      (\_ _ -> S.yield "unused image answer" $> "unused image answer")
      (\_ _ _ _ -> S.yield "unused image edit answer" $> "unused image edit answer")
      (\options messages -> do
          liftIO $ IORef.modifyIORef' generateCalls (<> map (audioGenerateCall options) messages)
          S.yield generatedAudio
          pure generatedAudio)
      (\_ _ -> S.each ["to", "ol"] $> chatAnswer "tool" []) $
      Chat.runChatWith
        Chat.ChatHandlers
          { handleReplyTo = \_ _ -> pure Nothing
          , handleReplyAudio = \_ audioRef caption -> do
              liftIO $ IORef.modifyIORef' audioReplies (<> [(audioRef, caption)])
              pure (Right (Just "50"))
          , handleUploadFile = noopUpload
          , handleEditMessage = noopEdit
          , handleDeleteMessage = noopDelete
          , handleReplyStreamStyle = noopReplyStreamStyle
          , handleGetMessageContent = noopFetch
          , handleGetSenderMemberInfo = noopSenderMember
          , handleGetMemberInfo = noopMember
          , handleGetUserAvatar = noopUserAvatar
          , handleListGroupMembers = noopMembers
          , handleMentionUser = noopMention
          , handleSetMemberTitle = noopSetMemberTitle
          } do
          runner <- AudioTools.generateAudioTool.start agentContext
          runner args
  case result of
    Agent.ToolSucceeded{content} ->
      assertBool "tool result should describe sent audio" ("Generated and sent audio message id" `Text.isInfixOf` content)
    Agent.ToolFailed{failure} ->
      assertFailure [i|expected audio generation success, got #{show failure :: String}|]
  IORef.readIORef generateCalls >>= (@?= [AudioGenerateCall "say hello" expectedOptions])
  IORef.readIORef audioReplies >>= (@?= [(generatedAudio, Nothing)])
  let tool = AudioTools.generateAudioTool :: Agent.Tool '[Chat.Chat, LLM.LLM]
  tool.noisy @?= True

testEditImageToolPassesImageRequestOptions :: IO ()
testEditImageToolPassesImageRequestOptions = do
  let inputImage = "https://example.test/input.png"
      editedImage = "[image] data:image/png;base64,edited"
      expectedOptions = imageOptions "medium" "1536x1024" "opaque" "auto"
      message = testMessageWithImages [inputImage]
      args =
        Aeson.object
          [ "prompt" Aeson..= ("make it cinematic" :: Text)
          , "quality" Aeson..= ("medium" :: Text)
          , "size" Aeson..= ("1536x1024" :: Text)
          , "background" Aeson..= (" opaque " :: Text)
          , "moderation" Aeson..= ("auto" :: Text)
          ]
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "image_edit" args]
    , chatAnswer "done" []
    ]
  editCalls <- IORef.newIORef ([] :: [ImageEditCall])
  replies <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [Text])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, _) <- runAgentWithImageEdit answers editCalls editedImage (ChatMock (Just replies) (Just "49") Nothing) do
    runAgentWithToolMessageCapture 4 (agentContext{Agent.message = message, Agent.input = inputWithImages message.text message.imageUrls}) Agent.defaultTools (startWithUser "edit this") recorded remembered
  answer @?= "done"
  IORef.readIORef editCalls >>= (@?= [ImageEditCall "make it cinematic" [inputImage] Nothing expectedOptions])
  IORef.readIORef replies >>= assertElem editedImage
  IORef.readIORef recorded >>= assertElem editedImage

imageOptions :: Text -> Text -> Text -> Text -> LLM.ImageRequestOptions
imageOptions quality size background moderation =
  LLM.ImageRequestOptions
    { quality = Just quality
    , size = Just size
    , background = Just background
    , moderation = Just moderation
    }

testAgentRequestMergesCurrentMessageContextIntoSystemPrompt :: IO ()
testAgentRequestMergesCurrentMessageContextIntoSystemPrompt = do
  answers <- IORef.newIORef [chatAnswer "done" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  _ <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    Agent.runAgent 4
      (agentContext
        { Agent.systemContext = Text.unlines
            [ "Current message context:"
            , "- platform: PlatformQQ"
            , "- bot_id: 2044933066"
            , "- sender_id: 295947730"
            ]
        })
      Agent.defaultTools
      (startWithSystemAndUser "base system prompt" "hello")
  requests <- IORef.readIORef captured
  case viaNonEmpty head requests of
    Just (message : secondMessage : _) -> do
      message.role @?= "system"
      assertBool "second request message is not system" (secondMessage.role /= "system")
      case message.content of
        Just (LLM.TextContent content) -> do
          assertBool "system context preserves configured prompt" ("base system prompt" `Text.isInfixOf` content)
          assertBool "system context contains bot id" ("- bot_id: 2044933066" `Text.isInfixOf` content)
          assertBool "system context contains sender id" ("- sender_id: 295947730" `Text.isInfixOf` content)
        other ->
          assertFailure [i|expected text system content, got #{show other :: String}|]
    other ->
      assertFailure [i|expected at least two captured LLM request messages, got #{show (requestRoles <$> other) :: String}|]

testAgentCompactsOldConversationContextBeforeModelTurn :: IO ()
testAgentCompactsOldConversationContextBeforeModelTurn = do
  answers <- IORef.newIORef [chatAnswer "done" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  let longConversation =
        Conversation (Seq.fromList [LLM.userText [i|message #{index}|] | index <- [1 .. 51 :: Int]])
  _ <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    Agent.runAgent 4 agentContext [] longConversation
  requests <- IORef.readIORef captured
  case requests of
    [_summaryRequest, compactedRequest] ->
      case compactedRequest of
        summaryMessage : remainingMessages -> do
          summaryMessage.role @?= "system"
          case summaryMessage.content of
            Just (LLM.TextContent content) ->
              assertBool "compacted summary is included" ("The earlier conversation was compacted." `Text.isInfixOf` content)
            other ->
              assertFailure [i|expected compacted summary text, got #{show other :: String}|]
          length remainingMessages @?= 20
          map (.role) remainingMessages @?= replicate 20 "user"
        other ->
          assertFailure [i|expected compacted request messages, got #{show other :: String}|]
    other ->
      assertFailure [i|expected summary request and compacted model request, got #{length other}|]

testAgentAnnouncesContextCompaction :: IO ()
testAgentAnnouncesContextCompaction = do
  answers <- IORef.newIORef [chatAnswer "done" []]
  replies <- IORef.newIORef ([] :: [Text])
  let longConversation =
        Conversation (Seq.fromList [LLM.userText [i|message #{index}|] | index <- [1 .. 51 :: Int]])
  _ <- runAgentWith answers (ChatMock (Just replies) (Just "46") Nothing) do
    agentRun <- Agent.startAgentRun agentContext []
    let program = Agent.defaultAgentProgram AgentAudit.agentAuditObserver 4 agentRun
    _ <- S.mapM_ (\_ -> pure ()) (Agent.runAgentProgramStreaming program longConversation)
    pure ()
  sent <- IORef.readIORef replies
  sent @?= ["正在整理较早的对话上下文..."]

testAskHandlerSystemContextIncludesConfiguredBotAndSenderIds :: IO ()
testAskHandlerSystemContextIncludesConfiguredBotAndSenderIds = do
  answers <- IORef.newIORef [chatAnswer "done" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  _ <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    conversations <- newConversationStore
    runHandlers (askHandlers Agent.defaultToolConfig askHandlerConfig conversations) askHandlerMessage
    waitUntil (liftIO $ not . null <$> IORef.readIORef captured)
  requests <- IORef.readIORef captured
  case viaNonEmpty head requests of
    Just (message : secondMessage : _) -> do
      message.role @?= "system"
      assertBool "second request message is not system" (secondMessage.role /= "system")
      case message.content of
        Just (LLM.TextContent content) -> do
          assertBool "ask handler system context preserves configured prompt" ("base system prompt" `Text.isInfixOf` content)
          assertBool "ask handler system context contains configured bot id" ("- bot_id: 2044933066 (cosmobot's own platform user id)" `Text.isInfixOf` content)
          assertBool "ask handler system context contains sender id" ("- sender_id: 295947730 (the platform user id of the user who sent this message)" `Text.isInfixOf` content)
        other ->
          assertFailure [i|expected text system content, got #{show other :: String}|]
    other ->
      assertFailure [i|expected at least two captured ask-handler LLM request messages, got #{show (requestRoles <$> other) :: String}|]

testAskHandlerSystemContextUsesMessageBotId :: IO ()
testAskHandlerSystemContextUsesMessageBotId = do
  answers <- IORef.newIORef [chatAnswer "done" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  _ <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    conversations <- newConversationStore
    let cfg = askHandlerConfig{botIds = []}
        message = askHandlerMessage{digest = askHandlerMessage.digest{botId = Just "2044933066"}}
    runHandlers (askHandlers Agent.defaultToolConfig cfg conversations) message
    waitUntil (liftIO $ not . null <$> IORef.readIORef captured)
  requests <- IORef.readIORef captured
  case viaNonEmpty head requests of
    Just (message : secondMessage : _) -> do
      message.role @?= "system"
      assertBool "second request message is not system" (secondMessage.role /= "system")
      case message.content of
        Just (LLM.TextContent content) -> do
          assertBool "ask handler system context contains message bot id" ("- bot_id: 2044933066 (cosmobot's own platform user id)" `Text.isInfixOf` content)
          assertBool "ask handler system context contains sender id" ("- sender_id: 295947730 (the platform user id of the user who sent this message)" `Text.isInfixOf` content)
        other ->
          assertFailure [i|expected text system content, got #{show other :: String}|]
    other ->
      assertFailure [i|expected at least two captured ask-handler LLM request messages, got #{show (requestRoles <$> other) :: String}|]

testAskHandlerInjectsStartupSkillMetadata :: IO ()
testAskHandlerInjectsStartupSkillMetadata = withTempDir "skills-test" \skillsDir -> do
  createDirectoryIfMissing True (skillsDir </> "haskell")
  TextIO.writeFile (skillsDir </> "haskell" </> "SKILL.md") $
    Text.unlines
      [ "---"
      , "name: haskell-refactor"
      , "description: Improve Haskell modules safely."
      , "---"
      , "Full skill body is loaded only when needed."
      ]
  answers <- IORef.newIORef [chatAnswer "done" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  _ <- runAgentCapturingMessagesWithSkills (SkillsStore.SkillsConfig skillsDir) captured answers (ChatMock Nothing Nothing Nothing) do
    conversations <- newConversationStore
    runHandlers (askHandlers Agent.defaultToolConfig askHandlerConfig conversations) askHandlerMessage
    waitUntil (liftIO $ not . null <$> IORef.readIORef captured)
  requests <- IORef.readIORef captured
  case viaNonEmpty head requests of
    Just (message : _) ->
      case message.content of
        Just (LLM.TextContent content) -> do
          assertBool "skill metadata block is included" ("<SKILLS>" `Text.isInfixOf` content)
          assertBool "skill name is included" ("haskell-refactor" `Text.isInfixOf` content)
          assertBool "skill description is included" ("Improve Haskell modules safely." `Text.isInfixOf` content)
          assertBool "skill path is included" (Text.pack (skillsDir </> "haskell" </> "SKILL.md") `Text.isInfixOf` content)
        other ->
          assertFailure [i|expected text system content, got #{show other :: String}|]
    other ->
      assertFailure [i|expected captured ask-handler LLM request messages, got #{show (requestRoles <$> other) :: String}|]

testAgentAuditRecordsToolEvents :: IO ()
testAgentAuditRecordsToolEvents = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "fetch_url" (Aeson.object ["url" Aeson..= ("https://example.test" :: Text)])]
    , chatAnswer "done" []
    ]
  fetches <- IORef.newIORef (0 :: Int)
  toolUses <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    agentRun <- Agent.startAgentRun (agentContext{Agent.toolConfig = Agent.defaultToolConfig{Agent.webFetch = True}}) [fakeWebFetchTool fetches]
    let program = Agent.defaultAgentProgram AgentAudit.agentAuditObserver 4 agentRun
    _ <- S.mapM_ (\_ -> pure ()) (Agent.runAgentProgramStreaming program (startWithUser "fetch it"))
    AgentAudit.queryRecentToolUses 10
  case toolUses of
    [toolUse] -> do
      toolUse.toolName @?= "fetch_url"
      case toolUse.status of
        AgentAudit.ToolUseFinished{status} ->
          status @?= "ok"
        other ->
          assertFailure ("expected finished tool use, got " <> show other)
      toolUse.result @?= Just "fetched"
    _ ->
      assertFailure [i|expected one tool use, got #{length toolUses}|]

testAgentAuditStorageOmitsLargeToolResults :: IO ()
testAgentAuditStorageOmitsLargeToolResults =
  withSQLiteTempPath "audit-large-tool-result" \dbPath ->
    withTempDir "audit-large-tool-result-media" \dir -> do
      let cfg = MediaConfig.defaultConfig{MediaConfig.cacheDir = dir </> "cache"}
          toolResultText = "{\"items\":[" <> Text.intercalate "," (replicate 5000 "\"value\"") <> "]}"
          resultBytes = TextEncoding.encodeUtf8 toolResultText
      answers <- IORef.newIORef
        [ chatAnswer "" [toolCall "call-1" "large_audit_result" (Aeson.object [])]
        , chatAnswer "done" []
        ]
      runResult <- runEff $
        runFileSystem $
          runProcess $
            runFail $
              runConcurrent $
                runPrim $
                  runTestLog $
                    StorageSQLite.runStorageSQLitePath dbPath $
                      MediaInterpreter.runMedia cfg $
                        LLMTest.runLLMWith
                          (\_ -> S.yield "unused text stream answer" $> "unused text stream answer")
                          (\_ _ -> S.yield "unused image answer" $> "unused image answer")
                          (\_ _ _ _ -> S.yield "unused image edit answer" $> "unused image edit answer")
                          (\_ _ -> S.yield "unused audio answer" $> "unused audio answer")
                          (\_ _ -> do
                              answer <- liftIO (popAnswer answers)
                              case answer of
                                LLM.ChatFinalAnswer{content} ->
                                  S.yield content
                                LLM.ChatToolRequest{content}
                                  | Text.null content -> pure ()
                                  | otherwise -> S.yield content
                              pure answer)
                          $
                            AgentAudit.runAgentAudit $
                              Chat.runChatWith
                                Chat.ChatHandlers
                                  { handleReplyTo = \_ _ -> pure Nothing
                                  , handleReplyAudio = noopReplyAudio
                                  , handleUploadFile = noopUpload
                                  , handleEditMessage = noopEdit
                                  , handleDeleteMessage = noopDelete
                                  , handleReplyStreamStyle = noopReplyStreamStyle
                                  , handleGetMessageContent = noopFetch
                                  , handleGetSenderMemberInfo = noopSenderMember
                                  , handleGetMemberInfo = noopMember
                                  , handleGetUserAvatar = noopUserAvatar
                                  , handleListGroupMembers = noopMembers
                                  , handleMentionUser = noopMention
                                  , handleSetMemberTitle = noopSetMemberTitle
                                  }
                              do
                                agentRun <- Agent.startAgentRun agentContext [largeAuditResultTool toolResultText]
                                void $ S.toList (Agent.runAgentProgramStreaming (Agent.defaultAgentProgram AgentAudit.agentAuditObserver 4 agentRun) (startWithUser "audit large result"))
                                uses <- AgentAudit.queryRecentToolUses 10
                                mediaFiles <- Media.listMediaFiles
                                pure (uses, mediaFiles)
      (toolUses, files) <- either assertFailure pure runResult
      case toolUses of
        [toolUse] -> do
          case toolUse.result of
            Just stored -> do
              assertBool "large audit result is replaced by omitted marker" ("[tool result omitted;" `Text.isPrefixOf` stored)
              assertBool "audit marker keeps inferred JSON MIME" ("mime=application/json" `Text.isInfixOf` stored)
              assertBool "audit marker points to media cache" ("media_id=mf_" `Text.isInfixOf` stored)
              assertBool "audit marker keeps a preview" ("preview=\"{\\\"items\\\"" `Text.isInfixOf` stored)
              assertBool "audit row should not retain the full result tail" (not ("\"value\"]}" `Text.isInfixOf` stored))
            Nothing ->
              assertFailure "expected stored audit result"
          case toolUse.status of
            AgentAudit.ToolUseFinished{} ->
              pure ()
            other ->
              assertFailure ("expected finished tool use, got " <> show other)
        _ ->
          assertFailure [i|expected one tool use, got #{length toolUses}|]
      case files of
        [file] -> do
          file.mimeType @?= "application/json"
          file.size @?= StrictByteString.length resultBytes
        other ->
          assertFailure [i|expected one cached result file, got #{length other}|]

largeAuditResultTool :: Text -> Agent.Tool es
largeAuditResultTool result =
  Agent.Tool
    { name = "large_audit_result"
    , description = "fake large audit result"
    , parameters = Aeson.object []
    , noisy = False
    , allowed = const True
    , start = \_ -> pure \_ ->
        pure (Agent.toolText result)
    }

testAgentOmitsLargeToolResultAfterOneModelTurnConsumesIt :: IO ()
testAgentOmitsLargeToolResultAfterOneModelTurnConsumesIt = do
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "large_result" (Aeson.object [])]
    , chatAnswer "done" []
    ]
  let largeResult = "large-result:" <> Text.replicate 5000 "x"
      oneShotLargeResultTool = Agent.Tool
        { name = "large_result"
        , description = "return a large result"
        , parameters = Aeson.object []
        , noisy = False
        , allowed = const True
        , start = \_ -> pure \_ -> pure (Agent.toolText largeResult)
        }
  (_, conversation) <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    Agent.runAgent 4 agentContext [oneShotLargeResultTool] (startWithUser "run it")
  requests <- IORef.readIORef captured
  case requests of
    [_firstRequest, secondRequest] -> do
      let encoded = jsonText secondRequest
      assertBool "current model turn keeps full large tool result" (largeResult `Text.isInfixOf` encoded)
      assertBool "current model turn is not replaced by persistence marker" (not ("[tool result omitted;" `Text.isInfixOf` encoded))
    other ->
      assertFailure [i|expected two LLM requests, got #{length other}|]
  continuationAnswers <- IORef.newIORef [chatAnswer "continued" []]
  _ <- runAgentCapturingMessages captured continuationAnswers (ChatMock Nothing Nothing Nothing) do
    Agent.runAgent 1 agentContext [] conversation
  continuedRequests <- IORef.readIORef captured
  case drop 2 continuedRequests of
    [continuedRequest] -> do
      let encoded = jsonText continuedRequest
      assertBool "later model turn sees omitted tool result" ("[tool result omitted;" `Text.isInfixOf` encoded)
      assertBool "later model turn does not keep full large tool result" (not (largeResult `Text.isInfixOf` encoded))
    other ->
      assertFailure [i|expected one continuation LLM request, got #{length other}|]

testAgentAuditRecordsStructuredToolFailureCategory :: IO ()
testAgentAuditRecordsStructuredToolFailureCategory = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "run_bash" (Aeson.object ["script" Aeson..= ("echo nope" :: Text)])]
    , chatAnswer "done" []
    ]
  toolUses <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    agentRun <- Agent.startAgentRun agentContext Agent.defaultTools
    let program = Agent.defaultAgentProgram AgentAudit.agentAuditObserver 4 agentRun
    _ <- S.mapM_ (\_ -> pure ()) (Agent.runAgentProgramStreaming program (startWithUser "run command"))
    AgentAudit.queryRecentToolUses 10
  case toolUses of
    [toolUse] ->
      case toolUse.status of
        AgentAudit.ToolUseFinished{status} ->
          status @?= "permission_denied"
        other ->
          assertFailure ("expected finished tool use, got " <> show other)
    _ ->
      assertFailure [i|expected one tool use, got #{length toolUses}|]

testAskHandlerAnnouncesNoisyToolCallsWithAuditId :: IO ()
testAskHandlerAnnouncesNoisyToolCallsWithAuditId = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "image_generate" (Aeson.object ["prompt" Aeson..= ("cat" :: Text)])]
    , chatAnswer "done" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  _ <- runAgentWith answers (ChatMock (Just replies) (Just "45") Nothing) do
    conversations <- newConversationStore
    runHandlers (askHandlers Agent.defaultToolConfig askHandlerConfig conversations) askHandlerMessage
    waitUntil (liftIO $ (>= 2) . length <$> IORef.readIORef replies)
    waitUntil do
      toolUses <- AgentAudit.queryRecentToolUses 10
      pure (any finishedGenerateImageUse toolUses)
  sent <- IORef.readIORef replies
  case sent of
    progress : _ ->
      assertBool
        [i|expected noisy tool progress message with audit id, got #{progress}|]
        ("正在调用 image_generate 工具...（id=" `Text.isPrefixOf` progress && "）" `Text.isSuffixOf` progress)
    _ ->
      assertFailure [i|expected noisy tool progress reply, got #{show sent :: String}|]

finishedGenerateImageUse :: AgentAudit.ToolUseDetail -> Bool
finishedGenerateImageUse toolUse =
  toolUse.toolName == "image_generate" && isFinished toolUse.status
  where
    isFinished = \case
      AgentAudit.ToolUseFinished{} -> True
      _ -> False

finishedEditImageUse :: AgentAudit.ToolUseDetail -> Bool
finishedEditImageUse toolUse =
  toolUse.toolName == "image_edit" && isFinished toolUse.status
  where
    isFinished = \case
      AgentAudit.ToolUseFinished{} -> True
      _ -> False

testAskHandlerFlushesStreamedContentBeforeToolCalls :: IO ()
testAskHandlerFlushesStreamedContentBeforeToolCalls = do
  answers <- IORef.newIORef
    [ StreamingAnswer
        { chunks = ["我会", "查天气"]
        , answer = chatAnswer "我会查天气" [toolCall "call-1" "get_weather" (Aeson.object ["location" Aeson..= ("Berlin" :: Text)])]
        }
    , StreamingAnswer
        { chunks = ["我已经查", "到天气"]
        , answer = chatAnswer "我已经查到天气" []
        }
    ]
  replies <- IORef.newIORef ([] :: [Text])
  _ <- runAgentWithStreamingAnswers answers (ChatMock (Just replies) (Just "46") Nothing) do
    conversations <- newConversationStore
    runHandlers (askHandlers Agent.defaultToolConfig askHandlerConfig conversations) askHandlerMessage
    waitUntil (liftIO $ (>= 2) . length <$> IORef.readIORef replies)
  IORef.readIORef replies >>= (@?= ["我会查天气", "我已经查到天气"])

testAgentStreamsToolRequestContentBeforeToolNotification :: IO ()
testAgentStreamsToolRequestContentBeforeToolNotification = do
  answers <- IORef.newIORef
    [ chatAnswer "我先查看当前消息。" [toolCall "call-1" "message_info" (Aeson.object [])]
    , chatAnswer "done" []
    ]
  outputs S.:> result <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    S.toList (Agent.runAgentStreaming 4 agentContext Agent.defaultTools (startWithUser "inspect"))
  streamAnswerText outputs @?= "我先查看当前消息。done"
  case outputs of
    [Agent.AgentContentDelta progress, Agent.AgentToolCallNotification toolCalls, Agent.AgentContentDelta finalChunk] -> do
      progress @?= "我先查看当前消息。"
      map (.name) (toList toolCalls) @?= ["message_info"]
      finalChunk @?= "done"
      case find ((not . null) . (.toolCalls)) (conversationMessagesList result) of
        Just LLM.ChatMessage{role, content = Just (LLM.TextContent content), toolCalls = savedToolCalls} -> do
          role @?= "assistant"
          content @?= "我先查看当前消息。"
          map (.name) savedToolCalls @?= ["message_info"]
        other ->
          assertFailure [i|expected assistant tool request snapshot, got #{show other :: String}|]
    other ->
      assertFailure [i|expected separated intermediate and final output, got #{showSeparatedOutputs other}|]

testChatAnswerJsonRemainsObjectCompatible :: IO ()
testChatAnswerJsonRemainsObjectCompatible = do
  let call = toolCall "call-1" "fetch_url" (Aeson.object ["url" Aeson..= ("https://example.test" :: Text)])
  Aeson.toJSON (chatAnswer "done" []) @?=
    Aeson.object
      [ "content" Aeson..= ("done" :: Text)
      , "toolCalls" Aeson..= ([] :: [LLM.ToolCall])
      ]
  Aeson.toJSON (chatAnswer "checking" [call]) @?=
    Aeson.object
      [ "content" Aeson..= ("checking" :: Text)
      , "toolCalls" Aeson..= [call]
      ]

testReplyBodyParsesStructuredContent :: IO ()
testReplyBodyParsesStructuredContent = do
  ReplyBody.replyContentFromBody
    (Text.unlines ["hello", "[image] https://example.test/a.png", "world", "  [image] file:///tmp/b.png  "])
    @?= ReplyBody.ReplyContent
      { text = "hello\nworld"
      , images = ["https://example.test/a.png", "file:///tmp/b.png"]
      }
  ReplyBody.replyContentToBody
    ReplyBody.ReplyContent
      { text = "hello"
      , images = ["https://example.test/a.png", "file:///tmp/b.png"]
      }
    @?= "hello\n[image] https://example.test/a.png\n[image] file:///tmp/b.png"
  ReplyBody.renderReplyBody "hello\n[image] https://example.test/a.png\nworld" @?= "hello\nworld"
  ReplyBody.replyImageUrls "hello\n[image] https://example.test/a.png\n[image] file:///tmp/b.png" @?=
    ["https://example.test/a.png", "file:///tmp/b.png"]

testReplySegmentAdapterFoldsDeltasIntoMessages :: IO ()
testReplySegmentAdapterFoldsDeltasIntoMessages = do
  ReplyBody.replySegmentMessages
    [ ReplyBody.ReplySegmentDelta "hello"
    , ReplyBody.ReplySegmentDelta " world"
    , ReplyBody.ReplySegmentBoundary
    , ReplyBody.ReplySegmentMessage ReplyBody.ReplyContent{text = "tool request", images = ["https://example.test/tool.png"]}
    , ReplyBody.ReplySegmentDelta "final"
    , ReplyBody.ReplySegmentDelta " answer"
    ]
    @?=
      [ ReplyBody.ReplyContent{text = "hello world", images = []}
      , ReplyBody.ReplyContent{text = "tool request", images = ["https://example.test/tool.png"]}
      , ReplyBody.ReplyContent{text = "final answer", images = []}
      ]
  ReplyBody.replyContentToBody
    (ReplyBody.replyContentFromBody "tool request\n[image] https://example.test/tool.png")
    @?= "tool request\n[image] https://example.test/tool.png"

testLLMToolRequestContentStreamsImmediatelyWhenEnabled :: IO ()
testLLMToolRequestContentStreamsImmediatelyWhenEnabled = do
  let payloads =
        [ streamPayload (Aeson.object ["content" Aeson..= ("我先查看当前消息。" :: Text)])
        , streamPayload
            ( Aeson.object
                [ "tool_calls" Aeson..=
                    [ Aeson.object
                        [ "index" Aeson..= (0 :: Int)
                        , "id" Aeson..= ("call-1" :: Text)
                        , "function" Aeson..=
                            Aeson.object
                              [ "name" Aeson..= ("message_info" :: Text)
                              , "arguments" Aeson..= ("{}" :: Text)
                              ]
                        ]
                    ]
                ]
            )
        ]
  case LLMTransport.chatStreamTextFromPayloads True payloads of
    Right (outputs, LLM.ChatToolRequest{content, toolCalls}) -> do
      outputs @?= ["我先查看当前消息。"]
      content @?= "我先查看当前消息。"
      map (.name) (toList toolCalls) @?= ["message_info"]
    Right other ->
      assertFailure [i|expected tool request stream result, got #{show other :: String}|]
    Left err ->
      assertFailure (Text.unpack err)

testLLMImageStreamRequestAsksOnlyForFinalImage :: IO ()
testLLMImageStreamRequestAsksOnlyForFinalImage =
  LLMTransport.imageGenerationStreamingRequestPayload imageStreamTestConfig LLM.defaultImageRequestOptions "gpt-image-2" "draw this"
    @?=
      Aeson.object
        [ "model" Aeson..= ("gpt-image-2" :: Text)
        , "prompt" Aeson..= ("draw this" :: Text)
        , "stream" Aeson..= True
        , "partial_images" Aeson..= (0 :: Int)
        ]

testLLMAudioSpeechRequestIncludesProviderOptions :: IO ()
testLLMAudioSpeechRequestIncludesProviderOptions =
  LLMTransport.audioSpeechRequestPayload audioSpeechTestConfig options "tts-model" "say this"
    @?=
      Aeson.object
        [ "model" Aeson..= ("tts-model" :: Text)
        , "input" Aeson..= ("say this" :: Text)
        , "voice" Aeson..= ("verse" :: Text)
        , "response_format" Aeson..= ("wav" :: Text)
        , "speed" Aeson..= (1.25 :: Double)
        , "instructions" Aeson..= ("speak warmly" :: Text)
        ]
  where
    options = LLM.AudioRequestOptions
      { LLM.voice = Just "verse"
      , LLM.responseFormat = Just "wav"
      , LLM.speed = Nothing
      , LLM.instructions = Nothing
      }

testLLMImageStreamCompletedEventYieldsFinalImage :: IO ()
testLLMImageStreamCompletedEventYieldsFinalImage =
  case LLMTransport.imageGenerationStreamTextFromPayloads imageStreamTestConfig [completed] of
    Right answer ->
      answer @?= "[image] data:image/png;base64,final-image\n"
    Left err ->
      assertFailure (Text.unpack err)
  where
    completed =
      Aeson.object
        [ "type" Aeson..= ("image_generation.completed" :: Text)
        , "b64_json" Aeson..= ("final-image" :: Text)
        ]

testLLMImageEditStreamCompletedEventYieldsFinalImage :: IO ()
testLLMImageEditStreamCompletedEventYieldsFinalImage =
  case LLMTransport.imageGenerationStreamTextFromPayloads imageStreamTestConfig [completed] of
    Right answer ->
      answer @?= "[image] data:image/png;base64,edited-image\n"
    Left err ->
      assertFailure (Text.unpack err)
  where
    completed =
      Aeson.object
        [ "type" Aeson..= ("image_edit.completed" :: Text)
        , "b64_json" Aeson..= ("edited-image" :: Text)
        ]

testLLMImageStreamIgnoresPartialEventWithoutFinalImage :: IO ()
testLLMImageStreamIgnoresPartialEventWithoutFinalImage =
  case LLMTransport.imageGenerationStreamTextFromPayloads imageStreamTestConfig [partial] of
    Left err ->
      err @?= "Image generation streaming response was empty: no image output."
    Right answer ->
      assertFailure [i|expected empty stream error, got #{answer}|]
  where
    partial =
      Aeson.object
        [ "type" Aeson..= ("image_generation.partial_image" :: Text)
        , "b64_json" Aeson..= ("partial-image" :: Text)
        , "partial_image_index" Aeson..= (0 :: Int)
        ]

testLLMLogJsonTruncatesBase64ImagePayloads :: IO ()
testLLMLogJsonTruncatesBase64ImagePayloads = do
  let payload = Text.replicate 160 "A"
      imageRef = "data:image/png;base64," <> payload
      logged = Log.logJsonText [LLM.userWithImages "look" [imageRef]]
  assertBool "log JSON should not contain the full base64 payload" (not (payload `Text.isInfixOf` logged))
  assertBool "log JSON should keep a recognizable truncated data URL" (("data:image/png;base64," <> Text.replicate 96 "A" <> "...") `Text.isInfixOf` logged)

imageStreamTestConfig :: LLMConfig.ImageProviderConfig
imageStreamTestConfig =
  LLMConfig.defaultImageProviderConfig

audioSpeechTestConfig :: LLMConfig.AudioProviderConfig
audioSpeechTestConfig =
  LLMConfig.defaultAudioProviderConfig
    { LLMConfig.speed = Just 1.25
    , LLMConfig.instructions = Just "speak warmly"
    }

testLLMStreamingEffectPreservesYieldedChunks :: IO ()
testLLMStreamingEffectPreservesYieldedChunks = do
  chunks S.:> answer <- runEff $
    LLMTest.runLLMWith
      (\_ -> S.each ["he", "llo"] $> "hello")
      (\_ _ -> S.yield "unused image answer" $> "unused image answer")
      (\_ _ _ _ -> S.yield "unused image edit answer" $> "unused image edit answer")
      (\_ _ -> S.yield "unused audio answer" $> "unused audio answer")
      (\_ _ -> S.each ["to", "ol"] $> chatAnswer "tool" []) do
        S.toList (LLM.askWithToolsStreaming [] [LLM.userText "hello"])
  chunks @?= ["to", "ol"]
  case answer of
    LLM.ChatFinalAnswer{content} ->
      content @?= "tool"
    other ->
      assertFailure [i|expected final streaming answer, got #{show other :: String}|]

testChatStreamingChunksRepliesAndYieldsUpdates :: IO ()
testChatStreamingChunksRepliesAndYieldsUpdates = do
  replies <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  updates <- IORef.newIORef ([] :: [(Maybe MessageId, [MessageId], Text)])
  nextReplyId <- IORef.newIORef (1 :: Integer)
  (responseId, result) <- runEff $ runPrim $
    Chat.runChatWith
      Chat.ChatHandlers
        { handleReplyTo = recordReply replies nextReplyId
        , handleReplyAudio = noopReplyAudio
        , handleUploadFile = noopUpload
        , handleEditMessage = noopEdit
        , handleDeleteMessage = noopDelete
        , handleReplyStreamStyle = \_ -> pure (Chat.ChunkedReply 4)
        , handleGetMessageContent = noopFetch
        , handleGetSenderMemberInfo = noopSenderMember
        , handleGetMemberInfo = noopMember
        , handleGetUserAvatar = noopUserAvatar
        , handleListGroupMembers = noopMembers
        , handleMentionUser = noopMention
        , handleSetMemberTitle = noopSetMemberTitle
        } $
        S.mapM_
          (\update -> liftIO $ IORef.modifyIORef' updates (<> [(update.responseId, update.sentResponseIds, update.answer)]))
          (Chat.streamReplyTo testMessage id (S.each ["ab", "cd", "ef"] $> "abcdef"))
  responseId @?= Just "1"
  result @?= "abcdef"
  IORef.readIORef replies >>= (@?= [(Just "300", "abcd"), (Just "1", "ef")])
  IORef.readIORef updates >>= (@?= [(Nothing, [], "ab"), (Just "1", ["1"], "abcd"), (Just "1", [], "abcdef"), (Just "1", ["2"], "abcdef")])

testEditableSegmentedRepliesOpenNewTail :: IO ()
testEditableSegmentedRepliesOpenNewTail = do
  replies <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  edits <- IORef.newIORef ([] :: [(MessageId, Text)])
  updates <- IORef.newIORef ([] :: [(Maybe MessageId, [MessageId], Text)])
  nextReplyId <- IORef.newIORef (1 :: Integer)
  (responseId, result) <- runEff $ runPrim $
    Chat.runChatWith
      Chat.ChatHandlers
        { handleReplyTo = recordReply replies nextReplyId
        , handleReplyAudio = noopReplyAudio
        , handleUploadFile = noopUpload
        , handleEditMessage = recordEdit edits
        , handleDeleteMessage = noopDelete
        , handleReplyStreamStyle = \_ -> pure (Chat.EditableReply 2 100)
        , handleGetMessageContent = noopFetch
        , handleGetSenderMemberInfo = noopSenderMember
        , handleGetMemberInfo = noopMember
        , handleGetUserAvatar = noopUserAvatar
        , handleListGroupMembers = noopMembers
        , handleMentionUser = noopMention
        , handleSetMemberTitle = noopSetMemberTitle
        } $
        S.mapM_
          (\update -> liftIO $ IORef.modifyIORef' updates (<> [(update.responseId, update.sentResponseIds, update.answer)]))
          ( Chat.streamReplySegmentsTo
              testMessage
              id
              ( S.each
                  [ ReplyBody.ReplySegmentDelta "ab"
                  , ReplyBody.ReplySegmentMessage ReplyBody.ReplyContent{text = "tool", images = []}
                  , ReplyBody.ReplySegmentDelta "cd"
                  , ReplyBody.ReplySegmentDelta "ef"
                  ]
                  $> "cdef"
              )
          )
  responseId @?= Just "2"
  result @?= "cdef"
  IORef.readIORef replies >>= (@?= [(Just "300", "ab"), (Just "300", "cd")])
  IORef.readIORef edits >>= (@?= [("2", "cdef")])
  IORef.readIORef updates >>= (@?= [(Just "1", ["1"], "ab"), (Just "1", [], "ab"), (Just "2", ["2"], "cd"), (Just "2", [], "cdef"), (Just "2", [], "cdef")])

testSegmentedRepliesFlushFinalOpenSegment :: IO ()
testSegmentedRepliesFlushFinalOpenSegment = do
  replies <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  updates <- IORef.newIORef ([] :: [(Maybe MessageId, [MessageId], Text)])
  nextReplyId <- IORef.newIORef (1 :: Integer)
  (responseId, result) <- runEff $ runPrim $
    Chat.runChatWith
      Chat.ChatHandlers
        { handleReplyTo = recordReply replies nextReplyId
        , handleReplyAudio = noopReplyAudio
        , handleUploadFile = noopUpload
        , handleEditMessage = noopEdit
        , handleDeleteMessage = noopDelete
        , handleReplyStreamStyle = \_ -> pure (Chat.ChunkedReply 100)
        , handleGetMessageContent = noopFetch
        , handleGetSenderMemberInfo = noopSenderMember
        , handleGetMemberInfo = noopMember
        , handleGetUserAvatar = noopUserAvatar
        , handleListGroupMembers = noopMembers
        , handleMentionUser = noopMention
        , handleSetMemberTitle = noopSetMemberTitle
        } $
        S.mapM_
          (\update -> liftIO $ IORef.modifyIORef' updates (<> [(update.responseId, update.sentResponseIds, update.answer)]))
          ( Chat.streamReplySegmentsTo
              testMessage
              id
              (S.each [ReplyBody.ReplySegmentDelta "last ", ReplyBody.ReplySegmentDelta "segment"] $> "last segment")
          )
  responseId @?= Just "1"
  result @?= "last segment"
  IORef.readIORef replies >>= (@?= [(Just "300", "last segment")])
  IORef.readIORef updates >>= (@?= [(Nothing, [], "last "), (Nothing, [], "last segment"), (Just "1", ["1"], "last segment")])

testEditableChatStreamingSplitsLongReplies :: IO ()
testEditableChatStreamingSplitsLongReplies = do
  replies <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  edits <- IORef.newIORef ([] :: [(MessageId, Text)])
  updates <- IORef.newIORef ([] :: [(Maybe MessageId, [MessageId], Text)])
  nextReplyId <- IORef.newIORef (1 :: Integer)
  (responseId, result) <- runEff $ runPrim $
    Chat.runChatWith
      Chat.ChatHandlers
        { handleReplyTo = recordReply replies nextReplyId
        , handleReplyAudio = noopReplyAudio
        , handleUploadFile = noopUpload
        , handleEditMessage = recordEdit edits
        , handleDeleteMessage = noopDelete
        , handleReplyStreamStyle = \_ -> pure (Chat.EditableReply 2 4)
        , handleGetMessageContent = noopFetch
        , handleGetSenderMemberInfo = noopSenderMember
        , handleGetMemberInfo = noopMember
        , handleGetUserAvatar = noopUserAvatar
        , handleListGroupMembers = noopMembers
        , handleMentionUser = noopMention
        , handleSetMemberTitle = noopSetMemberTitle
        } $
        S.mapM_
          (\update -> liftIO $ IORef.modifyIORef' updates (<> [(update.responseId, update.sentResponseIds, update.answer)]))
          (Chat.streamReplyTo testMessage id (S.each ["ab", "cd", "ef", "gh", "ij", "kl"] $> "abcdefghijkl"))
  responseId @?= Just "1"
  result @?= "abcdefghijkl"
  IORef.readIORef replies >>= (@?= [(Just "300", "ab"), (Just "1", "efgh"), (Just "2", "ijkl")])
  IORef.readIORef edits >>= (@?= [("1", "abcd")])
  IORef.readIORef updates >>= (@?= [(Just "1", [], "ab"), (Just "1", [], "abcd"), (Just "1", [], "abcdef"), (Just "1", [], "abcdefgh"), (Just "1", [], "abcdefghij"), (Just "1", [], "abcdefghijkl"), (Just "1", ["2", "3"], "abcdefghijkl")])

testChunkedActiveConversationAliasesEverySentReply :: IO ()
testChunkedActiveConversationAliasesEverySentReply = runEff $ runConcurrent $ runPrim $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ Media.runMediaPassthrough do
  store <- newConversationStore
  threadId <- forkIO (threadDelay 60_000_000)
  let baseConversation = startWithUser "hello"
      partialConversation = appendAssistant "partial answer" baseConversation
  active <- fromMaybe (error "expected active conversation") <$> rememberActiveConversation store Nothing (Just (messageKey 1)) threadId baseConversation
  addActiveConversationMessage store active (messageKey 2)
  updateActiveConversation active partialConversation
  halted <- haltConversation store (messageKey 2)
  firstLookup <- lookupConversation store (messageKey 1)
  secondLookup <- lookupConversation store (messageKey 2)
  liftIO do
    halted @?= True
    (show firstLookup :: String) @?= show (Just partialConversation)
    (show secondLookup :: String) @?= show (Just partialConversation)

testWebFetchMaxUsesLimitsCalls :: IO ()
testWebFetchMaxUsesLimitsCalls = do
  answers <- IORef.newIORef
    [ chatAnswer ""
        [ toolCall "call-1" "fetch_url" (Aeson.object ["url" Aeson..= ("https://example.test/1" :: Text)])
        , toolCall "call-2" "fetch_url" (Aeson.object ["url" Aeson..= ("https://example.test/2" :: Text)])
        ]
    , chatAnswer "done" []
    ]
  fetches <- IORef.newIORef (0 :: Int)
  (answer, _) <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    Agent.runAgent 4 (agentContext{Agent.toolConfig = Agent.defaultToolConfig{Agent.webFetch = True, Agent.webFetchMaxUses = Just 1}}) [fakeWebFetchTool fetches] (startWithUser "fetch twice")
  answer @?= "done"
  IORef.readIORef fetches >>= (@?= 1)

fakeWebFetchTool :: IOE :> es => IORef.IORef Int -> Agent.Tool es
fakeWebFetchTool fetches = Agent.Tool
  { name = "fetch_url"
  , description = "fake web fetch"
  , parameters = Aeson.object []
  , noisy = False
  , allowed = const True
  , start = \context -> do
      checkUseLimit <- newUseLimiter context.toolConfig.webFetchMaxUses
      pure \_ -> do
        checkUseLimit >>= \case
          UseLimitReached currentUses ->
            pure (Agent.toolText [i|fetch_url use limit reached for this agent run: #{currentUses}.|])
          UseAllowed -> do
            liftIO $ IORef.modifyIORef' fetches (+ 1)
            pure (Agent.toolText "fetched")
  }

testConversationRepliesKeepSnapshots :: IO ()
testConversationRepliesKeepSnapshots = runEff $ runConcurrent $ runPrim $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ Media.runMediaPassthrough do
  store <- newConversationStore
  let firstConversation = startWithUser "first"
      secondConversation = appendAssistant "second" firstConversation
  rememberConversation store (Just (messageKey 1)) firstConversation
  rememberConversationFrom store (Just (messageKey 1)) (Just (messageKey 2)) secondConversation
  firstLookup <- lookupConversation store (messageKey 1)
  secondLookup <- lookupConversation store (messageKey 2)
  liftIO do
    (show firstLookup :: String) @?= show (Just firstConversation)
    (show secondLookup :: String) @?= show (Just secondConversation)

testConversationBranchesDoNotOverwriteSiblings :: IO ()
testConversationBranchesDoNotOverwriteSiblings = runEff $ runConcurrent $ runPrim $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ Media.runMediaPassthrough do
  store <- newConversationStore
  let root = appendAssistant "root answer" (startWithUser "root")
      branchA = appendAssistant "A answer" (appendUser "A follow-up" root)
      branchB = appendAssistant "B answer" (appendUser "B follow-up" root)
      branchA2 = appendAssistant "A second answer" (appendUser "A second follow-up" branchA)
  rememberConversation store (Just (messageKey 1)) root
  rememberConversationFrom store (Just (messageKey 1)) (Just (messageKey 2)) branchA
  rememberConversationFrom store (Just (messageKey 1)) (Just (messageKey 3)) branchB
  rememberConversationFrom store (Just (messageKey 2)) (Just (messageKey 4)) branchA2
  rootLookup <- lookupConversation store (messageKey 1)
  branchALookup <- lookupConversation store (messageKey 2)
  branchBLookup <- lookupConversation store (messageKey 3)
  branchA2Lookup <- lookupConversation store (messageKey 4)
  liftIO do
    (show rootLookup :: String) @?= show (Just root)
    (show branchALookup :: String) @?= show (Just branchA)
    (show branchBLookup :: String) @?= show (Just branchB)
    (show branchA2Lookup :: String) @?= show (Just branchA2)

testConversationLookupIsScopedByChat :: IO ()
testConversationLookupIsScopedByChat = runEff $ runConcurrent $ runPrim $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ Media.runMediaPassthrough do
  store <- newConversationStore
  let chatA = testMessageInChat 100
      chatB = testMessageInChat 200
      keyA = conversationMessageKey chatA
      keyB = conversationMessageKey chatB
      conversationA = appendAssistant "answer A" (startWithUser "from chat A")
      conversationB = appendAssistant "answer B" (startWithUser "from chat B")
  rememberConversation store (Just (keyA "1")) conversationA
  rememberConversation store (Just (keyB "1")) conversationB
  lookupA <- lookupConversation store (keyA "1")
  lookupB <- lookupConversation store (keyB "1")
  liftIO do
    (show lookupA :: String) @?= show (Just conversationA)
    (show lookupB :: String) @?= show (Just conversationB)

testConversationBranchesPersistThroughSQLiteReload :: IO ()
testConversationBranchesPersistThroughSQLiteReload =
  withSQLiteTempPath "conversation-branches" \path -> runEff $ runConcurrent $ runPrim $ runTestLog do
    StorageSQLite.runStorageSQLitePath path $ Media.runMediaPassthrough do
      store <- newConversationStore
      let root = appendAssistant "root answer" (startWithUser "root")
          branchA = appendAssistant "A answer" (appendUser "A follow-up" root)
          branchB = appendAssistant "B answer" (appendUser "B follow-up" root)
      rememberConversation store (Just (messageKey 1)) root
      rememberConversationFrom store (Just (messageKey 1)) (Just (messageKey 2)) branchA
      rememberConversationFrom store (Just (messageKey 1)) (Just (messageKey 3)) branchB

      reloaded <- newConversationStore
      branchAAfterReload <- lookupConversation reloaded (messageKey 2)
      branchBAfterReload <- lookupConversation reloaded (messageKey 3)
      let branchA2 = appendAssistant "A second answer" (appendUser "A second follow-up" branchA)
      rememberConversationFrom reloaded (Just (messageKey 2)) (Just (messageKey 4)) branchA2
      rows <- loadConversationRows
      branchA2AfterReload <- lookupConversation reloaded (messageKey 4)

      liftIO do
        (show branchAAfterReload :: String) @?= show (Just branchA)
        (show branchBAfterReload :: String) @?= show (Just branchB)
        (show branchA2AfterReload :: String) @?= show (Just branchA2)
        map rowMessageId rows @?= ["1", "2", "3", "4"]
        map rowParentMessageId rows @?= [Nothing, Just "1", Just "1", Just "2"]
        map payloadMessageCount rows @?= [2, 2, 2, 2]
        assertBool "all nodes in the reloaded tree keep the same conversation id" (sameConversationIds rows)

testConversationCacheMissLoadsEvictedParent :: IO ()
testConversationCacheMissLoadsEvictedParent =
  withSQLiteTempPath "conversation-cache-miss" \path -> runEff $ runConcurrent $ runPrim $ runTestLog do
    StorageSQLite.runStorageSQLitePath path $ Media.runMediaPassthrough do
      store <- newConversationStore
      let root = appendAssistant "root answer" (startWithUser "root")
          child = appendAssistant "child answer" (appendUser "child follow-up" root)
      rememberConversation store (Just (messageKey 1)) root
      for_ [1000..1512] \messageId ->
        rememberConversation store (Just (messageKey messageId)) (startWithUser [i|filler #{messageId}|])
      rememberConversationFrom store (Just (messageKey 1)) (Just (messageKey 2)) child
      rootLookup <- lookupConversation store (messageKey 1)
      childLookup <- lookupConversation store (messageKey 2)
      rows <- loadConversationRows
      let childRow = find ((== "2") . rowMessageId) rows
      liftIO do
        (show rootLookup :: String) @?= show (Just root)
        (show childLookup :: String) @?= show (Just child)
        (rowParentMessageId =<< childRow) @?= Just "1"
        (payloadMessageCount <$> childRow) @?= Just 2

testConversationStorageOmitsLargeToolResults :: IO ()
testConversationStorageOmitsLargeToolResults =
  withSQLiteTempPath "conversation-large-tool-result" \dbPath ->
    withTempDir "conversation-large-tool-result-media" \dir -> do
      let cfg = MediaConfig.defaultConfig{MediaConfig.cacheDir = dir </> "cache"}
          result = "<!doctype html><html><body>" <> Text.replicate 5000 "x" <> "</body></html>"
          resultBytes = TextEncoding.encodeUtf8 result
          answers = [chatAnswer "" [toolCall "call-1" "large_tool" (Aeson.object [])], chatAnswer "done" []]
      answerRef <- IORef.newIORef answers
      runResult <- runEff $
        runFileSystem $
          runProcess $
            runFail $
              runConcurrent $
                runPrim $
                  runTestLog $
                    StorageSQLite.runStorageSQLitePath dbPath $
                      MediaInterpreter.runMedia cfg do
                        store <- newConversationStore
                        (_answer, conversation) <- LLMTest.runLLMWith
                          (\_ -> S.yield "unused text stream answer" $> "unused text stream answer")
                          (\_ _ -> S.yield "unused image answer" $> "unused image answer")
                          (\_ _ _ _ -> S.yield "unused image edit answer" $> "unused image edit answer")
                          (\_ _ -> S.yield "unused audio answer" $> "unused audio answer")
                          (\_ _ -> do
                              answer <- liftIO (popAnswer answerRef)
                              case answer of
                                LLM.ChatFinalAnswer{content} ->
                                  S.yield content
                                LLM.ChatToolRequest{content}
                                  | Text.null content -> pure ()
                                  | otherwise -> S.yield content
                              pure answer)
                          $
                            AgentAudit.runAgentAudit $
                              Chat.runChatWith
                                Chat.ChatHandlers
                                  { handleReplyTo = \_ _ -> pure Nothing
                                  , handleReplyAudio = noopReplyAudio
                                  , handleUploadFile = noopUpload
                                  , handleEditMessage = noopEdit
                                  , handleDeleteMessage = noopDelete
                                  , handleReplyStreamStyle = noopReplyStreamStyle
                                  , handleGetMessageContent = noopFetch
                                  , handleGetSenderMemberInfo = noopSenderMember
                                  , handleGetMemberInfo = noopMember
                                  , handleGetUserAvatar = noopUserAvatar
                                  , handleListGroupMembers = noopMembers
                                  , handleMentionUser = noopMention
                                  , handleSetMemberTitle = noopSetMemberTitle
                                  }
                              do
                                agentRun <- Agent.startAgentRun agentContext [largeResultTool result]
                                outputs S.:> agentResult <- S.toList (Agent.runAgentProgramStreaming (Agent.defaultAgentProgram AgentAudit.agentAuditObserver 4 agentRun) (startWithUser "fetch"))
                                pure (agentOutputText outputs, agentResult.conversation)
                        rememberConversation store (Just (messageKey 1)) conversation
                        loaded <- lookupConversation store (messageKey 1)
                        storedRows <- loadConversationRows
                        mediaFiles <- Media.listMediaFiles
                        pure (loaded, storedRows, mediaFiles)
      (cachedLookup, rows, files) <- either assertFailure pure runResult
      case cachedLookup of
        Just loaded ->
          assertBool "cached lookup should contain omitted marker" ("[tool result omitted;" `Text.isInfixOf` Text.unlines (toolOutputs loaded))
        Nothing ->
          assertFailure "expected cached conversation"
      case rows of
        [row] -> do
          contents <- decodeStoredToolContents row
          case contents of
            [stored] -> do
              assertBool "large conversation tool result is replaced by omitted marker" ("[tool result omitted;" `Text.isPrefixOf` stored)
              assertBool "conversation marker keeps inferred HTML MIME" ("mime=text/html; charset=utf-8" `Text.isInfixOf` stored)
              assertBool "conversation marker points to media cache" ("media_id=mf_" `Text.isInfixOf` stored)
              assertBool "conversation marker keeps a preview" ("preview=\"<!doctype html>" `Text.isInfixOf` stored)
              assertBool "conversation row should not retain the full result tail" (not ("</body></html>" `Text.isInfixOf` stored))
            other ->
              assertFailure [i|expected one stored tool result, got #{length other}|]
        other ->
          assertFailure [i|expected one conversation row, got #{length other}|]
      case files of
        [file] -> do
          file.mimeType @?= "text/html; charset=utf-8"
          file.size @?= StrictByteString.length resultBytes
        other ->
          assertFailure [i|expected one cached result file, got #{length other}|]

largeResultTool :: Text -> Agent.Tool es
largeResultTool result =
  Agent.Tool
    { name = "large_tool"
    , description = "fake large result"
    , parameters = Aeson.object []
    , noisy = False
    , allowed = const True
    , start = \_ -> pure \_ ->
        pure (Agent.toolText result)
    }

testConversationOmitsBase64GeneratedImageContext :: IO ()
testConversationOmitsBase64GeneratedImageContext = do
  let base64Image = "data:image/png;base64,AAAA"
      conversation = appendAssistant (ReplyBody.imageDirective base64Image) (startWithUser "draw")
      encoded = TextEncoding.decodeUtf8 (LazyByteString.toStrict (Aeson.encode conversation))
  imageContextUrls conversation @?= []
  assertBool "conversation history should not retain base64 image payloads" (not (base64Image `Text.isInfixOf` encoded))
  assertBool "conversation history should keep a small generated-image marker" ("Generated image." `Text.isInfixOf` encoded)

testLLMRequestOmitsBase64GeneratedImageContext :: IO ()
testLLMRequestOmitsBase64GeneratedImageContext = do
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  answers <- IORef.newIORef [chatAnswer "ok" []]
  let base64Image = "data:image/png;base64," <> Text.replicate 160 "A"
      conversation =
        appendUser
          "what did you draw?"
          (appendAssistant (ReplyBody.imageDirective base64Image) (startWithUser "draw"))
  _ <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    Agent.runAgent 1 agentContext Agent.defaultTools conversation
  requests <- IORef.readIORef captured
  let encoded = jsonText requests
  assertBool "captured LLM request should not contain generated image base64" (not (base64Image `Text.isInfixOf` encoded))
  assertBool "captured LLM request should retain generated-image marker" ("Generated image." `Text.isInfixOf` encoded)

testConversationJsonRemainsListCompatible :: IO ()
testConversationJsonRemainsListCompatible = do
  let conversation = appendAssistant "answer" (appendUser "follow-up" (startWithUser "hello"))
      encoded = Aeson.encode conversation
      decoded = Aeson.eitherDecode encoded :: Either String Conversation
      encodedValue = Aeson.eitherDecode encoded :: Either String Aeson.Value
  case decoded of
    Left err ->
      assertFailure err
    Right roundTripped ->
      (show roundTripped :: String) @?= show conversation
  encodedValue @?=
    Right (Aeson.object ["messages" Aeson..= Foldable.toList conversation.messages])

testMemoryToolManagesCurrentSenderMemory :: IO ()
testMemoryToolManagesCurrentSenderMemory = withMemoryTempDir \dir -> do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "sender_memory" (Aeson.object ["action" Aeson..= ("replace" :: Text), "memory" Aeson..= ("Prefers concise Chinese answers." :: Text)])]
    , chatAnswer "" [toolCall "call-2" "sender_memory" (Aeson.object ["action" Aeson..= ("view" :: Text)])]
    , chatAnswer "" [toolCall "call-3" "sender_memory" (Aeson.object ["action" Aeson..= ("clear" :: Text)])]
    , chatAnswer "done" []
    ]
  (answer, _) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing Nothing) do
    Agent.runAgent 8 agentContext Agent.defaultTools (startWithUser "remember this")
  answer @?= "done"
  exists <- doesFileExist (dir </> "telegram" </> "sender" </> "200.md")
  exists @?= False

testMemoryToolManagesCurrentChatMemory :: IO ()
testMemoryToolManagesCurrentChatMemory = withMemoryTempDir \dir -> do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "chat_memory" (Aeson.object ["action" Aeson..= ("replace" :: Text), "memory" Aeson..= ("This chat prefers terse status updates." :: Text)])]
    , chatAnswer "" [toolCall "call-2" "chat_memory" (Aeson.object ["action" Aeson..= ("view" :: Text)])]
    , chatAnswer "" [toolCall "call-3" "chat_memory" (Aeson.object ["action" Aeson..= ("clear" :: Text)])]
    , chatAnswer "done" []
    ]
  (answer, _) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing Nothing) do
    Agent.runAgent 8 agentContext Agent.defaultTools (startWithUser "remember this chat")
  answer @?= "done"
  exists <- doesFileExist (dir </> "telegram" </> "chat" </> "100.md")
  exists @?= False

testMemoryToolEnforcesLengthLimit :: IO ()
testMemoryToolEnforcesLengthLimit = withMemoryTempDir \dir -> do
  let longMemory = Text.replicate 1001 "x"
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "sender_memory" (Aeson.object ["action" Aeson..= ("replace" :: Text), "memory" Aeson..= longMemory])]
    , chatAnswer "rejected" []
    ]
  (answer, _) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing Nothing) do
    Agent.runAgent 4 agentContext Agent.defaultTools (startWithUser "remember too much")
  answer @?= "rejected"
  exists <- doesFileExist (dir </> "telegram" </> "sender" </> "200.md")
  exists @?= False

testRunBashCapturesStdoutAndStderr :: IO ()
testRunBashCapturesStdoutAndStderr = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "run_bash" (Aeson.object ["script" Aeson..= ("printf stdout; printf stderr >&2" :: Text), "timeout_seconds" Aeson..= (5 :: Int)])]
    , chatAnswer "done" []
    ]
  (answer, conversation) <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    Agent.runAgent 4 superuserContext Agent.defaultTools (startWithUser "run command")
  answer @?= "done"
  let output = Text.unlines (toolOutputs conversation)
  assertBool "stdout is included" ("stdout:\nstdout" `Text.isInfixOf` output)
  assertBool "stderr is included" ("stderr:\nstderr" `Text.isInfixOf` output)
  assertBool "exit code is included" ("exit code: ExitSuccess" `Text.isInfixOf` output)

testRunBashKillsTimedOutProcess :: IO ()
testRunBashKillsTimedOutProcess = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "run_bash" (Aeson.object ["script" Aeson..= ("sleep 2; printf late" :: Text), "timeout_seconds" Aeson..= (1 :: Int)])]
    , chatAnswer "done" []
    ]
  (answer, conversation) <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    Agent.runAgent 4 superuserContext Agent.defaultTools (startWithUser "run slow command")
  answer @?= "done"
  let output = Text.unlines (toolOutputs conversation)
  assertBool ("timeout is reported in: " <> Text.unpack output) ("Script timed out after 1 seconds and was killed." `Text.isInfixOf` output)
  assertBool ("post-timeout output is not included in: " <> Text.unpack output) (not ("late" `Text.isInfixOf` output))

testLLMResponseTimeoutSummaryIsConcise :: IO ()
testLLMResponseTimeoutSummaryIsConcise = do
  request <- HTTP.parseRequest "https://api.example.test/v1/chat/completions"
  let err = toException (HTTP.HttpExceptionRequest request HTTP.ResponseTimeout)
  LLM.llmExceptionSummary err @?= "HTTP error: ResponseTimeout"

testLLMExceptionSummaryDescribesLLMErrors :: IO ()
testLLMExceptionSummaryDescribesLLMErrors =
  LLM.llmExceptionSummary (toException (LLM.LLMException "OpenAI response was empty: no text output."))
    @?= "LLM error: OpenAI response was empty: no text output."

testLLMStatusErrorSummaryIsConcise :: IO ()
testLLMStatusErrorSummaryIsConcise = do
  err <- expiredQQImageReqException
  let summary = LLM.llmExceptionSummary err
  assertBool "summary includes status" ("HTTP error: 400 Bad Request\n{" `Text.isPrefixOf` summary)
  assertBool "summary includes provider error JSON" ("\"error\"" `Text.isInfixOf` summary)
  assertBool "summary includes provider message" ("Error while downloading https://multimedia.nt.qq.com.cn/download" `Text.isInfixOf` summary)
  assertBool "summary omits the request dump" (not ("responseOriginalRequest" `Text.isInfixOf` summary))

testAgentFailureSummarizesReqHttpErrors :: IO ()
testAgentFailureSummarizesReqHttpErrors = do
  err <- expiredQQImageReqException
  let summary = LLM.llmExceptionSummary err
  let failure = AgentTypes.agentFailureFromException err
  failure.userMessage @?= summary

expiredQQImageReqException :: IO SomeException
expiredQQImageReqException = do
  request <- HTTP.parseRequest "https://api.example.test/v1/chat/completions"
  let response = HTTPInternal.Response
        { HTTPInternal.responseStatus = HTTPStatus.status400
        , HTTPInternal.responseVersion = HTTPVersion.http11
        , HTTPInternal.responseHeaders = []
        , HTTPInternal.responseBody = ()
        , HTTPInternal.responseCookieJar = HTTP.createCookieJar []
        , HTTPInternal.responseClose' = HTTPInternal.ResponseClose (pure ())
        , HTTPInternal.responseOriginalRequest = request
        , HTTPInternal.responseEarlyHints = []
        }
      httpErr = HTTP.HttpExceptionRequest request (HTTP.StatusCodeException response expiredQQImageErrorBody)
  pure (toException (Req.VanillaHttpException httpErr))

expiredQQImageErrorBody :: ByteString
expiredQQImageErrorBody =
  LazyByteString.toStrict $
    Aeson.encode $
      Aeson.object
        [ "error" Aeson..= Text.unlines
            [ "Error: Current provider response failed: {'detail': '{"
            , "  \"error\": {"
            , "    \"message\": \"Error while downloading https://multimedia.nt.qq.com.cn/download?appid=1407&rkey=secret. Upstream status code: 400.\","
            , "    \"param\": \"url\","
            , "    \"code\": \"invalid_value\""
            , "  }"
            , "}'}"
            ]
        ]

withMemoryTempDir :: (FilePath -> IO a) -> IO a
withMemoryTempDir action = do
  withTempDir "memory-test" action

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir label action = do
  runEff $ runFileSystem do
    root <- FS.getTemporaryDirectory
    unique <- liftIO (hashUnique <$> newUnique)
    let dir = root </> [i|cosmobot-#{label}-#{unique}|]
    bracket
      (FS.createDirectory dir $> dir)
      FS.removeDirectoryRecursive
      (liftIO . action)

withSQLiteTempPath :: String -> (FilePath -> IO a) -> IO a
withSQLiteTempPath label action =
  withTempDir label \dir ->
    action (dir </> "test.sqlite")

sameConversationIds :: [ConversationRow] -> Bool
sameConversationIds rows =
  case map (.conversationId) rows of
    [] ->
      True
    firstId : rest ->
      isJust firstId && all (== firstId) rest

payloadMessageCount :: ConversationRow -> Int
payloadMessageCount row =
  case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 row.messagesJson) :: Either String [LLM.ChatMessage] of
    Left err ->
      error (Text.pack err)
    Right messages ->
      length messages

decodeStoredToolContents :: ConversationRow -> IO [Text]
decodeStoredToolContents row =
  case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 row.messagesJson) :: Either String [LLM.ChatMessage] of
    Left err ->
      assertFailure err
    Right messages ->
      pure
        [ text
        | message <- messages
        , message.role == "tool"
        , Just (LLM.TextContent text) <- [message.content]
        ]

rowMessageId :: ConversationRow -> MessageId
rowMessageId row =
  row.messageKey.messageId

rowParentMessageId :: ConversationRow -> Maybe MessageId
rowParentMessageId row =
  (.messageId) <$> row.parentMessageKey

messageKey :: Integer -> ConversationMessageKey
messageKey =
  conversationMessageKey testMessage . integerMessageId

testMessageInChat :: Integer -> IncomingMessage
testMessageInChat chatId =
  IncomingMessage
    { platform = testMessage.platform
    , kind = testMessage.kind
    , chatId = Just chatId
    , chatAliases = testMessage.chatAliases
    , digest = testMessage.digest
    , senderId = testMessage.senderId
    , senderUsername = testMessage.senderUsername
    , messageId = testMessage.messageId
    , replyToMessageId = testMessage.replyToMessageId
    , mentions = testMessage.mentions
    , mentionUsernames = testMessage.mentionUsernames
    , imageUrls = testMessage.imageUrls
    , text = testMessage.text
    , raw = testMessage.raw
    }

testMessageWithImages :: [Text] -> IncomingMessage
testMessageWithImages imageUrls =
  IncomingMessage
    { platform = testMessage.platform
    , kind = testMessage.kind
    , chatId = testMessage.chatId
    , chatAliases = testMessage.chatAliases
    , digest = testMessage.digest
    , senderId = testMessage.senderId
    , senderUsername = testMessage.senderUsername
    , messageId = testMessage.messageId
    , replyToMessageId = testMessage.replyToMessageId
    , mentions = testMessage.mentions
    , mentionUsernames = testMessage.mentionUsernames
    , imageUrls = imageUrls
    , text = testMessage.text
    , raw = testMessage.raw
    }

chatLogMessage :: Integer -> Text -> Integer -> Text -> IncomingMessage
chatLogMessage messageId senderId chatId text =
  testMessage
    { messageId = Just (integerMessageId messageId)
    , senderId = Just senderId
    , chatId = Just chatId
    , text = text
    }

toolOutputs :: Conversation -> [Text]
toolOutputs (Conversation messages) =
  [ text
  | message <- Foldable.toList messages
  , message.role == "tool"
  , Just (LLM.TextContent text) <- [message.content]
  ]

conversationMessagesList :: Conversation -> [LLM.ChatMessage]
conversationMessagesList (Conversation messages) =
  Foldable.toList messages

showSeparatedOutputs :: [Agent.AgentStreamOutput] -> String
showSeparatedOutputs =
  show . map render
  where
    render :: Agent.AgentStreamOutput -> (String, Text)
    render = \case
      Agent.AgentContentDelta text ->
        ("content", text)
      Agent.AgentToolCallNotification calls ->
        ("tool", Text.intercalate ", " (toList (fmap (.name) calls)))

decodeSingleChatLogToolOutput :: Conversation -> IO [ChatLog.ChatLogEntry]
decodeSingleChatLogToolOutput conversation =
  case toolOutputs conversation of
    [output] ->
      case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 output) of
        Left err ->
          assertFailure err >> pure []
        Right entries ->
          pure entries
    outputs ->
      assertFailure [i|expected one tool output, got #{length outputs}|] >> pure []

streamAnswerText :: [Agent.AgentStreamOutput] -> Text
streamAnswerText =
  Text.strip . foldMap \case
    Agent.AgentContentDelta text ->
      text
    Agent.AgentToolCallNotification{} ->
      ""

imageContextUrls :: Conversation -> [Text]
imageContextUrls (Conversation messages) =
  [ url
  | message <- Foldable.toList messages
  , message.role == "user"
  , Just (LLM.PartsContent parts) <- [message.content]
  , LLM.ImageUrlPart url <- parts
  ]

requestRoles :: [LLM.ChatMessage] -> [Text]
requestRoles =
  map (.role)

imageGenerateCall :: LLM.ImageRequestOptions -> LLM.ChatMessage -> ImageGenerateCall
imageGenerateCall options message =
  ImageGenerateCall
    { prompt = messagePromptText message
    , imageRefs = messageImageRefs message
    , options = options
    }

audioGenerateCall :: LLM.AudioRequestOptions -> LLM.ChatMessage -> AudioGenerateCall
audioGenerateCall options message =
  AudioGenerateCall
    { prompt = messagePromptText message
    , options = options
    }

messagePromptText :: LLM.ChatMessage -> Text
messagePromptText message =
  case message.content of
    Just (LLM.TextContent text) ->
      text
    Just (LLM.PartsContent parts) ->
      Text.concat [text | LLM.TextPart text <- parts]
    Nothing ->
      ""

messageImageRefs :: LLM.ChatMessage -> [Text]
messageImageRefs message =
  case message.content of
    Just (LLM.PartsContent parts) ->
      [url | LLM.ImageUrlPart url <- parts]
    _ ->
      []

runAgentWith
  :: IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWith answers chatMock action =
  runAgentWithMemory (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused") answers chatMock action

runAgentWithImageGenerate
  :: IORef.IORef [LLM.ChatAnswer]
  -> IORef.IORef [ImageGenerateCall]
  -> Text
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithImageGenerate answers generateCalls generateAnswer chatMock action = do
  rendered <- IORef.newIORef ([] :: [Text])
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    defaultTestSkillsConfig
    rendered
    Nothing
    answers
    chatMock
    Nothing
    (\options messages -> do
        IORef.modifyIORef' generateCalls (<> map (imageGenerateCall options) messages)
        pure generateAnswer)
    (\_ _ _ _ -> pure "unused image edit answer")
    action

runAgentWithImageEdit
  :: IORef.IORef [LLM.ChatAnswer]
  -> IORef.IORef [ImageEditCall]
  -> Text
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithImageEdit answers editCalls editAnswer chatMock action = do
  rendered <- IORef.newIORef ([] :: [Text])
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageEdit
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    defaultTestSkillsConfig
    rendered
    Nothing
    answers
    chatMock
    (\options prompt imageRefs maskRef -> do
        IORef.modifyIORef' editCalls (<> [ImageEditCall{prompt, imageRefs, maskRef, options}])
        pure editAnswer)
    action

runAgentWithImageEditAndReferencedMessage
  :: IORef.IORef [LLM.ChatAnswer]
  -> IORef.IORef [ImageEditCall]
  -> Text
  -> Maybe ReferencedMessage
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithImageEditAndReferencedMessage answers editCalls editAnswer referencedMessage chatMock action = do
  rendered <- IORef.newIORef ([] :: [Text])
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageEditAndReferenced
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    defaultTestSkillsConfig
    rendered
    Nothing
    answers
    chatMock
    referencedMessage
    (\options prompt imageRefs maskRef -> do
        IORef.modifyIORef' editCalls (<> [ImageEditCall{prompt, imageRefs, maskRef, options}])
        pure editAnswer)
    action

runAgentCapturingMessages
  :: IORef.IORef [[LLM.ChatMessage]]
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentCapturingMessages captured answers chatMock action = do
  rendered <- IORef.newIORef ([] :: [Text])
  runAgentWithMemoryAndTypstAndCapture
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    rendered
    (Just captured)
    answers
    chatMock
    action

runAgentCapturingMessagesWithSkills
  :: SkillsStore.SkillsConfig
  -> IORef.IORef [[LLM.ChatMessage]]
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentCapturingMessagesWithSkills skillsCfg captured answers chatMock action = do
  rendered <- IORef.newIORef ([] :: [Text])
  runAgentWithMemorySkillsAndTypstAndCapture
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    skillsCfg
    rendered
    (Just captured)
    answers
    chatMock
    action

runAgentWithTypst
  :: IORef.IORef [Text]
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithTypst rendered answers chatMock action =
  runAgentWithMemoryAndTypstAndCapture
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    rendered
    Nothing
    answers
    chatMock
    action

runAgentWithMemory
  :: MemoryStore.MemoryConfig
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithMemory memoryCfg answers chatMock action = do
  rendered <- IORef.newIORef ([] :: [Text])
  runAgentWithMemoryAndTypst memoryCfg rendered answers chatMock action

runAgentWithMemoryAndTypst
  :: MemoryStore.MemoryConfig
  -> IORef.IORef [Text]
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithMemoryAndTypst memoryCfg rendered answers chatMock action = do
  runAgentWithMemoryAndTypstAndCapture memoryCfg rendered Nothing answers chatMock action

runAgentWithMemoryAndTypstAndCapture
  :: MemoryStore.MemoryConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [[LLM.ChatMessage]])
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithMemoryAndTypstAndCapture memoryCfg rendered captured answers chatMock action = do
  runAgentWithMemorySkillsAndTypstAndCapture memoryCfg defaultTestSkillsConfig rendered captured answers chatMock action

runAgentWithMemorySkillsAndTypstAndCapture
  :: MemoryStore.MemoryConfig
  -> SkillsStore.SkillsConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [[LLM.ChatMessage]])
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithMemorySkillsAndTypstAndCapture memoryCfg skillsCfg rendered captured answers chatMock action = do
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEdit
    memoryCfg
    skillsCfg
    rendered
    captured
    answers
    chatMock
    (\_ _ -> pure "unused image answer")
    (\_ _ _ _ -> pure "unused image edit answer")
    action

runAgentWithMemorySkillsAndTypstAndCaptureAndImageEdit
  :: MemoryStore.MemoryConfig
  -> SkillsStore.SkillsConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [[LLM.ChatMessage]])
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> (LLM.ImageRequestOptions -> Text -> [Text] -> Maybe Text -> IO Text)
  -> Eff AgentStack a
  -> IO a
runAgentWithMemorySkillsAndTypstAndCaptureAndImageEdit memoryCfg skillsCfg rendered captured answers chatMock imageEdit action = do
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
    memoryCfg
    skillsCfg
    rendered
    captured
    answers
    chatMock
    Nothing
    (\_ _ -> pure "unused image answer")
    imageEdit
    action

runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEdit
  :: MemoryStore.MemoryConfig
  -> SkillsStore.SkillsConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [[LLM.ChatMessage]])
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> (LLM.ImageRequestOptions -> [LLM.ChatMessage] -> IO Text)
  -> (LLM.ImageRequestOptions -> Text -> [Text] -> Maybe Text -> IO Text)
  -> Eff AgentStack a
  -> IO a
runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEdit memoryCfg skillsCfg rendered captured answers chatMock imageGenerate imageEdit action = do
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
    memoryCfg
    skillsCfg
    rendered
    captured
    answers
    chatMock
    Nothing
    imageGenerate
    imageEdit
    action

runAgentWithMemorySkillsAndTypstAndCaptureAndImageEditAndReferenced
  :: MemoryStore.MemoryConfig
  -> SkillsStore.SkillsConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [[LLM.ChatMessage]])
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Maybe ReferencedMessage
  -> (LLM.ImageRequestOptions -> Text -> [Text] -> Maybe Text -> IO Text)
  -> Eff AgentStack a
  -> IO a
runAgentWithMemorySkillsAndTypstAndCaptureAndImageEditAndReferenced memoryCfg skillsCfg rendered captured answers chatMock referencedMessage imageEdit action = do
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
    memoryCfg
    skillsCfg
    rendered
    captured
    answers
    chatMock
    referencedMessage
    (\_ _ -> pure "unused image answer")
    imageEdit
    action

runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
  :: MemoryStore.MemoryConfig
  -> SkillsStore.SkillsConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [[LLM.ChatMessage]])
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Maybe ReferencedMessage
  -> (LLM.ImageRequestOptions -> [LLM.ChatMessage] -> IO Text)
  -> (LLM.ImageRequestOptions -> Text -> [Text] -> Maybe Text -> IO Text)
  -> Eff AgentStack a
  -> IO a
runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced memoryCfg skillsCfg rendered captured answers chatMock referencedMessage imageGenerate imageEdit action = do
  result <-
    runEff $
      runFileSystem $
        runProcess $
          runTimeout $
            runConcurrent $
              runFail $
                runPrim $
                  runTestLog $
                    StorageSQLite.runStorageSQLitePath ":memory:" $
                      TypstTest.runTypstWith (mockTypstRender rendered) $
                        Scheduler.runScheduler $
                          Memory.runMemory memoryCfg $
                            Skills.runSkills skillsCfg $
                              Media.runMediaPassthrough $
                                LLMTest.runLLMWith
                                (\messages -> do
                                    lift $ captureMessages captured messages
                                    S.yield "unused text stream answer"
                                    pure "unused text stream answer")
                                (\options messages -> do
                                    lift $ captureMessages captured messages
                                    answer <- liftIO (imageGenerate options messages)
                                    S.yield answer
                                    pure answer)
                                (\options prompt imageRefs maskRef -> do
                                    answer <- liftIO (imageEdit options prompt imageRefs maskRef)
                                    S.yield answer
                                    pure answer)
                                (\_ messages -> do
                                    lift $ captureMessages captured messages
                                    S.yield "unused audio answer"
                                    pure "unused audio answer")
                                (\_ messages -> do
                                    lift $ captureMessages captured messages
                                    answer <- liftIO (popAnswer answers)
                                    case answer of
                                      LLM.ChatFinalAnswer{content} ->
                                        S.yield content
                                      LLM.ChatToolRequest{content}
                                        | Text.null content -> pure ()
                                        | otherwise -> S.yield content
                                    pure answer) $
                                ChatLog.runChatLog $
                                  AgentAudit.runAgentAudit $
                                    Chat.runChatWith
                                      Chat.ChatHandlers
                                        { handleReplyTo = mockReply chatMock
                                        , handleReplyAudio = noopReplyAudio
                                        , handleUploadFile = noopUpload
                                        , handleEditMessage = noopEdit
                                        , handleDeleteMessage = noopDelete
                                        , handleReplyStreamStyle = noopReplyStreamStyle
                                        , handleGetMessageContent = \_ _ -> pure referencedMessage
                                        , handleGetSenderMemberInfo = noopSenderMember
                                        , handleGetMemberInfo = noopMember
                                        , handleGetUserAvatar = mockUserAvatar chatMock
                                        , handleListGroupMembers = noopMembers
                                        , handleMentionUser = noopMention
                                        , handleSetMemberTitle = noopSetMemberTitle
                                        }
                                      action
  either assertFailure pure result

runAgentWithStreamingAnswers
  :: IORef.IORef [StreamingAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithStreamingAnswers answers chatMock action = do
  rendered <- IORef.newIORef ([] :: [Text])
  result <-
    runEff $
      runFileSystem $
        runProcess $
          runTimeout $
            runConcurrent $
              runFail $
                runPrim $
                  runTestLog $
                    StorageSQLite.runStorageSQLitePath ":memory:" $
                      TypstTest.runTypstWith (mockTypstRender rendered) $
                        Scheduler.runScheduler $
                          Memory.runMemory (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused") $
                            Skills.runSkills defaultTestSkillsConfig $
                              Media.runMediaPassthrough $
                                LLMTest.runLLMWith
                                (\_ -> S.yield "unused text stream answer" $> "unused text stream answer")
                                (\_ _ -> S.yield "unused image answer" $> "unused image answer")
                                (\_ _ _ _ -> S.yield "unused image edit answer" $> "unused image edit answer")
                                (\_ _ -> S.yield "unused audio answer" $> "unused audio answer")
                                (\_ _ -> do
                                    streamingAnswer <- liftIO (popStreamingAnswer answers)
                                    traverse_ S.yield streamingAnswer.chunks
                                    pure streamingAnswer.answer) $
                                ChatLog.runChatLog $
                                  AgentAudit.runAgentAudit $
                                    Chat.runChatWith
                                      Chat.ChatHandlers
                                        { handleReplyTo = mockReply chatMock
                                        , handleReplyAudio = noopReplyAudio
                                        , handleUploadFile = noopUpload
                                        , handleEditMessage = noopEdit
                                        , handleDeleteMessage = noopDelete
                                        , handleReplyStreamStyle = noopReplyStreamStyle
                                        , handleGetMessageContent = noopFetch
                                        , handleGetSenderMemberInfo = noopSenderMember
                                        , handleGetMemberInfo = noopMember
                                        , handleGetUserAvatar = mockUserAvatar chatMock
                                        , handleListGroupMembers = noopMembers
                                        , handleMentionUser = noopMention
                                        , handleSetMemberTitle = noopSetMemberTitle
                                        }
                                      action
  either assertFailure pure result

defaultTestSkillsConfig :: SkillsStore.SkillsConfig
defaultTestSkillsConfig =
  SkillsStore.SkillsConfig "/tmp/cosmobot-agent-spec-unused-skills"

captureMessages :: IOE :> es => Maybe (IORef.IORef [[LLM.ChatMessage]]) -> [LLM.ChatMessage] -> Eff es ()
captureMessages captured messages =
  traverse_ (\ref -> liftIO $ IORef.modifyIORef' ref (<> [messages])) captured

mockTypstRender :: IOE :> es => IORef.IORef [Text] -> Text -> (FilePath -> Eff es r) -> Eff es r
mockTypstRender rendered source action = do
  liftIO $ IORef.modifyIORef' rendered (<> [source])
  action "/tmp/cosmobot-agent-spec-typst.png"

runTestLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runTestLog action = startKatipE "agent-spec" "test" action

popAnswer :: IORef.IORef [LLM.ChatAnswer] -> IO LLM.ChatAnswer
popAnswer answers =
  IORef.atomicModifyIORef' answers \case
    [] ->
      ([], chatAnswer "unexpected extra LLM call" [])
    answer : rest ->
      (rest, answer)

popStreamingAnswer :: IORef.IORef [StreamingAnswer] -> IO StreamingAnswer
popStreamingAnswer answers =
  IORef.atomicModifyIORef' answers \case
    [] ->
      ([], StreamingAnswer{chunks = ["unexpected extra LLM call"], answer = chatAnswer "unexpected extra LLM call" []})
    answer : rest ->
      (rest, answer)

chatAnswer :: Text -> [LLM.ToolCall] -> LLM.ChatAnswer
chatAnswer content calls =
  case nonEmpty calls of
    Nothing ->
      LLM.ChatFinalAnswer content
    Just toolCalls ->
      LLM.ChatToolRequest{content, toolCalls}

toolCall :: Text -> Text -> Aeson.Value -> LLM.ToolCall
toolCall callId name arguments =
  LLM.ToolCall
    { id = callId
    , name = name
    , arguments = jsonText arguments
    }

agentContext :: Agent.AgentContext es
agentContext =
  Agent.AgentContext
    { message = testMessage
    , input = inputWithImages testMessage.text testMessage.imageUrls
    , superuser = False
    , systemContext = ""
    , askCommand = "!ask"
    , toolConfig = Agent.defaultToolConfig
    }

superuserContext :: Agent.AgentContext es
superuserContext =
  agentContext{Agent.superuser = True}

runAgentWithToolMessageCapture
  :: Int
  -> Agent.AgentContext AgentStack
  -> [Agent.Tool AgentStack]
  -> Conversation
  -> IORef.IORef [Text]
  -> IORef.IORef [Maybe MessageId]
  -> Eff AgentStack (Text, Conversation)
runAgentWithToolMessageCapture maxTurns context tools conversation recorded remembered = do
  agentRun <- Agent.startAgentRun context tools
  let sink = Agent.ToolEmittedMessageSink \messageId ->
        liftIO $ IORef.modifyIORef' remembered (<> [messageId])
      program =
        ( Agent.withRecordingToolSelfMessages \body ->
            liftIO $ IORef.modifyIORef' recorded (<> [body])
        )
          . Agent.withLinkingToolEmittedMessagesToConversation sink
          $ Agent.defaultAgentProgram AgentAudit.agentAuditObserver maxTurns agentRun
  outputs S.:> result <- S.toList (Agent.runAgentProgramStreaming program conversation)
  pure (agentOutputText outputs, result.conversation)

agentOutputText :: [Agent.AgentStreamOutput] -> Text
agentOutputText =
  Text.strip . foldMap \case
    Agent.AgentContentDelta chunk ->
      chunk
    Agent.AgentToolCallNotification{} ->
      ""

assertElem :: (Eq a, Show a) => a -> [a] -> Assertion
assertElem expected actual =
  assertBool [i|expected #{show expected :: String} in #{show actual :: String}|] (expected `elem` actual)

mockReply :: IOE :> es => ChatMock -> IncomingMessage -> Text -> Eff es (Maybe MessageId)
mockReply ChatMock{replies, replyId} _ body = do
  traverse_ (\ref -> liftIO $ IORef.modifyIORef' ref (<> [body])) replies
  pure replyId

recordReply :: IOE :> es => IORef.IORef [(Maybe MessageId, Text)] -> IORef.IORef Integer -> IncomingMessage -> Text -> Eff es (Maybe MessageId)
recordReply replies nextReplyId message body = do
  liftIO $ IORef.modifyIORef' replies (<> [(message.messageId, body)])
  liftIO $ IORef.atomicModifyIORef' nextReplyId \replyId ->
    (replyId + 1, Just (integerMessageId replyId))

noopFetch :: IncomingMessage -> MessageId -> Eff es (Maybe ReferencedMessage)
noopFetch _ _ =
  pure Nothing

noopUpload :: IncomingMessage -> FilePath -> Eff es (Either Text (Maybe MessageId))
noopUpload _ _ =
  pure (Right Nothing)

noopReplyAudio :: IncomingMessage -> Text -> Maybe Text -> Eff es (Either Text (Maybe MessageId))
noopReplyAudio _ _ _ =
  pure (Right Nothing)

noopEdit :: IncomingMessage -> MessageId -> Text -> Eff es Bool
noopEdit _ _ _ =
  pure False

recordEdit :: IOE :> es => IORef.IORef [(MessageId, Text)] -> IncomingMessage -> MessageId -> Text -> Eff es Bool
recordEdit edits _ messageId body = do
  liftIO $ IORef.modifyIORef' edits (<> [(messageId, body)])
  pure True

noopDelete :: IncomingMessage -> MessageId -> Eff es Bool
noopDelete _ _ =
  pure False

noopReplyStreamStyle :: IncomingMessage -> Eff es Chat.ReplyStreamStyle
noopReplyStreamStyle _ =
  pure (Chat.ChunkedReply 1800)

noopSenderMember :: IncomingMessage -> Eff es (Maybe Aeson.Value)
noopSenderMember _ =
  pure Nothing

noopMember :: IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
noopMember _ _ =
  pure Nothing

noopUserAvatar :: IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
noopUserAvatar _ _ =
  pure Nothing

noopMembers :: IncomingMessage -> Eff es (Maybe Aeson.Value)
noopMembers _ =
  pure Nothing

noopMention :: IncomingMessage -> Text -> Text -> Eff es (Maybe MessageId)
noopMention _ _ _ =
  pure Nothing

noopSetMemberTitle :: IncomingMessage -> Text -> Text -> Eff es Bool
noopSetMemberTitle _ _ _ =
  pure False

mockUserAvatar :: ChatMock -> IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
mockUserAvatar ChatMock{userAvatar} _ _ =
  pure userAvatar

testMessage :: IncomingMessage
testMessage =
  IncomingMessage
    { platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just 100
    , chatAliases = []
    , digest = emptyMessageDigest
    , senderId = Just "200"
    , senderUsername = Just "alice"
    , messageId = Just "300"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , text = "!ask"
    , raw = Aeson.Null
    }

askHandlerConfig :: AskHandlerConfig
askHandlerConfig =
  AskHandlerConfig
    { name = Just "krkr"
    , command = "!ask"
    , drawCommand = "!draw"
    , systemPrompt = "base system prompt"
    , agentMaxTurns = 4
    , botIds = [(PlatformQQ, "2044933066")]
    }

askHandlerMessage :: IncomingMessage
askHandlerMessage =
  IncomingMessage
    { platform = PlatformQQ
    , kind = ChatGroup
    , chatId = Just 906230260
    , chatAliases = []
    , digest = emptyMessageDigest
        { chatIsAllowed = True
        , senderIsAllowed = True
        , senderIsSuperuser = True
        }
    , senderId = Just "295947730"
    , senderUsername = Nothing
    , messageId = Just "294869878"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , text = "krkr 看下我的头像"
    , raw = Aeson.Null
    }

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  decodeUtf8 . toStrict . Aeson.encode

streamPayload :: Aeson.Value -> Aeson.Value
streamPayload delta =
  Aeson.object
    [ "choices" Aeson..=
        [ Aeson.object
            [ "delta" Aeson..= delta
            ]
        ]
    ]

waitUntil :: (Concurrent :> es, IOE :> es) => Eff es Bool -> Eff es ()
waitUntil predicate =
  go (50 :: Int)
  where
    go 0 =
      liftIO $ assertFailure "timed out waiting for condition"
    go remaining = do
      done <- predicate
      unless done do
        threadDelay 20_000
        go (remaining - 1)
