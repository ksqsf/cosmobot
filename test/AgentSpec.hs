module Main (main) where

import qualified Bot.Agent as Agent
import Bot.Agent.Tools.Common (UseLimit (..), newUseLimiter)
import Bot.Core.Conversation
import qualified Bot.Core.ReplyBody as ReplyBody
import Bot.Core.Route (runHandlers)
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.LLM.Transport as LLMTransport
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
import Bot.Storage.Conversation
import Bot.Prelude
import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import Data.Unique
import qualified Network.HTTP.Client as HTTP
import qualified Streaming.Prelude as S
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

type AgentStack =
  '[ Chat.Chat
   , AgentAudit.AgentAudit
   , ChatLog.ChatLog
   , LLM.LLM
   , Skills.Skills
   , Memory.Memory
   , Scheduler.Scheduler
   , Typst.Typst
   , StorageEffect.Storage
   , Log
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

data ImageEditCall = ImageEditCall
  { prompt :: !Text
  , imageRefs :: ![Text]
  , maskRef :: !(Maybe Text)
  }
  deriving (Eq, Show)

main :: IO ()
main =
  defaultMain $
    testGroup "agent"
      [ testCase "schedule tool creates a queryable pending schedule" testScheduleToolCreatesQueryableSchedule
      , testCase "send reply tool uses chat effect and records bot message" testSendReplyToolUsesChatEffect
      , testCase "current sender chatlog tool queries matching sender messages" testCurrentSenderChatLogToolQueriesChatLog
      , testCase "user avatar tool queries chat effect" testUserAvatarToolQueriesChatEffect
      , testCase "user avatar tool requires user id" testUserAvatarToolRequiresUserId
      , testCase "user avatar tool rejects zero user id" testUserAvatarToolRejectsZeroUserId
      , testCase "typst_to_image tool renders and sends an image" testTypstToImageToolRendersAndSendsImage
      , testCase "edit_image tool edits current message image and sends result" testEditImageToolEditsCurrentMessageImageAndSendsResult
      , testCase "ask handler passes referenced images to edit_image tool" testAskHandlerPassesReferencedImagesToEditImageTool
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
      , testCase "agent audit records structured tool failure category" testAgentAuditRecordsStructuredToolFailureCategory
      , testCase "chat answer JSON remains object compatible" testChatAnswerJsonRemainsObjectCompatible
      , testCase "reply body parses structured content" testReplyBodyParsesStructuredContent
      , testCase "reply segment adapter folds deltas into messages" testReplySegmentAdapterFoldsDeltasIntoMessages
      , testCase "LLM tool request content streams immediately when enabled" testLLMToolRequestContentStreamsImmediatelyWhenEnabled
      , testCase "LLM image stream request asks only for final image" testLLMImageStreamRequestAsksOnlyForFinalImage
      , testCase "LLM image stream completed event yields final image" testLLMImageStreamCompletedEventYieldsFinalImage
      , testCase "LLM image stream ignores partial event without final image" testLLMImageStreamIgnoresPartialEventWithoutFinalImage
      , testCase "LLM streaming effect preserves yielded chunks" testLLMStreamingEffectPreservesYieldedChunks
      , testCase "chat streaming chunks replies and yields updates" testChatStreamingChunksRepliesAndYieldsUpdates
      , testCase "editable segmented replies open a new tail after tool messages" testEditableSegmentedRepliesOpenNewTail
      , testCase "segmented replies flush final open segment" testSegmentedRepliesFlushFinalOpenSegment
      , testCase "editable chat streaming splits long replies and yields aliases" testEditableChatStreamingSplitsLongReplies
      , testCase "chunked active conversation aliases every sent reply" testChunkedActiveConversationAliasesEverySentReply
      , testCase "web_fetch max_uses limits fetch calls" testWebFetchMaxUsesLimitsCalls
      , testCase "conversation replies keep parent and child snapshots" testConversationRepliesKeepSnapshots
      , testCase "conversation branches do not overwrite siblings" testConversationBranchesDoNotOverwriteSiblings
      , testCase "conversation lookup is scoped by chat" testConversationLookupIsScopedByChat
      , testCase "conversation branches persist through SQLite reload" testConversationBranchesPersistThroughSQLiteReload
      , testCase "conversation cache miss loads evicted parent from SQLite" testConversationCacheMissLoadsEvictedParent
      , testCase "conversation omits base64 generated image context" testConversationOmitsBase64GeneratedImageContext
      , testCase "conversation JSON remains list compatible" testConversationJsonRemainsListCompatible
      , testCase "memory tool manages current sender memory" testMemoryToolManagesCurrentSenderMemory
      , testCase "memory tool manages current chat memory" testMemoryToolManagesCurrentChatMemory
      , testCase "memory tool enforces non-superuser length limit" testMemoryToolEnforcesLengthLimit
      , testCase "run_bash captures stdout and stderr" testRunBashCapturesStdoutAndStderr
      , testCase "run_bash kills timed out process" testRunBashKillsTimedOutProcess
      , testCase "LLM response timeout summary is concise" testLLMResponseTimeoutSummaryIsConcise
      ]

testScheduleToolCreatesQueryableSchedule :: IO ()
testScheduleToolCreatesQueryableSchedule = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "schedule_agent_action" (Aeson.object ["delay_seconds" Aeson..= (60 :: Int), "prompt" Aeson..= ("check oven" :: Text)])]
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
    [ chatAnswer "" [toolCall "call-1" "send_reply_to_current_chat" (Aeson.object ["text" Aeson..= ("hello" :: Text), "image_urls" Aeson..= ["https://example.test/image.png" :: Text]])]
    , chatAnswer "sent" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, _) <- runAgentWith answers (ChatMock (Just replies) (Just "42") Nothing) do
    Agent.runAgent 4 (agentContextWith recorded remembered) Agent.defaultTools (startWithUser "send it")
  answer @?= "sent"
  IORef.readIORef replies >>= (@?= ["hello\n[image] https://example.test/image.png"])
  IORef.readIORef recorded >>= (@?= [(Just "42", "hello\n[image] https://example.test/image.png")])
  IORef.readIORef remembered >>= (@?= [Just "42"])

testCurrentSenderChatLogToolQueriesChatLog :: IO ()
testCurrentSenderChatLogToolQueriesChatLog = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "query_current_sender_chatlog" (Aeson.object ["keywords" Aeson..= ([["needle"] :: [Text]] :: [[Text]]), "limit" Aeson..= (10 :: Int)])]
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
    [ chatAnswer "" [toolCall "call-1" "get_user_avatar" (Aeson.object ["user_id" Aeson..= ("200" :: Text)])]
    , chatAnswer "found" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, conversation) <- runAgentWith answers (ChatMock (Just replies) (Just "44") (Just avatar)) do
    Agent.runAgent 4 (agentContextWith recorded remembered) Agent.defaultTools (startWithUser "avatar?")
  answer @?= "found"
  Text.unlines (toolOutputs conversation) @?= jsonText avatar <> "\n"
  imageContextUrls conversation @?= ["https://example.test/avatar.jpg"]
  -- The avatar tool should emit the avatar as a chat image, not only return JSON to the model.
  IORef.readIORef replies >>= (@?= ["[image] https://example.test/avatar.jpg"])
  IORef.readIORef recorded >>= (@?= [(Just "44", "[image] https://example.test/avatar.jpg")])
  IORef.readIORef remembered >>= (@?= [Just "44"])

testUserAvatarToolRequiresUserId :: IO ()
testUserAvatarToolRequiresUserId = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "get_user_avatar" (Aeson.object [])]
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
    [ chatAnswer "" [toolCall "call-1" "get_user_avatar" (Aeson.object ["user_id" Aeson..= (0 :: Integer)])]
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
    [ chatAnswer "" [toolCall "call-1" "typst_to_image" (Aeson.object ["source" Aeson..= source, "caption" Aeson..= ("demo" :: Text)])]
    , chatAnswer "sent" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  rendered <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, _) <- runAgentWithTypst rendered answers (ChatMock (Just replies) (Just "43") Nothing) do
    Agent.runAgent 4 (agentContextWith recorded remembered) Agent.defaultTools (startWithUser "render typst")
  answer @?= "sent"
  IORef.readIORef rendered >>= (@?= [source])
  IORef.readIORef replies >>= (@?= ["[image] file:///tmp/cosmobot-agent-spec-typst.png"])
  IORef.readIORef recorded >>= (@?= [(Just "43", "[typst image]")])
  IORef.readIORef remembered >>= (@?= [Just "43"])

testEditImageToolEditsCurrentMessageImageAndSendsResult :: IO ()
testEditImageToolEditsCurrentMessageImageAndSendsResult = do
  let inputImage = "https://example.test/input.png"
      maskImage = "https://example.test/mask.png"
      editedImage = "[image] data:image/png;base64,edited"
      message = testMessageWithImages [inputImage]
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "edit_image" (Aeson.object ["prompt" Aeson..= ("make it brighter" :: Text), "mask_image_url" Aeson..= maskImage])]
    , chatAnswer "done" []
    ]
  editCalls <- IORef.newIORef ([] :: [ImageEditCall])
  replies <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, _) <- runAgentWithImageEdit answers editCalls editedImage (ChatMock (Just replies) (Just "47") Nothing) do
    Agent.runAgent 4 ((agentContextWith recorded remembered){Agent.message = message, Agent.input = inputWithImages message.text message.imageUrls}) Agent.defaultTools (startWithUser "edit this")
  answer @?= "done"
  IORef.readIORef editCalls >>= (@?= [ImageEditCall "make it brighter" [inputImage] (Just maskImage)])
  IORef.readIORef replies >>= (@?= [editedImage])
  IORef.readIORef recorded >>= (@?= [(Just "47", editedImage)])

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
    [ chatAnswer "" [toolCall "call-1" "edit_image" (Aeson.object ["prompt" Aeson..= prompt])]
    , chatAnswer "done" []
    ]
  editCalls <- IORef.newIORef ([] :: [ImageEditCall])
  replies <- IORef.newIORef ([] :: [Text])
  _ <- runAgentWithImageEditAndReferencedMessage answers editCalls editedImage (Just referenced) (ChatMock (Just replies) (Just "47") Nothing) do
    conversations <- liftIO newConversationStore
    runHandlers (askHandlers Agent.defaultToolConfig askHandlerConfig conversations) message
    liftIO $ waitUntil ((>= 3) . length <$> IORef.readIORef replies)
  IORef.readIORef editCalls >>= (@?= [ImageEditCall prompt [referencedImage] Nothing])

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
    conversations <- liftIO newConversationStore
    runHandlers (askHandlers Agent.defaultToolConfig askHandlerConfig conversations) askHandlerMessage
    liftIO $ waitUntil (not . null <$> IORef.readIORef captured)
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
    conversations <- liftIO newConversationStore
    let cfg = askHandlerConfig{botIds = []}
        message = askHandlerMessage{digest = askHandlerMessage.digest{botId = Just "2044933066"}}
    runHandlers (askHandlers Agent.defaultToolConfig cfg conversations) message
    liftIO $ waitUntil (not . null <$> IORef.readIORef captured)
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
    conversations <- liftIO newConversationStore
    runHandlers (askHandlers Agent.defaultToolConfig askHandlerConfig conversations) askHandlerMessage
    liftIO $ waitUntil (not . null <$> IORef.readIORef captured)
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
    [ chatAnswer "" [toolCall "call-1" "web_fetch" (Aeson.object ["url" Aeson..= ("https://example.test" :: Text)])]
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
      toolUse.toolName @?= "web_fetch"
      case toolUse.status of
        AgentAudit.ToolUseFinished{status} ->
          status @?= "ok"
        other ->
          assertFailure ("expected finished tool use, got " <> show other)
      toolUse.result @?= Just "fetched"
    _ ->
      assertFailure [i|expected one tool use, got #{length toolUses}|]

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
    [ chatAnswer "" [toolCall "call-1" "generate_image" (Aeson.object ["prompt" Aeson..= ("cat" :: Text)])]
    , chatAnswer "done" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  _ <- runAgentWith answers (ChatMock (Just replies) (Just "45") Nothing) do
    conversations <- liftIO newConversationStore
    runHandlers (askHandlers Agent.defaultToolConfig askHandlerConfig conversations) askHandlerMessage
    liftIO $ waitUntil ((>= 2) . length <$> IORef.readIORef replies)
  sent <- IORef.readIORef replies
  case sent of
    progress : _ ->
      assertBool
        [i|expected noisy tool progress message with audit id, got #{progress}|]
        ("正在调用 generate_image 工具...（id=" `Text.isPrefixOf` progress && "）" `Text.isSuffixOf` progress)
    _ ->
      assertFailure [i|expected noisy tool progress reply, got #{show sent :: String}|]

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
    conversations <- liftIO newConversationStore
    runHandlers (askHandlers Agent.defaultToolConfig askHandlerConfig conversations) askHandlerMessage
    liftIO $ waitUntil ((>= 2) . length <$> IORef.readIORef replies)
  IORef.readIORef replies >>= (@?= ["我会查天气", "我已经查到天气"])

testAgentStreamsToolRequestContentBeforeToolNotification :: IO ()
testAgentStreamsToolRequestContentBeforeToolNotification = do
  answers <- IORef.newIORef
    [ chatAnswer "我先查看当前消息。" [toolCall "call-1" "get_current_message_info" (Aeson.object [])]
    , chatAnswer "done" []
    ]
  outputs S.:> result <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    S.toList (Agent.runAgentStreaming 4 agentContext Agent.defaultTools (startWithUser "inspect"))
  streamAnswerText outputs @?= "我先查看当前消息。done"
  case outputs of
    [Agent.AgentContentDelta progress, Agent.AgentToolCallNotification toolCalls, Agent.AgentContentDelta finalChunk] -> do
      progress @?= "我先查看当前消息。"
      map (.name) (toList toolCalls) @?= ["get_current_message_info"]
      finalChunk @?= "done"
      case find ((not . null) . (.toolCalls)) (conversationMessagesList result) of
        Just LLM.ChatMessage{role, content = Just (LLM.TextContent content), toolCalls = savedToolCalls} -> do
          role @?= "assistant"
          content @?= "我先查看当前消息。"
          map (.name) savedToolCalls @?= ["get_current_message_info"]
        other ->
          assertFailure [i|expected assistant tool request snapshot, got #{show other :: String}|]
    other ->
      assertFailure [i|expected separated intermediate and final output, got #{showSeparatedOutputs other}|]

testChatAnswerJsonRemainsObjectCompatible :: IO ()
testChatAnswerJsonRemainsObjectCompatible = do
  let call = toolCall "call-1" "web_fetch" (Aeson.object ["url" Aeson..= ("https://example.test" :: Text)])
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
                              [ "name" Aeson..= ("get_current_message_info" :: Text)
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
      map (.name) (toList toolCalls) @?= ["get_current_message_info"]
    Right other ->
      assertFailure [i|expected tool request stream result, got #{show other :: String}|]
    Left err ->
      assertFailure (Text.unpack err)

testLLMImageStreamRequestAsksOnlyForFinalImage :: IO ()
testLLMImageStreamRequestAsksOnlyForFinalImage =
  LLMTransport.imageGenerationStreamingRequestPayload imageStreamTestConfig "gpt-image-2" "draw this"
    @?=
      Aeson.object
        [ "model" Aeson..= ("gpt-image-2" :: Text)
        , "prompt" Aeson..= ("draw this" :: Text)
        , "stream" Aeson..= True
        , "partial_images" Aeson..= (0 :: Int)
        ]

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

imageStreamTestConfig :: LLM.Config
imageStreamTestConfig =
  LLM.defaultConfig

testLLMStreamingEffectPreservesYieldedChunks :: IO ()
testLLMStreamingEffectPreservesYieldedChunks = do
  chunks S.:> answer <- runEff $
    LLM.runLLMWith
      (\_ -> pure "unused text answer")
      (\_ -> S.each ["he", "llo"] $> "hello")
      (\_ -> pure "unused image answer")
      (\_ _ _ -> pure "unused image edit answer")
      (\_ _ -> pure (chatAnswer "unused tool answer" []))
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
  (responseId, result) <- runEff $
    Chat.runChatWith
      Chat.ChatHandlers
        { handleReplyTo = recordReply replies nextReplyId
        , handleEditMessage = noopEdit
        , handleDeleteMessage = noopDelete
        , handleReplyStreamStyle = \_ -> pure (Chat.ChunkedReply 4)
        , handleGetMessageContent = noopFetch
        , handleGetSenderMemberInfo = noopSenderMember
        , handleGetMemberInfo = noopMember
        , handleGetUserAvatar = noopUserAvatar
        , handleListGroupMembers = noopMembers
        , handleMentionUser = noopMention
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
  (responseId, result) <- runEff $
    Chat.runChatWith
      Chat.ChatHandlers
        { handleReplyTo = recordReply replies nextReplyId
        , handleEditMessage = recordEdit edits
        , handleDeleteMessage = noopDelete
        , handleReplyStreamStyle = \_ -> pure (Chat.EditableReply 2 100)
        , handleGetMessageContent = noopFetch
        , handleGetSenderMemberInfo = noopSenderMember
        , handleGetMemberInfo = noopMember
        , handleGetUserAvatar = noopUserAvatar
        , handleListGroupMembers = noopMembers
        , handleMentionUser = noopMention
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
  (responseId, result) <- runEff $
    Chat.runChatWith
      Chat.ChatHandlers
        { handleReplyTo = recordReply replies nextReplyId
        , handleEditMessage = noopEdit
        , handleDeleteMessage = noopDelete
        , handleReplyStreamStyle = \_ -> pure (Chat.ChunkedReply 100)
        , handleGetMessageContent = noopFetch
        , handleGetSenderMemberInfo = noopSenderMember
        , handleGetMemberInfo = noopMember
        , handleGetUserAvatar = noopUserAvatar
        , handleListGroupMembers = noopMembers
        , handleMentionUser = noopMention
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
  (responseId, result) <- runEff $
    Chat.runChatWith
      Chat.ChatHandlers
        { handleReplyTo = recordReply replies nextReplyId
        , handleEditMessage = recordEdit edits
        , handleDeleteMessage = noopDelete
        , handleReplyStreamStyle = \_ -> pure (Chat.EditableReply 2 4)
        , handleGetMessageContent = noopFetch
        , handleGetSenderMemberInfo = noopSenderMember
        , handleGetMemberInfo = noopMember
        , handleGetUserAvatar = noopUserAvatar
        , handleListGroupMembers = noopMembers
        , handleMentionUser = noopMention
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
testChunkedActiveConversationAliasesEverySentReply = runEff $ runTestLog $ StorageEffect.runStorageSQLitePath ":memory:" do
  store <- liftIO newConversationStore
  threadId <- liftIO $ forkIO (threadDelay 60_000_000)
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
        [ toolCall "call-1" "web_fetch" (Aeson.object ["url" Aeson..= ("https://example.test/1" :: Text)])
        , toolCall "call-2" "web_fetch" (Aeson.object ["url" Aeson..= ("https://example.test/2" :: Text)])
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
  { name = "web_fetch"
  , description = "fake web fetch"
  , parameters = Aeson.object []
  , noisy = False
  , allowed = const True
  , start = \context -> do
      checkUseLimit <- newUseLimiter context.toolConfig.webFetchMaxUses
      pure \_ -> do
        checkUseLimit >>= \case
          UseLimitReached currentUses ->
            pure (Agent.toolText [i|web_fetch use limit reached for this agent run: #{currentUses}.|])
          UseAllowed -> do
            liftIO $ IORef.modifyIORef' fetches (+ 1)
            pure (Agent.toolText "fetched")
  }

testConversationRepliesKeepSnapshots :: IO ()
testConversationRepliesKeepSnapshots = runEff $ runTestLog $ StorageEffect.runStorageSQLitePath ":memory:" do
  store <- liftIO newConversationStore
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
testConversationBranchesDoNotOverwriteSiblings = runEff $ runTestLog $ StorageEffect.runStorageSQLitePath ":memory:" do
  store <- liftIO newConversationStore
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
testConversationLookupIsScopedByChat = runEff $ runTestLog $ StorageEffect.runStorageSQLitePath ":memory:" do
  store <- liftIO newConversationStore
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
  withSQLiteTempPath "conversation-branches" \path -> runEff $ runTestLog do
    StorageEffect.runStorageSQLitePath path do
      store <- liftIO newConversationStore
      let root = appendAssistant "root answer" (startWithUser "root")
          branchA = appendAssistant "A answer" (appendUser "A follow-up" root)
          branchB = appendAssistant "B answer" (appendUser "B follow-up" root)
      rememberConversation store (Just (messageKey 1)) root
      rememberConversationFrom store (Just (messageKey 1)) (Just (messageKey 2)) branchA
      rememberConversationFrom store (Just (messageKey 1)) (Just (messageKey 3)) branchB

      reloaded <- liftIO newConversationStore
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
  withSQLiteTempPath "conversation-cache-miss" \path -> runEff $ runTestLog do
    StorageEffect.runStorageSQLitePath path do
      store <- liftIO newConversationStore
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

testConversationOmitsBase64GeneratedImageContext :: IO ()
testConversationOmitsBase64GeneratedImageContext = do
  let base64Image = "data:image/png;base64,AAAA"
      conversation = appendAssistant (ReplyBody.imageDirective base64Image) (startWithUser "draw")
      encoded = TextEncoding.decodeUtf8 (LazyByteString.toStrict (Aeson.encode conversation))
  imageContextUrls conversation @?= []
  assertBool "conversation history should not retain base64 image payloads" (not (base64Image `Text.isInfixOf` encoded))
  assertBool "conversation history should keep a small generated-image marker" ("Generated image." `Text.isInfixOf` encoded)

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
    [ chatAnswer "" [toolCall "call-1" "manage_current_sender_memory" (Aeson.object ["action" Aeson..= ("replace" :: Text), "memory" Aeson..= ("Prefers concise Chinese answers." :: Text)])]
    , chatAnswer "" [toolCall "call-2" "manage_current_sender_memory" (Aeson.object ["action" Aeson..= ("view" :: Text)])]
    , chatAnswer "" [toolCall "call-3" "manage_current_sender_memory" (Aeson.object ["action" Aeson..= ("clear" :: Text)])]
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
    [ chatAnswer "" [toolCall "call-1" "manage_current_chat_memory" (Aeson.object ["action" Aeson..= ("replace" :: Text), "memory" Aeson..= ("This chat prefers terse status updates." :: Text)])]
    , chatAnswer "" [toolCall "call-2" "manage_current_chat_memory" (Aeson.object ["action" Aeson..= ("view" :: Text)])]
    , chatAnswer "" [toolCall "call-3" "manage_current_chat_memory" (Aeson.object ["action" Aeson..= ("clear" :: Text)])]
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
    [ chatAnswer "" [toolCall "call-1" "manage_current_sender_memory" (Aeson.object ["action" Aeson..= ("replace" :: Text), "memory" Aeson..= longMemory])]
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
  LLM.llmExceptionSummary err @?= "ResponseTimeout"

withMemoryTempDir :: (FilePath -> IO a) -> IO a
withMemoryTempDir action = do
  withTempDir "memory-test" action

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir label action = do
  root <- getTemporaryDirectory
  unique <- hashUnique <$> newUnique
  let dir = root </> [i|cosmobot-#{label}-#{unique}|]
  Exception.bracket
    (createDirectory dir $> dir)
    removeDirectoryRecursive
    action

withSQLiteTempPath :: String -> (FilePath -> IO a) -> IO a
withSQLiteTempPath label action = do
  root <- getTemporaryDirectory
  unique <- hashUnique <$> newUnique
  let path = root </> [i|cosmobot-#{label}-#{unique}.sqlite|]
  Exception.bracket_
    (removeIfExists path)
    (removeIfExists path)
    (action path)

removeIfExists :: FilePath -> IO ()
removeIfExists path =
  removeFile path `Exception.catch` \(_ :: IOException) -> pure ()

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

runAgentWith
  :: IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWith answers chatMock action =
  runAgentWithMemory (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused") answers chatMock action

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
    (\prompt imageRefs maskRef -> do
        IORef.modifyIORef' editCalls (<> [ImageEditCall{prompt, imageRefs, maskRef}])
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
    (\prompt imageRefs maskRef -> do
        IORef.modifyIORef' editCalls (<> [ImageEditCall{prompt, imageRefs, maskRef}])
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
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageEdit
    memoryCfg
    skillsCfg
    rendered
    captured
    answers
    chatMock
    (\_ _ _ -> pure "unused image edit answer")
    action

runAgentWithMemorySkillsAndTypstAndCaptureAndImageEdit
  :: MemoryStore.MemoryConfig
  -> SkillsStore.SkillsConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [[LLM.ChatMessage]])
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> (Text -> [Text] -> Maybe Text -> IO Text)
  -> Eff AgentStack a
  -> IO a
runAgentWithMemorySkillsAndTypstAndCaptureAndImageEdit memoryCfg skillsCfg rendered captured answers chatMock imageEdit action = do
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageEditAndReferenced
    memoryCfg
    skillsCfg
    rendered
    captured
    answers
    chatMock
    Nothing
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
  -> (Text -> [Text] -> Maybe Text -> IO Text)
  -> Eff AgentStack a
  -> IO a
runAgentWithMemorySkillsAndTypstAndCaptureAndImageEditAndReferenced memoryCfg skillsCfg rendered captured answers chatMock referencedMessage imageEdit action = do
  runEff $
    runTestLog $
      StorageEffect.runStorageSQLitePath ":memory:" $
        Typst.runTypstWith (mockTypstRender rendered) $
          Scheduler.runScheduler $
            Memory.runMemory memoryCfg $
              Skills.runSkills skillsCfg $
                LLM.runLLMWith
                  (\messages -> captureMessages captured messages >> pure "unused text answer")
                  (\messages -> do
                      lift $ captureMessages captured messages
                      S.yield "unused text stream answer"
                      pure "unused text stream answer")
                  (\messages -> captureMessages captured messages >> pure "unused image answer")
                  (\prompt imageRefs maskRef -> liftIO (imageEdit prompt imageRefs maskRef))
                  (\_ messages -> captureMessages captured messages >> liftIO (popAnswer answers))
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
                          , handleEditMessage = noopEdit
                          , handleDeleteMessage = noopDelete
                          , handleReplyStreamStyle = noopReplyStreamStyle
                          , handleGetMessageContent = \_ _ -> pure referencedMessage
                          , handleGetSenderMemberInfo = noopSenderMember
                          , handleGetMemberInfo = noopMember
                          , handleGetUserAvatar = mockUserAvatar chatMock
                          , handleListGroupMembers = noopMembers
                          , handleMentionUser = noopMention
                          }
                        action

runAgentWithStreamingAnswers
  :: IORef.IORef [StreamingAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithStreamingAnswers answers chatMock action = do
  rendered <- IORef.newIORef ([] :: [Text])
  runEff $
    runTestLog $
      StorageEffect.runStorageSQLitePath ":memory:" $
        Typst.runTypstWith (mockTypstRender rendered) $
          Scheduler.runScheduler $
            Memory.runMemory (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused") $
              Skills.runSkills defaultTestSkillsConfig $
                LLM.runLLMWith
                  (\_ -> pure "unused text answer")
                  (\_ -> S.yield "unused text stream answer" $> "unused text stream answer")
                  (\_ -> pure "unused image answer")
                  (\_ _ _ -> pure "unused image edit answer")
                  (\_ _ -> (.answer) <$> liftIO (popStreamingAnswer answers))
                  (\_ _ -> do
                      streamingAnswer <- liftIO (popStreamingAnswer answers)
                      traverse_ S.yield streamingAnswer.chunks
                      pure streamingAnswer.answer) $
                  ChatLog.runChatLog $
                    AgentAudit.runAgentAudit $
                      Chat.runChatWith
                        Chat.ChatHandlers
                          { handleReplyTo = mockReply chatMock
                          , handleEditMessage = noopEdit
                          , handleDeleteMessage = noopDelete
                          , handleReplyStreamStyle = noopReplyStreamStyle
                          , handleGetMessageContent = noopFetch
                          , handleGetSenderMemberInfo = noopSenderMember
                          , handleGetMemberInfo = noopMember
                          , handleGetUserAvatar = mockUserAvatar chatMock
                          , handleListGroupMembers = noopMembers
                          , handleMentionUser = noopMention
                          }
                        action

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

runTestLog :: IOE :> es => Eff (Log : es) a -> Eff es a
runTestLog action = do
  logger <- liftIO $ mkLogger "agent-spec" \_ -> pure ()
  runLog "agent-spec" logger LogTrace action

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
    , remember = \_ _ -> pure ()
    , recordBotMessage = \_ _ -> pure ()
    }

superuserContext :: Agent.AgentContext es
superuserContext =
  agentContext{Agent.superuser = True}

agentContextWith :: IOE :> es => IORef.IORef [(Maybe MessageId, Text)] -> IORef.IORef [Maybe MessageId] -> Agent.AgentContext es
agentContextWith recorded remembered =
  agentContext
    { Agent.remember = \messageId _ -> liftIO $ IORef.modifyIORef' remembered (<> [messageId])
    , Agent.recordBotMessage = \messageId body -> liftIO $ IORef.modifyIORef' recorded (<> [(messageId, body)])
    }

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

noopMember :: IncomingMessage -> Integer -> Eff es (Maybe Aeson.Value)
noopMember _ _ =
  pure Nothing

noopUserAvatar :: IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
noopUserAvatar _ _ =
  pure Nothing

noopMembers :: IncomingMessage -> Eff es (Maybe Aeson.Value)
noopMembers _ =
  pure Nothing

noopMention :: IncomingMessage -> Integer -> Text -> Eff es (Maybe MessageId)
noopMention _ _ _ =
  pure Nothing

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

waitUntil :: IO Bool -> IO ()
waitUntil predicate =
  go (50 :: Int)
  where
    go 0 =
      assertFailure "timed out waiting for condition"
    go remaining = do
      done <- predicate
      unless done do
        threadDelay 20_000
        go (remaining - 1)
