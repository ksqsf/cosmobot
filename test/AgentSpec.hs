module Main (main) where

import qualified Bot.Agent as Agent
import Bot.Agent.Tools.Common (UseLimit (..), newUseLimiter)
import Bot.Core.Conversation
import qualified Bot.Effect.AgentTrace as AgentTrace
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Memory as MemoryStore
import qualified Bot.Storage.SQLite as Storage
import Bot.Core.Message
import Bot.Prelude
import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Unique
import qualified Streaming.Prelude as S
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

type AgentStack =
  '[ Chat.Chat
   , AgentTrace.AgentTrace
   , ChatLog.ChatLog
   , LLM.LLM
   , Memory.Memory
   , Scheduler.Scheduler
   , Log
   , IOE
   ]

data ChatMock = ChatMock
  { replies :: !(Maybe (IORef.IORef [Text]))
  , replyId :: !(Maybe Integer)
  }

main :: IO ()
main =
  defaultMain $
    testGroup "agent"
      [ testCase "schedule tool creates a queryable pending schedule" testScheduleToolCreatesQueryableSchedule
      , testCase "send reply tool uses chat effect and records bot message" testSendReplyToolUsesChatEffect
      , testCase "agent streaming yields answer chunks" testAgentStreamingYieldsAnswerChunks
      , testCase "agent streaming yields tool request content" testAgentStreamingYieldsToolRequestContent
      , testCase "agent trace records model and tool events" testAgentTraceRecordsModelAndToolEvents
      , testCase "chat answer JSON remains object compatible" testChatAnswerJsonRemainsObjectCompatible
      , testCase "chat streaming chunks replies and yields updates" testChatStreamingChunksRepliesAndYieldsUpdates
      , testCase "chunked active conversation aliases every sent reply" testChunkedActiveConversationAliasesEverySentReply
      , testCase "web_fetch max_uses limits fetch calls" testWebFetchMaxUsesLimitsCalls
      , testCase "conversation replies keep parent and child snapshots" testConversationRepliesKeepSnapshots
      , testCase "conversation branches do not overwrite siblings" testConversationBranchesDoNotOverwriteSiblings
      , testCase "conversation branches persist through SQLite reload" testConversationBranchesPersistThroughSQLiteReload
      , testCase "conversation cache miss loads evicted parent from SQLite" testConversationCacheMissLoadsEvictedParent
      , testCase "conversation JSON remains list compatible" testConversationJsonRemainsListCompatible
      , testCase "memory tool manages current sender memory" testMemoryToolManagesCurrentSenderMemory
      , testCase "memory tool manages current chat memory" testMemoryToolManagesCurrentChatMemory
      , testCase "memory tool enforces non-superuser length limit" testMemoryToolEnforcesLengthLimit
      , testCase "run_bash captures stdout and stderr" testRunBashCapturesStdoutAndStderr
      , testCase "run_bash kills timed out process" testRunBashKillsTimedOutProcess
      ]

testScheduleToolCreatesQueryableSchedule :: IO ()
testScheduleToolCreatesQueryableSchedule = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "schedule_agent_action" (Aeson.object ["delay_seconds" Aeson..= (60 :: Int), "prompt" Aeson..= ("check oven" :: Text)])]
    , chatAnswer "scheduled" []
    ]
  (answer, schedules) <- runAgentWith answers (ChatMock Nothing Nothing) do
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
  recorded <- IORef.newIORef ([] :: [(Maybe Integer, Text)])
  remembered <- IORef.newIORef ([] :: [Maybe Integer])
  (answer, _) <- runAgentWith answers (ChatMock (Just replies) (Just 42)) do
    Agent.runAgent 4 (agentContextWith recorded remembered) Agent.defaultTools (startWithUser "send it")
  answer @?= "sent"
  IORef.readIORef replies >>= (@?= ["hello\n[image] https://example.test/image.png"])
  IORef.readIORef recorded >>= (@?= [(Just 42, "hello\n[image] https://example.test/image.png")])
  IORef.readIORef remembered >>= (@?= [Just 42])

testAgentStreamingYieldsAnswerChunks :: IO ()
testAgentStreamingYieldsAnswerChunks = do
  answers <- IORef.newIORef [chatAnswer "streamed answer" []]
  chunks <- IORef.newIORef ([] :: [Text])
  (answer, _) <- runAgentWith answers (ChatMock Nothing Nothing) do
    S.mapM_
      (\chunk -> liftIO $ IORef.modifyIORef' chunks (<> [chunk]))
      (Agent.runAgentStreaming 4 agentContext Agent.defaultTools (startWithUser "stream it"))
  answer @?= "streamed answer"
  IORef.readIORef chunks >>= (@?= ["streamed answer"])

testAgentStreamingYieldsToolRequestContent :: IO ()
testAgentStreamingYieldsToolRequestContent = do
  answers <- IORef.newIORef
    [ chatAnswer "checking" [toolCall "call-1" "web_fetch" (Aeson.object ["url" Aeson..= ("https://example.test" :: Text)])]
    , chatAnswer "done" []
    ]
  fetches <- IORef.newIORef (0 :: Int)
  chunks <- IORef.newIORef ([] :: [Text])
  (answer, _) <- runAgentWith answers (ChatMock Nothing Nothing) do
    S.mapM_
      (\chunk -> liftIO $ IORef.modifyIORef' chunks (<> [chunk]))
      (Agent.runAgentStreaming 4 (agentContext{Agent.toolConfig = Agent.defaultToolConfig{Agent.webFetch = True}}) [fakeWebFetchTool fetches] (startWithUser "fetch it"))
  answer @?= "done"
  IORef.readIORef fetches >>= (@?= 1)
  IORef.readIORef chunks >>= (@?= ["checking", "done"])

testAgentTraceRecordsModelAndToolEvents :: IO ()
testAgentTraceRecordsModelAndToolEvents = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "web_fetch" (Aeson.object ["url" Aeson..= ("https://example.test" :: Text)])]
    , chatAnswer "done" []
    ]
  fetches <- IORef.newIORef (0 :: Int)
  events <- runAgentWith answers (ChatMock Nothing Nothing) do
    _ <- Agent.runAgent 4 (agentContext{Agent.toolConfig = Agent.defaultToolConfig{Agent.webFetch = True}}) [fakeWebFetchTool fetches] (startWithUser "fetch it")
    allEvents <- AgentTrace.queryAll
    pure allEvents
  map traceEventName events @?=
    [ "AgentRunStarted"
    , "ModelTurnStarted"
    , "ModelTurnFinished"
    , "ToolCallStarted"
    , "ToolCallFinished"
    , "ModelTurnStarted"
    , "ModelTurnFinished"
    , "AgentRunFinished"
    ]
  [status | AgentTrace.ToolCallFinished{status} <- events] @?= ["ok"]

traceEventName :: AgentTrace.AgentTraceEvent -> Text
traceEventName = \case
  AgentTrace.AgentRunStarted{} -> "AgentRunStarted"
  AgentTrace.ModelTurnStarted{} -> "ModelTurnStarted"
  AgentTrace.ModelTurnFinished{} -> "ModelTurnFinished"
  AgentTrace.ToolCallStarted{} -> "ToolCallStarted"
  AgentTrace.ToolCallFinished{} -> "ToolCallFinished"
  AgentTrace.AgentRunFinished{} -> "AgentRunFinished"
  AgentTrace.AgentConversationLinked{} -> "AgentConversationLinked"

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

testChatStreamingChunksRepliesAndYieldsUpdates :: IO ()
testChatStreamingChunksRepliesAndYieldsUpdates = do
  replies <- IORef.newIORef ([] :: [(Maybe Integer, Text)])
  updates <- IORef.newIORef ([] :: [(Maybe Integer, [Integer], Text)])
  nextReplyId <- IORef.newIORef (1 :: Integer)
  (responseId, result) <- runEff $
    Chat.runChatWith
      Chat.ChatHandlers
        { handleReplyTo = recordReply replies nextReplyId
        , handleEditMessage = noopEdit
        , handleReplyStreamStyle = \_ -> pure (Chat.ChunkedReply 4)
        , handleGetMessageContent = noopFetch
        , handleGetSenderMemberInfo = noopSenderMember
        , handleGetMemberInfo = noopMember
        , handleListGroupMembers = noopMembers
        , handleMentionUser = noopMention
        } $
        S.mapM_
          (\update -> liftIO $ IORef.modifyIORef' updates (<> [(update.responseId, update.sentResponseIds, update.answer)]))
          (Chat.streamReplyTo testMessage id (S.each ["ab", "cd", "ef"] $> "abcdef"))
  responseId @?= Just 1
  result @?= "abcdef"
  IORef.readIORef replies >>= (@?= [(Just 300, "abcd"), (Just 1, "ef")])
  IORef.readIORef updates >>= (@?= [(Nothing, [], "ab"), (Just 1, [1], "abcd"), (Just 1, [], "abcdef"), (Just 1, [2], "abcdef")])

testChunkedActiveConversationAliasesEverySentReply :: IO ()
testChunkedActiveConversationAliasesEverySentReply = runEff $ runTestLog do
  store <- liftIO (newConversationStore Nothing)
  threadId <- liftIO $ forkIO (threadDelay 60_000_000)
  let baseConversation = startWithUser "hello"
      partialConversation = appendAssistant "partial answer" baseConversation
  active <- fromMaybe (error "expected active conversation") <$> rememberActiveConversation store Nothing (Just 1) threadId baseConversation
  addActiveConversationMessage store active 2
  updateActiveConversation active partialConversation
  halted <- haltConversation store 2
  firstLookup <- lookupConversation store 1
  secondLookup <- lookupConversation store 2
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
  (answer, _) <- runAgentWith answers (ChatMock Nothing Nothing) do
    Agent.runAgent 4 (agentContext{Agent.toolConfig = Agent.defaultToolConfig{Agent.webFetch = True, Agent.webFetchMaxUses = Just 1}}) [fakeWebFetchTool fetches] (startWithUser "fetch twice")
  answer @?= "done"
  IORef.readIORef fetches >>= (@?= 1)

fakeWebFetchTool :: IOE :> es => IORef.IORef Int -> Agent.Tool es
fakeWebFetchTool fetches = Agent.Tool
  { name = "web_fetch"
  , description = "fake web fetch"
  , parameters = Aeson.object []
  , allowed = const True
  , start = \context -> do
      checkUseLimit <- newUseLimiter context.toolConfig.webFetchMaxUses
      pure \_ -> do
        checkUseLimit >>= \case
          UseLimitReached currentUses ->
            pure (Agent.ToolResult [i|web_fetch use limit reached for this agent run: #{currentUses}.|] [])
          UseAllowed -> do
            liftIO $ IORef.modifyIORef' fetches (+ 1)
            pure (Agent.ToolResult "fetched" [])
  }

testConversationRepliesKeepSnapshots :: IO ()
testConversationRepliesKeepSnapshots = runEff $ runTestLog do
  store <- liftIO (newConversationStore Nothing)
  let firstConversation = startWithUser "first"
      secondConversation = appendAssistant "second" firstConversation
  rememberConversation store (Just 1) firstConversation
  rememberConversationFrom store (Just 1) (Just 2) secondConversation
  firstLookup <- lookupConversation store 1
  secondLookup <- lookupConversation store 2
  liftIO do
    (show firstLookup :: String) @?= show (Just firstConversation)
    (show secondLookup :: String) @?= show (Just secondConversation)

testConversationBranchesDoNotOverwriteSiblings :: IO ()
testConversationBranchesDoNotOverwriteSiblings = runEff $ runTestLog do
  store <- liftIO (newConversationStore Nothing)
  let root = appendAssistant "root answer" (startWithUser "root")
      branchA = appendAssistant "A answer" (appendUser "A follow-up" root)
      branchB = appendAssistant "B answer" (appendUser "B follow-up" root)
      branchA2 = appendAssistant "A second answer" (appendUser "A second follow-up" branchA)
  rememberConversation store (Just 1) root
  rememberConversationFrom store (Just 1) (Just 2) branchA
  rememberConversationFrom store (Just 1) (Just 3) branchB
  rememberConversationFrom store (Just 2) (Just 4) branchA2
  rootLookup <- lookupConversation store 1
  branchALookup <- lookupConversation store 2
  branchBLookup <- lookupConversation store 3
  branchA2Lookup <- lookupConversation store 4
  liftIO do
    (show rootLookup :: String) @?= show (Just root)
    (show branchALookup :: String) @?= show (Just branchA)
    (show branchBLookup :: String) @?= show (Just branchB)
    (show branchA2Lookup :: String) @?= show (Just branchA2)

testConversationBranchesPersistThroughSQLiteReload :: IO ()
testConversationBranchesPersistThroughSQLiteReload =
  withSQLiteTempPath "conversation-branches" \path -> runEff $ runTestLog do
    sqliteStore <- liftIO (Storage.openSQLiteStore path)
    store <- liftIO (newConversationStore (Just sqliteStore))
    let root = appendAssistant "root answer" (startWithUser "root")
        branchA = appendAssistant "A answer" (appendUser "A follow-up" root)
        branchB = appendAssistant "B answer" (appendUser "B follow-up" root)
    rememberConversation store (Just 1) root
    rememberConversationFrom store (Just 1) (Just 2) branchA
    rememberConversationFrom store (Just 1) (Just 3) branchB

    reloaded <- liftIO (newConversationStore (Just sqliteStore))
    branchAAfterReload <- lookupConversation reloaded 2
    branchBAfterReload <- lookupConversation reloaded 3
    let branchA2 = appendAssistant "A second answer" (appendUser "A second follow-up" branchA)
    rememberConversationFrom reloaded (Just 2) (Just 4) branchA2
    rows <- liftIO (Storage.loadConversationRows sqliteStore)
    branchA2AfterReload <- lookupConversation reloaded 4

    liftIO do
      (show branchAAfterReload :: String) @?= show (Just branchA)
      (show branchBAfterReload :: String) @?= show (Just branchB)
      (show branchA2AfterReload :: String) @?= show (Just branchA2)
      map (.messageId) rows @?= [1, 2, 3, 4]
      map (.parentMessageId) rows @?= [Nothing, Just 1, Just 1, Just 2]
      map (.payloadKind) rows @?= replicate 4 Storage.ConversationPayloadMessages
      map payloadMessageCount rows @?= [2, 2, 2, 2]
      assertBool "all nodes in the reloaded tree keep the same conversation id" (sameConversationIds rows)

testConversationCacheMissLoadsEvictedParent :: IO ()
testConversationCacheMissLoadsEvictedParent =
  withSQLiteTempPath "conversation-cache-miss" \path -> runEff $ runTestLog do
    sqliteStore <- liftIO (Storage.openSQLiteStore path)
    store <- liftIO (newConversationStore (Just sqliteStore))
    let root = appendAssistant "root answer" (startWithUser "root")
        child = appendAssistant "child answer" (appendUser "child follow-up" root)
    rememberConversation store (Just 1) root
    for_ [1000..1512] \messageId ->
      rememberConversation store (Just messageId) (startWithUser [i|filler #{messageId}|])
    rememberConversationFrom store (Just 1) (Just 2) child
    rootLookup <- lookupConversation store 1
    childLookup <- lookupConversation store 2
    rows <- liftIO (Storage.loadConversationRows sqliteStore)
    let childRow = find ((== 2) . (.messageId)) rows
    liftIO do
      (show rootLookup :: String) @?= show (Just root)
      (show childLookup :: String) @?= show (Just child)
      ((.parentMessageId) <$> childRow) @?= Just (Just 1)
      (payloadMessageCount <$> childRow) @?= Just 2

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
  (answer, _) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing) do
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
  (answer, _) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing) do
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
  (answer, _) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing) do
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
  (answer, conversation) <- runAgentWith answers (ChatMock Nothing Nothing) do
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
  (answer, conversation) <- runAgentWith answers (ChatMock Nothing Nothing) do
    Agent.runAgent 4 superuserContext Agent.defaultTools (startWithUser "run slow command")
  answer @?= "done"
  let output = Text.unlines (toolOutputs conversation)
  assertBool ("timeout is reported in: " <> Text.unpack output) ("Script timed out after 1 seconds and was killed." `Text.isInfixOf` output)
  assertBool ("post-timeout output is not included in: " <> Text.unpack output) (not ("late" `Text.isInfixOf` output))

withMemoryTempDir :: (FilePath -> IO a) -> IO a
withMemoryTempDir action = do
  root <- getTemporaryDirectory
  unique <- hashUnique <$> newUnique
  let dir = root </> [i|cosmobot-memory-test-#{unique}|]
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

sameConversationIds :: [Storage.ConversationRow] -> Bool
sameConversationIds rows =
  case map (.conversationId) rows of
    [] ->
      True
    firstId : rest ->
      isJust firstId && all (== firstId) rest

payloadMessageCount :: Storage.ConversationRow -> Int
payloadMessageCount row =
  case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 row.payloadJson) :: Either String [LLM.ChatMessage] of
    Left err ->
      error (Text.pack err)
    Right messages ->
      length messages

toolOutputs :: Conversation -> [Text]
toolOutputs (Conversation messages) =
  [ text
  | message <- Foldable.toList messages
  , message.role == "tool"
  , Just (LLM.TextContent text) <- [message.content]
  ]

runAgentWith
  :: IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWith answers chatMock action =
  runAgentWithMemory (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused") answers chatMock action

runAgentWithMemory
  :: MemoryStore.MemoryConfig
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithMemory memoryCfg answers chatMock action =
  runEff $
  runTestLog $
    Scheduler.runScheduler $
      Memory.runMemory memoryCfg $
        LLM.runLLMWith
          (\_ -> pure "unused text answer")
          (\_ emit -> liftIO (emit "unused text stream answer") $> "unused text stream answer")
          (\_ -> pure "unused image answer")
          (\_ _ -> liftIO (popAnswer answers))
          (\_ _ emit -> do
              answer <- liftIO (popAnswer answers)
              case answer of
                LLM.ChatFinalAnswer{content} ->
                  liftIO (emit content)
                LLM.ChatToolRequest{} ->
                  pure ()
              pure answer) $
          ChatLog.runChatLog Nothing $
            AgentTrace.runAgentTrace Nothing $
              Chat.runChatWith
                Chat.ChatHandlers
                  { handleReplyTo = mockReply chatMock
                  , handleEditMessage = noopEdit
                  , handleReplyStreamStyle = noopReplyStreamStyle
                  , handleGetMessageContent = noopFetch
                  , handleGetSenderMemberInfo = noopSenderMember
                  , handleGetMemberInfo = noopMember
                  , handleListGroupMembers = noopMembers
                  , handleMentionUser = noopMention
                  }
                action

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
    , superuser = False
    , askCommand = "!ask"
    , toolConfig = Agent.defaultToolConfig
    , recordRunId = \_ -> pure ()
    , remember = \_ _ -> pure ()
    , recordBotMessage = \_ _ -> pure ()
    }

superuserContext :: Agent.AgentContext es
superuserContext =
  agentContext{Agent.superuser = True}

agentContextWith :: IOE :> es => IORef.IORef [(Maybe Integer, Text)] -> IORef.IORef [Maybe Integer] -> Agent.AgentContext es
agentContextWith recorded remembered =
  agentContext
    { Agent.remember = \messageId _ -> liftIO $ IORef.modifyIORef' remembered (<> [messageId])
    , Agent.recordBotMessage = \messageId body -> liftIO $ IORef.modifyIORef' recorded (<> [(messageId, body)])
    }

mockReply :: IOE :> es => ChatMock -> IncomingMessage -> Text -> Eff es (Maybe Integer)
mockReply ChatMock{replies, replyId} _ body = do
  traverse_ (\ref -> liftIO $ IORef.modifyIORef' ref (<> [body])) replies
  pure replyId

recordReply :: IOE :> es => IORef.IORef [(Maybe Integer, Text)] -> IORef.IORef Integer -> IncomingMessage -> Text -> Eff es (Maybe Integer)
recordReply replies nextReplyId message body = do
  liftIO $ IORef.modifyIORef' replies (<> [(message.messageId, body)])
  liftIO $ IORef.atomicModifyIORef' nextReplyId \replyId ->
    (replyId + 1, Just replyId)

noopFetch :: IncomingMessage -> Integer -> Eff es (Maybe ReferencedMessage)
noopFetch _ _ =
  pure Nothing

noopEdit :: IncomingMessage -> Integer -> Text -> Eff es Bool
noopEdit _ _ _ =
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

noopMembers :: IncomingMessage -> Eff es (Maybe Aeson.Value)
noopMembers _ =
  pure Nothing

noopMention :: IncomingMessage -> Integer -> Text -> Eff es (Maybe Integer)
noopMention _ _ _ =
  pure Nothing

testMessage :: IncomingMessage
testMessage =
  IncomingMessage
    { platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just 100
    , chatAliases = []
    , digest = emptyMessageDigest
    , senderId = Just 200
    , senderUsername = Just "alice"
    , messageId = Just 300
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , text = "!ask"
    , raw = Aeson.Null
    }

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  decodeUtf8 . toStrict . Aeson.encode
