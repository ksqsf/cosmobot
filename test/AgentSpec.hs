module Main (main) where

import qualified Bot.Agent as Agent
import Bot.Conversation
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Scheduler as Scheduler
import Bot.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import Test.Tasty
import Test.Tasty.HUnit

type AgentStack =
  '[ Chat.Chat
   , ChatLog.ChatLog
   , LLM.LLM
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
      , testCase "conversation replies share latest context" testConversationRepliesShareLatestContext
      ]

testScheduleToolCreatesQueryableSchedule :: IO ()
testScheduleToolCreatesQueryableSchedule = do
  answers <- IORef.newIORef
    [ LLM.ChatAnswer "" [toolCall "call-1" "schedule_agent_action" (Aeson.object ["delay_seconds" Aeson..= (60 :: Int), "prompt" Aeson..= ("check oven" :: Text)])]
    , LLM.ChatAnswer "scheduled" []
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
    [ LLM.ChatAnswer "" [toolCall "call-1" "send_reply_to_current_chat" (Aeson.object ["text" Aeson..= ("hello" :: Text), "image_urls" Aeson..= ["https://example.test/image.png" :: Text]])]
    , LLM.ChatAnswer "sent" []
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

testConversationRepliesShareLatestContext :: IO ()
testConversationRepliesShareLatestContext = runEff $ runTestLog do
  store <- liftIO (newConversationStore Nothing)
  let firstConversation = startWithUser "first"
      secondConversation = appendAssistant "second" firstConversation
  rememberConversation store (Just 1) firstConversation
  rememberConversationFrom store (Just 1) (Just 2) secondConversation
  firstLookup <- lookupConversation store 1
  secondLookup <- lookupConversation store 2
  liftIO do
    (show firstLookup :: String) @?= show (Just secondConversation)
    (show secondLookup :: String) @?= show (Just secondConversation)

runAgentWith
  :: IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWith answers chatMock action =
  runEff $
  runTestLog $
    Scheduler.runScheduler $
      LLM.runLLMWith
        (\_ -> pure "unused text answer")
        (\_ -> pure "unused image answer")
        (\_ _ -> liftIO (popAnswer answers)) $
        ChatLog.runChatLog Nothing $
          Chat.runChatWith (mockReply chatMock) noopFetch noopSenderMember noopMember noopMembers noopMention action

runTestLog :: IOE :> es => Eff (Log : es) a -> Eff es a
runTestLog action = do
  logger <- liftIO $ mkLogger "agent-spec" \_ -> pure ()
  runLog "agent-spec" logger LogTrace action

popAnswer :: IORef.IORef [LLM.ChatAnswer] -> IO LLM.ChatAnswer
popAnswer answers =
  IORef.atomicModifyIORef' answers \case
    [] ->
      ([], LLM.ChatAnswer "unexpected extra LLM call" [])
    answer : rest ->
      (rest, answer)

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
    , remember = \_ _ -> pure ()
    , recordBotMessage = \_ _ -> pure ()
    }

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

noopFetch :: IncomingMessage -> Integer -> Eff es (Maybe ReferencedMessage)
noopFetch _ _ =
  pure Nothing

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
