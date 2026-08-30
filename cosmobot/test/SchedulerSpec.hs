{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import qualified Bot.Concurrency.Manager as ConcurrencyManager
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Storage as StorageEffect
import qualified Bot.Scheduler.Interpreter as SchedulerInterpreter
import qualified Bot.Storage.SQLite as StorageSQLite
import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Database.Selda.Backend as SeldaBackend
import qualified Database.Selda.SQLite as SeldaSQLite
import Effectful.Timeout (Timeout, runTimeout, timeout)
import qualified Streaming.Prelude as S
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "scheduler"
      [ testCase "scheduled messages are scoped by current user" testScheduledMessagesAreScopedByCurrentUser
      , testCase "administrative schedule list includes every owner" testListAllScheduledMessages
      , testCase "administrative schedule delete ignores owner" testDeleteScheduledMessageById
      , testCase "scheduled messages are scoped by current chat" testScheduledMessagesAreScopedByCurrentChat
      , testCase "username scopes schedules when sender id is absent" testUsernameScopedSchedule
      , testCase "schedule ids increase in insertion order" testScheduleIdsIncrease
      , testCase "scheduled stream yields original message" testScheduledStreamYieldsOriginalMessage
      , testCase "scheduled delivery can prepare messages" testScheduledDeliveryCanPrepareMessages
      , testCase "recurring schedules repeat until deleted" testRecurringSchedulesRepeatUntilDeleted
      , testCase "elapsed schedule leaves pending list" testElapsedScheduleLeavesPendingList
      , testCase "same due time yields messages in schedule id order" testSameDueTimeYieldsInScheduleIdOrder
      , testCase "deleted elapsed schedule is not delivered" testDeletedElapsedScheduleIsNotDelivered
      , testCase "pending schedules persist across scheduler restart" testPendingSchedulesPersistAcrossSchedulerRestart
      , testCase "recurring schedules persist across scheduler restart" testRecurringSchedulesPersistAcrossSchedulerRestart
      , testCase "elapsed schedules persist across scheduler restart" testElapsedSchedulesPersistAcrossSchedulerRestart
      , testCase "storage failures do not commit scheduler memory state" testStorageFailuresDoNotCommitSchedulerState
      ]

testScheduledMessagesAreScopedByCurrentUser :: IO ()
testScheduledMessagesAreScopedByCurrentUser = runSchedulerTest do
  _ <- Scheduler.scheduleOneShotMessage 60 (messageFrom "200" "!ask remind me")
  ownSchedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
  otherSchedules <- Scheduler.listScheduledMessages (messageFrom "201" "what schedules?")
  liftIO $ length ownSchedules @?= 1
  liftIO $ length otherSchedules @?= 0
  liftIO $ assertBool "remaining seconds are positive" (all ((> 0) . (.remainingSeconds)) ownSchedules)
  liftIO $ assertBool "remaining seconds do not exceed delay" (all ((<= 60) . (.remainingSeconds)) ownSchedules)

testListAllScheduledMessages :: IO ()
testListAllScheduledMessages = runSchedulerTest do
  _ <- Scheduler.scheduleOneShotMessage 60 (messageFrom "200" "first")
  _ <- Scheduler.scheduleOneShotMessage 60 (messageFrom "201" "second")
  schedules <- Scheduler.listAllScheduledMessages
  liftIO $ map ((.senderId) . (.message)) schedules @?= [Just "200", Just "201"]

testDeleteScheduledMessageById :: IO ()
testDeleteScheduledMessageById = runSchedulerTest do
  _ <- Scheduler.scheduleOneShotMessage 60 (messageFrom "200" "first")
  deleted <- Scheduler.deleteScheduledMessageById 1
  missing <- Scheduler.deleteScheduledMessageById 1
  schedules <- Scheduler.listAllScheduledMessages
  liftIO do
    deleted @?= True
    missing @?= False
    assertBool "deleted schedule leaves the administrative list" (null schedules)

testScheduledMessagesAreScopedByCurrentChat :: IO ()
testScheduledMessagesAreScopedByCurrentChat = runSchedulerTest do
  _ <- Scheduler.scheduleOneShotMessage 60 (messageFrom "200" "!ask private")
  schedules <- Scheduler.listScheduledMessages (messageFromChat "200" 101 "what schedules?")
  liftIO $ length schedules @?= 0

testUsernameScopedSchedule :: IO ()
testUsernameScopedSchedule = runSchedulerTest do
  _ <- Scheduler.scheduleOneShotMessage 60 (messageFromUsername "alice" "!ask by username")
  ownSchedules <- Scheduler.listScheduledMessages (messageFromUsername "alice" "what schedules?")
  otherSchedules <- Scheduler.listScheduledMessages (messageFromUsername "bob" "what schedules?")
  liftIO $ length ownSchedules @?= 1
  liftIO $ length otherSchedules @?= 0

testScheduleIdsIncrease :: IO ()
testScheduleIdsIncrease = runSchedulerTest do
  _ <- Scheduler.scheduleOneShotMessage 60 (messageFrom "200" "!ask first")
  _ <- Scheduler.scheduleOneShotMessage 60 (messageFrom "200" "!ask second")
  schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
  liftIO $ map (.scheduleId) schedules @?= [1, 2]

testScheduledStreamYieldsOriginalMessage :: IO ()
testScheduledStreamYieldsOriginalMessage = runSchedulerTest do
  let scheduled = messageFrom "200" "!ask now"
  _ <- Scheduler.scheduleOneShotMessage 0 scheduled
  delivered <- S.head_ Scheduler.scheduledMessages
  liftIO $ ((.text) <$> delivered) @?= Just scheduled.text

testScheduledDeliveryCanPrepareMessages :: IO ()
testScheduledDeliveryCanPrepareMessages = runSchedulerStorage $
  SchedulerInterpreter.runSchedulerWith (\message -> pure message{replyToMessageId = Just "prepared"}) do
    _ <- Scheduler.scheduleOneShotMessage 0 (messageFrom "200" "original")
    delivered <- S.head_ Scheduler.scheduledMessages
    liftIO $ ((.replyToMessageId) =<< delivered) @?= Just "prepared"

testRecurringSchedulesRepeatUntilDeleted :: IO ()
testRecurringSchedulesRepeatUntilDeleted = runSchedulerTest do
  let scheduled = messageFrom "200" "repeat"
  _ <- Scheduler.scheduleRecurringMessage 1 scheduled
  firstDelivery <- S.head_ Scheduler.scheduledMessages
  afterFirst <- Scheduler.listScheduledMessages scheduled
  secondDelivery <- S.head_ Scheduler.scheduledMessages
  threadDelay 1100000
  deleted <- Scheduler.deleteScheduledMessage scheduled 1
  afterDelete <- timeout 100000 (S.head_ Scheduler.scheduledMessages)
  remaining <- Scheduler.listScheduledMessages scheduled
  liftIO do
    map (fmap (.text)) [firstDelivery, secondDelivery] @?= [Just "repeat", Just "repeat"]
    map (.scheduleId) afterFirst @?= [1]
    map (.intervalSeconds) afterFirst @?= [Just 1]
    deleted @?= True
    assertBool "deleted recurring schedule is not delivered" (isNothing afterDelete)
    assertBool "deleted recurring schedule leaves no pending task" (null remaining)

testElapsedScheduleLeavesPendingList :: IO ()
testElapsedScheduleLeavesPendingList = runSchedulerTest do
  _ <- Scheduler.scheduleOneShotMessage 0 (messageFrom "200" "!ask now")
  _ <- S.head_ Scheduler.scheduledMessages
  schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
  liftIO $ length schedules @?= 0

testSameDueTimeYieldsInScheduleIdOrder :: IO ()
testSameDueTimeYieldsInScheduleIdOrder = runSchedulerTest do
  _ <- Scheduler.scheduleOneShotMessage 0 (messageFrom "200" "!ask first")
  _ <- Scheduler.scheduleOneShotMessage 0 (messageFrom "200" "!ask second")
  _ <- Scheduler.scheduleOneShotMessage 0 (messageFrom "200" "!ask third")
  firstMessage <- S.head_ Scheduler.scheduledMessages
  secondMessage <- S.head_ Scheduler.scheduledMessages
  thirdMessage <- S.head_ Scheduler.scheduledMessages
  liftIO $ map (fmap (.text)) [firstMessage, secondMessage, thirdMessage] @?= map Just ["!ask first", "!ask second", "!ask third"]

testDeletedElapsedScheduleIsNotDelivered :: IO ()
testDeletedElapsedScheduleIsNotDelivered = runSchedulerTest do
  _ <- Scheduler.scheduleOneShotMessage 60 (messageFrom "200" "!ask deleted")
  _ <- Scheduler.scheduleOneShotMessage 0 (messageFrom "200" "!ask delivered")
  deleted <- Scheduler.deleteScheduledMessage (messageFrom "200" "delete") 1
  delivered <- S.head_ Scheduler.scheduledMessages
  schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
  liftIO do
    deleted @?= True
    ((.text) <$> delivered) @?= Just "!ask delivered"
    map (.scheduleId) schedules @?= []

testPendingSchedulesPersistAcrossSchedulerRestart :: IO ()
testPendingSchedulesPersistAcrossSchedulerRestart = runSchedulerStorage do
  Scheduler.runScheduler do
    _ <- Scheduler.scheduleOneShotMessage 60 (messageFrom "200" "!ask persisted")
    pure ()
  Scheduler.runScheduler do
    schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
    liftIO do
      map (.scheduleId) schedules @?= [1]
      map ((.text) . (.message)) schedules @?= ["!ask persisted"]

testRecurringSchedulesPersistAcrossSchedulerRestart :: IO ()
testRecurringSchedulesPersistAcrossSchedulerRestart = runSchedulerStorage do
  Scheduler.runScheduler do
    _ <- Scheduler.scheduleRecurringMessage 60 (messageFrom "200" "recurring")
    pure ()
  Scheduler.runScheduler do
    schedules <- Scheduler.listScheduledMessages (messageFrom "200" "list")
    liftIO do
      map (.scheduleId) schedules @?= [1]
      map (.intervalSeconds) schedules @?= [Just 60]

testElapsedSchedulesPersistAcrossSchedulerRestart :: IO ()
testElapsedSchedulesPersistAcrossSchedulerRestart = runSchedulerStorage do
  Scheduler.runScheduler do
    _ <- Scheduler.scheduleOneShotMessage 0 (messageFrom "200" "!ask after restart")
    pure ()
  Scheduler.runScheduler do
    delivered <- S.head_ Scheduler.scheduledMessages
    schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
    liftIO do
      ((.text) <$> delivered) @?= Just "!ask after restart"
      length schedules @?= 0

testStorageFailuresDoNotCommitSchedulerState :: IO ()
testStorageFailuresDoNotCommitSchedulerState = runEff $
  withSQLiteConnection \connection ->
    runTimeout $
      runConcurrent $
        runPrim $
          startKatipE "scheduler-spec" "test" $
          ConcurrencyManager.runConcurrencyManager $
            StorageSQLite.runStorageSQLite connection $
              Scheduler.runScheduler do
                _ <- Scheduler.scheduleOneShotMessage 60 (messageFrom "200" "!ask persisted")
                liftIO (SeldaBackend.seldaClose connection)
                failedCreate <- trySync $
                  Scheduler.scheduleOneShotMessage 60 (messageFrom "200" "!ask phantom")
                failedDelete <- trySync $
                  Scheduler.deleteScheduledMessage (messageFrom "200" "delete") 1
                schedules <- Scheduler.listScheduledMessages (messageFrom "200" "list")
                liftIO do
                  assertBool "create should report the storage failure" (isLeft failedCreate)
                  assertBool "delete should report the storage failure" (isLeft failedDelete)
                  map (.scheduleId) schedules @?= [1]

runSchedulerTest
  :: Eff '[Scheduler.Scheduler, StorageEffect.Storage, Concurrency.Concurrency, KatipE, Prim, Concurrent, Timeout, IOE] a
  -> IO a
runSchedulerTest action =
  runSchedulerStorage (Scheduler.runScheduler action)

runSchedulerStorage
  :: Eff '[StorageEffect.Storage, Concurrency.Concurrency, KatipE, Prim, Concurrent, Timeout, IOE] a
  -> IO a
runSchedulerStorage action =
  runEff $
    ( runTimeout
    . runConcurrent
    . runPrim
    . startKatipE "scheduler-spec" "test"
    . ConcurrencyManager.runConcurrencyManager
    . StorageSQLite.runStorageSQLitePath ":memory:"
    ) action

withSQLiteConnection
  :: (SeldaBackend.SeldaConnection SeldaSQLite.SQLite -> Eff '[IOE] a)
  -> Eff '[IOE] a
withSQLiteConnection =
  bracket
    (liftIO (SeldaSQLite.sqliteOpen ":memory:"))
    (\connection -> void (trySync (liftIO (SeldaBackend.seldaClose connection))))

messageFrom :: Text -> Text -> IncomingMessage
messageFrom senderId text =
  messageFromChat senderId 100 text

messageFromChat :: Text -> Integer -> Text -> IncomingMessage
messageFromChat senderId chatId text =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just (integerChatId chatId)
    , chatAliases = []
    , chatDisplayName = Nothing
    , digest = emptyMessageDigest
    , senderId = Just senderId
    , senderUsername = Just "alice"
    , senderDisplayName = Nothing
    , senderGlobalDisplayName = Nothing
    , messageId = Just "300"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = text
    , raw = Aeson.Null
    }

messageFromUsername :: Text -> Text -> IncomingMessage
messageFromUsername username text =
  (messageFrom "200" text)
    { senderId = Nothing
    , senderUsername = Just username
    }
