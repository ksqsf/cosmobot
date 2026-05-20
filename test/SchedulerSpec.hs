module Main (main) where

import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Storage as StorageEffect
import qualified Bot.Storage.SQLite as StorageSQLite
import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import Effectful.Timeout (Timeout, runTimeout)
import qualified Streaming.Prelude as S
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "scheduler"
      [ testCase "scheduled messages are scoped by current user" testScheduledMessagesAreScopedByCurrentUser
      , testCase "scheduled messages are scoped by current chat" testScheduledMessagesAreScopedByCurrentChat
      , testCase "username scopes schedules when sender id is absent" testUsernameScopedSchedule
      , testCase "schedule ids increase in insertion order" testScheduleIdsIncrease
      , testCase "scheduled stream yields original message" testScheduledStreamYieldsOriginalMessage
      , testCase "elapsed schedule leaves pending list" testElapsedScheduleLeavesPendingList
      , testCase "same due time yields messages in schedule id order" testSameDueTimeYieldsInScheduleIdOrder
      , testCase "deleted elapsed schedule is not delivered" testDeletedElapsedScheduleIsNotDelivered
      , testCase "pending schedules persist across scheduler restart" testPendingSchedulesPersistAcrossSchedulerRestart
      , testCase "elapsed schedules persist across scheduler restart" testElapsedSchedulesPersistAcrossSchedulerRestart
      ]

testScheduledMessagesAreScopedByCurrentUser :: IO ()
testScheduledMessagesAreScopedByCurrentUser = runSchedulerTest do
  _ <- Scheduler.scheduleMessage 60 (messageFrom "200" "!ask remind me")
  ownSchedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
  otherSchedules <- Scheduler.listScheduledMessages (messageFrom "201" "what schedules?")
  liftIO $ length ownSchedules @?= 1
  liftIO $ length otherSchedules @?= 0
  liftIO $ assertBool "remaining seconds are positive" (all ((> 0) . (.remainingSeconds)) ownSchedules)
  liftIO $ assertBool "remaining seconds do not exceed delay" (all ((<= 60) . (.remainingSeconds)) ownSchedules)

testScheduledMessagesAreScopedByCurrentChat :: IO ()
testScheduledMessagesAreScopedByCurrentChat = runSchedulerTest do
  _ <- Scheduler.scheduleMessage 60 (messageFrom "200" "!ask private")
  schedules <- Scheduler.listScheduledMessages (messageFromChat "200" 101 "what schedules?")
  liftIO $ length schedules @?= 0

testUsernameScopedSchedule :: IO ()
testUsernameScopedSchedule = runSchedulerTest do
  _ <- Scheduler.scheduleMessage 60 (messageFromUsername "alice" "!ask by username")
  ownSchedules <- Scheduler.listScheduledMessages (messageFromUsername "alice" "what schedules?")
  otherSchedules <- Scheduler.listScheduledMessages (messageFromUsername "bob" "what schedules?")
  liftIO $ length ownSchedules @?= 1
  liftIO $ length otherSchedules @?= 0

testScheduleIdsIncrease :: IO ()
testScheduleIdsIncrease = runSchedulerTest do
  _ <- Scheduler.scheduleMessage 60 (messageFrom "200" "!ask first")
  _ <- Scheduler.scheduleMessage 60 (messageFrom "200" "!ask second")
  schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
  liftIO $ map (.scheduleId) schedules @?= [1, 2]

testScheduledStreamYieldsOriginalMessage :: IO ()
testScheduledStreamYieldsOriginalMessage = runSchedulerTest do
  let scheduled = messageFrom "200" "!ask now"
  _ <- Scheduler.scheduleMessage 0 scheduled
  delivered <- S.head_ Scheduler.scheduledMessages
  liftIO $ ((.text) <$> delivered) @?= Just scheduled.text

testElapsedScheduleLeavesPendingList :: IO ()
testElapsedScheduleLeavesPendingList = runSchedulerTest do
  _ <- Scheduler.scheduleMessage 0 (messageFrom "200" "!ask now")
  _ <- S.head_ Scheduler.scheduledMessages
  schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
  liftIO $ length schedules @?= 0

testSameDueTimeYieldsInScheduleIdOrder :: IO ()
testSameDueTimeYieldsInScheduleIdOrder = runSchedulerTest do
  _ <- Scheduler.scheduleMessage 0 (messageFrom "200" "!ask first")
  _ <- Scheduler.scheduleMessage 0 (messageFrom "200" "!ask second")
  _ <- Scheduler.scheduleMessage 0 (messageFrom "200" "!ask third")
  firstMessage <- S.head_ Scheduler.scheduledMessages
  secondMessage <- S.head_ Scheduler.scheduledMessages
  thirdMessage <- S.head_ Scheduler.scheduledMessages
  liftIO $ map (fmap (.text)) [firstMessage, secondMessage, thirdMessage] @?= map Just ["!ask first", "!ask second", "!ask third"]

testDeletedElapsedScheduleIsNotDelivered :: IO ()
testDeletedElapsedScheduleIsNotDelivered = runSchedulerTest do
  _ <- Scheduler.scheduleMessage 60 (messageFrom "200" "!ask deleted")
  _ <- Scheduler.scheduleMessage 0 (messageFrom "200" "!ask delivered")
  deleted <- Scheduler.deleteScheduledMessage (messageFrom "200" "delete") 1
  delivered <- S.head_ Scheduler.scheduledMessages
  schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
  liftIO do
    deleted @?= True
    ((.text) <$> delivered) @?= Just "!ask delivered"
    map (.scheduleId) schedules @?= []

testPendingSchedulesPersistAcrossSchedulerRestart :: IO ()
testPendingSchedulesPersistAcrossSchedulerRestart = runEff $ runTimeout $ runConcurrent $ StorageSQLite.runStorageSQLitePath ":memory:" do
  Scheduler.runScheduler do
    _ <- Scheduler.scheduleMessage 60 (messageFrom "200" "!ask persisted")
    pure ()
  Scheduler.runScheduler do
    schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
    liftIO do
      map (.scheduleId) schedules @?= [1]
      map ((.text) . (.message)) schedules @?= ["!ask persisted"]

testElapsedSchedulesPersistAcrossSchedulerRestart :: IO ()
testElapsedSchedulesPersistAcrossSchedulerRestart = runEff $ runTimeout $ runConcurrent $ StorageSQLite.runStorageSQLitePath ":memory:" do
  Scheduler.runScheduler do
    _ <- Scheduler.scheduleMessage 0 (messageFrom "200" "!ask after restart")
    pure ()
  Scheduler.runScheduler do
    delivered <- S.head_ Scheduler.scheduledMessages
    schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
    liftIO do
      ((.text) <$> delivered) @?= Just "!ask after restart"
      length schedules @?= 0

runSchedulerTest
  :: Eff '[Scheduler.Scheduler, StorageEffect.Storage, Concurrent, Timeout, IOE] a
  -> IO a
runSchedulerTest action =
  runEff $ runTimeout $ runConcurrent $ StorageSQLite.runStorageSQLitePath ":memory:" $ Scheduler.runScheduler action

messageFrom :: Text -> Text -> IncomingMessage
messageFrom senderId text =
  messageFromChat senderId 100 text

messageFromChat :: Text -> Integer -> Text -> IncomingMessage
messageFromChat senderId chatId text =
  IncomingMessage
    { platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just chatId
    , chatAliases = []
    , digest = emptyMessageDigest
    , senderId = Just senderId
    , senderUsername = Just "alice"
    , messageId = Just "300"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , text = text
    , raw = Aeson.Null
    }

messageFromUsername :: Text -> Text -> IncomingMessage
messageFromUsername username text =
  (messageFrom "200" text)
    { senderId = Nothing
    , senderUsername = Just username
    }
