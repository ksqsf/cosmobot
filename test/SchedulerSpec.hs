module Main (main) where

import qualified Bot.Effect.Scheduler as Scheduler
import Bot.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Streaming.Prelude as S
import Test.Tasty
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
      ]

testScheduledMessagesAreScopedByCurrentUser :: IO ()
testScheduledMessagesAreScopedByCurrentUser = runEff $ Scheduler.runScheduler do
  _ <- Scheduler.scheduleMessage 60 (messageFrom 200 "!ask remind me")
  ownSchedules <- Scheduler.listScheduledMessages (messageFrom 200 "what schedules?")
  otherSchedules <- Scheduler.listScheduledMessages (messageFrom 201 "what schedules?")
  liftIO $ length ownSchedules @?= 1
  liftIO $ length otherSchedules @?= 0
  liftIO $ assertBool "remaining seconds are positive" (all ((> 0) . (.remainingSeconds)) ownSchedules)
  liftIO $ assertBool "remaining seconds do not exceed delay" (all ((<= 60) . (.remainingSeconds)) ownSchedules)

testScheduledMessagesAreScopedByCurrentChat :: IO ()
testScheduledMessagesAreScopedByCurrentChat = runEff $ Scheduler.runScheduler do
  _ <- Scheduler.scheduleMessage 60 (messageFrom 200 "!ask private")
  schedules <- Scheduler.listScheduledMessages (messageFromChat 200 101 "what schedules?")
  liftIO $ length schedules @?= 0

testUsernameScopedSchedule :: IO ()
testUsernameScopedSchedule = runEff $ Scheduler.runScheduler do
  _ <- Scheduler.scheduleMessage 60 (messageFromUsername "alice" "!ask by username")
  ownSchedules <- Scheduler.listScheduledMessages (messageFromUsername "alice" "what schedules?")
  otherSchedules <- Scheduler.listScheduledMessages (messageFromUsername "bob" "what schedules?")
  liftIO $ length ownSchedules @?= 1
  liftIO $ length otherSchedules @?= 0

testScheduleIdsIncrease :: IO ()
testScheduleIdsIncrease = runEff $ Scheduler.runScheduler do
  _ <- Scheduler.scheduleMessage 60 (messageFrom 200 "!ask first")
  _ <- Scheduler.scheduleMessage 60 (messageFrom 200 "!ask second")
  schedules <- Scheduler.listScheduledMessages (messageFrom 200 "what schedules?")
  liftIO $ map (.scheduleId) schedules @?= [1, 2]

testScheduledStreamYieldsOriginalMessage :: IO ()
testScheduledStreamYieldsOriginalMessage = runEff $ Scheduler.runScheduler do
  let scheduled = messageFrom 200 "!ask now"
  _ <- Scheduler.scheduleMessage 0 scheduled
  delivered <- S.head_ Scheduler.scheduledMessages
  liftIO $ ((.text) <$> delivered) @?= Just scheduled.text

testElapsedScheduleLeavesPendingList :: IO ()
testElapsedScheduleLeavesPendingList = runEff $ Scheduler.runScheduler do
  _ <- Scheduler.scheduleMessage 0 (messageFrom 200 "!ask now")
  _ <- S.head_ Scheduler.scheduledMessages
  schedules <- Scheduler.listScheduledMessages (messageFrom 200 "what schedules?")
  liftIO $ length schedules @?= 0

messageFrom :: Integer -> Text -> IncomingMessage
messageFrom senderId text =
  messageFromChat senderId 100 text

messageFromChat :: Integer -> Integer -> Text -> IncomingMessage
messageFromChat senderId chatId text =
  IncomingMessage
    { platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just chatId
    , senderId = Just senderId
    , senderUsername = Just "alice"
    , messageId = Just 300
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , text = text
    , raw = Aeson.Null
    }

messageFromUsername :: Text -> Text -> IncomingMessage
messageFromUsername username text =
  (messageFrom 200 text)
    { senderId = Nothing
    , senderUsername = Just username
    }
