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
      , testCase "elapsed schedule leaves pending list" testElapsedScheduleLeavesPendingList
      ]

testScheduledMessagesAreScopedByCurrentUser :: IO ()
testScheduledMessagesAreScopedByCurrentUser = runEff $ Scheduler.runScheduler do
  Scheduler.scheduleMessage 60 (messageFrom 200 "!ask remind me")
  ownSchedules <- Scheduler.listScheduledMessages (messageFrom 200 "what schedules?")
  otherSchedules <- Scheduler.listScheduledMessages (messageFrom 201 "what schedules?")
  liftIO $ length ownSchedules @?= 1
  liftIO $ length otherSchedules @?= 0
  liftIO $ assertBool "remaining seconds are positive" (all ((> 0) . (.remainingSeconds)) ownSchedules)

testElapsedScheduleLeavesPendingList :: IO ()
testElapsedScheduleLeavesPendingList = runEff $ Scheduler.runScheduler do
  Scheduler.scheduleMessage 0 (messageFrom 200 "!ask now")
  _ <- S.head_ Scheduler.scheduledMessages
  schedules <- Scheduler.listScheduledMessages (messageFrom 200 "what schedules?")
  liftIO $ length schedules @?= 0

messageFrom :: Integer -> Text -> IncomingMessage
messageFrom senderId text =
  IncomingMessage
    { platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just 100
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
