module Main (main) where

import qualified Bot.Effect.Scheduler as Scheduler
import Bot.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Streaming.Prelude as S

main :: IO ()
main = do
  testScheduledMessagesAreScopedByCurrentUser
  testElapsedScheduleLeavesPendingList

testScheduledMessagesAreScopedByCurrentUser :: IO ()
testScheduledMessagesAreScopedByCurrentUser = runEff $ Scheduler.runScheduler do
  Scheduler.scheduleMessage 60 (messageFrom 200 "!ask remind me")
  ownSchedules <- Scheduler.listScheduledMessages (messageFrom 200 "what schedules?")
  otherSchedules <- Scheduler.listScheduledMessages (messageFrom 201 "what schedules?")
  liftIO $ assertEqual "current user sees one pending schedule" 1 (length ownSchedules)
  liftIO $ assertEqual "other user cannot see pending schedule" 0 (length otherSchedules)
  liftIO $ assertBool "remaining seconds are positive" (all ((> 0) . (.remainingSeconds)) ownSchedules)

testElapsedScheduleLeavesPendingList :: IO ()
testElapsedScheduleLeavesPendingList = runEff $ Scheduler.runScheduler do
  Scheduler.scheduleMessage 0 (messageFrom 200 "!ask now")
  _ <- S.head_ Scheduler.scheduledMessages
  schedules <- Scheduler.listScheduledMessages (messageFrom 200 "what schedules?")
  liftIO $ assertEqual "elapsed schedule is no longer pending" 0 (length schedules)

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

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  unless (expected == actual) $
    fail (label <> ": expected " <> show expected <> ", got " <> show actual)

assertBool :: String -> Bool -> IO ()
assertBool label value =
  unless value (fail label)
