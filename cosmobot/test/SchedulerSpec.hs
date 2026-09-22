{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import qualified Bot.Concurrency.Manager as ConcurrencyManager
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Storage as StorageEffect
import Bot.Scheduler.Types (PendingDue (..), PendingMessage (..), SchedulerState (..))
import qualified Bot.Scheduler.State as State
import qualified Bot.Storage.Scheduler as Stored
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Bot.Scheduler.Interpreter as SchedulerInterpreter
import qualified Bot.Storage.SQLite as StorageSQLite
import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Database.Selda.Backend as SeldaBackend
import qualified Database.Selda.SQLite as SeldaSQLite
import qualified Effectful.Concurrent.MVar as MVar
import Effectful.Timeout (Timeout, runTimeout, timeout)
import qualified Streaming.Prelude as S
import System.IO.Error (userError)
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "scheduler"
      [ testCase "stored deadlines reject overflow without changing memory or disk" testStoredDeadlineBounds
      , testCase "storage ids cannot wrap onto another schedule" testStoredIdBounds
      , testCase "deletion during preparation prevents delivery" testDeletionDuringPreparation
      , testCase "queued one-shot messages remain cancellable" testQueuedOneShotDeletion
      , testCase "preparation failure preserves delivery for retry" testPreparationFailure
      , testCase "missing owner identity is rejected" testMissingOwner
      , testCase "pure queue agrees with a list model at deadline boundaries" testQueueModel
      , testCase "mixed schedules agree with a list model through repeated ticks and deletion" testMixedQueueModel
      , testCase "overdue recurrence fires once and advances from now" testOverdueRecurrence
      , testCase "remaining seconds clamp at Int bounds" testRemainingSecondsBounds
      , testCase "schedule deletion respects platform, chat kind, and sender identity" testOwnerBoundaries
      , testCase "scheduled messages are scoped by current user" testScheduledMessagesAreScopedByCurrentUser
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

testStoredDeadlineBounds :: IO ()
testStoredDeadlineBounds = runSchedulerStorage do
  let low = toInteger (minBound :: Int64)
      high = toInteger (maxBound :: Int64)
      owner = messageFrom "200" "boundary"
  for_ [low, low + 1, -1, 0, high - 1, high] \deadline -> do
    stored <- Stored.createScheduledMessage deadline Nothing owner
    loaded <- Stored.loadScheduledMessages
    liftIO $ map (.dueAtUnixSeconds) loaded @?= [deadline]
    Stored.deleteScheduledMessage stored.scheduleId
  for_ [low - 1, high + 1, 2 ^ (100 :: Int)] \deadline -> do
    failed <- trySync (Stored.createScheduledMessage deadline Nothing owner)
    loaded <- Stored.loadScheduledMessages
    liftIO do
      assertBool "unrepresentable deadline must fail" (isLeft failed)
      length loaded @?= 0
  stored <- Stored.createScheduledMessage 100 Nothing owner
  failed <- trySync (Stored.rescheduleScheduledMessages [(stored.scheduleId, 200), (stored.scheduleId, high + 1)])
  loaded <- Stored.loadScheduledMessages
  liftIO do
    assertBool "invalid batch must fail atomically" (isLeft failed)
    map (.dueAtUnixSeconds) loaded @?= [100]
  when (toInteger (maxBound :: Int) == high) $ Scheduler.runScheduler do
    failedSchedule <- trySync (Scheduler.scheduleOneShotMessage maxBound owner)
    pending <- Scheduler.listAllScheduledMessages
    persisted <- Stored.loadScheduledMessages
    liftIO do
      assertBool "epoch plus maxBound must not wrap on 64-bit hosts" (isLeft failedSchedule)
      map (.scheduleId) pending @?= [stored.scheduleId]
      map (.dueAtUnixSeconds) persisted @?= [100]

testStoredIdBounds :: IO ()
testStoredIdBounds = runSchedulerStorage do
  stored <- Stored.createScheduledMessage 100 Nothing (messageFrom "200" "keep")
  let alias = stored.scheduleId + 2 ^ (64 :: Int)
  for_ [Stored.rescheduleScheduledMessages [(alias, 200)], Stored.deleteScheduledMessage alias] \operation -> do
    failed <- trySync operation
    loaded <- Stored.loadScheduledMessages
    liftIO do
      assertBool "unrepresentable id must fail" (isLeft failed)
      [(row.scheduleId, row.dueAtUnixSeconds) | row <- loaded] @?= [(stored.scheduleId, 100)]

testDeletionDuringPreparation :: IO ()
testDeletionDuringPreparation = runSchedulerStorage do
  started <- MVar.newEmptyMVar
  proceed <- MVar.newEmptyMVar
  result <- MVar.newEmptyMVar
  SchedulerInterpreter.runSchedulerWith
    (\message -> MVar.putMVar started () >> MVar.takeMVar proceed >> pure message) do
    let owner = messageFrom "200" "cancel during preparation"
    _ <- Scheduler.scheduleOneShotMessage 0 owner
    worker <- Concurrency.fork "scheduler.test.delivery" do
      delivered <- timeout 1000000 (S.head_ Scheduler.scheduledMessages)
      MVar.putMVar result delivered
    ready <- timeout 1000000 (MVar.takeMVar started)
    liftIO $ ready @?= Just ()
    deleted <- Scheduler.deleteScheduledMessage owner 1
    MVar.putMVar proceed ()
    delivered <- MVar.takeMVar result
    Concurrency.await worker
    liftIO do
      deleted @?= True
      assertBool "cancellation wins over prepared delivery" (isNothing delivered)

testQueuedOneShotDeletion :: IO ()
testQueuedOneShotDeletion = runSchedulerTest do
  let owner = messageFrom "200" "cancel me"
  _ <- Scheduler.scheduleOneShotMessage 0 owner
  -- Give the worker time to enqueue; deletion must also work after the deadline.
  threadDelay 100000
  deleted <- Scheduler.deleteScheduledMessage owner 1
  delivered <- timeout 100000 (S.head_ Scheduler.scheduledMessages)
  liftIO do
    deleted @?= True
    assertBool "deleted queued message must not be delivered" (isNothing delivered)

testPreparationFailure :: IO ()
testPreparationFailure = do
  attempts <- IORef.newIORef (0 :: Int)
  runSchedulerStorage $
    SchedulerInterpreter.runSchedulerWith
      (\message -> do
        attempt <- liftIO $ IORef.atomicModifyIORef' attempts (\n -> (n + 1, n))
        when (attempt == 0) (throwIO (userError "prepare failed"))
        pure message) do
      let owner = messageFrom "200" "retry me"
      _ <- Scheduler.scheduleOneShotMessage 0 owner
      failed <- trySync (S.head_ Scheduler.scheduledMessages)
      persisted <- Stored.loadScheduledMessages
      pending <- Scheduler.listScheduledMessages owner
      delivered <- timeout 1000000 (S.head_ Scheduler.scheduledMessages)
      remainingStored <- Stored.loadScheduledMessages
      liftIO do
        assertBool "preparation failed" (isLeft failed)
        map (.scheduleId) persisted @?= [1]
        map (.scheduleId) pending @?= [1]
        fmap (fmap (.text)) delivered @?= Just (Just "retry me")
        map (.scheduleId) remainingStored @?= []

testMissingOwner :: IO ()
testMissingOwner = runSchedulerTest do
  let anonymous = (messageFrom "200" "task"){senderId = Nothing, senderUsername = Nothing}
  accepted <- Scheduler.scheduleOneShotMessage 60 anonymous
  missingChat <- Scheduler.scheduleOneShotMessage 60 (messageFrom "200" "task"){chatId = Nothing}
  schedules <- Scheduler.listAllScheduledMessages
  liftIO do
    accepted @?= False
    missingChat @?= False
    State.sameMessageOwner anonymous anonymous @?= False
    length schedules @?= 0

-- Exhaust all assignments around the deadline, including ties and insertion orders.
-- The oracle is a sorted list, independent of the production map/set queue.
testQueueModel :: IO ()
testQueueModel = for_ (sequence (replicate 3 [99, 100, 101])) \deadlines ->
  for_ (List.permutations (zip [1..3] deadlines)) \entries -> do
    let stored = [Stored.StoredScheduledMessage ident due Nothing (messageFrom "200" "task") | (ident, due) <- entries]
        initial = State.schedulerStateFromStoredMessages stored
        (remaining, delivered) = State.popDueMessagesFromState 100 initial
        expected = List.sort [(due, ident) | (ident, due) <- entries, due <= 100]
    [(pending.dueAtUnixSeconds, pending.scheduleId) | pending <- delivered] @?= expected
    Map.keys remaining.pendingById @?= [1, 2, 3]
    Set.size remaining.pendingByDue @?= length [() | (_, due) <- entries, due > 100]
    let (finished, later) = State.popDueMessagesFromState 101 remaining
    map (.scheduleId) later @?= map snd (List.sort [(due, ident) | (ident, due) <- entries, due > 100])
    assertBool "due index drains while deliveries await acknowledgement" (Set.null finished.pendingByDue)
    Map.keys finished.pendingById @?= [1, 2, 3]

-- Exhaust a small, structured state space instead of generating mostly empty
-- queues. The list oracle tracks queued one-shots separately from future dues.
testMixedQueueModel :: IO ()
testMixedQueueModel =
  for_ (sequence (replicate 2 [(due, interval) | due <- [99, 100, 101], interval <- [Nothing, Just 1, Just 3]])) \entries ->
    for_ [0, 1, 2] \deletedId -> do
      let initial = [(ident, due, interval, True) | (ident, (due, interval)) <- zip [1..] entries]
          stored = [Stored.StoredScheduledMessage ident due interval (messageFrom "200" "task") | (ident, due, interval, _) <- initial]
          check [] _ _ = pure ()
          check (now : ticks) actual model = do
            let (popped, delivered) = State.popDueMessagesFromState now actual
                expected = List.sort [(due, ident) | (ident, due, _, queued) <- model, queued, due <= now]
                advance (ident, due, interval, queued)
                  | queued && due <= now = case interval of
                      Nothing -> (ident, due, interval, False)
                      Just seconds -> (ident, now + toInteger seconds, interval, True)
                  | otherwise = (ident, due, interval, queued)
                advanced = map advance model
                -- Delete after the first pop: this exercises both a queued
                -- one-shot and a recurring entry whose deadline just moved.
                remove = if now == 100 then deletedId else 0
                (next, didDelete) = State.deletePendingMessageByIdFromState remove popped
                remaining = filter (\(ident, _, _, _) -> ident /= remove) advanced
            [(pending.dueAtUnixSeconds, pending.scheduleId) | pending <- delivered] @?= expected
            didDelete @?= any (\(ident, _, _, _) -> ident == remove) advanced
            [(pending.scheduleId, pending.dueAtUnixSeconds, pending.recurringIntervalSeconds) | pending <- Map.elems next.pendingById]
              @?= [(ident, due, interval) | (ident, due, interval, _) <- remaining]
            [(due.dueAtUnixSeconds, due.scheduleId) | due <- Set.toAscList next.pendingByDue]
              @?= List.sort [(due, ident) | (ident, due, _, True) <- remaining]
            check ticks next remaining
      check [100, 100, 101, 103, 110] (State.schedulerStateFromStoredMessages stored) initial

testRemainingSecondsBounds :: IO ()
testRemainingSecondsBounds = do
  let limit = toInteger (maxBound :: Int)
      now = 123
  for_ [-1, 0, 1, limit - 1, limit, limit + 1, 2 * limit] \delta -> do
    let pending = PendingMessage 1 (now + delta) Nothing (messageFrom "200" "boundary")
        expected = fromInteger (max 0 (min limit delta))
    (State.scheduledMessage now pending).remainingSeconds @?= expected

testOverdueRecurrence :: IO ()
testOverdueRecurrence = for_ [1, 7, 60] \interval -> do
  let stored = Stored.StoredScheduledMessage 1 10 (Just interval) (messageFrom "200" "repeat")
      (next, delivered) = State.popDueMessagesFromState 1000 (State.schedulerStateFromStoredMessages [stored])
      deadline = 1000 + toInteger interval
  map (.scheduleId) delivered @?= [1]
  map (.dueAtUnixSeconds) (Map.elems next.pendingById) @?= [deadline]
  let (_, before) = State.popDueMessagesFromState (deadline - 1) next
      (_, at) = State.popDueMessagesFromState deadline next
      (deleted, removed) = State.deletePendingMessageByIdFromState 1 next
  length before @?= 0
  map (.scheduleId) at @?= [1]
  removed @?= True
  assertBool "deletion removes the rescheduled due index" (Map.null deleted.pendingById && Set.null deleted.pendingByDue)

testOwnerBoundaries :: IO ()
testOwnerBoundaries = do
  let owner = messageFrom "200" "task"
      initial = State.schedulerStateFromStoredMessages [Stored.StoredScheduledMessage 1 100 Nothing owner]
      outsiders =
        [ owner{platform = PlatformDiscord}
        , owner{kind = ChatGroup}
        , owner{chatId = Just (integerChatId 101)}
        , owner{senderId = Just "201"}
        , owner{senderId = Nothing}
        ]
  for_ outsiders \outsider -> do
    let (remaining, deleted) = State.deletePendingMessageFromState outsider 1 initial
    deleted @?= False
    Map.keys remaining.pendingById @?= [1]
    Set.size remaining.pendingByDue @?= 1
  let (_, deleted) = State.deletePendingMessageFromState owner{senderUsername = Just "renamed"} 1 initial
  deleted @?= True

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
    , replyQuote = Nothing
    , timestamp = Nothing
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
