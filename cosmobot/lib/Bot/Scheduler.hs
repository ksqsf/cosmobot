{-|
Module      : Bot.Scheduler
Description : Scheduler application wiring
Stability   : experimental
-}

module Bot.Scheduler
  ( module Scheduler
  , runScheduler
  , linkToSourceThread
  )
where

import Bot.Core.Message
import Bot.Core.Thread (ThreadMessageKey (..))
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Concurrency as Concurrency
import Bot.Effect.Scheduler as Scheduler hiding (runScheduler)
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import qualified Bot.Scheduler.Interpreter as Interpreter
import Bot.Storage.Thread
import Effectful.Timeout (Timeout)

runScheduler
  :: (AgentAudit.AgentAudit :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, Prim :> es, Concurrent :> es, Timeout :> es, IOE :> es)
  => ThreadStore
  -> Eff (Scheduler : es) a
  -> Eff es a
runScheduler threads =
  Interpreter.runSchedulerWith (linkToSourceThread threads)

linkToSourceThread
  :: (AgentAudit.AgentAudit :> es, Prim :> es, Concurrent :> es)
  => ThreadStore
  -> IncomingMessage
  -> Eff es IncomingMessage
linkToSourceThread threads message =
  case scheduledRunId message of
    Nothing -> pure message
    Just runId -> do
      awaitActiveThreadByRunId threads runId
      records <- AgentAudit.queryRunAudit runId
      -- This deliberately fakes a platform reply so scheduled prompts reuse the
      -- existing Ask continuation route without coupling Scheduler to Ask.
      pure message
        { replyToMessageId = (.messageId) <$> latestLinkedMessageKey records
        }

latestLinkedMessageKey :: [AgentAudit.AgentAuditRecord] -> Maybe ThreadMessageKey
latestLinkedMessageKey records =
  listToMaybe
    [ messageKey
    | AgentAudit.AgentAuditRecord{event = AgentAudit.AgentThreadLinked{linkedMessageKey = Just messageKey}} <- reverse records
    ]
