{-# LANGUAGE DataKinds #-}
module Bot.Agent.Children (withStructuredChildren, waitAnyChild, childOutput) where

import Bot.Agent.Control (runControlTurn)
import Bot.Agent.Core
import Bot.Agent.Types
import Bot.Agent.Transcript (appendMessage)
import qualified Bot.Effect.Agent as Agent
import qualified Bot.Effect.AgentAudit as Audit
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.LLM.Types as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.MVar as MVar
import qualified Streaming.Prelude as S

-- Only handles and completion cells escape resource access, never resource objects.
withStructuredChildren
  :: forall es a. (Agent.Agent :> es, Concurrency.Concurrency :> es, Concurrent :> es, Audit.AgentAudit :> es)
  => BotRuntime '[] (Eff es)
  -> (BotRuntime '[] (Eff es) -> Eff es a)
  -> Eff es a
withStructuredChildren runtime use = do
  children <- MVar.newMVar ([] :: [(Agent.ChildRun, Bool)])
  let register :: Agent.ChildRun -> Eff es ()
      register child = MVar.modifyMVar_ children (pure . (<> [(child, False)]))
      consume worker = MVar.modifyMVar_ children $
        pure . map (\(child, consumed) -> (child, consumed || child.worker == worker))
      cleanup = do
        owned <- MVar.readMVar children
        for_ owned \(child, _) -> void (Concurrency.cancel child.worker.handleId)
        for_ owned \(child, _) -> Concurrency.await child.worker
      pending = map fst . filter (not . snd) <$> MVar.readMVar children
      collect owned = do
        let notify waiting = void $ Audit.recordEvent Audit.AgentChildrenWait
              { runId = runtime.runId, waiting, subagentIds = map (.name) owned }
        output <- (notify True *> do
          child <- waitAnyChild (fromMaybe (error "empty structured join") (nonEmpty owned))
          result <- childOutput child
          consume child.worker
          pure (Aeson.object ["resource" Aeson..= child.name, "output" Aeson..= result]))
          `finally` notify False
        pure . toolText . TextEncoding.decodeUtf8 . LazyByteString.toStrict $ Aeson.encode output
      configured = runtime
        { aroundProgram = \finalRuntime ->
            runtime.aroundProgram finalRuntime . joinChildren finalRuntime pending collect
        }
  interpose (\localEnv -> \case
      Agent.RegisterChild parent child | parent == runtime.runId -> register child
      Agent.ConsumeChild parent worker | parent == runtime.runId -> consume worker
      operation -> passthrough localEnv operation)
    (use configured) `finally` cleanup

joinChildren
  :: Monad m
  => Runtime host metadata '[] m
  -> m [Agent.ChildRun]
  -> ([Agent.ChildRun] -> m ToolResult)
  -> Program m Result
  -> Program m Result
joinChildren runtime pending collect (Program action) = Program $
  action >>= \case
    Finished result -> pure (Finished result)
    Continues next -> pure (Continues (go next))
    Visible event@(RunModel _) continue -> pure (Visible event (completeModel continue))
    Visible event continue -> pure (Visible event (go . continue))
  where
    go = joinChildren runtime pending collect
    completeModel continue response@(agentState, _) = Program $
      (continue response).observe >>= \case
        Finished completion | completion.status == "answered" -> do
          owned <- lift pending
          if null owned then pure (Finished completion) else do
            let call = LLM.ToolCall
                  { id = runtime.runId <> "/join/" <> show agentState.turn
                  , name = "subagent"
                  , arguments = TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode $
                      Aeson.object ["op" Aeson..= ("wait_any" :: Text), "resources" Aeson..= map (.name) owned]
                  }
                request = ToolRequest
                  { agentState = agentState{nextModelTranscript = Nothing, modelTokenUsage = completion.tokenUsage}
                  , answered = appendMessage (LLM.assistantAnswer (LLM.chatAnswer "" [call])) completion.transcript
                  , toolContent = ""
                  , toolCalls = call :| []
                  }
            S.yield ReplyBoundary
            S.yield (ToolCallNotification (call :| []))
            (nextState, result) <- lift (runControlTurn runtime request call (collect owned))
            if agentState.turn >= runtime.maxTurns
              then do
                let notice = "Child result received; model turn budget exhausted. Remaining children cancelled.\n" <> toolResultContent result
                S.yield (ContentDelta notice)
                pure . Finished $ completion
                  { transcript = appendMessage (LLM.assistantText notice) nextState.transcript
                  , finalText = notice
                  , status = "subagent_limit"
                  }
              else pure . Continues . go $ runModel nextState >>= continue
        Finished completion -> pure (Finished completion)
        Continues next -> pure (Continues (go next))
        Visible event next -> pure (Visible event (go . next))


-- A wait covers this exact set of runs, and wakes when any one is terminal.
waitAnyChild
  :: (Concurrency.Concurrency :> es, Concurrent :> es)
  => NonEmpty Agent.ChildRun -> Eff es Agent.ChildRun
waitAnyChild children = do
  winner <- Concurrency.awaitAny (fmap (.worker) children)
  pure (fromMaybe (error "missing winning child") (find ((== winner) . (.worker)) children))

childOutput
  :: (Concurrency.Concurrency :> es, Concurrent :> es)
  => Agent.ChildRun -> Eff es Text
childOutput child = do
  Concurrency.await child.worker
  fromMaybe "Subagent terminated without a result." <$> MVar.tryReadMVar child.output
