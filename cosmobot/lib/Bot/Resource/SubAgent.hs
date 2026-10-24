{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Bot.Resource.SubAgent
Description : Chat-scoped background agents
Stability   : experimental
-}
module Bot.Resource.SubAgent
  ( SubAgent
  , SubAgentArgs (..)
  , sendPrompt
  , steer
  , queryOutput
  , waitAnyOutput
  , waitAllOutputs
  )
where

import Bot.Agent.Children (waitAnyChild, childOutput)
import qualified Bot.Agent as Agent
import Bot.Agent.Tool
import Bot.Agent.Types
import Bot.Agent.Middleware.TranscriptSnapshot (withTranscriptSnapshots)
import Bot.Core.Message (MessageInput, inputWithImages)
import Bot.Core.Transcript
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Agent as AgentEffect
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Resource as Resource
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Storage.AgentTranscript as AgentTranscript
import Bot.Prelude
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.Resource as EffectfulResource
import qualified Streaming.Prelude as S

data SubAgentArgs = SubAgentArgs
  { systemContext :: !Text
  , toolNames :: ![Text]
  , ttlMinutes :: !Int
  }

data SubAgent = SubAgent
  { arguments :: !SubAgentArgs
  , state :: !(MVar.MVar SubAgentState)
  }

data SubAgentState = SubAgentState
  { transcript :: !(Maybe Transcript)
  , answer :: !(Maybe Text)
  , active :: !(Maybe ActiveRun)
  , runs :: ![Concurrency.Handle]
  , latestRun :: !(Maybe AgentEffect.ChildRun)
  }

data ActiveRun = ActiveRun
  { worker :: !Concurrency.Handle
  , steering :: !SteeringQueue
  }

type SteeringQueue = MVar.MVar (Maybe (Seq.Seq MessageInput))

instance
  (Resource.Resource :> es, Concurrency.Concurrency :> es, Concurrent :> es)
  => Resource.ResourceObject (Eff es) SubAgent
  where
  type CreationArgs SubAgent = SubAgentArgs

  resourceTypeName _ = "SubAgent"
  resourceScope _ = Resource.ChatResource
  resourceTTLSeconds = Resource.ttlFromMinutes . (.ttlMinutes)

  createResourceObject Resource.Init {arguments} =
    Right . SubAgent arguments
      <$> MVar.newMVar
        SubAgentState
          { transcript = Nothing
          , answer = Nothing
          , active = Nothing
          , runs = []
          , latestRun = Nothing
          }

  destroyResourceObject subagent = do
    snapshot <- MVar.readMVar subagent.state
    for_ snapshot.active \active -> do
      void (Concurrency.cancel active.worker.handleId)
      Concurrency.await active.worker
    results <- concat <$> traverse Resource.destroyAssociated snapshot.runs
    pure $ case lefts results of
      [] -> Right ()
      err : _ -> Left ("Failed to destroy a child resource: " <> show err)

  describeResourceObject subagent _ =
    pure (show (length subagent.arguments.toolNames) <> " tools")

  detailResourceObject subagent = do
    snapshot <- MVar.readMVar subagent.state
    pure $ Text.intercalate "\n"
      [ "status: " <> if isJust snapshot.active then "generating" else "ready"
      , "tools: " <> case subagent.arguments.toolNames of
          [] -> "(none)"
          names -> Text.intercalate ", " names
      , "system prompt:\n" <> subagent.arguments.systemContext
      , "output:\n" <> case snapshot.active of
          Just _ -> "Generating"
          Nothing -> fromMaybe "No prompt has been sent." snapshot.answer
      ]

  probeResourceObject subagent = do
    snapshot <- MVar.readMVar subagent.state
    pure (Right (if isJust snapshot.active then "generating" else "ready"))

sendPrompt
  :: ( AgentEffect.Agent :> es
     , AgentAudit.AgentAudit :> es
     , Chat.Chat :> es
     , Concurrency.Concurrency :> es
     , LLM.LLM :> es
     , Media.Media :> es
     , Storage.Storage :> es
     , KatipE :> es
     , Prim :> es
     , Concurrent :> es
     , IOE :> es
     )
  => ToolCallMetadata
  -> Text
  -> [BotTool (Eff es)]
  -> Context
  -> SubAgent
  -> Text
  -> Eff es (Either Text ())
sendPrompt metadata subagentId availableTools context subagent prompt =
  mask \restore -> do
    gate <- MVar.newEmptyMVar
    ready <- MVar.newEmptyMVar
    output <- MVar.newEmptyMVar
    started <- MVar.modifyMVar subagent.state \snapshot ->
      case snapshot.active of
        Just _ -> pure (snapshot, Left "Subagent is still generating.")
        Nothing -> do
          steering <- newSteeringQueue
          let transcript = maybe (startWithUser prompt) (appendUser prompt) snapshot.transcript
              requestedNames = subagent.arguments.toolNames
              requestedTools = filter ((`elem` requestedNames) . toolName) availableTools
              needsEnable = any (any isNamedTag . toolTags) requestedTools
              selectedTools = filter
                (\definition -> toolName definition `elem` requestedNames
                  || needsEnable && toolName definition == toolEnableName)
                availableTools
              childContext = context {Agent.systemContext = subagent.arguments.systemContext}
          worker <- Concurrency.forkWithHandle "subagent" \worker -> do
            let finish result = do
                  MVar.modifyMVar_ steering (const (pure Nothing))
                  MVar.modifyMVar_ subagent.state (pure . finishRun worker result)
                  void $ MVar.tryPutMVar output (either
                    (\err -> "Subagent failed: " <> Text.take 500 (show err)) fst result)
                cancelled = do
                  MVar.modifyMVar_ steering (const (pure Nothing))
                  MVar.modifyMVar_ subagent.state \current -> pure $
                    if ((.worker) <$> current.active) == Just worker
                      then current{active = Nothing, answer = Just "Subagent cancelled."}
                      else current
                  void $ MVar.tryPutMVar output "Subagent cancelled."
            (do
              MVar.putMVar ready ()
              result <- trySync $ restore do
                MVar.takeMVar gate
                EffectfulResource.runResource do
                  Agent.withAgentMetadata
                    (\runId -> ToolCallMetadata
                      { agentRunId = runId
                      , originRunId = metadata.originRunId
                      , resourceOwner = Just worker
                      }) $
                    Agent.withRun 8 ContextCompaction 1000000 childContext (map (hoistTool raise) selectedTools) \runtime -> do
                      void $ AgentAudit.agentAuditObserver SubAgentRunStarted
                        { runId = metadata.agentRunId
                        , childRunId = Agent.runIdOf runtime
                        , subagentId
                        }
                      _ S.:> agentResult <- S.toList
                        (Agent.agentStream
                          (Agent.withSteering (steeringControl steering)
                            (withTranscriptSnapshots
                              (\saved -> AgentTranscript.saveRunTranscript (Agent.runIdOf runtime) saved
                                `catchSync` \_ -> $(logWarning) "Could not persist subagent transcript") runtime)) transcript)
                      pure (agentResult.finalText, agentResult.transcript)
              finish result) `onException` cancelled
          let child = AgentEffect.ChildRun subagentId worker output
          pure (snapshot {active = Just ActiveRun{worker, steering}, runs = worker : snapshot.runs, latestRun = Just child}, Right child)
    case started of
      Left err -> pure (Left err)
      Right child -> do
        (AgentEffect.registerChild metadata.agentRunId child *> MVar.takeMVar ready *> MVar.putMVar gate ())
          `onException` do
            void (Concurrency.cancel child.worker.handleId)
            Concurrency.await child.worker
            MVar.modifyMVar_ subagent.state \current ->
              if ((.worker) <$> current.active) == Just child.worker
                then do
                  for_ current.active (\active -> MVar.modifyMVar_ active.steering (const (pure Nothing)))
                  pure current{active = Nothing, answer = Just "Subagent cancelled."}
                else pure current
            void $ MVar.tryPutMVar output "Subagent cancelled."
        pure (Right ())
  where
    isNamedTag = \case
      Named _ -> True
      Essential -> False

finishRun
  :: Concurrency.Handle
  -> Either SomeException (Text, Transcript)
  -> SubAgentState
  -> SubAgentState
finishRun worker result current
  | ((.worker) <$> current.active) /= Just worker = current
  | otherwise =
      case result of
        Right (answer, transcript) ->
          current{transcript = Just transcript, answer = Just answer, active = Nothing}
        Left err ->
          current{answer = Just ("Subagent failed: " <> Text.take 500 (show err)), active = Nothing}

steer :: Concurrent :> es => SubAgent -> Text -> Eff es (Either Text ())
steer subagent prompt = do
  active <- (.active) <$> MVar.readMVar subagent.state
  case active of
    Nothing -> pure (Left "Subagent is not generating.")
    Just run -> do
      accepted <- enqueueSteer run.steering (inputWithImages prompt [])
      pure $ if accepted then Right () else Left "Subagent is not generating."

newSteeringQueue :: Concurrent :> es => Eff es SteeringQueue
newSteeringQueue =
  MVar.newMVar (Just Seq.empty)

enqueueSteer :: Concurrent :> es => SteeringQueue -> MessageInput -> Eff es Bool
enqueueSteer steering input =
  MVar.modifyMVar steering \case
    Nothing -> pure (Nothing, False)
    Just queued -> pure (Just (queued Seq.|> input), True)

steeringControl :: Concurrent :> es => SteeringQueue -> Agent.SteeringControl es
steeringControl steering =
  Agent.SteeringControl
    { drain = MVar.modifyMVar steering \case
        Nothing -> pure (Nothing, [])
        Just queued -> pure (Just Seq.empty, Foldable.toList queued)
    , complete = MVar.modifyMVar steering \case
        Nothing -> pure (Nothing, Nothing)
        Just queued
          | Seq.null queued -> pure (Nothing, Nothing)
          | otherwise -> pure (Just Seq.empty, Just (Foldable.toList queued))
    }

queryOutput :: (AgentEffect.Agent :> es, Concurrent :> es) => Text -> SubAgent -> Eff es Text
queryOutput parent subagent = do
  snapshot <- MVar.readMVar subagent.state
  case snapshot.active of
    Just _ -> pure "The subagent is still generating."
    Nothing -> do
      for_ snapshot.latestRun (AgentEffect.consumeChild parent . (.worker))
      pure (fromMaybe "No prompt has been sent." snapshot.answer)

waitAnyOutput
  :: (AgentEffect.Agent :> es, Concurrency.Concurrency :> es, Concurrent :> es)
  => Text -> NonEmpty (Text, SubAgent) -> Eff es (Text, Text)
waitAnyOutput parent subagents = do
  snapshots <- traverse snapshotRun subagents
  ready <- filterM (maybe (pure True) (fmap isJust . MVar.tryReadMVar . (.output)) . snd) (toList snapshots)
  case ready of
    readyRun : _ -> outputFor parent readyRun
    [] -> do
      let runs = mapMaybe snd (toList snapshots)
      winner <- waitAnyChild (fromMaybe (error "missing subagent workers") (nonEmpty runs))
      outputFor parent (fromMaybe (error "missing winning subagent")
        (find ((== Just winner.worker) . fmap (.worker) . snd) snapshots))

waitAllOutputs
  :: (AgentEffect.Agent :> es, Concurrency.Concurrency :> es, Concurrent :> es)
  => Text -> NonEmpty (Text, SubAgent) -> Eff es (NonEmpty (Text, Text))
waitAllOutputs parent subagents = do
  snapshots <- traverse snapshotRun subagents
  results <- forM snapshots \(name, child) ->
    (name,) <$> maybe (pure "No prompt has been sent.") childOutput child
  for_ snapshots (traverse_ (AgentEffect.consumeChild parent . (.worker)) . snd)
  pure results

snapshotRun :: Concurrent :> es => (Text, SubAgent) -> Eff es (Text, Maybe AgentEffect.ChildRun)
snapshotRun (name, subagent) = (name,) . (.latestRun) <$> MVar.readMVar subagent.state

outputFor
  :: (AgentEffect.Agent :> es, Concurrency.Concurrency :> es, Concurrent :> es)
  => Text -> (Text, Maybe AgentEffect.ChildRun) -> Eff es (Text, Text)
outputFor _ (name, Nothing) = pure (name, "No prompt has been sent.")
outputFor parent (name, Just child) = do
  output <- childOutput child
  AgentEffect.consumeChild parent child.worker
  pure (name, output)
