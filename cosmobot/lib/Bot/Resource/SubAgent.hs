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
  , SubAgentRunner
  , sendPrompt
  , queryOutput
  , waitAnyOutput
  , waitAllOutputs
  )
where

import Bot.Agent.Types
import qualified Bot.Agent.Types as Agent
import Bot.Core.Transcript
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Data.Text as Text
import qualified Effectful.Concurrent.MVar as MVar

type SubAgentRunner es =
  ToolCallMetadata
  -> Text
  -> Concurrency.Handle
  -> AgentContext es
  -> [Tool es]
  -> Transcript
  -> Eff es (Text, Transcript)

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
  , active :: !(Maybe Concurrency.Handle)
  , runs :: ![Concurrency.Handle]
  }

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
          }

  destroyResourceObject subagent = do
    snapshot <- MVar.readMVar subagent.state
    for_ snapshot.active \worker -> do
      void (Concurrency.cancel worker.handleId)
      Concurrency.await worker
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
  :: (Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es)
  => SubAgentRunner es
  -> ToolCallMetadata
  -> Text
  -> [Tool es]
  -> AgentContext es
  -> SubAgent
  -> Text
  -> Eff es (Either Text ())
sendPrompt runner metadata subagentId availableTools context subagent prompt =
  MVar.modifyMVar subagent.state \snapshot ->
    case snapshot.active of
      Just _ -> pure (snapshot, Left "Subagent is still generating.")
      Nothing -> do
        let transcript = maybe (startWithUser prompt) (appendUser prompt) snapshot.transcript
            selectedTools = filter ((`elem` subagent.arguments.toolNames) . (.name)) availableTools
            childContext = context {Agent.systemContext = subagent.arguments.systemContext}
        worker <- Concurrency.forkWithHandle "subagent" \worker -> do
          result <- trySync (runner metadata subagentId worker childContext selectedTools transcript)
          MVar.modifyMVar_ subagent.state (pure . finishRun worker result)
        pure (snapshot {active = Just worker, runs = worker : snapshot.runs}, Right ())

finishRun
  :: Concurrency.Handle
  -> Either SomeException (Text, Transcript)
  -> SubAgentState
  -> SubAgentState
finishRun worker result current
  | current.active /= Just worker = current
  | otherwise =
      case result of
        Right (answer, transcript) ->
          current{transcript = Just transcript, answer = Just answer, active = Nothing}
        Left err ->
          current{answer = Just ("Subagent failed: " <> Text.take 500 (show err)), active = Nothing}

queryOutput :: (Concurrent :> es) => SubAgent -> Eff es Text
queryOutput subagent = do
  snapshot <- MVar.readMVar subagent.state
  pure $ case snapshot.active of
    Just _ -> "The subagent is still generating."
    Nothing -> fromMaybe "No prompt has been sent." snapshot.answer

waitAnyOutput
  :: (Concurrency.Concurrency :> es, Concurrent :> es)
  => NonEmpty (Text, SubAgent)
  -> Eff es (Text, Text)
waitAnyOutput subagents = do
  snapshots <- traverse snapshot subagents
  case traverse requireActive snapshots of
    Nothing ->
      answerFor (fst (fromMaybe (error "missing ready subagent") (find (isNothing . snd) snapshots)))
    Just activeSubagents -> do
      winner <- Concurrency.awaitAny (snd <$> activeSubagents)
      answerFor (fst (fromMaybe (error "missing winning subagent") (find ((== winner) . snd) activeSubagents)))
  where
    snapshot named@(_, subagent) =
      (named,) . (.active) <$> MVar.readMVar subagent.state

    requireActive (named, worker) =
      (named,) <$> worker

waitAllOutputs
  :: (Concurrency.Concurrency :> es, Concurrent :> es)
  => NonEmpty (Text, SubAgent)
  -> Eff es (NonEmpty (Text, Text))
waitAllOutputs =
  traverse outputFor

outputFor
  :: (Concurrency.Concurrency :> es, Concurrent :> es)
  => (Text, SubAgent)
  -> Eff es (Text, Text)
outputFor (name, subagent) = do
  snapshot <- MVar.readMVar subagent.state
  traverse_ Concurrency.await snapshot.active
  answerFor (name, subagent)

answerFor
  :: Concurrent :> es
  => (Text, SubAgent)
  -> Eff es (Text, Text)
answerFor (name, subagent) = do
  finished <- MVar.readMVar subagent.state
  pure (name, fromMaybe "No prompt has been sent." finished.answer)
