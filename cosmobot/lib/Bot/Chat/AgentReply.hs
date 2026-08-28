{-# LANGUAGE DataKinds #-}
{-|
Module      : Bot.Chat.AgentReply
Description : Agent reply streaming over the chat capability
Stability   : experimental
-}

module Bot.Chat.AgentReply
  ( AgentReply (..)
  , streamAgentReplyWith
  ) where

import qualified Bot.Agent as Agent
import qualified Bot.Agent.Failure as Failure
import Bot.Core.Message
import Bot.Core.Transcript
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Streaming.Prelude as S

data AgentReply = AgentReply
  { responseId :: !(Maybe MessageId)
  , answer :: !Text
  , result :: !Agent.Result
  }

streamAgentReplyWith
  :: ( Chat.Chat :> es, ChatLog.ChatLog :> es, Concurrency.Concurrency :> es
     , LLM.LLM :> es, Media.Media :> es, Storage.Storage :> es
     , KatipE :> es, Prim :> es, Concurrent :> es )
  => Agent.Runtime '[] (Eff es)
  -> Agent.SteeringControl es
  -> Agent.ToolEmittedMessageSink es
  -> (Chat.MessageOutResult -> Eff es ())
  -> IncomingMessage
  -> Transcript
  -> Eff es AgentReply
streamAgentReplyWith runtime steering sink recordUpdate message transcript = do
  let program =
        ( Agent.withSteering steering
        . Agent.withRecordingToolSelfMessages (ChatLog.recordSelfMessageWithFiles message)
        . Agent.withLinkingToolEmittedMessagesToThread sink
        . Agent.withNormalizingToolReplies
        ) runtime
  (lastReply, replyResult) <-
    S.mapM_ recordUpdate $
      Chat.streamMultipleRepliesTo message (agentReplyTextSegments (Agent.agentStream program transcript))
  let responseId = lastReply.responseId
      (answer, result) = replyResult
  pure AgentReply{responseId, answer, result}
  `catchSync` \err -> do
    $(logWarning) [i|LLM request failed: #{show err :: String}|]
    let failureMessage = "LLM request failed: " <> (Failure.failureFromException err).userMessage
    responseId <- listToMaybe . rights <$> Chat.replyTo message failureMessage
    pure AgentReply
      { responseId
      , answer = failureMessage
      , result = Agent.Result
          { runId = Agent.runIdOf runtime, transcript, status = "failed"
          , finalText = failureMessage, turnsUsed = 0, tokenUsage = Nothing }
      }

agentReplyTextSegments
  :: Prim :> es
  => Stream (Of Agent.Output) (Eff es) Agent.Result
  -> Stream (Stream (Of Text) (Eff es)) (Eff es) (Text, Agent.Result)
agentReplyTextSegments =
  S.maps (S.mapMaybe id) . S.breaks isNothing . agentReplyTextEvents

agentReplyTextEvents
  :: Prim :> es
  => Stream (Of Agent.Output) (Eff es) Agent.Result
  -> Stream (Of (Maybe Text)) (Eff es) (Text, Agent.Result)
agentReplyTextEvents = go mempty
  where
    go answer stream = do
      next <- lift (S.next stream)
      case next of
        Left result -> pure (renderReplyText answer, result)
        Right (Agent.ContentDelta chunk, rest) -> S.yield (Just chunk) >> go (answer <> TextBuilder.fromText chunk) rest
        Right (Agent.ToolCallNotification{}, rest) -> S.yield Nothing >> go answer rest
        Right (Agent.ReplyBoundary, rest) -> S.yield Nothing >> go answer rest

renderReplyText :: TextBuilder.Builder -> Text
renderReplyText = Text.strip . LazyText.toStrict . TextBuilder.toLazyText
