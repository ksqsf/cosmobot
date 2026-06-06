{-|
Module      : Bot.Agent.Middleware.Typing
Description : Agent typing notification middleware
Stability   : experimental
-}

module Bot.Agent.Middleware.Typing
  ( withTypingNotification
  )
where

import Bot.Agent.Core
import Bot.Agent.Types (AgentContext (..))
import Bot.Core.Message (IncomingMessage (..))
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Concurrency as Concurrency
import Bot.Prelude
import qualified Bot.Util.Stream as StreamUtil
import qualified Streaming as S

withTypingNotification
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, KatipE :> es)
  => AgentProgram transient context es
  -> AgentProgram transient context es
withTypingNotification program =
  program
    { aroundAgentRun = \context action ->
        withTypingScope message (program.aroundAgentRun context action)
    }
  where
    message =
      program.agentRun.context.message

withTypingScope
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, KatipE :> es)
  => IncomingMessage
  -> Stream (Of AgentStreamOutput) (Eff es) AgentCompletion
  -> Stream (Of AgentStreamOutput) (Eff es) AgentCompletion
withTypingScope message stream = do
  S.lift (safeSetTyping message typingNotificationTimeoutMillis)
  StreamUtil.bracketStream
    (Concurrency.spawnTopLevelTask "agent.typing" (typingNotificationLoop message))
    cancelAndAwaitTyping
    \_ -> stream

cancelAndAwaitTyping :: Concurrency.Concurrency :> es => Concurrency.ResourceHandle -> Eff es ()
cancelAndAwaitTyping typingHandle = do
  void (Concurrency.cancelResource typingHandle.resourceId)
  Concurrency.awaitResource typingHandle

typingNotificationLoop
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, KatipE :> es)
  => IncomingMessage
  -> Eff es ()
typingNotificationLoop message = do
  Concurrency.sleepMicroseconds typingNotificationRefreshMicroseconds
  safeSetTyping message typingNotificationTimeoutMillis
  typingNotificationLoop message

safeSetTyping
  :: (Chat.Chat :> es, KatipE :> es)
  => IncomingMessage
  -> Int
  -> Eff es ()
safeSetTyping message timeoutMillis =
  Chat.setTyping message timeoutMillis
    `catchSync` \err -> do
      let platform = message.platform
          chatId = message.chatId
          chatAliases = message.chatAliases
      logWarning [i|Typing notification failed: platform=#{platform} chat_id=#{chatId} chat_aliases=#{chatAliases} error=#{displayException err}|]

typingNotificationTimeoutMillis :: Int
typingNotificationTimeoutMillis =
  30000

typingNotificationRefreshMicroseconds :: Int
typingNotificationRefreshMicroseconds =
  20 * 1000000
