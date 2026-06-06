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
import qualified Effectful.Prim.IORef as IORef

withTypingNotification
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, KatipE :> es, Prim :> es)
  => AgentProgram transient context es
  -> AgentProgram transient context es
withTypingNotification program =
  program
    { aroundAgentRun = \context action ->
        StreamUtil.bracketStream
          (startTypingNotification message)
          stopTypingNotification
          \_ -> program.aroundAgentRun context action
    }
  where
    message =
      program.agentRun.context.message

startTypingNotification
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, KatipE :> es, Prim :> es)
  => IncomingMessage
  -> Eff es (IORef.IORef Bool)
startTypingNotification message = do
  active <- IORef.newIORef True
  safeSetTyping message typingNotificationTimeoutMillis
  Concurrency.startTask "agent.typing" (typingNotificationLoop active message)
  pure active

stopTypingNotification :: Prim :> es => IORef.IORef Bool -> Eff es ()
stopTypingNotification active =
  IORef.writeIORef active False

typingNotificationLoop
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, KatipE :> es, Prim :> es)
  => IORef.IORef Bool
  -> IncomingMessage
  -> Eff es ()
typingNotificationLoop active message = do
  Concurrency.sleepMicroseconds typingNotificationRefreshMicroseconds
  stillActive <- IORef.readIORef active
  when stillActive do
    safeSetTyping message typingNotificationTimeoutMillis
    typingNotificationLoop active message

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
  10000

typingNotificationRefreshMicroseconds :: Int
typingNotificationRefreshMicroseconds =
  5 * 1000000
