{-|
Module      : Bot.Handler.ShutUp
Description : Delete configured matches and repeated chat messages
Stability   : experimental
-}

module Bot.Handler.ShutUp
  ( shutUpHandlers
  )
where

import Bot.Core.Message
import Bot.Core.Route
import qualified Bot.Effect.Chat as Chat
import Bot.Handler.ShutUp.Config
import Bot.Prelude
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Effectful.Prim.IORef as IORef
import Text.Regex.TDFA (matchTest)

shutUpHandlers :: (Chat.Chat :> es, Prim :> es) => ShutUpConfig -> Eff es [RouteHandler (Eff es)]
shutUpHandlers cfg = do
  repeats <- IORef.newIORef Map.empty
  pure [stopOnM (deleteMatch repeats) deleteMatchedMessage]
  where
    deleteMatch repeats message = do
      repeated <- case (message.eventKind, message.chatId) of
        (IncomingMessageCreated, Just chatId) ->
          IORef.atomicModifyIORef' repeats \states ->
            let key = (message.platform, chatId)
                (next, remove) = stepRepeat message.text (Map.lookup key states)
                updated = maybe (Map.delete key states) (\value -> Map.insert key value states) next
            in (updated, remove)
        _ -> pure False
      pure (() <$ guard (repeated || isJust ((deletePattern cfg).runMessageFilter message)))

stepRepeat :: Text -> Maybe (Text, Int) -> (Maybe (Text, Int), Bool)
stepRepeat body previous
  | Text.null body = (Nothing, False)
  | Just (pattern_, 2) <- previous, pattern_ `Text.isInfixOf` body =
      (previous, True)
  | Just (pattern_, count) <- previous, pattern_ == body =
      (Just (body, count + 1), False)
  | otherwise = (Just (body, 1), False)

deletePattern :: ShutUpConfig -> MessageFilter DeletePattern
deletePattern cfg =
  MessageFilter \message ->
    List.find (`matchesMessage` message) cfg.deletePatterns

matchesMessage :: DeletePattern -> IncomingMessage -> Bool
matchesMessage pattern_ message =
  matchTest pattern_.regex message.text

deleteMatchedMessage :: Chat.Chat :> es => IncomingMessage -> a -> Eff es ()
deleteMatchedMessage message _ =
  traverse_ (void . Chat.deleteMessage message) message.messageId
