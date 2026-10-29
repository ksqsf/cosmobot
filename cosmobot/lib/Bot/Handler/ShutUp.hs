{-|
Module      : Bot.Handler.ShutUp
Description : Moderate guest bots, configured matches, and repeated chat messages
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
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Effectful.Prim.IORef as IORef
import Text.Regex.TDFA (matchTest)

shutUpHandlers :: (Chat.Chat :> es, Prim :> es, KatipE :> es) => ShutUpConfig -> Eff es [RouteHandler (Eff es)]
shutUpHandlers cfg = do
  repeats <- IORef.newIORef Map.empty
  pure [stopOn guestBot banGuestBot, stopOnM (deleteMatch repeats) deleteMatchedMessage]
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

-- | Require both user identities before issuing destructive moderation actions.
guestBot :: MessageFilter (Text, Text)
guestBot = MessageFilter \message -> do
  guard (message.platform == PlatformTelegram && message.kind == ChatGroup)
  guard (message.eventKind == IncomingMessageCreated)
  _ <- message.chatId
  bot <- message.senderId
  caller <- either (const Nothing) Just (AesonTypes.parseEither guestCaller message.raw)
  guard (not (Text.null bot))
  pure (bot, caller)

banGuestBot :: (Chat.Chat :> es, KatipE :> es) => IncomingMessage -> (Text, Text) -> Eff es ()
banGuestBot message (bot, caller) =
  katipAddContext (sl "moderation" ("guest_bot" :: Text)
    <> sl "chat_id" (fmap chatIdText message.chatId)
    <> sl "guest_bot_id" bot <> sl "caller_id" caller) do
    results <- sequence
      [ logModeration "ban_bot" bot (Chat.banMember message bot)
      , logModeration "ban_caller" caller (Chat.banMember message caller)
      , deleteTarget "delete_guest_reply" message.messageId
      , deleteTarget "delete_trigger" (guestTrigger message caller)
      ]
    traverse_ (either throwIO pure) results
  where
    deleteTarget action = \case
      Nothing -> katipAddContext (sl "moderation_action" action <> sl "result" ("skipped" :: Text)) do
        $(logWarning) "Guest bot moderation skipped deletion: missing or unverified message target"
        pure (Right ())
      Just target -> logModeration action (messageIdText target) (Chat.deleteMessage message target)

logModeration :: KatipE :> es => Text -> Text -> Eff es Bool -> Eff es (Either SomeException ())
logModeration action target operation =
  katipAddContext (sl "moderation_action" action <> sl "target_id" target) do
    katipAddContext (sl "result" ("started" :: Text)) $
      $(logInfo) "Guest bot moderation started"
    result <- trySync operation
    case result of
      Left err -> do
        katipAddContext (sl "result" ("exception" :: Text)) $
          $(logWarning) "Guest bot moderation failed with an exception"
        pure (Left err)
      Right success -> do
        katipAddContext (sl "result" (if success then "succeeded" else "failed" :: Text)) $
          if success
            then $(logInfo) "Guest bot moderation succeeded"
            else $(logWarning) "Guest bot moderation failed"
        pure (Right ())

guestCaller :: Aeson.Value -> AesonTypes.Parser Text
guestCaller = Aeson.withObject "Telegram message" \o -> do
  sender <- o Aeson..: "from"
  isBot <- sender Aeson..: "is_bot"
  guard isBot
  caller <- o Aeson..: "guest_bot_caller_user"
  userId <- caller Aeson..: "id"
  guard (userId > (0 :: Integer))
  pure (Text.pack (show userId))

-- | A reply target is destructive evidence only when its author and chat match.
guestTrigger :: IncomingMessage -> Text -> Maybe MessageId
guestTrigger message caller = do
  chatId <- message.chatId >>= chatIdInteger
  either (const Nothing) Just $ AesonTypes.parseEither (Aeson.withObject "Telegram message" \o -> do
    reply <- o Aeson..: "reply_to_message"
    author <- reply Aeson..: "from"
    authorId <- author Aeson..: "id" :: AesonTypes.Parser Integer
    guard (Text.pack (show authorId) == caller)
    chat <- reply Aeson..: "chat"
    replyChatId <- chat Aeson..: "id"
    guard (replyChatId == chatId)
    target <- reply Aeson..: "message_id"
    guard (target > 0)
    pure (integerMessageId target)) message.raw
