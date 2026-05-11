{-|
Module      : Bot.Core.Route
Description : Composable message filters and routes
Stability   : experimental
-}

module Bot.Core.Route
  ( -- * Message filters
    MessageFilter (..)
  , anything
  , matching
  , rejecting
  , fromGroups
  , command
  , prefixedText
  , notCommand
  , notReply
  , replyToMessage
  , promptOrImages
  , (<&&>)

    -- * Routing
  , Route (..)
  , RouteDecision (..)
  , RouteHelp (..)
  , isAllowedGroup
  , isAllowedPrivate
  , isSuperuser
  , canStartConversation
  , canStartFromReply
  , mentionsConfiguredBot
  , RouteHandler
  , continueOn
  , stopOn
  , requireAuth
  , withHelp
  , runRouteDecision
  , runRoute
  , runHandlers
  , consumeWith
  )
where

import Bot.Core.Message
import Bot.Prelude
import qualified Data.Text as Text
import qualified Streaming.Prelude as S

-- | Pure matcher that may extract a value from an incoming message.
newtype MessageFilter a = MessageFilter
  { runMessageFilter :: IncomingMessage -> Maybe a
  }

instance Functor MessageFilter where
  fmap f (MessageFilter g) = MessageFilter (fmap f . g)

instance Applicative MessageFilter where
  pure value = MessageFilter \_ -> Just value
  MessageFilter left <*> MessageFilter right =
    MessageFilter \message -> left message <*> right message

instance Alternative MessageFilter where
  empty =
    MessageFilter \_ -> Nothing
  MessageFilter left <|> MessageFilter right =
    MessageFilter \message -> left message <|> right message

infixl 4 <&&>

-- | Run two filters against the same message and keep both extracted values.
(<&&>) :: MessageFilter a -> MessageFilter b -> MessageFilter (a, b)
MessageFilter left <&&> MessageFilter right =
  MessageFilter $ \message -> (,) <$> left message <*> right message

-- | Match every message.
anything :: MessageFilter IncomingMessage
anything = MessageFilter Just

-- | Keep messages satisfying a predicate.
matching :: (IncomingMessage -> Bool) -> MessageFilter IncomingMessage
matching predicate =
  MessageFilter \message ->
    if predicate message
      then Just message
      else Nothing

-- | Keep messages that do not satisfy a predicate.
rejecting :: (IncomingMessage -> Bool) -> MessageFilter IncomingMessage
rejecting predicate =
  matching (not . predicate)

-- | Match group messages whose chat id is in the whitelist.
fromGroups :: [Integer] -> MessageFilter IncomingMessage
fromGroups allowed =
  MessageFilter $ \message ->
    if message.kind == ChatGroup && maybe False (`elem` allowed) message.chatId
      then Just message
      else Nothing

-- | Match a text command prefix and return the stripped remainder.
command :: Text -> MessageFilter Text
command prefix =
  MessageFilter $ \message ->
    Text.strip <$> Text.stripPrefix prefix message.text

-- | Match a text prefix and keep the full stripped message text.
prefixedText :: Text -> MessageFilter Text
prefixedText prefix =
  MessageFilter $ \message ->
    Text.strip message.text <$ guard (not (Text.null prefix) && prefix `Text.isPrefixOf` message.text)

-- | Reject messages starting with a command prefix.
notCommand :: Text -> MessageFilter IncomingMessage
notCommand prefix =
  rejecting (Text.isPrefixOf prefix . (.text))

-- | Reject replies.
notReply :: MessageFilter IncomingMessage
notReply =
  rejecting (isJust . (.replyToMessageId))

-- | Extract the referenced message id from replies.
replyToMessage :: MessageFilter Integer
replyToMessage =
  MessageFilter (.replyToMessageId)

-- | Match messages that contain either non-empty text or images.
promptOrImages :: MessageFilter Text
promptOrImages =
  MessageFilter \message ->
    let prompt = Text.strip message.text
    in if Text.null prompt && null message.imageUrls
      then Nothing
      else Just prompt

-- | Whether a group message is from an explicitly allowed chat.
isAllowedGroup :: IncomingMessage -> Bool
isAllowedGroup message =
  message.kind == ChatGroup && message.digest.chatIsAllowed

-- | Whether the message mentions the configured bot identity.
mentionsConfiguredBot :: IncomingMessage -> Bool
mentionsConfiguredBot message =
  message.digest.mentionsBot

-- | Whether a private message may start a conversation.
isAllowedPrivate :: IncomingMessage -> Bool
isAllowedPrivate message =
  message.kind == ChatPrivate && message.digest.senderIsSuperuser

-- | Whether the message sender may use privileged routes/tools.
isSuperuser :: IncomingMessage -> Bool
isSuperuser message =
  message.digest.senderIsSuperuser

-- | General admission predicate for starting a new conversation.
canStartConversation :: IncomingMessage -> Bool
canStartConversation message =
  case message.kind of
    ChatPrivate -> isAllowedPrivate message
    ChatGroup   -> isAllowedGroup message || mentionsConfiguredBot message
    _           -> False

-- | Admission predicate for starting from a reply to an unknown message.
canStartFromReply :: IncomingMessage -> Bool
canStartFromReply message =
  case message.kind of
    ChatPrivate -> isAllowedPrivate message
    ChatGroup   -> isAllowedGroup message && mentionsConfiguredBot message
    _           -> False

-- | The routing algebra: skip, handle and continue, or handle and stop.
data RouteDecision es
  = Skip
  | ContinueWith (Eff es ())
  | StopWith (Eff es ())

-- | Metadata that can be folded into user-visible help without inspecting the route function.
data RouteHelp = RouteHelp
  { label       :: !Text
  , description :: !Text
  }
  deriving (Eq, Show)

-- | A route is an executable decision function plus optional help metadata.
data Route es = Route
  { help   :: !(Maybe RouteHelp)
  , decide :: IncomingMessage -> Eff es (RouteDecision es)
  }

type RouteHandler es = Route es

continueOn :: MessageFilter a -> (IncomingMessage -> a -> Eff es ()) -> Route es
continueOn =
  onMatch ContinueWith

stopOn :: MessageFilter a -> (IncomingMessage -> a -> Eff es ()) -> Route es
stopOn =
  onMatch StopWith

onMatch
  :: (Eff es () -> RouteDecision es)
  -> MessageFilter a
  -> (IncomingMessage -> a -> Eff es ())
  -> Route es
onMatch matched (MessageFilter filt) handler =
  Route
    { help = Nothing
    , decide = \message ->
        pure case filt message of
          Nothing ->
            Skip
          Just value ->
            matched (handler message value)
    }

requireAuth
  :: (IncomingMessage -> Bool)
  -> (IncomingMessage -> Eff es ())
  -> Route es
  -> Route es
requireAuth allowed denied route =
  route
    { decide = \message -> do
        decision <- route.decide message
        pure case decision of
          Skip ->
            Skip
          ContinueWith action
            | allowed message ->
                ContinueWith action
            | otherwise ->
                StopWith (denied message)
          StopWith action
            | allowed message ->
                StopWith action
            | otherwise ->
                StopWith (denied message)
    }

withHelp :: RouteHelp -> Route es -> Route es
withHelp help route =
  route{help = Just help}

-- | Execute a route action and return whether routing should continue.
runRouteDecision :: RouteDecision es -> Eff es Bool
runRouteDecision = \case
  Skip                -> pure True
  ContinueWith action -> action $> True
  StopWith action     -> action $> False

runRoute :: Route es -> IncomingMessage -> Eff es (RouteDecision es)
runRoute route message =
  route.decide message

-- | Run handlers left-to-right until one asks the router to stop.
runHandlers :: [RouteHandler es] -> IncomingMessage -> Eff es ()
runHandlers [] _ =
  pure ()
runHandlers (handler : handlers) message = do
  continue <- runRoute handler message >>= runRouteDecision
  when continue (runHandlers handlers message)

-- | Consume a message stream with a fixed handler pipeline.
consumeWith
  :: [RouteHandler es]
  -> Stream (Of IncomingMessage) (Eff es) ()
  -> Eff es ()
consumeWith handlers =
  S.mapM_ (runHandlers handlers)
