{-|
Module      : Bot.Filter
Description : Composable message filters and routes
Stability   : experimental
-}

module Bot.Filter
  ( -- * Message filters
    MessageFilter (..)
  , anything
  , matching
  , rejecting
  , fromGroups
  , command
  , notCommand
  , notReply
  , replyToMessage
  , promptOrImages
  , (<&&>)

    -- * Routing
  , RouteResult (..)
  , RouteHandler
  , route
  , routeStop
  , routeWith
  , runRouteResult
  , runHandlers
  , consumeWith
  )
where

import Bot.Message
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

-- | A handler decision and the action to run for matched messages.
data RouteResult es
  = MatchedAndContinue (Eff es ())
  | MatchedAndStop (Eff es ())
  | UnmatchedAndContinue
  | UnmatchedAndStop

-- | A route inspects a message and decides whether routing should continue.
type RouteHandler es = IncomingMessage -> Eff es (RouteResult es)

-- | Build a route that continues after a successful match.
route :: MessageFilter a -> (IncomingMessage -> a -> Eff es ()) -> RouteHandler es
route =
  routeWith MatchedAndContinue UnmatchedAndContinue

-- | Build a route that stops after a successful match.
routeStop :: MessageFilter a -> (IncomingMessage -> a -> Eff es ()) -> RouteHandler es
routeStop =
  routeWith MatchedAndStop UnmatchedAndContinue

-- | General route builder for custom match/miss decisions.
routeWith
  :: (Eff es () -> RouteResult es)
  -> RouteResult es
  -> MessageFilter a
  -> (IncomingMessage -> a -> Eff es ())
  -> RouteHandler es
routeWith matched unmatched (MessageFilter filt) handler message =
  pure case filt message of
    Just value -> matched (handler message value)
    Nothing    -> unmatched

-- | Execute a route action and return whether routing should continue.
runRouteResult :: RouteResult es -> Eff es Bool
runRouteResult = \case
  MatchedAndContinue action -> action $> True
  MatchedAndStop action     -> action $> False
  UnmatchedAndContinue      -> pure True
  UnmatchedAndStop          -> pure False

-- | Run handlers left-to-right until one asks the router to stop.
runHandlers :: [RouteHandler es] -> IncomingMessage -> Eff es ()
runHandlers [] _ =
  pure ()
runHandlers (handler : handlers) message = do
  continue <- handler message >>= runRouteResult
  when continue (runHandlers handlers message)

-- | Consume a message stream with a fixed handler pipeline.
consumeWith
  :: [RouteHandler es]
  -> Stream (Of IncomingMessage) (Eff es) ()
  -> Eff es ()
consumeWith handlers =
  S.mapM_ (runHandlers handlers)
