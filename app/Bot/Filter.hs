{-
Module      : Bot.Filter
Description : Composable message filters and routes
Stability   : experimental
-}

module Bot.Filter where

import Bot.Message
import Bot.Prelude
import qualified Data.Text as Text
import qualified Streaming.Prelude as S

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

(<&&>) :: MessageFilter a -> MessageFilter b -> MessageFilter (a, b)
MessageFilter left <&&> MessageFilter right =
  MessageFilter $ \message -> (,) <$> left message <*> right message

anything :: MessageFilter IncomingMessage
anything = MessageFilter Just

matching :: (IncomingMessage -> Bool) -> MessageFilter IncomingMessage
matching predicate =
  MessageFilter \message ->
    if predicate message
      then Just message
      else Nothing

rejecting :: (IncomingMessage -> Bool) -> MessageFilter IncomingMessage
rejecting predicate =
  matching (not . predicate)

fromGroups :: [Integer] -> MessageFilter IncomingMessage
fromGroups allowed =
  MessageFilter $ \message ->
    if message.kind == ChatGroup && maybe False (`elem` allowed) message.chatId
      then Just message
      else Nothing

command :: Text -> MessageFilter Text
command prefix =
  MessageFilter $ \message ->
    Text.strip <$> Text.stripPrefix prefix message.text

notCommand :: Text -> MessageFilter IncomingMessage
notCommand prefix =
  rejecting (Text.isPrefixOf prefix . (.text))

notReply :: MessageFilter IncomingMessage
notReply =
  rejecting (isJust . (.replyToMessageId))

replyToMessage :: MessageFilter Integer
replyToMessage =
  MessageFilter (.replyToMessageId)

promptOrImages :: MessageFilter Text
promptOrImages =
  MessageFilter \message ->
    let prompt = Text.strip message.text
    in if Text.null prompt && null message.imageUrls
      then Nothing
      else Just prompt

data RouteResult es
  = MatchedAndContinue (Eff es ())
  | MatchedAndStop (Eff es ())
  | UnmatchedAndContinue
  | UnmatchedAndStop

type RouteHandler es = IncomingMessage -> Eff es (RouteResult es)

route :: MessageFilter a -> (IncomingMessage -> a -> Eff es ()) -> RouteHandler es
route =
  routeWith MatchedAndContinue UnmatchedAndContinue

routeStop :: MessageFilter a -> (IncomingMessage -> a -> Eff es ()) -> RouteHandler es
routeStop =
  routeWith MatchedAndStop UnmatchedAndContinue

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

runRouteResult :: RouteResult es -> Eff es Bool
runRouteResult = \case
  MatchedAndContinue action -> action $> True
  MatchedAndStop action     -> action $> False
  UnmatchedAndContinue      -> pure True
  UnmatchedAndStop          -> pure False

routeMatched :: RouteResult es -> Bool
routeMatched = \case
  MatchedAndContinue _ -> True
  MatchedAndStop _     -> True
  UnmatchedAndContinue -> False
  UnmatchedAndStop     -> False

routeContinues :: RouteResult es -> Bool
routeContinues = \case
  MatchedAndContinue _ -> True
  MatchedAndStop _     -> False
  UnmatchedAndContinue -> True
  UnmatchedAndStop     -> False

runHandlers :: [RouteHandler es] -> IncomingMessage -> Eff es ()
runHandlers [] _ =
  pure ()
runHandlers (handler : handlers) message = do
  continue <- handler message >>= runRouteResult
  when continue (runHandlers handlers message)

consumeWith
  :: [RouteHandler es]
  -> Stream (Of IncomingMessage) (Eff es) ()
  -> Eff es ()
consumeWith handlers =
  S.mapM_ (runHandlers handlers)
