module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import Bot.Filter
import Bot.Message
import Bot.Prelude

main :: IO ()
main = do
  testCommandFilter
  testRouteStopPreventsLaterHandlers
  testRouteContinueRunsLaterHandlers

testCommandFilter :: IO ()
testCommandFilter = do
  let matched = applyFilter (command "!ask") (message "!ask hello")
      unmatched = applyFilter (command "!ask") (message "!draw hello")
  assertEqual "command strips and trims the prefix" (Just "hello") matched
  assertEqual "command rejects other prefixes" Nothing unmatched

testRouteStopPreventsLaterHandlers :: IO ()
testRouteStopPreventsLaterHandlers = do
  calls <- IORef.newIORef ([] :: [Text])
  runEff $ runHandlers
    [ appendStopCall calls "first"
    , appendStopCall calls "second"
    ]
    (message "hello")
  assertEqual "matched stop route prevents later handlers" ["first"] =<< IORef.readIORef calls

testRouteContinueRunsLaterHandlers :: IO ()
testRouteContinueRunsLaterHandlers = do
  calls <- IORef.newIORef ([] :: [Text])
  runEff $ runHandlers
    [ appendContinueCall calls "first"
    , appendStopCall calls "second"
    ]
    (message "hello")
  assertEqual "matched continue route runs later handlers" ["first", "second"] =<< IORef.readIORef calls

appendContinueCall :: IORef.IORef [Text] -> Text -> RouteHandler '[IOE]
appendContinueCall calls label =
  appendCall route calls label

appendStopCall :: IORef.IORef [Text] -> Text -> RouteHandler '[IOE]
appendStopCall calls label =
  appendCall routeStop calls label

appendCall
  :: (MessageFilter IncomingMessage -> (IncomingMessage -> IncomingMessage -> Eff '[IOE] ()) -> RouteHandler '[IOE])
  -> IORef.IORef [Text]
  -> Text
  -> RouteHandler '[IOE]
appendCall mkRoute calls label =
  mkRoute anything \_ _ ->
    liftIO $ IORef.modifyIORef' calls (<> [label])

applyFilter :: MessageFilter a -> IncomingMessage -> Maybe a
applyFilter (MessageFilter filt) =
  filt

message :: Text -> IncomingMessage
message text =
  IncomingMessage
    { platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just 100
    , senderId = Just 200
    , senderUsername = Just "alice"
    , messageId = Just 300
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , text = text
    , raw = Aeson.Null
    }

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  unless (expected == actual) $
    fail (label <> ": expected " <> show expected <> ", got " <> show actual)
