module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import Bot.Filter
import Bot.Message
import Bot.Prelude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "filter"
      [ testCase "command strips prefix and rejects other commands" testCommandFilter
      , testCase "routeStop prevents later handlers" testRouteStopPreventsLaterHandlers
      , testCase "route continues to later handlers" testRouteContinueRunsLaterHandlers
      ]

testCommandFilter :: IO ()
testCommandFilter = do
  let matched = applyFilter (command "!ask") (message "!ask hello")
      unmatched = applyFilter (command "!ask") (message "!draw hello")
  matched @?= Just "hello"
  unmatched @?= Nothing

testRouteStopPreventsLaterHandlers :: IO ()
testRouteStopPreventsLaterHandlers = do
  calls <- IORef.newIORef ([] :: [Text])
  runEff $ runHandlers
    [ appendStopCall calls "first"
    , appendStopCall calls "second"
    ]
    (message "hello")
  IORef.readIORef calls >>= (@?= ["first"])

testRouteContinueRunsLaterHandlers :: IO ()
testRouteContinueRunsLaterHandlers = do
  calls <- IORef.newIORef ([] :: [Text])
  runEff $ runHandlers
    [ appendContinueCall calls "first"
    , appendStopCall calls "second"
    ]
    (message "hello")
  IORef.readIORef calls >>= (@?= ["first", "second"])

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
message =
  messageFrom 200

messageFrom :: Integer -> Text -> IncomingMessage
messageFrom senderId text =
  IncomingMessage
    { platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just 100
    , senderId = Just senderId
    , senderUsername = Just "alice"
    , messageId = Just 300
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , text = text
    , raw = Aeson.Null
    }
