module Main (main) where

import Bot.Chat.Driver.Types (ChatDriverEffects)
import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Core.Route
import Bot.Handler.ShutUp
import Bot.Handler.ShutUp.Config
import Bot.Prelude
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.IORef as IORef
import Test.Tasty
import Test.Tasty.HUnit
import Text.Regex.TDFA
  ( defaultCompOpt
  , defaultExecOpt
  , makeRegexOptsM
  )

newtype DeleteChatDriver es =
  DeleteChatDriver (IncomingMessage -> MessageId -> Eff es Bool)

instance Driver.ChatDriver (DeleteChatDriver es0) where
  type ChatDriverEffects (DeleteChatDriver es0) es = es ~ es0
  driverPlatform _ = PlatformTelegram
  deleteMessage (DeleteChatDriver delete) =
    delete

newtype ModerationChatDriver es = ModerationChatDriver (Text -> Eff es ())

instance Driver.ChatDriver (ModerationChatDriver es0) where
  type ChatDriverEffects (ModerationChatDriver es0) es = es ~ es0
  driverPlatform _ = PlatformTelegram
  banMember (ModerationChatDriver record) _ user = record ("ban:" <> user) $> True
  deleteMessage (ModerationChatDriver record) _ mid = record ("delete:" <> messageIdText mid) $> True

main :: IO ()
main =
  defaultMain $
    testGroup "shutup"
      [ testCase "guest bots ban both identities and delete, stopping routing" testGuestBot
      , testCase "matching message is deleted and routing stops" testMatchingMessageIsDeleted
      , testCase "nonmatching message continues routing" testNonmatchingMessageContinues
      , testCase "third repeat and containing suffixes are deleted" $
          checkStream (map messageWithText ["a", "a", "a", "a", "xa", "ax", "b", "b", "b", "b"]) [3,4,5,6,9,10]
      , testCase "arming requires exact consecutive text" $
          checkStream (map messageWithText ["a", "xa", "a", "a", "b", "a", "a", "a", "a"]) [8,9]
      , testCase "empty messages reset without arming" $
          checkStream (map messageWithText ["", "", "", "hello", "a", "a", "", "a"]) []
      , testCase "pattern is literal and preserved after substring matches" $
          checkStream (map messageWithText [".*", ".*", ".*", "x.*", ".*", "anything"]) [3,4,5]
      , testCase "chat and platform scope are independent" $
          let a = messageWithText "a"
              b = a{chatId = Just "101"}
              c = a{platform = PlatformMatrix}
          in checkStream [a,b,c,a,b,c,a,b,c,a,b,c] [7,8,9,10,11,12]
      , testCase "missing chat identity does not contribute" $
          let a = messageWithText "a"
          in checkStream [a,a{chatId = Nothing},a,a,a] [4,5]
      , testCase "deletion events do not contribute or reset" $
          let a = messageWithText "a"
          in checkStream [a,a,a{eventKind = IncomingMessageDeleted},a,(messageWithText "b"){eventKind = IncomingMessageDeleted},a] [4,6]
      ]

checkStream :: [IncomingMessage] -> [Int] -> IO ()
checkStream messages expected = do
  deleted <- IORef.newIORef []
  continued <- IORef.newIORef []
  runEff $ runPrim $ startKatipE "shutup-spec" "test" $ Chat.runChatWith
    (DeleteChatDriver \_ messageId -> liftIO (IORef.modifyIORef' deleted (<> [messageId])) $> True) do
      handlers <- shutUpHandlers defaultShutUpConfig
      let later = continueOn anything \message _ ->
            liftIO $ IORef.modifyIORef' continued (<> [message.messageId])
      forM_ (zip [1 :: Int ..] messages) \(index, message) ->
        runHandlers (handlers <> [later]) message{Bot.Core.Message.messageId = Just (fromString (show index))}
  IORef.readIORef deleted >>= (@?= map (fromString . show) expected)
  IORef.readIORef continued >>= (@?= [Just (fromString (show index)) | index <- [1..length messages], index `notElem` expected])

testMatchingMessageIsDeleted :: IO ()
testMatchingMessageIsDeleted = do
  deleted <- IORef.newIORef ([] :: [MessageId])
  later <- IORef.newIORef False
  cfg <- either assertFailure pure (compiledConfig ["spam[[:space:]]+message"])
  runShutUp deleted later cfg (messageWithText "this is a spam message")
  IORef.readIORef deleted >>= (@?= ["300"])
  IORef.readIORef later >>= (@?= False)

testNonmatchingMessageContinues :: IO ()
testNonmatchingMessageContinues = do
  deleted <- IORef.newIORef ([] :: [MessageId])
  later <- IORef.newIORef False
  cfg <- either assertFailure pure (compiledConfig ["spam[[:space:]]+message"])
  runShutUp deleted later cfg (messageWithText "ordinary message")
  IORef.readIORef deleted >>= (@?= [])
  IORef.readIORef later >>= (@?= True)

runShutUp :: IORef.IORef [MessageId] -> IORef.IORef Bool -> ShutUpConfig -> IncomingMessage -> IO ()
runShutUp deleted later cfg incoming =
  runEff $ runPrim $ startKatipE "shutup-spec" "test" $
    Chat.runChatWith
      ( DeleteChatDriver \_ messageId -> do
          liftIO $ IORef.modifyIORef' deleted (<> [messageId])
          pure True
      ) $
      do
        handlers <- shutUpHandlers cfg
        runHandlers (handlers <> [laterRoute]) incoming
  where
    laterRoute =
      continueOn anything \_ _ ->
        liftIO $ IORef.writeIORef later True

compiledConfig :: [Text] -> Either String ShutUpConfig
compiledConfig patterns =
  ShutUpConfig <$> traverse compile patterns
  where
    compile source =
      case makeRegexOptsM defaultCompOpt defaultExecOpt source of
        Left err -> Left err
        Right regex -> Right DeletePattern{source, regex}

messageWithText :: Text -> IncomingMessage
messageWithText body =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , replyQuote = Nothing
    , timestamp = Nothing
    , platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just "100"
    , chatAliases = []
    , chatDisplayName = Nothing
    , digest = emptyMessageDigest
    , senderId = Just "200"
    , senderUsername = Just "alice"
    , senderDisplayName = Nothing
    , senderGlobalDisplayName = Nothing
    , messageId = Just "300"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = body
    , raw = Aeson.Null
    }


testGuestBot :: IO ()
testGuestBot = do
  let guest = (messageWithText "guest response")
        { kind = ChatGroup
        , raw = Aeson.object
            [ "from" Aeson..= Aeson.object ["id" Aeson..= (200 :: Int), "is_bot" Aeson..= True]
            , "guest_bot_caller_user" Aeson..= Aeson.object ["id" Aeson..= (987654 :: Int)]
            ]
        }
      expected = ["ban:200", "ban:987654", "delete:300"]
  checkModeration guest Nothing expected False
  let withReply author chat target = guest
        { raw = case guest.raw of
            Aeson.Object o -> Aeson.Object (KeyMap.insert "reply_to_message" (Aeson.object
              [ "from" Aeson..= Aeson.object ["id" Aeson..= (author :: Integer)]
              , "chat" Aeson..= Aeson.object ["id" Aeson..= (chat :: Integer)]
              , "message_id" Aeson..= (target :: Integer)
              ]) o)
            value -> value
        }
      linkedGuest = withReply 987654 100 182
      linkedExpected = expected <> ["delete:182"]
  checkModeration linkedGuest Nothing linkedExpected False
  for_ ["ban:200", "ban:987654", "delete:300", "delete:182"] \failed ->
    checkModeration linkedGuest (Just failed) linkedExpected True
  for_ [withReply 987655 100 182, withReply 987654 101 182, withReply 987654 100 0] \unrelated ->
    checkModeration unrelated Nothing expected False
  checkModerationCancellation linkedGuest
  for_ ["ban:200", "ban:987654", "delete:300"] \failed ->
    checkModeration guest (Just failed) expected True
  for_ [ guest{platform = PlatformMatrix}
       , guest{kind = ChatPrivate}
       , guest{eventKind = IncomingMessageDeleted}
       , guest{senderId = Nothing}
       , guest{chatId = Nothing}
       , guest{raw = Aeson.Null}
       , guest{raw = Aeson.object ["from" Aeson..= Aeson.object ["is_bot" Aeson..= False], "guest_bot_caller_user" Aeson..= Aeson.object ["id" Aeson..= (987654 :: Int)]]}
       , guest{raw = Aeson.object ["from" Aeson..= Aeson.object ["is_bot" Aeson..= True], "via_bot" Aeson..= Aeson.object ["id" Aeson..= (987654 :: Int)]]}
       ] \ordinary -> checkModeration ordinary Nothing ["continued"] False

checkModeration :: IncomingMessage -> Maybe Text -> [Text] -> Bool -> IO ()
checkModeration message failure expected shouldFail = do
  actions <- IORef.newIORef []
  let record :: IOE :> es => Text -> Eff es ()
      record action = do
        liftIO (IORef.modifyIORef' actions (<> [action]))
        when (failure == Just action) (throwIO (Exception.ErrorCall "moderation failed"))
  result <- runEff $ runPrim $ startKatipE "shutup-spec" "test" $ trySync $ Chat.runChatWith (ModerationChatDriver record) do
        handlers <- shutUpHandlers defaultShutUpConfig
        runHandlers (handlers <> [continueOn anything (\_ _ -> record "continued")]) message
  IORef.readIORef actions >>= (@?= expected)
  isLeft result @?= shouldFail


checkModerationCancellation :: IncomingMessage -> IO ()
checkModerationCancellation message = do
  actions <- IORef.newIORef ([] :: [Text])
  result <- Exception.try @Exception.AsyncException $ runEff $ runPrim $ startKatipE "shutup-spec" "test" $
    Chat.runChatWithHandler
      (\_ -> \case
        Chat.BanMember _ user -> do
          liftIO (IORef.modifyIORef' actions (<> [user]))
          throwIO Exception.ThreadKilled
        _ -> error "cancellation must stop moderation") do
          handlers <- shutUpHandlers defaultShutUpConfig
          runHandlers handlers message
  result @?= Left Exception.ThreadKilled
  IORef.readIORef actions >>= (@?= ["200"])
