{-
Module      : Bot.Conversation
Description : In-memory conversation graph
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Conversation where

import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Storage.SQLite as Storage
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text as Text
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map

data ConversationStore = ConversationStore
  { unConversationStore :: IORef.IORef (Map Integer Conversation)
  , sqliteStore :: !(Maybe Storage.SQLiteStore)
  }

newtype Conversation = Conversation
  { messages :: [LLM.ChatMessage]
  }
  deriving (Show, Generic)

instance Aeson.ToJSON Conversation where
  toJSON Conversation{messages} =
    Aeson.object
      [ "messages" Aeson..= messages
      ]

instance Aeson.FromJSON Conversation where
  parseJSON = Aeson.withObject "Conversation" $ \o ->
    Conversation <$> o Aeson..: "messages"

newConversationStore :: Maybe Storage.SQLiteStore -> IO ConversationStore
newConversationStore sqliteStore = do
  conversations <- maybe (pure Map.empty) loadStoredConversations sqliteStore
  ref <- IORef.newIORef conversations
  pure ConversationStore{unConversationStore = ref, sqliteStore}

loadStoredConversations :: Storage.SQLiteStore -> IO (Map Integer Conversation)
loadStoredConversations store =
  Map.mapMaybe decodeConversation <$> Storage.loadConversationRows store

decodeConversation :: Text -> Maybe Conversation
decodeConversation =
  either (const Nothing) Just . Aeson.eitherDecodeStrict' . TextEncoding.encodeUtf8

startWithUser :: Text -> Conversation
startWithUser prompt =
  startWithSystemAndUserContext "" prompt []

startWithUserContext :: Text -> [Text] -> Conversation
startWithUserContext prompt imageUrls =
  startWithSystemAndUserContext "" prompt imageUrls

startWithSystemAndUser :: Text -> Text -> Conversation
startWithSystemAndUser systemPrompt prompt =
  startWithSystemAndUserContext systemPrompt prompt []

startWithSystemAndUserContext :: Text -> Text -> [Text] -> Conversation
startWithSystemAndUserContext systemPrompt prompt imageUrls =
  Conversation (systemMessages <> [LLM.userWithImages prompt imageUrls])
  where
    systemMessages
      | Text.null systemPrompt = []
      | otherwise         = [LLM.systemText systemPrompt]

appendUser :: Text -> Conversation -> Conversation
appendUser prompt =
  appendUserContext prompt []

appendUserContext :: Text -> [Text] -> Conversation -> Conversation
appendUserContext prompt imageUrls (Conversation history) =
  Conversation (history <> [LLM.userWithImages prompt imageUrls])

appendAssistant :: Text -> Conversation -> Conversation
appendAssistant answer (Conversation history) =
  Conversation (history <> assistantContext answer)

assistantContext :: Text -> [LLM.ChatMessage]
assistantContext answer =
  assistantTextContext <> imageContext
  where
    answerText = Chat.renderReplyBody answer
    imageUrls = Chat.replyImageUrls answer
    assistantTextContext =
      [ LLM.assistantText answerText | not (Text.null answerText) ] <>
      [ LLM.assistantText "Generated image." | Text.null answerText && not (null imageUrls) ]
    imageContext =
      [ LLM.userWithImages "The previous assistant response generated this image. Use it as visual context for follow-up questions." imageUrls
      | not (null imageUrls)
      ]

lookupConversation :: IOE :> es => ConversationStore -> Integer -> Eff es (Maybe Conversation)
lookupConversation ConversationStore{unConversationStore = ref} messageId =
  liftIO $ Map.lookup messageId <$> IORef.readIORef ref

rememberConversation :: (IOE :> es, Log :> es) => ConversationStore -> Maybe Integer -> Conversation -> Eff es ()
rememberConversation _ Nothing _ =
  pure ()
rememberConversation ConversationStore{unConversationStore = ref, sqliteStore} (Just messageId) conversation = do
  liftIO $ IORef.modifyIORef' ref (Map.insert messageId conversation)
  traverse_
    ( \store ->
        liftIO (Storage.saveConversationJson store messageId (conversationJson conversation))
          `catch` \(err :: SomeException) ->
            logInfo "Failed to persist conversation" (show err :: String)
    )
    sqliteStore

conversationJson :: Conversation -> Text
conversationJson =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode
