{-
Module      : Bot.Conversation
Description : In-memory conversation graph
Stability   : experimental
-}

module Bot.Conversation where

import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Text as Text
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map

newtype ConversationStore = ConversationStore
  { unConversationStore :: IORef.IORef (Map Integer Conversation)
  }

newtype Conversation = Conversation
  { messages :: [LLM.ChatMessage]
  }
  deriving (Show)

newConversationStore :: IO ConversationStore
newConversationStore =
  ConversationStore <$> IORef.newIORef Map.empty

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
lookupConversation (ConversationStore ref) messageId =
  liftIO $ Map.lookup messageId <$> IORef.readIORef ref

rememberConversation :: IOE :> es => ConversationStore -> Maybe Integer -> Conversation -> Eff es ()
rememberConversation _ Nothing _ =
  pure ()
rememberConversation (ConversationStore ref) (Just messageId) conversation =
  liftIO $ IORef.modifyIORef' ref (Map.insert messageId conversation)
