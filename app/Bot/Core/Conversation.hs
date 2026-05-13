{-|
Module      : Bot.Core.Conversation
Description : Conversation graph
Stability   : experimental
-}

module Bot.Core.Conversation
  ( Conversation (..)
  , startWithUser
  , startWithUserContext
  , startWithSystemAndUser
  , startWithSystemAndUserContext
  , appendUser
  , appendUserContext
  , appendAssistant
  )
where

import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

-- | Ordered chat history sent to the LLM.
newtype Conversation = Conversation
  { messages :: Seq.Seq LLM.ChatMessage
  }
  deriving (Show, Generic)

instance Aeson.ToJSON Conversation where
  toJSON Conversation{messages} =
    Aeson.object
      [ "messages" Aeson..= Foldable.toList messages
      ]

instance Aeson.FromJSON Conversation where
  parseJSON = Aeson.withObject "Conversation" $ \o ->
    Conversation . Seq.fromList <$> o Aeson..: "messages"

-- | Start a conversation with a single user text message.
startWithUser :: Text -> Conversation
startWithUser prompt =
  startWithSystemAndUserContext "" prompt []

-- | Start a conversation with user text and image context.
startWithUserContext :: Text -> [Text] -> Conversation
startWithUserContext prompt imageUrls =
  startWithSystemAndUserContext "" prompt imageUrls

-- | Start with a system prompt and user text.
startWithSystemAndUser :: Text -> Text -> Conversation
startWithSystemAndUser systemPrompt prompt =
  startWithSystemAndUserContext systemPrompt prompt []

-- | Start with a system prompt plus user text and image context.
startWithSystemAndUserContext :: Text -> Text -> [Text] -> Conversation
startWithSystemAndUserContext systemPrompt prompt imageUrls =
  Conversation (Seq.fromList (systemMessages <> [LLM.userWithImages prompt imageUrls]))
  where
    systemMessages
      | Text.null systemPrompt = []
      | otherwise         = [LLM.systemText systemPrompt]

-- | Append a text-only user turn.
appendUser :: Text -> Conversation -> Conversation
appendUser prompt =
  appendUserContext prompt []

-- | Append a user turn with optional image context.
appendUserContext :: Text -> [Text] -> Conversation -> Conversation
appendUserContext prompt imageUrls (Conversation history) =
  Conversation (history Seq.|> LLM.userWithImages prompt imageUrls)

-- | Append an assistant reply, preserving generated image references as context.
appendAssistant :: Text -> Conversation -> Conversation
appendAssistant answer (Conversation history) =
  Conversation (history <> Seq.fromList (assistantContext answer))

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
