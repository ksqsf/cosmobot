{-|
Module      : Bot.Core.Conversation
Description : Core conversation value
Stability   : experimental
-}

module Bot.Core.Conversation
  ( Conversation (..)
  , ConversationMessageKey (..)
  , conversationMessageKey
  , ConversationTreeNode (..)
  , ConversationTree (..)
  , emptyConversationTree
  , lookupConversationTreeNode
  , insertConversationTreeNode
  , conversationTreeEntries
  , startWithUser
  , startWithUserContext
  , startWithSystemAndUser
  , startWithSystemAndUserContext
  , appendUser
  , appendUserContext
  , appendSystemAndUserContext
  , appendAssistant
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

-- | Platform-neutral conversation history.
--
-- A 'Conversation' is the exact ordered message context that will be sent back
-- to the LLM when a user continues a thread. It deliberately does not contain
-- chat ids, message ids, parent links, active stream handles, or persistence
-- details; those belong to the storage layer that indexes and reloads
-- conversations for a concrete bot runtime.
--
-- Keeping this type small matters because handlers can treat it as an
-- immutable value: start with the admitted user prompt, append normalized user
-- and assistant turns, then hand the resulting value to the LLM effect or to
-- storage.
newtype Conversation = Conversation
  { messages :: Seq.Seq LLM.ChatMessage
  }
  deriving (Show, Generic)

-- Conversation JSON is intentionally an object instead of a bare list, so the
-- core value can grow without breaking persisted rows immediately.
instance Aeson.ToJSON Conversation where
  toJSON Conversation{messages} =
    Aeson.object
      [ "messages" Aeson..= Foldable.toList messages
      ]

instance Aeson.FromJSON Conversation where
  parseJSON = Aeson.withObject "Conversation" $ \o ->
    Conversation . Seq.fromList <$> o Aeson..: "messages"

-- | Chat-scoped identity for a message that can anchor a conversation node.
--
-- Platform message ids are not globally unique. Telegram message ids, for
-- example, are scoped to a chat, so the key must carry the normalized platform
-- and chat identity together with the message id.
data ConversationMessageKey = ConversationMessageKey
  { platform :: !ChatPlatform
  , chatId :: !(Maybe Integer)
  , messageId :: !Integer
  }
  deriving (Eq, Ord, Show)

conversationMessageKey :: IncomingMessage -> Integer -> ConversationMessageKey
conversationMessageKey message messageId =
  ConversationMessageKey
    { platform = message.platform
    , chatId = message.chatId
    , messageId = messageId
    }

-- | One node in the reply tree.
--
-- The node keeps the accumulated LLM conversation for its message and an
-- optional parent key. It does not know how the node is cached or persisted.
data ConversationTreeNode = ConversationTreeNode
  { messageKey :: !ConversationMessageKey
  , parentMessageKey :: !(Maybe ConversationMessageKey)
  , conversation :: !Conversation
  }
  deriving (Show)

-- | Pure conversation tree indexed by chat-scoped message keys.
--
-- The tree is represented as keyed nodes with parent links instead of a nested
-- child list because the main runtime operation is reply lookup by message id.
-- Storage modules can still derive branches by following 'parentMessageKey'.
newtype ConversationTree = ConversationTree
  { nodes :: Map.Map ConversationMessageKey ConversationTreeNode
  }
  deriving (Show)

emptyConversationTree :: ConversationTree
emptyConversationTree =
  ConversationTree Map.empty

lookupConversationTreeNode :: ConversationMessageKey -> ConversationTree -> Maybe ConversationTreeNode
lookupConversationTreeNode messageKey tree =
  Map.lookup messageKey tree.nodes

insertConversationTreeNode :: ConversationTreeNode -> ConversationTree -> ConversationTree
insertConversationTreeNode node tree =
  ConversationTree (Map.insert node.messageKey node tree.nodes)

conversationTreeEntries :: ConversationTree -> [(ConversationMessageKey, ConversationTreeNode)]
conversationTreeEntries =
  Map.toList . (.nodes)

-- | Start a conversation from a single text-only user prompt.
--
-- This is the common path for commands that do not provide an explicit system
-- prompt and do not attach image context.
startWithUser :: Text -> Conversation
startWithUser prompt =
  startWithSystemAndUserContext "" prompt []

-- | Start a conversation from user text plus image URLs.
--
-- Image URLs are encoded into the initial user message because the LLM API sees
-- them as part of the same user turn, not as independent bot state.
startWithUserContext :: Text -> [Text] -> Conversation
startWithUserContext prompt imageUrls =
  startWithSystemAndUserContext "" prompt imageUrls

-- | Start with a system prompt followed by a text-only user prompt.
--
-- The system prompt is omitted entirely when it is empty, so callers do not
-- create blank system messages by accident.
startWithSystemAndUser :: Text -> Text -> Conversation
startWithSystemAndUser systemPrompt prompt =
  startWithSystemAndUserContext systemPrompt prompt []

-- | Start with optional system instructions plus the first user turn.
--
-- This is the most general constructor. The resulting sequence is always
-- system messages first, then exactly one user message containing the prompt
-- and any image context.
startWithSystemAndUserContext :: Text -> Text -> [Text] -> Conversation
startWithSystemAndUserContext systemPrompt prompt imageUrls =
  Conversation (Seq.fromList (systemMessages <> [LLM.userWithImages prompt imageUrls]))
  where
    systemMessages
      | Text.null systemPrompt = []
      | otherwise         = [LLM.systemText systemPrompt]

-- | Append a text-only user turn to an existing history.
appendUser :: Text -> Conversation -> Conversation
appendUser prompt =
  appendUserContext prompt []

-- | Append a user turn with optional image context.
--
-- Continuations use this when a reply adds a new prompt to an already persisted
-- conversation. Parent/child thread linkage is intentionally handled outside
-- this value.
appendUserContext :: Text -> [Text] -> Conversation -> Conversation
appendUserContext prompt imageUrls (Conversation history) =
  Conversation (history Seq.|> LLM.userWithImages prompt imageUrls)

appendSystemAndUserContext :: Text -> Text -> [Text] -> Conversation -> Conversation
appendSystemAndUserContext systemPrompt prompt imageUrls conversation
  | Text.null (Text.strip systemPrompt) =
      appendUserContext prompt imageUrls conversation
appendSystemAndUserContext systemPrompt prompt imageUrls (Conversation history) =
  Conversation (history Seq.|> LLM.systemText systemPrompt Seq.|> LLM.userWithImages prompt imageUrls)

-- | Append an assistant reply.
--
-- Generated image references are preserved as a synthetic follow-up user
-- context message. That makes image results available to later turns even
-- though the assistant message itself is text-oriented in the OpenAI-compatible
-- chat format we use here.
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
