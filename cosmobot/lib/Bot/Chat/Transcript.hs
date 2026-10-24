module Bot.Chat.Transcript
  ( startWithUserInput, startWithSystemAndUserInput, appendUserInput, appendAssistant ) where

import Bot.Core.Message
import qualified Bot.Core.ReplyBody as ReplyBody
import Bot.Core.Transcript (Transcript (..))
import qualified Bot.Core.Transcript as Transcript
import qualified Bot.LLM.Types as LLM
import Bot.Prelude
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

startWithUserInput :: MessageInput -> Transcript
startWithUserInput =
  startWithSystemAndUserInput ""

startWithSystemAndUserInput :: Text -> MessageInput -> Transcript
startWithSystemAndUserInput systemPrompt input =
  Transcript.startWithSystemAndUserContext systemPrompt input.text (messageInputImageUrls input)

appendUserInput :: MessageInput -> Transcript -> Transcript
appendUserInput input =
  Transcript.appendUserContext input.text (messageInputImageUrls input)

-- | Append an assistant reply.
--
-- Generated image references are preserved as a synthetic follow-up user
-- context message. That makes image results available to later turns even
-- though the assistant message itself is text-oriented in the OpenAI-compatible
-- chat format we use here.
appendAssistant :: Text -> Transcript -> Transcript
appendAssistant answer (Transcript history) =
  Transcript (history <> Seq.fromList (assistantContext answer))

assistantContext :: Text -> [LLM.ChatMessage]
assistantContext answer =
  assistantTextContext <> imageContext
  where
    answerText = ReplyBody.renderReplyBody answer
    imageUrls = ReplyBody.replyImageUrls answer
    contextImageUrls = filter (not . ReplyBody.isBase64ImageRef) imageUrls
    assistantTextContext =
      [ LLM.assistantText answerText | not (Text.null answerText) ] <>
      [ LLM.assistantText "Generated image." | Text.null answerText && not (null imageUrls) ]
    imageContext =
      [ LLM.syntheticWithImages "Assistant-generated image." contextImageUrls
      | not (null contextImageUrls)
      ]
