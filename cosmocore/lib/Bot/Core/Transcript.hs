module Bot.Core.Transcript
  ( Transcript (..), startWithUser, startWithUserContext
  , startWithSystemAndUser, startWithSystemAndUserContext
  , appendUser, appendUserContext, appendAssistant
  ) where

import Bot.Prelude
import Bot.Util.Aeson
import qualified Bot.LLM.Types as LLM
import qualified Data.Aeson as Aeson
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

-- | Platform-neutral LLM message history.
--
-- A 'Transcript' is the exact ordered message context that will be sent back
-- to the LLM when a user continues a thread. It deliberately does not contain
-- chat ids, message ids, parent links, active stream handles, or persistence
-- details; those belong to the storage layer that indexes and reloads threads
-- for a concrete bot runtime.
newtype Transcript = Transcript
  { messages :: Seq.Seq LLM.ChatMessage
  }
  deriving (Show, Generic)
    deriving (Aeson.ToJSON, Aeson.FromJSON) via (SnakeJSON Transcript)

startWithUser :: Text -> Transcript
startWithUser prompt = startWithUserContext prompt []

startWithUserContext :: Text -> [Text] -> Transcript
startWithUserContext = startWithSystemAndUserContext ""

startWithSystemAndUser :: Text -> Text -> Transcript
startWithSystemAndUser system prompt = startWithSystemAndUserContext system prompt []

startWithSystemAndUserContext :: Text -> Text -> [Text] -> Transcript
startWithSystemAndUserContext system prompt images =
  Transcript (Seq.fromList ([LLM.systemText system | not (Text.null system)] <> [LLM.userWithImages prompt images]))

appendUser :: Text -> Transcript -> Transcript
appendUser prompt = appendUserContext prompt []

appendUserContext :: Text -> [Text] -> Transcript -> Transcript
appendUserContext prompt images (Transcript messages) =
  Transcript (messages Seq.|> LLM.userWithImages prompt images)

appendAssistant :: Text -> Transcript -> Transcript
appendAssistant text (Transcript messages) =
  Transcript (messages Seq.|> LLM.assistantText text)
