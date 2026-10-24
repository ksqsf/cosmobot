{-# LANGUAGE OverloadedLabels #-}
module Bot.Storage.AgentTranscript (saveRunTranscript, loadRunTranscript) where

import Bot.Core.Transcript (Transcript (..))
import qualified Bot.Effect.Storage as Storage
import qualified Bot.LLM.Types as LLM
import Bot.Prelude
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Encoding as TextEncoding

data RunTranscriptRow = RunTranscriptRow
  { run_id :: Text
  , messages_json :: Text
  }
  deriving (Generic)

instance SqlRow RunTranscriptRow

runTranscripts :: Table RunTranscriptRow
runTranscripts = table "agent_run_transcripts" [#run_id :- primary]

saveRunTranscript :: Storage.Storage :> es => Text -> Transcript -> Eff es ()
saveRunTranscript runId transcript = do
  runSelda (tryCreateTable runTranscripts)
  runSelda $ transaction do
    deleteFrom_ runTranscripts (\row -> row ! #run_id .== literal runId)
    insert_ runTranscripts [RunTranscriptRow runId encoded]
  where
    encoded = TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode $
      map withoutProviderState (toList transcript.messages)
    withoutProviderState :: LLM.ChatMessage -> LLM.ChatMessage
    withoutProviderState message = message{LLM.providerState = Nothing}

loadRunTranscript :: Storage.Storage :> es => Text -> Eff es (Maybe [LLM.ChatMessage])
loadRunTranscript runId = do
  runSelda (tryCreateTable runTranscripts)
  rows <- runSelda $ query do
    row <- select runTranscripts
    restrict (row ! #run_id .== literal runId)
    pure (row ! #messages_json)
  pure $ listToMaybe rows >>= Aeson.decodeStrict' . TextEncoding.encodeUtf8
