{-|
Module      : Bot.Agent.Tools.Audio
Description : Agent audio-generation tool
Stability   : experimental
-}

module Bot.Agent.Tools.Audio
  ( generateAudioTool
  )
where

import Bot.Agent.Failure (externalServiceFailure)
import Bot.Agent.Tools.Common
import Bot.Agent.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

generateAudioTool :: (Chat.Chat :> es, LLM.LLM :> es) => Tool es
generateAudioTool = Tool
  { name = "audio_generate"
  , description = "Generate speech or other audio from a prompt and send it to the current chat. Use this when the user asks to create, synthesize, speak, narrate, or generate an audio clip. After using this tool, keep the final answer brief and do not repeat the audio reference."
  , parameters = objectSchema
      [ fieldText "prompt" "The words to be converted into audio"
      ]
      ["prompt"]
  , noisy = True
  , allowed = everyone
  , start = \context -> pure \args -> withParsedToolArgs parseGenerateAudioArgs args \generateArgs -> do
      generated <- LLM.askAudioWithHistoryWithOptions generateArgs.options [LLM.userText generateArgs.prompt]
      sent <- Chat.replyAudio context.message generated Nothing
      case sent of
        Right messageId -> do
          let sentText = show messageId :: String
          pure (toolText [i|Generated and sent audio message id: #{sentText}|])
        Left err ->
          pure (toolFailure (externalServiceFailure ("发送音频失败：" <> err) err).failure)
  }

data GenerateAudioArgs = GenerateAudioArgs
  { prompt :: !Text
  , options :: !LLM.AudioRequestOptions
  }

parseGenerateAudioArgs :: Aeson.Value -> AesonTypes.Parser GenerateAudioArgs
parseGenerateAudioArgs =
  Aeson.withObject "audio_generate arguments" \o -> do
    prompt <- Text.strip <$> o Aeson..: Key.fromText "prompt"
    pure GenerateAudioArgs
      { prompt = prompt
      , options = LLM.defaultAudioRequestOptions
      }
