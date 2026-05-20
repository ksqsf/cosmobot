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
  { name = "generate_audio"
  , description = "Generate speech or other audio from a prompt and send it to the current chat. Use this when the user asks to create, synthesize, speak, narrate, or generate an audio clip. After using this tool, keep the final answer brief and do not repeat the audio reference."
  , parameters = objectSchema
      [ fieldText "prompt" "Audio generation prompt. Include the requested words, narration style, tone, language, and sound requirements."
      , fieldText "voice" "Optional provider-supported voice control, such as alloy, verse, aria, or another configured voice."
      , fieldText "format" "Optional provider-supported output format, such as mp3, wav, opus, flac, or pcm."
      , fieldNumber "speed" "Optional provider-supported speaking speed."
      , fieldText "instructions" "Optional provider-supported voice or delivery instructions."
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
          pure (toolMessage messageId [i|Generated and sent audio message id: #{sentText}|])
        Left err ->
          pure (toolFailure (externalServiceFailure ("发送音频失败：" <> err) err).failure)
  }

data GenerateAudioArgs = GenerateAudioArgs
  { prompt :: !Text
  , options :: !LLM.AudioRequestOptions
  }

parseGenerateAudioArgs :: Aeson.Value -> AesonTypes.Parser GenerateAudioArgs
parseGenerateAudioArgs =
  Aeson.withObject "generate_audio arguments" \o -> do
    prompt <- Text.strip <$> o Aeson..: Key.fromText "prompt"
    options <- parseAudioRequestOptions o
    pure GenerateAudioArgs
      { prompt = prompt
      , options = options
      }

parseAudioRequestOptions :: Aeson.Object -> AesonTypes.Parser LLM.AudioRequestOptions
parseAudioRequestOptions o = do
  voice <- optionalTextField "voice"
  responseFormat <- optionalTextField "format"
  speed <- o Aeson..:? Key.fromText "speed"
  instructions <- optionalTextField "instructions"
  pure LLM.AudioRequestOptions{voice, responseFormat, speed, instructions}
  where
    optionalTextField name =
      (fmap Text.strip <$> o Aeson..:? Key.fromText name) <&> (>>= nonEmptyText)

fieldNumber :: Text -> Text -> (Text, Aeson.Value)
fieldNumber name description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "number"
      , "description" Aeson..= description
      ]
  )

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped
