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
import Bot.Agent.Tool
import Bot.Agent.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Text as Text

generateAudioTool :: (Chat.Chat :> es, LLM.LLM :> es) => Tool es
generateAudioTool =
  tagged [workTag]
  . noisy
  . withDescription "Generate speech or other audio from a prompt and send it to the current chat. Use this when the user asks to create, synthesize, speak, narrate, or generate an audio clip. After using this tool, keep the final answer brief and do not repeat the audio reference."
  $ tool "audio_generate"
      (requiredText "prompt" "The words to be converted into audio")
      \rawPrompt -> do
        context <- askToolContext
        let prompt = Text.strip rawPrompt
        generated <- LLM.askAudioWithHistoryWithOptions LLM.defaultAudioRequestOptions [LLM.userText prompt]
        sent <- Chat.replyAudio context.message generated Nothing
        case sent of
          Right messageId -> do
            let sentText = show messageId :: String
            pure (toolText [i|Generated and sent audio message id: #{sentText}|])
          Left err ->
            pure (toolFailure (externalServiceFailure ("发送音频失败：" <> err) err))
