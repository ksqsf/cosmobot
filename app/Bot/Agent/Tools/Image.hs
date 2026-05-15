{-|
Module      : Bot.Agent.Tools.Image
Description : Agent image-generation tool
Stability   : experimental
-}

module Bot.Agent.Tools.Image
  ( generateImageTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude

generateImageTool :: (Chat.Chat :> es, LLM.LLM :> es) => Tool es
generateImageTool = Tool
  { name = "generate_image"
  , description = "Generate an actual image from a prompt and send it to the current chat. Use this when the user *literally* asks to *draw*, *create*, or *generate* an image, including scheduled future image requests. After using this tool, keep the final answer brief and do not repeat the image URL. Never use this when the user is merely asking for, finding, or searching for an image; instead, use the web search tool."
  , parameters = objectSchema
      [ fieldText "prompt" "Image generation prompt. Include the user's visual requirements, style, subject, text, and constraints."
      ]
      ["prompt"]
  , noisy = True
  , allowed = everyone
  , start = \context -> pure \args -> withTextArg "prompt" (\prompt -> do
      generated <- LLM.askImageWithHistory [LLM.userWithImages prompt context.message.imageUrls]
      case Chat.replyImageUrls generated of
        [] ->
          pure (toolText generated)
        _ -> do
          sent <- Chat.replyTo context.message generated
          context.recordBotMessage sent generated
          let sentText = show sent :: String
          pure (toolMessage sent [i|Generated and sent image message id: #{sentText}|])
      ) args
  }
