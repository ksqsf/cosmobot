{-|
Module      : Bot.Agent.Tools.Image
Description : Agent image-generation tool
Stability   : experimental
-}

module Bot.Agent.Tools.Image
  ( generateImageTool
  , editImageTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

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

editImageTool :: (Chat.Chat :> es, LLM.LLM :> es) => Tool es
editImageTool = Tool
  { name = "edit_image"
  , description = "Edit one or more existing images with the configured image edit model and send the result to the current chat. Use this when the user asks to modify, restyle, inpaint, combine, or use attached/reference images to create an edited image. Omit image_urls to edit images attached to the current message. Use mask_image_url only when the user supplies an explicit mask image; the mask applies to the first input image."
  , parameters = objectSchema
      [ fieldText "prompt" "Image edit instruction. Describe exactly what should change and what should stay preserved."
      , fieldTextArray "image_urls" "Optional input image URLs or data image references. Omit this to use the images attached to the current user message. GPT image edit models accept up to 16 input images."
      , fieldText "mask_image_url" "Optional mask image URL or data image reference. The mask must match the first input image size and format and contain an alpha channel."
      ]
      ["prompt"]
  , noisy = True
  , allowed = everyone
  , start = \context -> pure \args -> withParsedToolArgs parseEditImageArgs args \editArgs -> do
      let imageRefs = editImageInputRefs context editArgs
      case validateEditImageRefs imageRefs of
        Just failure ->
          pure (toolFailure failure)
        Nothing -> do
          edited <- LLM.askImageEdit editArgs.prompt imageRefs editArgs.maskImageUrl
          case Chat.replyImageUrls edited of
            [] ->
              pure (toolText edited)
            _ -> do
              sent <- Chat.replyTo context.message edited
              context.recordBotMessage sent edited
              let sentText = show sent :: String
              pure (toolMessage sent [i|Edited and sent image message id: #{sentText}|])
  }

data EditImageArgs = EditImageArgs
  { prompt :: !Text
  , imageUrls :: ![Text]
  , maskImageUrl :: !(Maybe Text)
  }

parseEditImageArgs :: Aeson.Value -> AesonTypes.Parser EditImageArgs
parseEditImageArgs =
  Aeson.withObject "edit_image arguments" \o -> do
    prompt <- Text.strip <$> o Aeson..: Key.fromText "prompt"
    imageUrls <- map Text.strip . fromMaybe [] <$> o Aeson..:? Key.fromText "image_urls"
    maskImageUrl <- fmap Text.strip <$> o Aeson..:? Key.fromText "mask_image_url"
    pure EditImageArgs
      { prompt = prompt
      , imageUrls = filter (not . Text.null) imageUrls
      , maskImageUrl = maskImageUrl >>= nonEmptyText
      }

editImageInputRefs :: AgentContext es -> EditImageArgs -> [Text]
editImageInputRefs context editArgs =
  if null editArgs.imageUrls
    then filter (not . Text.null) (map Text.strip context.message.imageUrls)
    else editArgs.imageUrls

validateEditImageRefs :: [Text] -> Maybe AgentFailure
validateEditImageRefs imageRefs
  | null imageRefs =
      Just ((permanentArgumentFailure "edit_image requires at least one input image." "edit_image requires at least one input image. Attach an image to the message or provide image_urls.").failure)
  | length imageRefs > 16 =
      Just ((permanentArgumentFailure "edit_image accepts at most 16 input images." "edit_image accepts at most 16 input images.").failure)
  | otherwise =
      Nothing

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped
