{-|
Module      : Bot.Agent.Tools.Image
Description : Agent image-generation tool
Stability   : experimental
-}

module Bot.Agent.Tools.Image
  ( generateImageTool
  , editImageTool
  , viewImageTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Streaming.Prelude as S

generateImageTool :: (Chat.Chat :> es, LLM.LLM :> es) => Tool es
generateImageTool = Tool
  { name = "image_generate"
  , description = "Generate an actual image from a prompt and send it to the current chat. Use this when the user *literally* asks to *draw*, *create*, or *generate* an image, including scheduled future image requests. After using this tool, keep the final answer brief and do not repeat the image URL. Never use this when the user is merely asking for, finding, or searching for an image; instead, use the web search tool."
  , parameters = objectSchema
      [ fieldText "prompt" "Image generation prompt. Include the user's visual requirements, style, subject, text, and constraints."
      , fieldText "quality" "Optional provider-supported image quality control, such as low, medium, high, auto, standard, or hd."
      , fieldText "size" "Optional provider-supported image size control, such as 1024x1024, 1024x1536, 1536x1024, or auto."
      , fieldText "background" "Optional provider-supported background control, such as transparent, opaque, or auto."
      , fieldText "moderation" "Optional provider-supported moderation control, such as auto or low."
      ]
      ["prompt"]
  , noisy = True
  , allowed = everyone
  , start = \context -> pure \args -> withParsedToolArgs parseGenerateImageArgs args \generateArgs -> do
      generated <- LLM.askImageWithHistoryWithOptions generateArgs.options [LLM.userWithImages generateArgs.prompt (contextDefaultImageUrls context)]
      case Chat.replyImageUrls generated of
        [] ->
          pure (toolText generated)
        imageRefs ->
          sendImageToolResult context.message "Generated" imageRefs generated
  }

editImageTool :: (Chat.Chat :> es, LLM.LLM :> es) => Tool es
editImageTool = Tool
  { name = "image_edit"
  , description = "Edit one or more existing images with the configured image edit model and send the result to the current chat. Use this when the user asks to modify, restyle, inpaint, combine, or use attached/reference images to create an edited image. Omit image_urls to edit images attached to the current message. Use mask_image_url only when the user supplies an explicit mask image; the mask applies to the first input image."
  , parameters = objectSchema
      [ fieldText "prompt" "Image edit instruction. Describe exactly what should change and what should stay preserved."
      , fieldTextArray "image_urls" "Optional input image URLs or data image references. Omit this to use the images attached to the current user message. GPT image edit models accept up to 16 input images. Only base64, https://, or file:// is supported."
      , fieldText "mask_image_url" "Optional mask image URL or data image reference. The mask must match the first input image size and format and contain an alpha channel."
      , fieldText "quality" "Optional provider-supported image quality control, such as low, medium, high, auto, standard, or hd."
      , fieldText "size" "Optional provider-supported image size control, such as 1024x1024, 1024x1536, 1536x1024, or auto."
      , fieldText "background" "Optional provider-supported background control, such as transparent, opaque, or auto."
      , fieldText "moderation" "Optional provider-supported moderation control, such as auto or low."
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
          edited <- S.effects (LLM.askImageEditStreamingWithOptions editArgs.options editArgs.prompt imageRefs editArgs.maskImageUrl)
          case Chat.replyImageUrls edited of
            [] ->
              pure (toolText edited)
            editedRefs ->
              sendImageToolResult context.message "Edited" editedRefs edited
  }

viewImageTool :: Media.Media :> es => Tool es
viewImageTool = Tool
  { name = "image_view"
  , description = "Make an image URL available as image context."
  , parameters = objectSchema
      [ fieldText "url" "Image URL to add to the current model context. Use an http://, https://, data:image/*, mxc:// (in Matrix), or existing media: media ID."
      ]
      ["url"]
  , noisy = False
  , allowed = everyone
  , start = \_ -> pure \args -> withTextArg "url" viewImageUrl args
  }

data GenerateImageArgs = GenerateImageArgs
  { prompt :: !Text
  , options :: !LLM.ImageRequestOptions
  }

data EditImageArgs = EditImageArgs
  { prompt :: !Text
  , imageUrls :: ![Text]
  , maskImageUrl :: !(Maybe Text)
  , options :: !LLM.ImageRequestOptions
  }

parseGenerateImageArgs :: Aeson.Value -> AesonTypes.Parser GenerateImageArgs
parseGenerateImageArgs =
  Aeson.withObject "image_generate arguments" \o -> do
    prompt <- Text.strip <$> o Aeson..: Key.fromText "prompt"
    options <- parseImageRequestOptions o
    pure GenerateImageArgs
      { prompt = prompt
      , options = options
      }

parseEditImageArgs :: Aeson.Value -> AesonTypes.Parser EditImageArgs
parseEditImageArgs =
  Aeson.withObject "image_edit arguments" \o -> do
    prompt <- Text.strip <$> o Aeson..: Key.fromText "prompt"
    imageUrls <- map Text.strip . fromMaybe [] <$> o Aeson..:? Key.fromText "image_urls"
    maskImageUrl <- fmap Text.strip <$> o Aeson..:? Key.fromText "mask_image_url"
    options <- parseImageRequestOptions o
    pure EditImageArgs
      { prompt = prompt
      , imageUrls = filter (not . Text.null) imageUrls
      , maskImageUrl = maskImageUrl >>= nonEmptyText
      , options = options
      }

parseImageRequestOptions :: Aeson.Object -> AesonTypes.Parser LLM.ImageRequestOptions
parseImageRequestOptions o = do
  quality <- optionalTextField "quality"
  size <- optionalTextField "size"
  background <- optionalTextField "background"
  moderation <- optionalTextField "moderation"
  pure LLM.ImageRequestOptions{quality, size, background, moderation}
  where
    optionalTextField name =
      (fmap Text.strip <$> o Aeson..:? Key.fromText name) <&> (>>= nonEmptyText)

editImageInputRefs :: AgentContext es -> EditImageArgs -> [Text]
editImageInputRefs context editArgs =
  if null editArgs.imageUrls
    then contextDefaultImageUrls context
    else editArgs.imageUrls

contextDefaultImageUrls :: AgentContext es -> [Text]
contextDefaultImageUrls context =
  messageInputImageUrls context.input

validateEditImageRefs :: [Text] -> Maybe AgentFailure
validateEditImageRefs imageRefs
  | null imageRefs =
      Just ((permanentArgumentFailure "image_edit requires at least one input image." "image_edit requires at least one input image. Attach an image to the message or provide image_urls.").failure)
  | length imageRefs > 16 =
      Just ((permanentArgumentFailure "image_edit accepts at most 16 input images." "image_edit accepts at most 16 input images.").failure)
  | otherwise =
      Nothing

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped

viewImageUrl :: Media.Media :> es => Text -> Eff es ToolResult
viewImageUrl rawUrl = do
  let url = Text.strip rawUrl
  mediaRef <- Media.normalizeMediaRef url
  if isMediaRef mediaRef
    then cachedImageContext mediaRef
    else pure (toolFailure (permanentArgumentFailure "image_cache could not cache an image URL." "image_cache could not cache the image URL.").failure)

isMediaRef :: Text -> Bool
isMediaRef ref =
  "media:" `Text.isPrefixOf` Text.strip ref

cachedImageContext :: Media.Media :> es => Text -> Eff es ToolResult
cachedImageContext mediaRef =
  Media.mediaFileInfoByRef mediaRef >>= \case
    Just info
      | "image/" `Text.isPrefixOf` Text.toLower info.mimeType ->
          pure (toolTextWithImages [i|Added image to current context: #{mediaRef}|] [mediaRef])
    _ ->
      pure (toolFailure (permanentArgumentFailure "image_cache URL is not a cached image." "image_cache URL is not a cached image.").failure)

sendImageToolResult :: Chat.Chat :> es => IncomingMessage -> Text -> [Text] -> Text -> Eff es ToolResult
sendImageToolResult message label imageRefs body = do
  sent <- Chat.replyTo message body
  let sentText = show sent :: String
      mediaRefs = filter isMediaRef imageRefs
      mediaText
        | null mediaRefs = ""
        | otherwise = "\nMedia ids: " <> Text.intercalate ", " mediaRefs
  pure (toolTextWithImages [i|#{label} and sent image message id: #{sentText}#{mediaText}|] mediaRefs)
