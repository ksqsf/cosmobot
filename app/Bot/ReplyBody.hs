{-|
Module      : Bot.ReplyBody
Description : Reply body directives shared by chat backends
Stability   : experimental
-}

module Bot.ReplyBody
  ( imageDirective
  , renderReplyBody
  , replyImageUrls
  , traverseReplyImageUrls
  )
where

import Bot.Prelude
import qualified Data.Text as Text

imageDirective :: Text -> Text
imageDirective ref =
  "[image] " <> ref

-- | Remove image directives from a reply body before storing it as text.
renderReplyBody :: Text -> Text
renderReplyBody body =
  Text.strip (Text.unlines (filter (not . isImageLine) (Text.lines body)))

-- | Extract image URLs from @\[image\] ...@ reply directives.
replyImageUrls :: Text -> [Text]
replyImageUrls body =
  mapMaybe imageLineUrl (Text.lines body)

traverseReplyImageUrls :: Applicative f => (Text -> f Text) -> Text -> f Text
traverseReplyImageUrls update body =
  Text.unlines <$> traverse updateLine (Text.lines body)
  where
    updateLine line =
      case imageLineUrl line of
        Nothing ->
          pure line
        Just ref ->
          imageDirective <$> update ref

isImageLine :: Text -> Bool
isImageLine =
  isJust . imageLineUrl

imageLineUrl :: Text -> Maybe Text
imageLineUrl line =
  let marker = "[image] "
      stripped = Text.strip line
  in Text.strip <$> Text.stripPrefix marker stripped
