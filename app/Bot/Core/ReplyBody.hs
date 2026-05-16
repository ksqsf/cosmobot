{-|
Module      : Bot.Core.ReplyBody
Description : Reply body directives shared by chat backends
Stability   : experimental
-}

module Bot.Core.ReplyBody
  ( ReplyContent (..)
  , ReplySegmentEvent (..)
  , imageDirective
  , replyContentFromBody
  , replyContentToBody
  , replySegmentMessages
  , renderReplyBody
  , replyImageUrls
  , traverseReplyImageUrls
  )
where

import Bot.Prelude
import qualified Data.Text as Text

data ReplyContent = ReplyContent
  { text :: !Text
  , images :: ![Text]
  }
  deriving (Eq, Show)

data ReplySegmentEvent
  = ReplySegmentDelta !Text
  | ReplySegmentMessage !ReplyContent
  | ReplySegmentBoundary
  deriving (Eq, Show)

imageDirective :: Text -> Text
imageDirective ref =
  "[image] " <> ref

replyContentFromBody :: Text -> ReplyContent
replyContentFromBody body =
  ReplyContent
    { text = Text.strip (Text.unlines textLines)
    , images = mapMaybe imageLineUrl lines_
    }
  where
    lines_ = Text.lines body
    textLines = filter (not . isImageLine) lines_

replyContentToBody :: ReplyContent -> Text
replyContentToBody ReplyContent{text, images} =
  Text.strip (Text.unlines (textLines <> imageLines))
  where
    textLines =
      [text | not (Text.null (Text.strip text))]
    imageLines =
      map imageDirective images

replySegmentMessages :: [ReplySegmentEvent] -> [ReplyContent]
replySegmentMessages =
  finish . foldl' step (mempty, [])
  where
    step (pending, messages) = \case
      ReplySegmentDelta chunk ->
        (pending <> chunk, messages)
      ReplySegmentMessage content ->
        ("", messages <> maybeToList (textReplyContent pending) <> [content])
      ReplySegmentBoundary ->
        ("", messages <> maybeToList (textReplyContent pending))

    finish (pending, messages) =
      messages <> maybeToList (textReplyContent pending)

    textReplyContent pending =
      let text = Text.strip pending
      in if Text.null text
        then Nothing
        else Just ReplyContent{text, images = []}

-- | Remove image directives from a reply body before storing it as text.
renderReplyBody :: Text -> Text
renderReplyBody =
  (.text) . replyContentFromBody

-- | Extract image URLs from @\[image\] ...@ reply directives.
replyImageUrls :: Text -> [Text]
replyImageUrls =
  (.images) . replyContentFromBody

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
