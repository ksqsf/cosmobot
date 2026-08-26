{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Bot.Chat.Driver.Matrix.Markdown
Description : Matrix-flavoured Markdown rendering
Stability   : experimental
-}

module Bot.Chat.Driver.Matrix.Markdown
  ( formatMatrixMarkdown
  , formatMatrixMarkdownWithMentionNames
  , matrixMentionDisplayBody
  , matrixMentionDisplayName
  , matrixUserIdsInText
  , matrixUserIdTrailingPunctuation
  , isMatrixUserId
  )
where

import Bot.Prelude
import Commonmark
import Commonmark.Extensions
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

formatMatrixMarkdown :: Text -> Maybe Text
formatMatrixMarkdown =
  formatMatrixMarkdownWithMentionNames Map.empty

formatMatrixMarkdownWithMentionNames :: Map Text Text -> Text -> Maybe Text
formatMatrixMarkdownWithMentionNames mentionNames input =
  case runIdentity (commonmarkWith matrixMarkdownSyntax "matrix-message" (linkifyMatrixMentions mentionNames input)) :: Either ParseError (Html ()) of
    Left _ -> Nothing
    Right html -> nonEmptyText (Text.strip (LazyText.toStrict (renderHtml html)))

linkifyMatrixMentions :: Map Text Text -> Text -> Text
linkifyMatrixMentions mentionNames =
  Text.concat . map (linkifyToken mentionNames) . Text.groupBy sameWhitespace

linkifyToken :: Map Text Text -> Text -> Text
linkifyToken mentionNames token
  | Text.all Char.isSpace token = token
  | isMatrixUserId userId =
      "[" <> escapeMarkdownLinkLabel displayName <> "](" <> matrixToUserUrl userId <> ")" <> suffix
  | otherwise = token
  where
    userId = Text.dropWhileEnd (`elem` matrixUserIdTrailingPunctuation) token
    suffix = Text.drop (Text.length userId) token
    displayName = matrixMentionDisplayText mentionNames userId

matrixMentionDisplayBody :: Map Text Text -> Text -> Text
matrixMentionDisplayBody mentionNames =
  Text.concat . map replaceToken . Text.groupBy sameWhitespace
  where
    replaceToken token
      | Text.all Char.isSpace token = token
      | isMatrixUserId userId = matrixMentionDisplayText mentionNames userId <> suffix
      | otherwise = token
      where
        userId = Text.dropWhileEnd (`elem` matrixUserIdTrailingPunctuation) token
        suffix = Text.drop (Text.length userId) token

matrixMentionDisplayText :: Map Text Text -> Text -> Text
matrixMentionDisplayText mentionNames userId =
  fromMaybe userId (Map.lookup userId mentionNames >>= matrixMentionDisplayName)

matrixMentionDisplayName :: Text -> Maybe Text
matrixMentionDisplayName name = do
  displayName <- nonEmptyText name
  pure if "@" `Text.isPrefixOf` displayName then displayName else "@" <> displayName

matrixUserIdsInText :: Text -> [Text]
matrixUserIdsInText =
  mapMaybe matrixUserIdToken . Text.words

matrixUserIdToken :: Text -> Maybe Text
matrixUserIdToken raw =
  let token = Text.dropWhileEnd (`elem` matrixUserIdTrailingPunctuation) raw
  in token <$ guard (isMatrixUserId token)

matrixUserIdTrailingPunctuation :: [Char]
matrixUserIdTrailingPunctuation =
  ".,;:!?)]}>\"'"

isMatrixUserId :: Text -> Bool
isMatrixUserId token =
  "@" `Text.isPrefixOf` token && ":" `Text.isInfixOf` token

sameWhitespace :: Char -> Char -> Bool
sameWhitespace left right =
  Char.isSpace left == Char.isSpace right

escapeMarkdownLinkLabel :: Text -> Text
escapeMarkdownLinkLabel =
  Text.concatMap \case
    '\\' -> "\\\\"
    '[' -> "\\["
    ']' -> "\\]"
    '\n' -> " "
    '\r' -> " "
    char -> Text.singleton char

matrixToUserUrl :: Text -> Text
matrixToUserUrl userId =
  "https://matrix.to/#/" <> userId

matrixMarkdownSyntax :: SyntaxSpec Identity (Html ()) (Html ())
matrixMarkdownSyntax =
  gfmExtensions <> mathSpec <> footnoteSpec <> defaultSyntaxSpec

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  text <$ guard (not (Text.null text))
