{-|
Module      : Bot.Chat.Driver.Discord.Markdown
Description : Discord Markdown renderer
Stability   : experimental
-}

module Bot.Chat.Driver.Discord.Markdown
  ( formatDiscordMarkdown
  )
where

import Bot.Prelude
import Commonmark
import qualified Commonmark.Entity as Commonmark
import Commonmark.Extensions
import qualified Data.Char as Char
import qualified Data.Text as Text

newtype DiscordMarkdown = DiscordMarkdown
  { discordMarkdownText :: Text
  }
  deriving (Show, Eq, Typeable)

instance Semigroup DiscordMarkdown where
  DiscordMarkdown left <> DiscordMarkdown right =
    DiscordMarkdown (left <> right)

instance Monoid DiscordMarkdown where
  mempty =
    DiscordMarkdown ""

instance Rangeable DiscordMarkdown where
  ranged _ = id

instance HasAttributes DiscordMarkdown where
  addAttributes _ = id

instance ToPlainText DiscordMarkdown where
  toPlainText =
    (.discordMarkdownText)

instance IsInline DiscordMarkdown where
  lineBreak = discordMarkdownTextOnly "\n"
  softBreak = discordMarkdownTextOnly "\n"
  str = discordMarkdownTextOnly . escapeDiscordMarkdown
  entity raw =
    discordMarkdownTextOnly (escapeDiscordMarkdown (fromMaybe raw (Commonmark.lookupEntity (Text.drop 1 raw))))
  escapedChar = discordMarkdownTextOnly . escapeDiscordMarkdown . Text.singleton
  emph body = discordMarkdownWrap "*" "*" body
  strong body = discordMarkdownWrap "**" "**" body
  link target _title body
    | Text.null body.discordMarkdownText = discordMarkdownTextOnly target
    | otherwise = DiscordMarkdown ("[" <> body.discordMarkdownText <> "](" <> escapeDiscordLinkTarget target <> ")")
  image target _title description
    | Text.null description.discordMarkdownText = discordMarkdownTextOnly target
    | otherwise = description <> discordMarkdownTextOnly (" " <> target)
  code text = discordMarkdownTextOnly ("`" <> escapeDiscordInlineCode text <> "`")
  rawInline _ text = discordMarkdownTextOnly text

instance IsBlock DiscordMarkdown DiscordMarkdown where
  paragraph body = body <> discordMarkdownTextOnly "\n\n"
  plain body = body <> discordMarkdownTextOnly "\n"
  thematicBreak = discordMarkdownTextOnly "--------\n\n"
  blockQuote body =
    discordMarkdownTextOnly (quoteDiscordBlock (Text.stripEnd body.discordMarkdownText) <> "\n\n")
  codeBlock info text =
    discordMarkdownTextOnly ("```" <> Text.takeWhile (not . Char.isSpace) info <> "\n" <> escapeDiscordCodeBlock text <> "\n```\n\n")
  heading level body =
    discordMarkdownTextOnly (Text.replicate (max 1 (min 3 level)) "#" <> " ")
      <> body
      <> discordMarkdownTextOnly "\n\n"
  rawBlock _ text = discordMarkdownTextOnly text
  referenceLinkDefinition _ _ = mempty
  list listType _ items =
    mconcat (zipWith renderItem [(1 :: Int)..] items) <> discordMarkdownTextOnly "\n"
    where
      renderItem index item =
        discordMarkdownTextOnly (discordListItemPrefix listType index)
          <> indentDiscordContinuation "  " (trimDiscordMarkdownEnd item)
          <> discordMarkdownTextOnly "\n"

instance HasEmoji DiscordMarkdown where
  emoji _keyword value =
    discordMarkdownTextOnly value

instance HasStrikethrough DiscordMarkdown where
  strikethrough =
    discordMarkdownWrap "~~" "~~"

instance HasMath DiscordMarkdown where
  inlineMath text =
    discordMarkdownTextOnly ("`" <> escapeDiscordInlineCode text <> "`")
  displayMath text =
    discordMarkdownTextOnly ("```\n" <> escapeDiscordCodeBlock text <> "\n```")

instance HasTaskList DiscordMarkdown DiscordMarkdown where
  taskList _ _ items =
    mconcat (map renderItem items) <> discordMarkdownTextOnly "\n"
    where
      renderItem (checked, item) =
        discordMarkdownTextOnly (if checked then "- [x] " else "- [ ] ")
          <> indentDiscordContinuation "  " (trimDiscordMarkdownEnd item)
          <> discordMarkdownTextOnly "\n"

instance HasFootnote DiscordMarkdown DiscordMarkdown where
  footnote number _label body =
    discordMarkdownTextOnly ("[" <> show number <> "]: ")
      <> indentDiscordContinuation "    " (trimDiscordMarkdownEnd body)
      <> discordMarkdownTextOnly "\n"
  footnoteList =
    mconcat
  footnoteRef number _label _body =
    discordMarkdownTextOnly ("[" <> number <> "]")

instance HasPipeTable DiscordMarkdown DiscordMarkdown where
  pipeTable _alignments headerCells rows =
    discordMarkdownTextOnly "```\n"
      <> discordMarkdownTextOnly (Text.unlines (map pipeRow (headerCells : rows)))
      <> discordMarkdownTextOnly "```\n\n"
    where
      pipeRow cells =
        Text.intercalate " | " (map (Text.strip . (.discordMarkdownText)) cells)

instance HasAlerts DiscordMarkdown DiscordMarkdown where
  alert alertType body =
    discordMarkdownTextOnly ("> **" <> alertName alertType <> "**\n")
      <> DiscordMarkdown (quoteDiscordBlock (Text.stripEnd body.discordMarkdownText))
      <> discordMarkdownTextOnly "\n\n"

formatDiscordMarkdown :: Text -> Text
formatDiscordMarkdown input =
  case runIdentity (commonmarkWith discordMarkdownSyntax "discord-message" (input <> "\n")) of
    Left _ ->
      input
    Right formatted ->
      Text.stripEnd formatted.discordMarkdownText

discordMarkdownSyntax :: SyntaxSpec Identity DiscordMarkdown DiscordMarkdown
discordMarkdownSyntax =
  gfmExtensions
    <> mathSpec
    <> defaultSyntaxSpec

discordMarkdownTextOnly :: Text -> DiscordMarkdown
discordMarkdownTextOnly =
  DiscordMarkdown

discordMarkdownWrap :: Text -> Text -> DiscordMarkdown -> DiscordMarkdown
discordMarkdownWrap left right body
  | Text.null body.discordMarkdownText = body
  | otherwise = DiscordMarkdown (left <> body.discordMarkdownText <> right)

trimDiscordMarkdownEnd :: DiscordMarkdown -> DiscordMarkdown
trimDiscordMarkdownEnd =
  DiscordMarkdown . Text.dropWhileEnd Char.isSpace . (.discordMarkdownText)

indentDiscordContinuation :: Text -> DiscordMarkdown -> DiscordMarkdown
indentDiscordContinuation indent formatted =
  DiscordMarkdown (Text.intercalate "\n" (indentLines (Text.lines formatted.discordMarkdownText)))
  where
    indentLines [] = []
    indentLines (firstLine : rest) =
      firstLine : map (indent <>) rest

discordListItemPrefix :: ListType -> Int -> Text
discordListItemPrefix (BulletList _) _ =
  "- "
discordListItemPrefix (OrderedList start _ delimiter) index =
  discordOrderedItemPrefix delimiter (start + index - 1)

discordOrderedItemPrefix :: DelimiterType -> Int -> Text
discordOrderedItemPrefix Period number =
  show number <> ". "
discordOrderedItemPrefix OneParen number =
  show number <> ") "
discordOrderedItemPrefix TwoParens number =
  "(" <> show number <> ") "

quoteDiscordBlock :: Text -> Text
quoteDiscordBlock text =
  Text.unlines (map ("> " <>) (Text.lines text))

escapeDiscordMarkdown :: Text -> Text
escapeDiscordMarkdown =
  Text.concatMap \case
    '\\' -> "\\\\"
    '`' -> "\\`"
    '*' -> "\\*"
    '_' -> "\\_"
    '~' -> "\\~"
    '|' -> "\\|"
    '[' -> "\\["
    ']' -> "\\]"
    '(' -> "\\("
    ')' -> "\\)"
    c -> Text.singleton c

escapeDiscordInlineCode :: Text -> Text
escapeDiscordInlineCode =
  Text.replace "`" "\\`"

escapeDiscordCodeBlock :: Text -> Text
escapeDiscordCodeBlock =
  Text.replace "```" "`\8203``"

escapeDiscordLinkTarget :: Text -> Text
escapeDiscordLinkTarget =
  Text.replace ")" "%29"
