{-|
Module      : Bot.Chat.Driver.Telegram.Markdown
Description : Telegram rich-message Markdown rendering
Stability   : experimental
-}

module Bot.Chat.Driver.Telegram.Markdown
  ( formatTelegramRichHtml
  , escapeHtml
  )
where

import Bot.Prelude
import qualified Bot.Media.Mime as Mime
import Commonmark hiding (escapeHtml)
import Commonmark.Blocks
  ( BlockData (..)
  , BlockSpec (..)
  , BlockStartResult (..)
  , addNodeToStack
  , defBlockData
  , defaultFinalizer
  , getBlockText
  )
import Commonmark.Extensions
  ( ColAlignment (..)
  , HasFootnote (..)
  , HasMath (..)
  , HasPipeTable (..)
  , HasStrikethrough (..)
  , HasSubscript (..)
  , HasSuperscript (..)
  , HasTaskList (..)
  , autolinkSpec
  , footnoteSpec
  , mathSpec
  , pipeTableSpec
  , strikethroughSpec
  , subscriptSpec
  , superscriptSpec
  , taskListSpec
  )
import Commonmark.Inlines (InlineParser, withAttributes)
import Commonmark.TokParsers (anyTok, nonindentSpaces, symbol)
import Data.List (lookup)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Tree (Tree (..))
import qualified Text.Parsec as Parsec

data TelegramRichHtml = TelegramRichHtml
  { value :: !(Html ())
  , containsBlock :: !Bool
  }
  deriving (Show)

instance Semigroup TelegramRichHtml where
  left <> right = TelegramRichHtml
    { value = left.value <> right.value
    , containsBlock = left.containsBlock || right.containsBlock
    }

instance Monoid TelegramRichHtml where
  mempty = TelegramRichHtml mempty False

instance Rangeable TelegramRichHtml where
  ranged _ = id

instance HasAttributes TelegramRichHtml where
  addAttributes attrs html =
    html{value = addAttributes attrs html.value}

instance ToPlainText TelegramRichHtml where
  toPlainText = toPlainText . (.value)

instance IsInline TelegramRichHtml where
  lineBreak = inlineHtml (lineBreak :: Html ())
  softBreak = inlineHtml (softBreak :: Html ())
  str = inlineHtml . str
  entity = inlineHtml . entity
  escapedChar = inlineHtml . escapedChar
  emph = mapRichHtml emph
  strong = mapRichHtml strong
  link target title = mapRichHtml (link target title)
  image = telegramRichMedia
  code = inlineHtml . code
  rawInline format = inlineHtml . rawInline format

instance IsBlock TelegramRichHtml TelegramRichHtml where
  paragraph html
    | html.containsBlock = html
    | otherwise = blockHtml (paragraph html.value)
  plain html
    | html.containsBlock = html
    | otherwise = inlineHtml (plain html.value)
  thematicBreak = blockHtml (thematicBreak :: Html ())
  blockQuote = blockHtml . blockQuote . (.value)
  codeBlock info = blockHtml . codeBlock info
  heading level = blockHtml . heading level . (.value)
  rawBlock format = blockHtml . rawBlock format
  referenceLinkDefinition _ _ = mempty
  list listType spacing = blockHtml . list listType spacing . map (.value)

instance HasStrikethrough TelegramRichHtml where
  strikethrough = mapRichHtml strikethrough

instance HasSubscript TelegramRichHtml where
  subscript = mapRichHtml subscript

instance HasSuperscript TelegramRichHtml where
  superscript = mapRichHtml superscript

instance HasMath TelegramRichHtml where
  inlineMath = inlineHtml . htmlInline "tg-math" . Just . htmlText
  displayMath = blockHtml . htmlBlock "tg-math-block" . Just . htmlText

instance HasTaskList TelegramRichHtml TelegramRichHtml where
  taskList listType spacing items =
    blockHtml $ list listType spacing (map taskItem items)
    where
      taskItem (checked, item) =
        checkbox checked <> item.value
      checkbox checked =
        addAttribute ("type", "checkbox")
        . (if checked then addAttribute ("checked", "") else id)
        $ htmlInline "input" Nothing

instance HasPipeTable TelegramRichHtml TelegramRichHtml where
  pipeTable alignments headers rows =
    blockHtml $ htmlBlock "table" $ Just $
      tableRow "th" alignments headers <> mconcat (map (tableRow "td" alignments) rows)

instance HasFootnote TelegramRichHtml TelegramRichHtml where
  footnote _ label body =
    blockHtml $ addAttribute ("name", "note-" <> label) $
      htmlBlock "tg-reference" (Just body.value)
  footnoteList = mconcat
  footnoteRef number label _ =
    inlineHtml $ addAttribute ("href", "#note-" <> label) $
      htmlInline "a" (Just (htmlText ("[" <> number <> "]")))

inlineHtml :: Html () -> TelegramRichHtml
inlineHtml value =
  TelegramRichHtml{value, containsBlock = False}

blockHtml :: Html () -> TelegramRichHtml
blockHtml value =
  TelegramRichHtml{value, containsBlock = True}

mapRichHtml :: (Html () -> Html ()) -> TelegramRichHtml -> TelegramRichHtml
mapRichHtml action html =
  html{value = action html.value}

telegramRichMedia :: Text -> Text -> TelegramRichHtml -> TelegramRichHtml
telegramRichMedia target title description
  | Just emojiId <- Text.stripPrefix "tg://emoji?id=" target =
      inlineHtml $
        htmlRaw ("<tg-emoji emoji-id=\"" <> escapeHtml emojiId <> "\">")
          <> description.value
          <> htmlRaw "</tg-emoji>"
  | Just (unix, format) <- telegramTime target =
      inlineHtml $
        htmlRaw ("<tg-time unix=\"" <> escapeHtml unix <> "\"" <> maybe "" (\value -> " format=\"" <> escapeHtml value <> "\"") format <> ">")
          <> description.value
          <> htmlRaw "</tg-time>"
  | isHttpUrl target =
      blockHtml $ case nonEmptyText (Text.strip title) of
        Nothing -> media
        Just caption -> htmlBlock "figure" (Just (media <> htmlBlock "figcaption" (Just (htmlText caption))))
  | otherwise = description
  where
    tag = telegramMediaTag target
    media =
      addAttribute ("src", target) $
        if tag == "img" then htmlBlock tag Nothing else htmlBlock tag (Just mempty)

telegramTime :: Text -> Maybe (Text, Maybe Text)
telegramTime target = do
  query <- Text.stripPrefix "tg://time?" target
  let parameters = map (Text.breakOn "=") (Text.splitOn "&" query)
      parameter name = Text.drop 1 <$> lookup name parameters
  unix <- parameter "unix"
  pure (unix, parameter "format")

telegramMediaTag :: Text -> Text
telegramMediaTag target
  | mime == "image/gif" = "video"
  | "video/" `Text.isPrefixOf` mime = "video"
  | "audio/" `Text.isPrefixOf` mime = "audio"
  | otherwise = "img"
  where
    mime = Text.toLower (Mime.mimeFromName (Text.takeWhile (`notElem` ['?', '#']) target))

isHttpUrl :: Text -> Bool
isHttpUrl target =
  let lower = Text.toLower (Text.strip target)
  in "http://" `Text.isPrefixOf` lower || "https://" `Text.isPrefixOf` lower

tableRow :: Text -> [ColAlignment] -> [TelegramRichHtml] -> Html ()
tableRow cellTag alignments cells =
  htmlBlock "tr" . Just . mconcat $
    zipWith renderCell (alignments <> repeat DefaultAlignedCol) cells
  where
    renderCell alignment cell =
      htmlRaw ("<" <> cellTag <> alignmentAttribute alignment <> ">")
        <> cell.value
        <> htmlRaw ("</" <> cellTag <> ">")
    alignmentAttribute LeftAlignedCol = " align=\"left\""
    alignmentAttribute CenterAlignedCol = " align=\"center\""
    alignmentAttribute RightAlignedCol = " align=\"right\""
    alignmentAttribute DefaultAlignedCol = ""

telegramRichHtmlSyntax :: SyntaxSpec Identity TelegramRichHtml TelegramRichHtml
telegramRichHtmlSyntax =
  latexMathSpec
    <> mathSpec
    <> subscriptSpec
    <> superscriptSpec
    <> strikethroughSpec
    <> taskListSpec
    <> footnoteSpec
    <> autolinkSpec
    <> defaultSyntaxSpec
    <> pipeTableSpec

latexMathSpec :: (Monad m, IsBlock il bl, IsInline il, HasMath il, HasMath bl) => SyntaxSpec m il bl
latexMathSpec =
  mempty
    { syntaxBlockSpecs = [latexDisplayMathBlockSpec]
    , syntaxInlineParsers = [withAttributes parseLatexMath]
    }

parseLatexMath :: (Monad m, HasMath il) => InlineParser m il
parseLatexMath = Parsec.try do
  void (symbol '\\')
  void (symbol '(')
  inlineMath . untokenize <$> latexMathContents ')'

latexMathContents :: Monad m => Char -> InlineParser m [Tok]
latexMathContents closing = do
  token <- anyTok
  case token.tokType of
    Symbol '\\' -> do
      next <- anyTok
      case next.tokType of
        Symbol char | char == closing -> pure []
        _ -> (token :) . (next :) <$> latexMathContents closing
    _ -> (token :) <$> latexMathContents closing

latexDisplayMathBlockSpec :: (Monad m, IsBlock il bl, HasMath bl) => BlockSpec m il bl
latexDisplayMathBlockSpec = BlockSpec
  { blockType = "LatexDisplayMath"
  , blockStart = Parsec.try do
      nonindentSpaces
      position <- Parsec.getPosition
      void (symbol '\\')
      void (symbol '[')
      contents <- Parsec.manyTill anyTok (Parsec.try (symbol '\\' *> symbol ']'))
      addNodeToStack $ Node
        (defBlockData latexDisplayMathBlockSpec)
          { blockLines = [contents]
          , blockStartPos = [position]
          }
        []
      pure BlockStartMatch
  , blockCanContain = const False
  , blockContainsLines = False
  , blockParagraph = False
  , blockContinue = const empty
  , blockConstructor = pure . displayMath . untokenize . getBlockText
  , blockFinalize = defaultFinalizer
  }

formatTelegramRichHtml :: Text -> Text
formatTelegramRichHtml input =
  case runIdentity (commonmarkWith telegramRichHtmlSyntax "telegram-message" input) of
    Left _ -> escapeHtml input
    Right html -> Text.strip (LazyText.toStrict (renderHtml html.value))

escapeHtml :: Text -> Text
escapeHtml =
  Text.concatMap \case
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    c -> Text.singleton c

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  text <$ guard (not (Text.null text))
