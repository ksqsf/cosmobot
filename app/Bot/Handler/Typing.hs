{-|
Module      : Bot.Handler.Typing
Description : Typing website commands
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Handler.Typing
  ( typingHandlers
  )
where

import qualified Bot.Effect.Chat as Chat
import Bot.Core.Route
import qualified Bot.Util.Html as Html
import qualified Bot.Util.Image as Image
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import Data.Time
import Network.HTTP.Req
import System.Directory (removeDirectoryRecursive)
import System.Exit
import System.FilePath
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process
import Text.Printf

championshipRankCommand :: Text
championshipRankCommand = "!jbscj"

tigerRankCommand :: Text
tigerRankCommand = "!hbcj"

-- | Routes that render typing leaderboard snapshots.
typingHandlers
  :: (Chat.Chat :> es, Log :> es, IOE :> es)
  => [RouteHandler es]
typingHandlers =
  [ rankRoute championshipRankCommand "锦标赛成绩" "锦标赛排行榜生成失败。" fetchChampionshipRows
  , rankRoute tigerRankCommand "虎杯成绩" "虎杯成绩生成失败。" fetchTigerRows
  ]

rankRoute
  :: (Chat.Chat :> es, Log :> es, IOE :> es)
  => Text
  -> Text
  -> Text
  -> IO [[Text]]
  -> RouteHandler es
rankRoute commandText titleSuffix failureMessage fetchRows =
  requireAuth canStartConversation (\_ -> pure ()) $
    stopOn (command commandText) \message _ -> do
      logInfo "matched typing rank route" (commandText <> " " <> incomingMessageLogLine message)
      forkEff (sendRankImage titleSuffix failureMessage fetchRows message)

sendRankImage
  :: (Chat.Chat :> es, Log :> es, IOE :> es)
  => Text
  -> Text
  -> IO [[Text]]
  -> IncomingMessage
  -> Eff es ()
sendRankImage titleSuffix failureMessage fetchRows message =
  handleError do
    logInfo "Fetching typing rank rows" titleSuffix
    title <- liftIO (rankTitle titleSuffix)
    rows <- liftIO fetchRows
    logInfo "Fetched typing rank rows" (title, length rows)
    withRenderedRankImage title rows \imagePath -> do
      logInfo "Rendered typing rank image" imagePath
      sent <- Chat.replyTo message (ReplyBody.imageDirective ("file://" <> Text.pack imagePath))
      logInfo "Sent typing rank image" sent
      when (isNothing sent) do
        void $ Chat.replyTo message [i|#{title}已生成，但图片发送失败。|]
  where
    handleError action =
      action `catch` \(err :: SomeException) -> do
        logAttention "Failed to render typing rank" (show err :: String)
        void $ Chat.replyTo message failureMessage

withRenderedRankImage :: IOE :> es => Text -> [[Text]] -> (FilePath -> Eff es a) -> Eff es a
withRenderedRankImage title rows action =
  bracket acquire release \dir -> do
    imagePath <- liftIO (renderRankImage dir title rows)
    action imagePath
  where
    acquire = liftIO do
      tmp <- getCanonicalTemporaryDirectory
      createTempDirectory tmp "cosmobot-typing-"
    release =
      liftIO . removeDirectoryRecursive

rankTitle :: Text -> IO Text
rankTitle suffix = do
  date <- currentDateText
  pure (date <> suffix)

currentDateText :: IO Text
currentDateText = do
  now <- getCurrentTime
  let tz = hoursToTimeZone 8
      day = localDay (utcToLocalTime tz now)
  pure (Text.pack (formatTime defaultTimeLocale "%F" day))

fetchChampionshipRows :: IO [[Text]]
fetchChampionshipRows = do
  html <- fetchChampionshipPage
  table <- maybe (fail "championship rank table not found") pure (extractRankTable html)
  let rows = rankRows table
  case rows of
    [] -> fail "championship rank table is empty"
    _  -> pure rows

fetchChampionshipPage :: IO Text
fetchChampionshipPage = do
  body <- runReq defaultHttpConfig $
    responseBody <$> req GET url NoReqBody bsResponse mempty
  pure (TextEncoding.decodeUtf8Lenient body)
  where
    -- The site's TLS endpoint fails with Haskell's TLS stack because it does
    -- not support Extended Main Secret. The same page is available over HTTP.
    url = http "www.jsxiaoshi.com" /: "championships_rank.html"

fetchTigerRows :: IO [[Text]]
fetchTigerRows = do
  date <- currentDateText
  value <- runReq defaultHttpConfig $
    responseBody <$> req GET (tigerLeaderboardUrl date) NoReqBody jsonResponse ("limit" =: (50 :: Int))
  maybe (fail "tiger leaderboard response had unexpected shape") pure
    (AesonTypes.parseMaybe tigerRows value)

tigerLeaderboardUrl :: Text -> Url 'Https
tigerLeaderboardUrl date =
  https "race.tiger-code.com" /: "api" /: "leaderboard" /: "date" /: date

tigerRows :: Aeson.Value -> AesonTypes.Parser [[Text]]
tigerRows =
  Aeson.withObject "TigerLeaderboardResponse" $ \o -> do
    data_ <- o Aeson..: "data"
    leaderboard <- data_ Aeson..: "leaderboard"
    body <- traverse tigerRow leaderboard
    pure (tigerHeader : body)

tigerHeader :: [Text]
tigerHeader =
  ["排名", "用户名", "VIP", "速度", "击键", "码长", "打词率", "时间", "键准", "输入法"]

tigerRow :: Aeson.Value -> AesonTypes.Parser [Text]
tigerRow =
  Aeson.withObject "TigerLeaderboardRow" $ \o -> do
    rank <- o Aeson..:? "rank" :: AesonTypes.Parser (Maybe Int)
    username <- o Aeson..:? "username" :: AesonTypes.Parser (Maybe Text)
    vipLevel <- o Aeson..:? "vip_level" :: AesonTypes.Parser (Maybe Int)
    speed <- o Aeson..:? "speed" :: AesonTypes.Parser (Maybe Double)
    hitRate <- o Aeson..:? "hit_rate" :: AesonTypes.Parser (Maybe Double)
    kpw <- o Aeson..:? "kpw" :: AesonTypes.Parser (Maybe Double)
    wordRatio <- o Aeson..:? "word_ratio" :: AesonTypes.Parser (Maybe Double)
    time <- (o Aeson..:? "total_time" <|> o Aeson..:? "time") :: AesonTypes.Parser (Maybe Double)
    accuracy <- o Aeson..:? "accuracy" :: AesonTypes.Parser (Maybe Double)
    inputMethod <- o Aeson..:? "input_method" :: AesonTypes.Parser (Maybe Text)
    pure
      [ maybeText rank
      , fromMaybe "-" username
      , maybeText vipLevel
      , maybeNumber speed
      , maybeNumber hitRate
      , maybeNumber kpw
      , maybePercent ((* 100) <$> wordRatio)
      , maybe "-" formatSeconds time
      , maybePercent accuracy
      , fromMaybe "-" inputMethod
      ]

extractRankTable :: Text -> Maybe Text
extractRankTable html = do
  let (beforeId, fromId) = Text.breakOn "id=\"sdph\"" html
  guard (not (Text.null fromId))
  tablePrefix <- viaNonEmpty last (Text.splitOn "<table" beforeId)
  let tableStart = "<table" <> tablePrefix <> fromId
      (table, rest) = Text.breakOn "</table>" tableStart
  guard (not (Text.null rest))
  pure (table <> "</table>")

rankRows :: Text -> [[Text]]
rankRows table =
  case parsedRows of
    [] -> []
    heading : body ->
      let keep = keepIndexes heading
          heading' = select keep heading
      in heading' : map (normalizeRow (length heading') . select keep) body
  where
    parsedRows =
      filter (not . null)
        $ map parseRow
        $ drop 1
        $ Text.splitOn "<tr" table

keepIndexes :: [Text] -> [Int]
keepIndexes heading =
  [ index
  | (index, name) <- zip [0 :: Int ..] heading
  , name `notElem` ["头像", "操作"]
  ]

select :: [Int] -> [Text] -> [Text]
select indexes cells =
  mapMaybe (`safeIndex` cells) indexes

safeIndex :: Int -> [a] -> Maybe a
safeIndex index values =
  viaNonEmpty head (drop index values)

normalizeRow :: Int -> [Text] -> [Text]
normalizeRow size row =
  take size (row <> repeat "")

parseRow :: Text -> [Text]
parseRow row =
  parseCells row []

parseCells :: Text -> [Text] -> [Text]
parseCells input acc =
  case Text.breakOn "<td" input of
    (_, "") ->
      reverse acc
    (_, rest) ->
      let (tag, afterTag) = Text.breakOn ">" rest
          contentStart = Text.drop 1 afterTag
          (content, afterCell) = Text.breakOn "</td>" contentStart
          next = Text.drop (Text.length "</td>") afterCell
      in if "hidden" `Text.isInfixOf` tag
        then parseCells next acc
        else parseCells next (cleanCell content : acc)

cleanCell :: Text -> Text
cleanCell =
  Text.unwords
    . Text.words
    . Html.htmlDecode
    . Html.stripHtmlTags

renderRankImage :: FilePath -> Text -> [[Text]] -> IO FilePath
renderRankImage dir title rows = do
  let typstPath = dir </> "typing-rank" <.> "typ"
      pngPath = dir </> "typing-rank" <.> "png"
  TextIO.writeFile typstPath (typstDocument title rows)
  (code, _out, err) <- readProcessWithExitCode "typst" ["compile", typstPath, pngPath] ""
  case code of
    ExitSuccess -> pure pngPath
    ExitFailure _ -> do
      cleanupRenderFiles typstPath pngPath
      fail ("typst failed: " <> err)

cleanupRenderFiles :: FilePath -> FilePath -> IO ()
cleanupRenderFiles typstPath pngPath =
  Image.removeFilesIfExists [typstPath, pngPath]

typstDocument :: Text -> [[Text]] -> Text
typstDocument title rows =
  Text.unlines
    [ "#let table-width = " <> tableWidth rows
    , "#set page(width: table-width + 36pt, height: auto, margin: 18pt)"
    , "#set text(font: (\"Droid Sans Fallback\", \"Noto Sans\", \"DejaVu Sans\"), size: 8pt)"
    , "#align(center)[#text(size: 16pt, weight: \"bold\")[" <> typstEscape title <> "]]"
    , "#v(8pt)"
    , "#block(width: table-width)["
    , "#table("
    , "  columns: " <> tableColumns rows <> ","
    , "  inset: 3pt,"
    , "  stroke: rgb(\"d8dee9\"),"
    , "  fill: (_, y) => if y == 0 { rgb(\"edf2f7\") } else if calc.rem(y, 2) == 0 { rgb(\"fbfbfb\") } else { white },"
    , "  align: center + horizon,"
    , cells
    , ")"
    , "]"
    ]
  where
    cells =
      Text.intercalate ",\n"
        [ "  " <> typstCell cell
        | row <- rows
        , cell <- row
        ]

tableColumns :: [[Text]] -> Text
tableColumns rows =
  case maybe 0 length (viaNonEmpty head rows) of
    14 -> "(36pt, 72pt, 42pt, 112pt, 48pt, 48pt, 48pt, 42pt, 64pt, 38pt, 52pt, 52pt, 112pt, 92pt)"
    10 -> "(36pt, 104pt, 34pt, 58pt, 48pt, 48pt, 54pt, 58pt, 52pt, 116pt)"
    n  -> "(" <> Text.intercalate ", " (replicate n "auto") <> ")"

tableWidth :: [[Text]] -> Text
tableWidth rows =
  case maybe 0 length (viaNonEmpty head rows) of
    14 -> "828pt"
    10 -> "608pt"
    _  -> "auto"

typstCell :: Text -> Text
typstCell cell =
  "[" <> typstEscape cell <> "]"

typstEscape :: Text -> Text
typstEscape =
  Text.concatMap \case
    '\\' -> "\\\\"
    '['  -> "\\["
    ']'  -> "\\]"
    '#'  -> "\\#"
    '$'  -> "\\$"
    '_'  -> "\\_"
    '*'  -> "\\*"
    '`'  -> "\\`"
    c    -> Text.singleton c

maybeText :: Show a => Maybe a -> Text
maybeText =
  maybe "-" (Text.pack . show)

maybeNumber :: Maybe Double -> Text
maybeNumber =
  maybe "-" (Text.pack . printf "%.2f")

maybePercent :: Maybe Double -> Text
maybePercent =
  maybe "-" (Text.pack . printf "%.2f%%")

formatSeconds :: Double -> Text
formatSeconds seconds =
  Text.pack (printf "%02d:%02d" minutes restSeconds)
  where
    totalSeconds = max 0 (round seconds :: Int)
    minutes = totalSeconds `div` 60
    restSeconds = totalSeconds `mod` 60
