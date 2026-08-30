{-|
Module      : Bot.Config.Edit
Description : Pure, span-preserving TOML configuration edits
Stability   : experimental
-}

module Bot.Config.Edit
  ( ConfigChange (..)
  , ConfigEditError (..)
  , applyConfigChanges
  , semanticDiff
  )
where

import Bot.Prelude
import qualified Bot.Config as Config
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (isAlphaNum, isAscii)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Toml.Syntax.Parser as TomlParser
import qualified Toml.Syntax.Position as Position
import qualified Toml.Syntax.Types as Syntax

data ConfigChange
  = SetOption ![Text] !Aeson.Value
  | RemoveOption ![Text]
  | ReplaceSecret ![Text] !Text
  | ClearSecret ![Text]
  | AddSection ![Text]
  | RemoveSection ![Text]
  deriving (Eq, Show)

instance Aeson.FromJSON ConfigChange where
  parseJSON = Aeson.withObject "configuration change" \object -> do
    operation <- object Aeson..: "operation"
    path <- object Aeson..: "path" >>= nonEmptyPath
    case operation :: Text of
      "set" -> SetOption path <$> object Aeson..: "value"
      "remove" -> pure (RemoveOption path)
      "replace_secret" -> ReplaceSecret path <$> object Aeson..: "value"
      "clear_secret" -> pure (ClearSecret path)
      "add_section" -> pure (AddSection path)
      "remove_section" -> pure (RemoveSection path)
      _ -> fail "operation must be set, remove, replace_secret, clear_secret, add_section, or remove_section"
    where
      nonEmptyPath [] = fail "path must not be empty"
      nonEmptyPath path = pure path

data ConfigEditError = ConfigEditError
  { code :: !Text
  , message :: !Text
  , path :: ![Text]
  }
  deriving (Eq, Show)

applyConfigChanges :: Config.ConfigDocument -> [ConfigChange] -> Either ConfigEditError Text
applyConfigChanges document changes = do
  validateChangeSet changes
  foldlM (applyChange document) (Config.configDocumentSource document) changes

applyChange :: Config.ConfigDocument -> Text -> ConfigChange -> Either ConfigEditError Text
applyChange document source = \case
  SetOption path value
    | Config.configOptionIsSecretAt document path -> invalid path "secret options require replace_secret or clear_secret"
    | otherwise -> render path value >>= setAssignment source path
  RemoveOption path
    | not (Config.configOptionKnownAt document path) -> invalid path "unknown configuration path"
    | otherwise -> removeAssignment source path
  ReplaceSecret path value
    | not (Config.configOptionIsSecretAt document path) -> invalid path "replace_secret requires a secret option"
    | otherwise -> render path (Aeson.String value) >>= setAssignment source path
  ClearSecret path
    | not (Config.configOptionIsSecretAt document path) -> invalid path "clear_secret requires a secret option"
    | otherwise -> removeAssignment source path
  AddSection path
    | not (knownSection document path) -> invalid path "unknown or non-repeatable configuration section"
    | otherwise -> addSection source path
  RemoveSection path
    | not (knownSection document path) -> invalid path "unknown configuration section"
    | otherwise -> removeSection source path
  where
    render path value = first (\message -> ConfigEditError "invalid_change" message path) (Config.renderConfigValueAt document path value)

knownSection :: Config.ConfigDocument -> [Text] -> Bool
knownSection document path =
  Config.configRepeatableSection path
    || any ((== path) . safeInit) (Map.keys (Config.configDocumentOptionValues document))

validateChangeSet :: [ConfigChange] -> Either ConfigEditError ()
validateChangeSet changes = do
  let paths = map changePath changes
      duplicates = [path | (index, path) <- zip [0 :: Int ..] paths, path `elem` take index paths]
  unless (null duplicates) (invalid (headOr [] duplicates) "the same path is changed more than once")
  for_ changes \case
    RemoveSection parent ->
      when (any (strictPrefix parent) (filter (/= parent) paths)) $
        invalid parent "remove_section conflicts with a child change"
    _ -> pure ()

changePath :: ConfigChange -> [Text]
changePath = \case
  SetOption path _ -> path
  RemoveOption path -> path
  ReplaceSecret path _ -> path
  ClearSecret path -> path
  AddSection path -> path
  RemoveSection path -> path

strictPrefix :: Eq a => [a] -> [a] -> Bool
strictPrefix prefix value = prefix /= value && prefix `isPrefixOf` value

data LocatedAssignment = LocatedAssignment
  { path :: ![Text]
  , valueStart :: !Int
  , valueEnd :: !Int
  , lineStart :: !Int
  , lineEnd :: !Int
  , inlineTable :: !Bool
  }

data LocatedTable = LocatedTable
  { path :: ![Text]
  , start :: !Int
  , end :: !Int
  , arrayTable :: !Bool
  }

data LocatedSource = LocatedSource
  { assignments :: ![LocatedAssignment]
  , tables :: ![LocatedTable]
  }

locateSource :: Text -> Either ConfigEditError LocatedSource
locateSource source = case TomlParser.parseRawToml source of
  Left located -> Left (ConfigEditError "invalid_change" (toText located.locThing) [])
  Right expressions -> Right (locateExpressions source expressions)

locateExpressions :: Text -> [Syntax.Expr Position.Position] -> LocatedSource
locateExpressions source expressions = LocatedSource (reverse assignments) tables
  where
    (_, assignments, rawTables) = foldl' step ([], [], []) expressions
    step (section, foundAssignments, foundTables) expression = case expression of
      Syntax.KeyValExpr key value ->
        let fullPath = section <> keyText key
            valueStart = valuePosition value
            valueEnd = scanValueEnd source valueStart
            assignment = LocatedAssignment
              { path = fullPath
              , valueStart
              , valueEnd
              , lineStart = startOfLine source (keyPosition key)
              , lineEnd = endOfLine source valueEnd
              , inlineTable = case value of Syntax.ValTable{} -> True; _ -> False
              }
        in (section, assignment : foundAssignments, foundTables)
      Syntax.TableExpr key ->
        let path = keyText key
        in (path, foundAssignments, (path, startOfLine source (keyPosition key), False) : foundTables)
      Syntax.ArrayTableExpr key ->
        let path = keyText key
        in (path, foundAssignments, (path, startOfLine source (keyPosition key), True) : foundTables)
    orderedTables = reverse rawTables
    tables = zipWith toTable orderedTables (drop 1 orderedTables <> [([], Text.length source, False)])
    toTable (path, start, arrayTable) (_, nextStart, _) = LocatedTable{path, start, end = nextStart, arrayTable}

keyText :: Syntax.Key annotation -> [Text]
keyText = map snd . toList

keyPosition :: Syntax.Key Position.Position -> Int
keyPosition = (.posIndex) . fst . head

valuePosition :: Syntax.Val Position.Position -> Int
valuePosition = \case
  Syntax.ValInteger position _ -> position.posIndex
  Syntax.ValFloat position _ -> position.posIndex
  Syntax.ValArray position _ -> position.posIndex
  Syntax.ValTable position _ -> position.posIndex
  Syntax.ValBool position _ -> position.posIndex
  Syntax.ValString position _ -> position.posIndex
  Syntax.ValTimeOfDay position _ -> position.posIndex
  Syntax.ValZonedTime position _ -> position.posIndex
  Syntax.ValLocalTime position _ -> position.posIndex
  Syntax.ValDay position _ -> position.posIndex

setAssignment :: Text -> [Text] -> Text -> Either ConfigEditError Text
setAssignment source path rendered = do
  located <- locateSource source
  case find ((== path) . (.path)) located.assignments of
    Just assignment -> pure (replaceSpan assignment.valueStart assignment.valueEnd rendered source)
    Nothing
      | unsupportedInlineTarget located path -> unsupported path
      | otherwise -> insertAssignment located source path rendered

removeAssignment :: Text -> [Text] -> Either ConfigEditError Text
removeAssignment source path = do
  located <- locateSource source
  case find ((== path) . (.path)) located.assignments of
    Just assignment -> pure (replaceSpan assignment.lineStart assignment.lineEnd "" source)
    Nothing
      | unsupportedInlineTarget located path -> unsupported path
      | otherwise -> pure source

insertAssignment :: LocatedSource -> Text -> [Text] -> Text -> Either ConfigEditError Text
insertAssignment located source path rendered =
  case listUnsnoc path of
    Nothing -> invalid path "option path must not be empty"
    Just (section, key) ->
      let line = quoteKey key <> " = " <> rendered <> newlineStyle source
      in case find ((== section) . (.path)) located.tables of
        Just table
          | table.arrayTable -> unsupported path
          | otherwise -> pure (insertAt table.end line source)
        Nothing
          | null section -> pure (insertAt (rootEnd located source) line source)
          | otherwise -> pure (appendTable source section line)

addSection :: Text -> [Text] -> Either ConfigEditError Text
addSection source path = do
  located <- locateSource source
  when (any ((== path) . (.path)) located.tables) (invalid path "configuration section already exists")
  when (unsupportedInlineTarget located path) (unsupported path)
  pure (appendTable source path "")

removeSection :: Text -> [Text] -> Either ConfigEditError Text
removeSection source path = do
  located <- locateSource source
  case find ((== path) . (.path)) located.tables of
    Nothing
      | unsupportedInlineTarget located path -> unsupported path
      | otherwise -> pure source
    Just table
      | table.arrayTable -> unsupported path
      | otherwise ->
          let owned = takeWhile (\candidate -> path `isPrefixOf` candidate.path) (dropWhile ((< table.start) . (.start)) located.tables)
              sectionEnd = maybe table.end (.end) (viaNonEmpty last owned)
          in pure (replaceSpan table.start sectionEnd "" source)

unsupportedInlineTarget :: LocatedSource -> [Text] -> Bool
unsupportedInlineTarget located target =
  any (\assignment -> assignment.inlineTable && assignment.path `isPrefixOf` target) located.assignments

unsupported :: [Text] -> Either ConfigEditError a
unsupported path = Left (ConfigEditError "unsupported_source_shape" "inline tables and array tables cannot be edited safely" path)

invalid :: [Text] -> Text -> Either ConfigEditError a
invalid path message = Left (ConfigEditError "invalid_change" message path)

appendTable :: Text -> [Text] -> Text -> Text
appendTable source path body =
  source <> separator <> "[" <> Text.intercalate "." (map quoteKey path) <> "]" <> newline <> body
  where
    newline = newlineStyle source
    separator
      | Text.null source || newline `Text.isSuffixOf` source = ""
      | otherwise = newline

insertAt :: Int -> Text -> Text -> Text
insertAt index insertion source =
  let (before, after) = Text.splitAt index source
      newline = newlineStyle source
      prefix
        | Text.null before || newline `Text.isSuffixOf` before = ""
        | otherwise = newline
  in before <> prefix <> insertion <> after

replaceSpan :: Int -> Int -> Text -> Text -> Text
replaceSpan start end replacement source =
  Text.take start source <> replacement <> Text.drop end source

rootEnd :: LocatedSource -> Text -> Int
rootEnd located source = maybe (Text.length source) (.start) (viaNonEmpty head located.tables)

startOfLine :: Text -> Int -> Int
startOfLine source index =
  Text.length before - Text.length (Text.takeWhileEnd (/= '\n') before)
  where before = Text.take index source

endOfLine :: Text -> Int -> Int
endOfLine source index =
  let rest = Text.drop index source
      lineLength = Text.length (Text.takeWhile (/= '\n') rest)
  in min (Text.length source) (index + lineLength + if Text.length rest > lineLength then 1 else 0)

newlineStyle :: Text -> Text
newlineStyle source
  | "\r\n" `Text.isInfixOf` source = "\r\n"
  | otherwise = "\n"

quoteKey :: Text -> Text
quoteKey key
  | not (Text.null key) && Text.all bareKeyChar key = key
  | otherwise = TextEncoding.decodeUtf8 . LazyByteString.toStrict $ Aeson.encode key
  where bareKeyChar character = isAscii character && (isAlphaNum character || character == '_' || character == '-')

data ScanMode = Normal | Basic | Literal | MultiBasic | MultiLiteral
  deriving (Eq)

scanValueEnd :: Text -> Int -> Int
scanValueEnd source start = trimEnd (go start Normal 0 0 False)
  where
    length_ = Text.length source
    at index = Text.index source index
    starts index token = token `Text.isPrefixOf` Text.drop index source
    go :: Int -> ScanMode -> Int -> Int -> Bool -> Int
    go index mode squares curlies escaped
      | index >= length_ = length_
      | otherwise = case mode of
          Normal
            | starts index "\"\"\"" -> go (index + 3) MultiBasic squares curlies False
            | starts index "'''" -> go (index + 3) MultiLiteral squares curlies False
            | at index == '"' -> go (index + 1) Basic squares curlies False
            | at index == '\'' -> go (index + 1) Literal squares curlies False
            | at index == '[' -> go (index + 1) Normal (squares + 1) curlies False
            | at index == ']' -> go (index + 1) Normal (max 0 (squares - 1)) curlies False
            | at index == '{' -> go (index + 1) Normal squares (curlies + 1) False
            | at index == '}' -> go (index + 1) Normal squares (max 0 (curlies - 1)) False
            | at index == '#' && squares + curlies == 0 -> index
            | at index == '#' -> go (skipComment index) Normal squares curlies False
            | at index `elem` ['\n', '\r'] && squares + curlies == 0 -> index
            | otherwise -> go (index + 1) Normal squares curlies False
          Basic
            | escaped -> go (index + 1) Basic squares curlies False
            | at index == '\\' -> go (index + 1) Basic squares curlies True
            | at index == '"' -> go (index + 1) Normal squares curlies False
            | otherwise -> go (index + 1) Basic squares curlies False
          Literal
            | at index == '\'' -> go (index + 1) Normal squares curlies False
            | otherwise -> go (index + 1) Literal squares curlies False
          MultiBasic
            | starts index "\"\"\"" -> go (index + 3) Normal squares curlies False
            | escaped -> go (index + 1) MultiBasic squares curlies False
            | at index == '\\' -> go (index + 1) MultiBasic squares curlies True
            | otherwise -> go (index + 1) MultiBasic squares curlies False
          MultiLiteral
            | starts index "'''" -> go (index + 3) Normal squares curlies False
            | otherwise -> go (index + 1) MultiLiteral squares curlies False
    skipComment index = index + Text.length (Text.takeWhile (/= '\n') (Text.drop index source))
    trimEnd end =
      let value = Text.take (end - start) (Text.drop start source)
      in end - Text.length (Text.takeWhileEnd (`elem` [' ', '\t']) value)

semanticDiff :: Config.ConfigDocument -> Config.ConfigDocument -> [Aeson.Value]
semanticDiff before after = map render changedPaths
  where
    beforeValues = Config.configDocumentOptionValues before
    afterValues = Config.configDocumentOptionValues after
    paths = Set.toList (Map.keysSet beforeValues <> Map.keysSet afterValues)
    changedPaths = filter (\path -> Map.lookup path beforeValues /= Map.lookup path afterValues) paths
    render path = Aeson.object
      [ "path" Aeson..= path
      , "before" Aeson..= Map.lookup path beforeValues
      , "after" Aeson..= Map.lookup path afterValues
      , "activation" Aeson..= ("restart" :: Text)
      ]

safeInit :: [a] -> [a]
safeInit [] = []
safeInit [_] = []
safeInit (value : rest) = value : safeInit rest

headOr :: a -> [a] -> a
headOr fallback = fromMaybe fallback . viaNonEmpty head

listUnsnoc :: [a] -> Maybe ([a], a)
listUnsnoc = \case
  [] -> Nothing
  values -> case reverse values of
    [] -> Nothing
    value : rest -> Just (reverse rest, value)
