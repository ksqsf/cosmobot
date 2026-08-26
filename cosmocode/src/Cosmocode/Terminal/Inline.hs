{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Cosmocode.Terminal.Inline
  ( runTerminalIO
  ) where

import Control.Monad (forever, when)
import Cosmocode.Terminal.Internal (Terminal (..))
import Cosmocode.Types
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (isControl)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import Effectful
import Effectful.Concurrent (Concurrent)
import qualified Effectful.Concurrent.Async as Async
import qualified Effectful.Concurrent.STM as STM
import Effectful.Dispatch.Dynamic
import Effectful.Exception (bracket, finally)
import Graphics.Vty.Config (VtyUserConfig (..), defaultConfig)
import Graphics.Vty.Image (wcswidth)
import Graphics.Vty.Input
import Graphics.Vty.Platform.Unix.Input (buildInput)
import Graphics.Vty.Platform.Unix.Settings (defaultSettings)
import System.Console.ANSI
import System.IO (hFlush, stdout)

data UiEvent = KeyEvent !Event | ServerEvent !ServerEvent | Resize !Int

data Editor = Editor
  { before :: !Text
  , after :: !Text
  }

data UiState = UiState
  { model :: !Model
  , editor :: !Editor
  , printed :: !(Set.Set Text)
  , width :: !Int
  , renderMetrics :: !RenderMetrics
  }

data RenderMetrics = RenderMetrics
  { rows :: !Int
  , cursorRow :: !Int
  }

emptyMetrics :: RenderMetrics
emptyMetrics = RenderMetrics 0 0

type SendMessage es = Text -> Text -> Eff es (Either Text ())

runTerminalIO
  :: (Concurrent :> es, IOE :> es)
  => (Text -> Text -> Eff es (Either Text ()))
  -> Eff (Terminal : es) a
  -> Eff es a
runTerminalIO sendMessage inner = do
  events <- STM.atomically STM.newTChan
  interpret (runTerminalOperation sendMessage events) inner

runTerminalOperation
  :: (Concurrent :> es, IOE :> es)
  => (Text -> Text -> Eff es (Either Text ()))
  -> STM.TChan UiEvent
  -> LocalEnv localEs es
  -> Terminal (Eff localEs) a
  -> Eff es a
runTerminalOperation sendMessage events _ = \case
  PublishServerEvent event -> STM.atomically (STM.writeTChan events (ServerEvent event))
  RunSessionUi model ->
    runInline sendMessage events model

runInline
  :: (Concurrent :> es, IOE :> es)
  => SendMessage es
  -> STM.TChan UiEvent
  -> Model
  -> Eff es ()
runInline sendMessage events model = do
  terminalWidth <- liftIO (maybe 80 snd <$> getTerminalSize)
  settings <- liftIO defaultSettings
  let inputConfig = defaultConfig
        { configInputMap =
            [ (Nothing, "\ESC[13;5u", EvKey KEnter [MCtrl])
            , (Nothing, "\ESC[27;5;13~", EvKey KEnter [MCtrl])
            ]
        }
  bracket
    (liftIO (buildInput inputConfig settings))
    (liftIO . shutdownInput)
    \input -> flip finally (liftIO disableTerminal) do
      liftIO enableTerminal
      let initial = UiState
            { model
            , editor = Editor "" ""
            , printed = Set.fromList (map (.messageId) model.messages)
            , width = max 20 terminalWidth
            , renderMetrics = emptyMetrics
            }
      liftIO do
        printSession model
        mapM_ printMessage model.messages
      initial' <- liftIO (drawLive initial)
      final <- Async.withAsync (readInput input events) \_ ->
        uiLoop sendMessage events initial'
      liftIO (clearLive final.renderMetrics)

readInput :: (Concurrent :> es, IOE :> es) => Input -> STM.TChan UiEvent -> Eff es ()
readInput input events = forever do
  internal <- STM.atomically (STM.readTChan input.eventChannel)
  event <- case internal of
    InputEvent keyEvent -> pure (KeyEvent keyEvent)
    ResumeAfterInterrupt -> Resize . maybe 80 snd <$> liftIO getTerminalSize
  STM.atomically (STM.writeTChan events event)

uiLoop :: (Concurrent :> es, IOE :> es) => SendMessage es -> STM.TChan UiEvent -> UiState -> Eff es UiState
uiLoop sendMessage events state = do
  event <- STM.atomically (STM.readTChan events)
  case event of
    KeyEvent (EvKey (KChar 'c') [MCtrl]) -> pure state
    KeyEvent (EvKey KEnter modifiers)
      | MCtrl `elem` modifiers -> do
          next <- sendEditor sendMessage state
          redraw next >>= uiLoop sendMessage events
    KeyEvent keyEvent ->
      redraw state{editor = edit keyEvent state.editor} >>= uiLoop sendMessage events
    ServerEvent serverEvent -> do
      next <- liftIO (applyAndPrint serverEvent state)
      redraw next >>= uiLoop sendMessage events
    Resize columns ->
      redraw state{width = max 20 columns} >>= uiLoop sendMessage events

sendEditor :: IOE :> es => SendMessage es -> UiState -> Eff es UiState
sendEditor sendMessage state = do
  let body = state.editor.before <> state.editor.after
  if Text.null (Text.strip body)
    then pure state
    else sendMessage state.model.sessionId body >>= \case
      Left err -> pure state{model = state.model{status = Disconnected err}}
      Right () -> pure state{editor = Editor "" ""}

edit :: Event -> Editor -> Editor
edit event editor = case event of
  EvKey KEnter [] -> insert "\n"
  EvKey (KChar char) [] -> insert (Text.singleton char)
  EvKey KBS _ -> editor{before = Text.dropEnd 1 editor.before}
  EvKey KDel _ -> editor{after = Text.drop 1 editor.after}
  EvKey KLeft _ -> case Text.unsnoc editor.before of
    Nothing -> editor
    Just (rest, char) -> Editor rest (Text.cons char editor.after)
  EvKey KRight _ -> case Text.uncons editor.after of
    Nothing -> editor
    Just (char, rest) -> Editor (Text.snoc editor.before char) rest
  EvKey KHome _ -> moveToLineStart editor
  EvKey KEnd _ -> moveToLineEnd editor
  EvPaste bytes -> insert (decodePaste bytes)
  _ -> editor
  where
    insert text = Editor (editor.before <> text) editor.after

decodePaste :: ByteString.ByteString -> Text
decodePaste = either (const "") id . TextEncoding.decodeUtf8'

moveToLineStart :: Editor -> Editor
moveToLineStart editor =
  let (prefix, line) = Text.breakOnEnd "\n" editor.before
  in Editor prefix (line <> editor.after)

moveToLineEnd :: Editor -> Editor
moveToLineEnd editor =
  let (line, suffix) = Text.breakOn "\n" editor.after
  in Editor (editor.before <> line) suffix

applyAndPrint :: ServerEvent -> UiState -> IO UiState
applyAndPrint event state = do
  clearLive state.renderMetrics
  let model = applyServerEvent event state.model
      printable = case event of
        MessageReceived message
          | message.sessionId == model.sessionId && message.sender == "user" -> Just message
        MessageDone sessionId messageId
          | sessionId == model.sessionId -> findMessage messageId model.messages
        _ -> Nothing
  printed <- case printable of
    Just message | message.messageId `Set.notMember` state.printed ->
      printMessage message >> pure (Set.insert message.messageId state.printed)
    _ -> pure state.printed
  pure state{model, printed, renderMetrics = emptyMetrics}

findMessage :: Text -> [SessionMessage] -> Maybe SessionMessage
findMessage messageId = find ((== messageId) . (.messageId))

redraw :: IOE :> es => UiState -> Eff es UiState
redraw state = liftIO do
  clearLive state.renderMetrics
  drawLive state{renderMetrics = emptyMetrics}

drawLive :: UiState -> IO UiState
drawLive state = do
  let columns = state.width
      activity = concatMap (wrapText (columns - 1)) (activityLines state.model)
      drafts = concatMap (messageLines columns) (liveMessages state)
      editorRows = wrapText (columns - 3) (state.editor.before <> state.editor.after)
      (editorCursorRow, editorCursorColumn) = editorCursor (columns - 3) state.editor.before
      beforeEditor = length activity + length drafts
      totalRows = beforeEditor + length editorRows
      cursorRow = beforeEditor + editorCursorRow
  mapM_ plainLine (activity <> drafts)
  mapM_ (inputLine columns) editorRows
  cursorUpLine (totalRows - cursorRow)
  cursorForward (2 + editorCursorColumn)
  hFlush stdout
  pure state{renderMetrics = RenderMetrics totalRows cursorRow}

clearLive :: RenderMetrics -> IO ()
clearLive metrics = when (metrics.rows > 0) do
  cursorDownLine (metrics.rows - metrics.cursorRow)
  cursorUpLine metrics.rows
  clearFromCursorToScreenEnd

activityLines :: Model -> [Text]
activityLines model =
  reasoningLine <> toolLines <> statusLines model.status
  where
    reasoningLine = ["thinking…" | isJust model.reasoning]
    toolLines = ["tool  " <> name <> "…" | name <- Map.elems model.activeTools]

statusLines :: ConnectionStatus -> [Text]
statusLines Connected = []
statusLines (Disconnected reason) = ["disconnected: " <> reason]
statusLines (RpcFailed err) = ["RPC error: " <> err]

liveMessages :: UiState -> [SessionMessage]
liveMessages state =
  [ message
  | message <- state.model.messages
  , message.sender /= "user"
  , message.messageId `Set.notMember` state.printed
  ]

messageLines :: Int -> SessionMessage -> [Text]
messageLines columns message =
  wrapText (columns - 1) (role <> "\n" <> message.text <> "\n")
  where
    role = if message.sender == "user" then "you" else message.sender

printSession :: Model -> IO ()
printSession model = do
  setSGR [SetColor Foreground Dull Cyan]
  TextIO.putStr (safeTerminalText (model.server <> "  session=" <> model.sessionId) <> "\r\n\r\n")
  setSGR [Reset]

printMessage :: SessionMessage -> IO ()
printMessage message = do
  setSGR [SetConsoleIntensity BoldIntensity]
  TextIO.putStr (safeTerminalText role <> "\r\n")
  setSGR [Reset]
  TextIO.putStr (normalizeNewlines (safeTerminalText message.text) <> "\r\n\r\n")
  where
    role = if message.sender == "user" then "you" else message.sender

plainLine :: Text -> IO ()
plainLine line = TextIO.putStr (line <> "\r\n")

inputLine :: Int -> Text -> IO ()
inputLine columns line = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]
  TextIO.putStr "┃ "
  setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid White, SetColor Background Dull Blue]
  TextIO.putStr (line <> Text.replicate padding " ")
  setSGR [Reset]
  TextIO.putStr "\r\n"
  where
    padding = max 0 (columns - 3 - displayWidth line)

wrapText :: Int -> Text -> [Text]
wrapText width = concatMap (wrapLine (max 1 width)) . Text.splitOn "\n" . safeTerminalText

wrapLine :: Int -> Text -> [Text]
wrapLine width text
  | Text.null text = [""]
  | otherwise = go "" 0 (Text.unpack text)
  where
    go current _ [] = [current]
    go current used (char : rest)
      | used > 0 && used + charWidth char > width = current : go "" 0 (char : rest)
      | otherwise = go (Text.snoc current char) (used + charWidth char) rest

editorCursor :: Int -> Text -> (Int, Int)
editorCursor width before =
  let logicalLines = Text.splitOn "\n" (safeTerminalText before)
      wrapped = map (wrapLine (max 1 width)) logicalLines
      row = sum (map length (init wrapped)) + length (last wrapped) - 1
      column = displayWidth (last (last wrapped))
  in (row, column)

displayWidth :: Text -> Int
displayWidth = max 0 . wcswidth . Text.unpack

charWidth :: Char -> Int
charWidth = max 0 . wcswidth . pure

normalizeNewlines :: Text -> Text
normalizeNewlines = Text.replace "\n" "\r\n"

safeTerminalText :: Text -> Text
safeTerminalText = Text.concatMap \char ->
  if char == '\n' || not (isControl char) then Text.singleton char else "�"

enableTerminal :: IO ()
enableTerminal = TextIO.putStr "\ESC[?2004h"

disableTerminal :: IO ()
disableTerminal = do
  setSGR [Reset]
  TextIO.putStr "\ESC[?2004l"
  hFlush stdout
