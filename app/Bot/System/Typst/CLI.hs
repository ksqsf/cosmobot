{-|
Module      : Bot.System.Typst.CLI
Description : Typst CLI-backed renderer
Stability   : experimental
-}

module Bot.System.Typst.CLI
  ( runTypst
  , withRenderedTypstPng
  )
where

import Bot.Prelude
import qualified Bot.Effect.Typst as Typst
import qualified Bot.Util.Image as Image
import qualified Data.Text.IO as TextIO
import Effectful.FileSystem (FileSystem)
import Effectful.Process
import System.Directory (removeDirectoryRecursive)
import System.Exit
import System.FilePath
import System.IO.Error (userError)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

runTypst
  :: (IOE :> es, Log :> es, Fail :> es, FileSystem :> es, Process :> es)
  => Eff (Typst.Typst : es) a
  -> Eff es a
runTypst = interpret $ \localEnv operation ->
  localSeqUnlift localEnv \runLocal ->
    case operation of
      Typst.WithTypstPng source action ->
        withRenderedTypstPng source (runLocal . action)

withRenderedTypstPng
  :: (IOE :> es, Log :> es, Fail :> es, FileSystem :> es, Process :> es)
  => Text
  -> (FilePath -> Eff es a)
  -> Eff es a
withRenderedTypstPng source action =
  bracket acquire release \dir -> do
    imagePath <- renderTypstPng dir source
    action imagePath
  where
    acquire = liftIO do
      tmp <- getCanonicalTemporaryDirectory
      createTempDirectory tmp "cosmobot-typst-"
    release =
      liftIO . removeDirectoryRecursive

renderTypstPng :: (IOE :> es, Log :> es, Fail :> es, FileSystem :> es, Process :> es) => FilePath -> Text -> Eff es FilePath
renderTypstPng dir source = do
  let typstPath = dir </> "document" <.> "typ"
      pngPath = dir </> "document" <.> "png"
  liftIO $ TextIO.writeFile typstPath source
  logInfo_ [i|Rendering Typst document: #{typstPath}|]
  (code, _out, err) <-
    readProcessWithExitCode "typst" ["compile", "--root", dir, typstPath, pngPath] ""
  case code of
    ExitSuccess -> do
      logInfo_ [i|Rendered Typst PNG: #{pngPath}|]
      pure pngPath
    ExitFailure _ -> do
      Image.removeFilesIfExists [typstPath, pngPath]
      throwIO (userError ("typst failed: " <> err))
