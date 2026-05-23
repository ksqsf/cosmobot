{-|
Module      : Bot.Agent.Tools.Typst
Description : Agent Typst rendering tool
Stability   : experimental
-}

module Bot.Agent.Tools.Typst
  ( typstToImageTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Typst as Typst
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

typstToImageTool :: (Chat.Chat :> es, Typst.Typst :> es) => Tool es
typstToImageTool = Tool
  { name = "typst_render"
  , description = "Render a Typst document to a PNG image and send it to the current chat. Use this for diagrams, tables, formulas, posters, or other precise layouts that should be generated from Typst source. The source must be a complete Typst document."
  , parameters = objectSchema
      [ fieldText "source" "Complete Typst source to compile into a PNG image. Use self-contained content; external files are not available."
      , fieldText "caption" "Optional short caption to include in the tool result for context. It is not sent as a separate message."
      ]
      ["source"]
  , noisy = False
  , allowed = everyone
  , start = \context -> pure \args ->
      withParsedToolArgs typstArgs args \toolArgs -> do
        Typst.withTypstPng toolArgs.source \imagePath -> do
          sent <- Chat.replyTo context.message (Chat.imageDirective ("file://" <> Text.pack imagePath))
          let sentText = show sent :: String
              captionText :: Text
              captionText =
                maybe "" (" Caption: " <>) toolArgs.caption
          pure (toolText [i|Rendered and sent Typst image message id: #{sentText}.#{captionText}|])
  }

data TypstArgs = TypstArgs
  { source :: !Text
  , caption :: !(Maybe Text)
  }

typstArgs :: Aeson.Value -> AesonTypes.Parser TypstArgs
typstArgs =
  Aeson.withObject "typst_render arguments" \o -> do
    source <- o Aeson..: Key.fromText "source"
    caption <- o Aeson..:? Key.fromText "caption"
    pure TypstArgs{source, caption}
