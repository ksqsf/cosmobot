{-|
Module      : Bot.Agent.Tools.Typst
Description : Agent Typst rendering tool
Stability   : experimental
-}

module Bot.Agent.Tools.Typst
  ( typstRenderTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Typst as Typst
import Bot.Prelude
import Bot.System.Typst.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

typstRenderTool :: (Chat.Chat :> es, Typst.Typst :> es) => Tool es
typstRenderTool = Tool
  { name = "typst_render"
  , description = "Render a Typst document and send it to the current chat. Use this for diagrams, tables, formulas, posters, or other precise layouts that should be generated from Typst source. The source must be a complete Typst document."
  , parameters = objectSchema
      [ fieldText "source" "Complete Typst source. Use self-contained content; external files are not available."
      , fieldText "format" "'png' or 'pdf'. For QQ: only use PNG."
      , fieldText "caption" "Optional short caption to include in the tool result for context. It is not sent as a separate message."
      ]
      ["source", "format"]
  , noisy = False
  , allowed = everyone
  , start = \context -> pure \args ->
      withParsedToolArgs typstArgs args \toolArgs -> do
        Typst.withTypst toolArgs.format toolArgs.source \outputPath -> do
          sent <- case toolArgs.format of
            TypstOutputPNG -> Chat.replyTo context.message (Chat.imageDirective ("file://" <> Text.pack outputPath))
            TypstOutputPDF -> Chat.uploadFile context.message outputPath
          let sentText = show sent :: String
              captionText :: Text
              captionText =
                maybe "" (" Caption: " <>) toolArgs.caption
          pure (toolText [i|Rendered and sent Typst document message id: #{sentText}.#{captionText}|])
  }

data TypstArgs = TypstArgs
  { source :: !Text
  , format :: !TypstOutputFormat
  , caption :: !(Maybe Text)
  }

typstArgs :: Aeson.Value -> AesonTypes.Parser TypstArgs
typstArgs =
  Aeson.withObject "typst_render arguments" \o -> do
    source <- o Aeson..: Key.fromText "source"
    format <- o Aeson..: Key.fromText "format"
    caption <- o Aeson..:? Key.fromText "caption"
    pure TypstArgs{source, format, caption}
