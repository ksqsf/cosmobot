module Bot.Agent.Tools.Telegram
  ( telegramRequestTool
  )
where

import Bot.Agent.Failure (externalServiceFailure)
import Bot.Agent.Tool
import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Core.Message (ChatPlatform (PlatformTelegram), IncomingMessage (..))
import qualified Bot.Effect.Telegram as Telegram
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Char (isAscii, isAlpha)

telegramRequestTool :: Telegram.Telegram :> es => Tool (Eff es)
telegramRequestTool =
  tagged [chatTag]
  . allowWhen telegramSuperuser
  . withDescription "Call a Telegram Bot API method with JSON parameters using the configured bot token. Returns the API result. No multipart uploads; use file_id or URLs for files. Prefer the chat tools for sending messages so they are tracked."
  $ tool "telegram_request"
      ( validateArgument validateMethod (requiredArgument (fieldText "method" "Bot API method name, e.g. getMe or getChat. Not an HTTP verb or URL."))
      , withDefault mempty (optionalArgument ("parameters", Aeson.object ["type" Aeson..= ("object" :: Text), "additionalProperties" Aeson..= True]))
      )
      \method parameters -> do
        context <- askToolContext
        if telegramSuperuser context
          then do
            result <- trySync (Telegram.telegramCall method parameters)
            pure $ either (toolFailure . externalServiceFailure "Telegram request failed." . toText . displayException) (toolText . jsonText) result
          else pure (toolFailure (permissionDeniedFailure "telegram_request requires a Telegram superuser." "telegram_request was called outside a Telegram superuser context."))

telegramSuperuser :: Context -> Bool
telegramSuperuser context =
  superuserOnly context && context.message.platform == PlatformTelegram

validateMethod :: Text -> Either Text Text
validateMethod method
  | not (Text.null method), Text.all (\c -> isAscii c && isAlpha c) method = Right method
  | otherwise = Left "method must be a non-empty Bot API method name containing ASCII letters only."
