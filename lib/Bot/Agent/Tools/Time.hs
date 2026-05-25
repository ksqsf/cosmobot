{-|
Module      : Bot.Agent.Tools.Time
Description : Agent datetime tool
Stability   : experimental
-}

module Bot.Agent.Tools.Time
  ( datetimeTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

datetimeTool :: IOE :> es => Tool es
datetimeTool = Tool
  { name = "now"
  , description = "Return the current date and time in UTC and in the bot host's local timezone."
  , parameters = objectSchema [] []
  , noisy = False
  , allowed = \context -> context.toolConfig.datetime
  , start = \_ -> pure \_ -> do
      now <- liftIO getCurrentTime
      zone <- liftIO getCurrentTimeZone
      let localTime = utcToLocalTime zone now
      pure (toolText (jsonText (Aeson.object
        [ "utc" Aeson..= Text.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
        , "local" Aeson..= Text.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" localTime)
        , "timezone_name" Aeson..= Text.pack (timeZoneName zone)
        , "timezone_offset_minutes" Aeson..= timeZoneMinutes zone
        , "unix_time" Aeson..= (realToFrac (utcTimeToPOSIXSeconds now) :: Double)
        ])))
  }
