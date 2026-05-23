module Bot.Log
  ( module Effectful.Katip
  , logDebug
  , logInfo
  , logNotice
  , logWarning
  , logError
  , logCritical
  , logAt
  , logExceptionAt
  )
where

import Effectful
import Effectful.Katip
import Relude

logDebug :: KatipE :> es => Text -> Eff es ()
logDebug =
  logAt DebugS

logInfo :: KatipE :> es => Text -> Eff es ()
logInfo =
  logAt InfoS

logNotice :: KatipE :> es => Text -> Eff es ()
logNotice =
  logAt NoticeS

logWarning :: KatipE :> es => Text -> Eff es ()
logWarning =
  logAt WarningS

logError :: KatipE :> es => Text -> Eff es ()
logError =
  logAt ErrorS

logCritical :: KatipE :> es => Text -> Eff es ()
logCritical =
  logAt CriticalS

logAt :: KatipE :> es => Severity -> Text -> Eff es ()
logAt severity message =
  logFM severity (logStr message)

logExceptionAt :: KatipE :> es => Severity -> Eff es a -> Eff es a
logExceptionAt severity action =
  action `logExceptionM` severity
