module Bot.Util.Time (secondsToMicroseconds) where

import Bot.Prelude

-- | Saturate before converting to Int; overflow can disable a timeout entirely.
-- Accept Integer so callers can also add grace periods without overflowing.
secondsToMicroseconds :: Integer -> Int
secondsToMicroseconds seconds =
  fromInteger (max 0 (min (toInteger (maxBound :: Int)) (seconds * 1_000_000)))
