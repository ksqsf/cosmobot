{-|
Module      : Bot.Util.Process
Description : Process output helpers
Stability   : experimental
-}

module Bot.Util.Process
  ( processOutputText
  )
where

import Bot.Prelude
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.STM as STM

processOutputText :: Concurrent :> es => STM.STM LazyByteString.ByteString -> Eff es Text
processOutputText =
  fmap (TextEncoding.decodeUtf8Lenient . LazyByteString.toStrict) . STM.atomically
