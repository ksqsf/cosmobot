{-|
Module      : Bot.Chat.Driver.Matrix.Types
Description : Shared Matrix driver types
Stability   : experimental
-}

module Bot.Chat.Driver.Matrix.Types
  ( Config (..)
  )
where

import Bot.Prelude
import qualified Prelude

data Config = Config
  { homeserver :: !Text
  , loginUser :: !(Maybe Text)
  , loginPassword :: !(Maybe Text)
  , deviceId :: !(Maybe Text)
  , directRooms :: ![Text]
  , userId :: !(Maybe Text)
  , allowedRooms :: ![Text]
  , superusers :: ![Text]
  }

instance Show Config where
  showsPrec _ _ = Prelude.showString "<Matrix.Config>"
