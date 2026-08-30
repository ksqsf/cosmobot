{-|
Module      : Bot.Chat.Driver.Telegram.Types
Description : Shared Telegram driver types
Stability   : experimental
-}

module Bot.Chat.Driver.Telegram.Types
  ( Config (..)
  )
where

import Bot.Prelude
import qualified Prelude

-- | Telegram Bot API credentials.
data Config = Config
  { botToken :: !Text
  , botIds :: ![Integer]
  , botUsernames :: ![Text]
  , allowedChatIds :: ![Integer]
  , allowedChatAliases :: ![Text]
  , superusers :: ![Text]
  }

instance Show Config where
  showsPrec _ _ = Prelude.showString "<Telegram.Config>"
