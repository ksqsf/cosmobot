{-|
Module      : Bot.Chat.Driver.Discord.Types
Description : Discord driver configuration
Stability   : experimental
-}

module Bot.Chat.Driver.Discord.Types
  ( Config (..)
  , defaultConfig
  )
where

import Bot.Prelude

data Config = Config
  { botToken :: !Text
  , botId :: !(Maybe Text)
  , applicationId :: !(Maybe Text)
  , allowedGuilds :: ![Integer]
  , allowedChannels :: ![Integer]
  , allowedUsers :: ![Text]
  , superusers :: ![Text]
  , gatewayHost :: !String
  , gatewayPath :: !String
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig = Config
  { botToken = ""
  , botId = Nothing
  , applicationId = Nothing
  , allowedGuilds = []
  , allowedChannels = []
  , allowedUsers = []
  , superusers = []
  , gatewayHost = "gateway.discord.gg"
  , gatewayPath = "/?v=10&encoding=json"
  }
