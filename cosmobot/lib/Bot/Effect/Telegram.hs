{-# LANGUAGE TypeFamilies #-}

module Bot.Effect.Telegram
  ( Telegram (..)
  , telegramCall
  )
where

import Bot.Prelude
import qualified Data.Aeson as Aeson

data Telegram :: Effect where
  TelegramCall :: Text -> Aeson.Object -> Telegram m Aeson.Value

type instance DispatchOf Telegram = Dynamic

telegramCall :: Telegram :> es => Text -> Aeson.Object -> Eff es Aeson.Value
telegramCall method = send . TelegramCall method
