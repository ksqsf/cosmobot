{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Effect.ACP
Description : ACP client capability facade
Stability   : experimental
-}

module Bot.Effect.ACP
  ( ACP (..)
  , readClientFile
  , writeClientFile
  )
where

import Bot.Core.Message
import Bot.Prelude

data ACP :: Effect where
  ReadClientFile :: IncomingMessage -> Text -> Maybe Int -> Maybe Int -> ACP m (Either Text Text)
  WriteClientFile :: IncomingMessage -> Text -> Text -> ACP m (Either Text ())

type instance DispatchOf ACP = Dynamic

readClientFile :: ACP :> es => IncomingMessage -> Text -> Maybe Int -> Maybe Int -> Eff es (Either Text Text)
readClientFile message path line limit =
  send (ReadClientFile message path line limit)

writeClientFile :: ACP :> es => IncomingMessage -> Text -> Text -> Eff es (Either Text ())
writeClientFile message path content =
  send (WriteClientFile message path content)
