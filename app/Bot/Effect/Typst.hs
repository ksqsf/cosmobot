{-|
Module      : Bot.Effect.Typst
Description : Typst rendering capability facade
Stability   : experimental
-}
module Bot.Effect.Typst
  ( Typst(..)
  , withTypstPng
  )
where

import Bot.Prelude

-- | Render Typst source into a PNG available for the duration of a continuation.
data Typst :: Effect where
  WithTypstPng
    :: Text
    -> (FilePath -> m a)
    -> Typst m a

type instance DispatchOf Typst = Dynamic

withTypstPng :: Typst :> es => Text -> (FilePath -> Eff es a) -> Eff es a
withTypstPng source action =
  send (WithTypstPng source action)
