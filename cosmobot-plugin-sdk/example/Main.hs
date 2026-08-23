{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cosmobot.Plugin

main :: IO ()
main = serve "1.0.0" [Chat] $ do
  command "!echo" "Echo the supplied text." $ \request -> do
    _ <- reply (arguments request)
    pure ""
  pure ()
