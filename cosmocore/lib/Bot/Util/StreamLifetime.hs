module Bot.Util.StreamLifetime (bracketStream, streamFinally) where

import Bot.Prelude
import qualified Effectful.Resource as Resource
import qualified Streaming.Prelude as S

bracketStream
  :: Resource.Resource :> es
  => Eff es resource
  -> (resource -> Eff es ())
  -> (resource -> Stream (Of item) (Eff es) result)
  -> Stream (Of item) (Eff es) result
bracketStream acquire release use = do
  (releaseKey, resource) <- lift (Resource.allocateEff acquire release)
  use resource `streamFinally` Resource.releaseEff releaseKey

streamFinally
  :: Stream (Of a) (Eff es) r
  -> Eff es ()
  -> Stream (Of a) (Eff es) r
streamFinally stream cleanup =
  go stream
  where
    go current = do
      next <- lift (S.next current `onException` cleanup)
      case next of
        Left result -> do
          lift cleanup
          pure result
        Right (item, rest) -> do
          S.yield item
          go rest
