{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Effect.Resource
Description : Long-running resource capability facade
Stability   : experimental
-}
module Bot.Effect.Resource
  ( Resource (..)
  , ResourceId
  , ResourceOwner (..)
  , ResourceAccess (..)
  , Init (..)
  , ResourceObject (..)
  , ResourcePersistence (..)
  , ResourceLoader (..)
  , resourceLoader
  , SomeResourceObject (..)
  , ResourceError (..)
  , ownerFromMessage
  , accessFromMessage
  , create
  , withResource
  , list
  , destroy
  )
where

import Bot.Effect.Concurrency (Handle)
import Bot.Prelude hiding (Handle)
import Bot.Resource.Types

data Resource :: Effect where
  Create :: ResourceObject m a => Proxy a -> Init (CreationArgs a) -> Resource m (Either ResourceError ResourceId)
  With :: ResourceObject m a => ResourceAccess -> ResourceId -> Maybe Handle -> (a -> m b) -> Resource m (Either ResourceError b)
  List :: ResourceAccess -> Resource m [SomeResourceObject]
  Destroy :: ResourceAccess -> ResourceId -> Resource m (Either ResourceError ())

type instance DispatchOf Resource = Dynamic

create :: forall a es. (Resource :> es, ResourceObject (Eff es) a) => Init (CreationArgs a) -> Eff es (Either ResourceError ResourceId)
create = send . Create (Proxy @a)

withResource
  :: forall a es b. (Resource :> es, ResourceObject (Eff es) a)
  => ResourceAccess
  -> ResourceId
  -> Maybe Handle
  -> (a -> Eff es b)
  -> Eff es (Either ResourceError b)
withResource access resourceId user callback =
  send (With access resourceId user callback)

list :: Resource :> es => ResourceAccess -> Eff es [SomeResourceObject]
list = send . List

destroy :: Resource :> es => ResourceAccess -> ResourceId -> Eff es (Either ResourceError ())
destroy access = send . Destroy access
