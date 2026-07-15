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
  , ResourceScope (..)
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
  , createNamed
  , createAssociated
  , createAssociatedNamed
  , withResource
  , list
  , detail
  , destroy
  , rename
  , destroyAssociated
  )
where

import Bot.Effect.Concurrency (Handle)
import Bot.Prelude hiding (Handle)
import Bot.Resource.Types

data Resource :: Effect where
  Create :: ResourceObject m a => Proxy a -> Maybe Handle -> Maybe ResourceId -> Init (CreationArgs a) -> Resource m (Either ResourceError ResourceId)
  With :: ResourceObject m a => ResourceAccess -> ResourceId -> Maybe Handle -> (a -> m b) -> Resource m (Either ResourceError b)
  List :: ResourceAccess -> Resource m [SomeResourceObject]
  Detail :: ResourceAccess -> ResourceId -> Resource m (Either ResourceError Text)
  Destroy :: ResourceAccess -> ResourceId -> Resource m (Either ResourceError ())
  Rename :: ResourceAccess -> ResourceId -> ResourceId -> Resource m (Either ResourceError ResourceId)
  DestroyAssociated :: Handle -> Resource m [Either ResourceError ()]

type instance DispatchOf Resource = Dynamic

create :: forall a es. (Resource :> es, ResourceObject (Eff es) a) => Init (CreationArgs a) -> Eff es (Either ResourceError ResourceId)
create = createAssociated @a Nothing

createNamed :: forall a es. (Resource :> es, ResourceObject (Eff es) a) => ResourceId -> Init (CreationArgs a) -> Eff es (Either ResourceError ResourceId)
createNamed = createAssociatedNamed @a Nothing

createAssociated :: forall a es. (Resource :> es, ResourceObject (Eff es) a) => Maybe Handle -> Init (CreationArgs a) -> Eff es (Either ResourceError ResourceId)
createAssociated parent = send . Create (Proxy @a) parent Nothing

createAssociatedNamed :: forall a es. (Resource :> es, ResourceObject (Eff es) a) => Maybe Handle -> ResourceId -> Init (CreationArgs a) -> Eff es (Either ResourceError ResourceId)
createAssociatedNamed parent resourceId = send . Create (Proxy @a) parent (Just resourceId)

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

detail :: Resource :> es => ResourceAccess -> ResourceId -> Eff es (Either ResourceError Text)
detail access = send . Detail access

destroy :: Resource :> es => ResourceAccess -> ResourceId -> Eff es (Either ResourceError ())
destroy access = send . Destroy access

rename :: Resource :> es => ResourceAccess -> ResourceId -> ResourceId -> Eff es (Either ResourceError ResourceId)
rename access resourceId = send . Rename access resourceId

destroyAssociated :: Resource :> es => Handle -> Eff es [Either ResourceError ()]
destroyAssociated = send . DestroyAssociated
