{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Resource
Description : Lifecycle and durable registrations for chat-owned long-running objects
Stability   : experimental
-}
module Bot.Resource
  ( ResourceId
  , ResourceOwner (..)
  , ResourceAccess (..)
  , Init (..)
  , ResourceObject (..)
  , SomeResourceObject (..)
  , ResourceError (..)
  , ownerFromMessage
  , accessFromMessage
  , runResourceManager
  , runResourceManagerWith
  , resourceLoader
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Resource as Resource
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude hiding (state)
import Bot.Resource.Types
import qualified Bot.Storage.Resource as ResourceStorage
import qualified Data.Dynamic as Dynamic
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Effectful.Concurrent.MVar as MVar

data Availability
  = Available !(Map Concurrency.Handle Int)
  | Destroying

data SomeResource es = SomeResource
  { owner :: !ResourceOwner
  , sessionId :: !(Maybe Text)
  , resourceType :: !Text
  , value :: !Dynamic.Dynamic
  , describe :: Either Text Text -> Eff es Text
  , probe :: Eff es (Either Text Text)
  , destroy :: Eff es (Either Text ())
  , persistent :: !Bool
  , availability :: !Availability
  }

data ManagerState es = ManagerState
  { nextId :: !Integer
  , nextCreate :: !Integer
  , accepting :: !Bool
  , creating :: !(Map Integer (MVar.MVar ()))
  , resources :: !(Map ResourceId (SomeResource es))
  }

runResourceManager
  :: (Storage.Storage :> es, Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es, Prim :> es)
  => Eff (Resource.Resource : es) a
  -> Eff es a
runResourceManager = runResourceManagerWith []

runResourceManagerWith
  :: (Storage.Storage :> es, Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es, Prim :> es)
  => [ResourceLoader (Eff es)]
  -> Eff (Resource.Resource : es) a
  -> Eff es a
runResourceManagerWith loaders inner = do
  stored <- ResourceStorage.loadResources
  loaded <- traverse (restoreStoredResource loaders) stored
  let resources = Map.fromList [(resource.resourceId, value) | (resource, value) <- zip stored loaded]
      nextId = List.maximum (1 : map ((+ 1) . resourceIdNumber . (.resourceId)) stored)
  stateRef <- newIORef ManagerState{nextId, nextCreate = 1, accepting = True, creating = Map.empty, resources}
  let runInner = interpret (runResourceOperation stateRef) inner
  runInner `finally` shutdown stateRef

restoreStoredResource
  :: forall es. (Concurrent :> es, IOE :> es)
  => [ResourceLoader (Eff es)]
  -> ResourceStorage.StoredResource
  -> Eff es (SomeResource es)
restoreStoredResource loaders stored =
  case List.find matches loaders of
    Nothing -> pure (unavailableResource "No loader is registered for this resource type.")
    Just loader -> restoreWith loader
  where
    matches (ResourceLoader (proxy :: Proxy a)) =
      resourceTypeName @(Eff es) @a proxy == stored.resourceType

    restoreWith (ResourceLoader (proxy :: Proxy a)) =
      case resourcePersistence @(Eff es) @a proxy of
        EphemeralResource ->
          pure (unavailableResource "The registered resource type is not persistent.")
        PersistentResource{restoreResource} ->
          trySync (restoreResource stored.payload) <&> \case
            Right (Right object) -> restoredResource proxy object
            Right (Left err) -> unavailableResource err
            Left err -> unavailableResource (conciseException err)

    restoredResource
      :: forall a. ResourceObject (Eff es) a
      => Proxy a
      -> a
      -> SomeResource es
    restoredResource proxy object = SomeResource
      { owner = stored.owner
      , sessionId = stored.sessionId
      , resourceType = resourceTypeName @(Eff es) @a proxy
      , value = Dynamic.toDyn object
      , describe = describeResourceObject object
      , probe = probeResourceObject object
      , destroy = destroyResourceObject object
      , persistent = True
      , availability = Available Map.empty
      }

    unavailableResource err = SomeResource
      { owner = stored.owner
      , sessionId = stored.sessionId
      , resourceType = stored.resourceType
      , value = Dynamic.toDyn ()
      , describe = const (pure "unavailable")
      , probe = pure (Left err)
      , destroy = pure (Right ())
      , persistent = True
      , availability = Available Map.empty
      }

resourceIdNumber :: ResourceId -> Integer
resourceIdNumber resourceId =
  fromMaybe 0 (Text.stripPrefix "res-" resourceId >>= readMaybe . Text.unpack)

conciseException :: Show e => e -> Text
conciseException = Text.take 500 . show

runResourceOperation
  :: (Storage.Storage :> es, Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es, Prim :> es)
  => IORef (ManagerState es)
  -> EffectHandler Resource.Resource es
runResourceOperation stateRef localEnv operation =
  localUnlift localEnv (ConcUnlift Persistent Unlimited) \unlift ->
    case operation of
      Resource.Create proxy initValue -> createIn stateRef unlift proxy initValue
      Resource.With access resourceId user callback -> withIn stateRef unlift access resourceId user callback
      Resource.List access -> listIn stateRef access
      Resource.Destroy access resourceId -> destroyIn stateRef access resourceId

createIn
  :: forall localEs es a. (ResourceObject (Eff localEs) a, Storage.Storage :> es, Concurrent :> es, Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> (forall x. Eff localEs x -> Eff es x)
  -> Proxy a
  -> Init (CreationArgs a)
  -> Eff es (Either ResourceError ResourceId)
createIn stateRef unlift _ initValue =
  case ownerFromMessage initValue.message of
    Left err -> pure (Left err)
    Right owner -> do
      gate <- MVar.newEmptyMVar
      reservation <- atomicModifyIORef' stateRef \state ->
        if state.accepting
          then
            let createId = state.nextCreate
                resourceId = "res-" <> show state.nextId
            in (state
                  { nextId = state.nextId + 1
                  , nextCreate = createId + 1
                  , creating = Map.insert createId gate state.creating
                  }
                , Right (createId, resourceId))
          else (state, Left ResourceUnavailable)
      case reservation of
        Left err -> pure (Left err)
        Right (createId, resourceId) ->
          (mask \restore -> createAndRegister restore owner resourceId) `finally` finishCreate createId gate
  where
    createAndRegister restore owner resourceId =
      restore (unlift (createResourceObject @(Eff localEs) @a initValue)) >>= \case
        Left err -> pure (Left (ResourceCreationFailed err))
        Right object -> do
          let persistence = resourcePersistence @(Eff localEs) @a (Proxy @a)
              isPersistent = case persistence of
                EphemeralResource -> False
                PersistentResource{} -> True
              resource = SomeResource
                { owner
                , sessionId = sessionIdFromMessage initValue.message
                , resourceType = resourceTypeName @(Eff localEs) @a (Proxy @a)
                , value = Dynamic.toDyn object
                , describe = unlift . describeResourceObject object
                , probe = unlift (probeResourceObject object)
                , destroy = unlift (destroyResourceObject object)
                , persistent = isPersistent
                , availability = Available Map.empty
                }
              discard = do
                when isPersistent $ void (trySync (ResourceStorage.deleteResource resourceId))
                void (trySync resource.destroy)
          persisted <- case persistence of
            EphemeralResource -> pure (Right ())
            PersistentResource{encodeResource} ->
              trySync
                (ResourceStorage.saveResource ResourceStorage.StoredResource
                  { resourceId
                  , resourceType = resource.resourceType
                  , owner
                  , sessionId = resource.sessionId
                  , payload = encodeResource object
                  } `onException` discard)
          case persisted of
            Left err -> discard $> Left (ResourceCreationFailed ("Failed to persist resource: " <> conciseException err))
            Right () -> do
              atomicModifyIORef' stateRef \state ->
                (state{resources = Map.insert resourceId resource state.resources}, ())
              pure (Right resourceId)

    finishCreate createId gate = do
      atomicModifyIORef' stateRef \state -> (state{creating = Map.delete createId state.creating}, ())
      void (MVar.tryPutMVar gate ())

withIn
  :: forall localEs es a b. (ResourceObject (Eff localEs) a, Concurrency.Concurrency :> es, Prim :> es)
  => IORef (ManagerState es)
  -> (forall x. Eff localEs x -> Eff es x)
  -> ResourceAccess
  -> ResourceId
  -> Maybe Concurrency.Handle
  -> (a -> Eff localEs b)
  -> Eff es (Either ResourceError b)
withIn stateRef unlift access resourceId user callback =
  acquire stateRef access resourceId user >>= \case
    Left err -> pure (Left err)
    Right dynamicValue ->
      case Dynamic.fromDynamic dynamicValue of
        Nothing -> releaseUser stateRef resourceId user $> Left ResourceTypeMismatch
        Just object -> Right <$> unlift (callback object) `finally` releaseUser stateRef resourceId user

listIn
  :: Prim :> es
  => IORef (ManagerState es)
  -> ResourceAccess
  -> Eff es [SomeResourceObject]
listIn stateRef access = do
  entries <- Map.toAscList . Map.filter (\resource -> mayList access resource.owner && isAvailable resource.availability) . (.resources) <$> readIORef stateRef
  traverse snapshot entries
  where
    snapshot (resourceId, resource) = do
      probeResult <- resource.probe
      description <- resource.describe probeResult
      pure $ SomeResourceObject resourceId resource.resourceType resource.sessionId description probeResult

    isAvailable = \case
      Available{} -> True
      Destroying -> False

sessionIdFromMessage :: IncomingMessage -> Maybe Text
sessionIdFromMessage message
  | message.platform `elem` [PlatformACP, PlatformRPC] = listToMaybe message.chatAliases
  | otherwise = Nothing

destroyIn
  :: (Storage.Storage :> es, Concurrency.Concurrency :> es, Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> ResourceAccess
  -> ResourceId
  -> Eff es (Either ResourceError ())
destroyIn stateRef access resourceId =
  beginDestroy stateRef access resourceId >>= \case
    Left err -> pure (Left err)
    Right (users, cleanup, persistent) -> do
      traverse_ cancelAndAwait users
      trySync cleanup >>= \case
        Right (Right ()) ->
          if persistent
            then trySync (ResourceStorage.deleteResource resourceId) >>= \case
              Right () -> removeDestroyed stateRef resourceId $> Right ()
              Left err -> restoreDestroyed stateRef resourceId $> Left (ResourceCleanupFailed (conciseException err))
            else removeDestroyed stateRef resourceId $> Right ()
        Right (Left err) -> restoreDestroyed stateRef resourceId $> Left (ResourceCleanupFailed err)
        Left err -> restoreDestroyed stateRef resourceId $> Left (ResourceCleanupFailed (show err))

acquire
  :: Prim :> es
  => IORef (ManagerState es)
  -> ResourceAccess
  -> ResourceId
  -> Maybe Concurrency.Handle
  -> Eff es (Either ResourceError Dynamic.Dynamic)
acquire stateRef access resourceId user =
  atomicModifyIORef' stateRef \state ->
    case Map.lookup resourceId state.resources of
      Nothing -> (state, Left ResourceNotFoundOrNotOwned)
      Just resource
        | resource.owner /= access.owner -> (state, Left ResourceNotFoundOrNotOwned)
        | Available users <- resource.availability ->
            let updated = resource{availability = Available (maybe users (\userHandle -> Map.insertWith (+) userHandle 1 users) user)}
            in (state{resources = Map.insert resourceId updated state.resources}, Right resource.value)
        | otherwise -> (state, Left ResourceUnavailable)

beginDestroy
  :: Prim :> es
  => IORef (ManagerState es)
  -> ResourceAccess
  -> ResourceId
  -> Eff es (Either ResourceError ([Concurrency.Handle], Eff es (Either Text ()), Bool))
beginDestroy stateRef access resourceId =
  atomicModifyIORef' stateRef \state ->
    case Map.lookup resourceId state.resources of
      Nothing -> (state, Left ResourceNotFoundOrNotOwned)
      Just resource
        | not (mayDestroy access resource.owner) -> (state, Left ResourceNotFoundOrNotOwned)
        | Available users <- resource.availability ->
            let updated = resource{availability = Destroying}
            in (state{resources = Map.insert resourceId updated state.resources}, Right (Map.keys users, resource.destroy, resource.persistent))
        | otherwise -> (state, Left ResourceUnavailable)

mayDestroy :: ResourceAccess -> ResourceOwner -> Bool
mayDestroy access owner =
  access.owner == owner
    || access.superuser

mayList :: ResourceAccess -> ResourceOwner -> Bool
mayList access owner = access.superuser || access.owner == owner

releaseUser :: Prim :> es => IORef (ManagerState es) -> ResourceId -> Maybe Concurrency.Handle -> Eff es ()
releaseUser _ _ Nothing = pure ()
releaseUser stateRef resourceId (Just user) =
  atomicModifyIORef' stateRef \state ->
    let clear resource = case resource.availability of
          Available users -> resource{availability = Available (Map.update decrement user users)}
          Destroying -> resource
    in (state{resources = Map.adjust clear resourceId state.resources}, ())
  where
    decrement count = if count <= 1 then Nothing else Just (count - 1)

cancelAndAwait :: Concurrency.Concurrency :> es => Concurrency.Handle -> Eff es ()
cancelAndAwait userHandle = do
  void (Concurrency.cancel userHandle.handleId)
  Concurrency.await userHandle

removeDestroyed :: Prim :> es => IORef (ManagerState es) -> ResourceId -> Eff es ()
removeDestroyed stateRef resourceId =
  atomicModifyIORef' stateRef \state -> (state{resources = Map.delete resourceId state.resources}, ())

restoreDestroyed :: Prim :> es => IORef (ManagerState es) -> ResourceId -> Eff es ()
restoreDestroyed stateRef resourceId =
  atomicModifyIORef' stateRef \state ->
    let restore resource = resource{availability = Available Map.empty}
    in (state{resources = Map.adjust restore resourceId state.resources}, ())

shutdown :: (Concurrency.Concurrency :> es, Concurrent :> es, Prim :> es) => IORef (ManagerState es) -> Eff es ()
shutdown stateRef = do
  createGates <- atomicModifyIORef' stateRef \state ->
    (state{accepting = False}, Map.elems state.creating)
  traverse_ MVar.takeMVar createGates
  entries <- atomicModifyIORef' stateRef \state ->
    let mark resource = resource{availability = Destroying}
        entries = Map.elems state.resources
    in (state{resources = Map.map mark state.resources}, entries)
  for_ entries \resource -> do
    case resource.availability of
      Available users -> traverse_ cancelAndAwait (Map.keys users)
      Destroying -> pure ()
    unless resource.persistent $ void (trySync resource.destroy)
