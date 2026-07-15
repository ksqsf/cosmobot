{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Resource
Description : In-memory lifecycle for chat-owned long-running objects
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
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude hiding (state)
import Bot.Resource.Types
import qualified Data.Dynamic as Dynamic
import qualified Data.Map.Strict as Map
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
  :: (Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es, Prim :> es)
  => Eff (Resource.Resource : es) a
  -> Eff es a
runResourceManager inner = do
  stateRef <- newIORef ManagerState{nextId = 1, nextCreate = 1, accepting = True, creating = Map.empty, resources = Map.empty}
  let runInner = interpret (runResourceOperation stateRef) inner
  runInner `finally` shutdown stateRef

runResourceOperation
  :: (Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es, Prim :> es)
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
  :: forall localEs es a. (ResourceObject (Eff localEs) a, Concurrent :> es, Prim :> es)
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
            in (state{nextCreate = createId + 1, creating = Map.insert createId gate state.creating}, Right createId)
          else (state, Left ResourceUnavailable)
      case reservation of
        Left err -> pure (Left err)
        Right createId -> (mask \restore -> createAndRegister restore owner) `finally` finishCreate createId gate
  where
    createAndRegister restore owner =
      restore (unlift (createResourceObject @(Eff localEs) @a initValue)) >>= \case
        Left err -> pure (Left (ResourceCreationFailed err))
        Right object -> do
          let resource = SomeResource
                { owner
                , sessionId = sessionIdFromMessage initValue.message
                , resourceType = resourceTypeName @(Eff localEs) @a (Proxy @a)
                , value = Dynamic.toDyn object
                , describe = unlift . describeResourceObject object
                , probe = unlift (probeResourceObject object)
                , destroy = unlift (destroyResourceObject object)
                , availability = Available Map.empty
                }
          registered <- atomicModifyIORef' stateRef \state ->
            if state.accepting
              then
                let resourceId = "res-" <> show state.nextId
                in (state{nextId = state.nextId + 1, resources = Map.insert resourceId resource state.resources}, Right resourceId)
              else (state, Left ResourceUnavailable)
          when (isLeft registered) $ void (trySync resource.destroy)
          pure registered

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
  entries <- Map.toAscList . Map.filter (\resource -> resource.owner == access.owner && isAvailable resource.availability) . (.resources) <$> readIORef stateRef
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
  :: (Concurrency.Concurrency :> es, Prim :> es)
  => IORef (ManagerState es)
  -> ResourceAccess
  -> ResourceId
  -> Eff es (Either ResourceError ())
destroyIn stateRef access resourceId =
  beginDestroy stateRef access resourceId >>= \case
    Left err -> pure (Left err)
    Right (users, cleanup) -> do
      traverse_ cancelAndAwait users
      trySync cleanup >>= \case
        Right (Right ()) -> removeDestroyed stateRef resourceId $> Right ()
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
  -> Eff es (Either ResourceError ([Concurrency.Handle], Eff es (Either Text ())))
beginDestroy stateRef access resourceId =
  atomicModifyIORef' stateRef \state ->
    case Map.lookup resourceId state.resources of
      Nothing -> (state, Left ResourceNotFoundOrNotOwned)
      Just resource
        | not (mayDestroy access resource.owner) -> (state, Left ResourceNotFoundOrNotOwned)
        | Available users <- resource.availability ->
            let updated = resource{availability = Destroying}
            in (state{resources = Map.insert resourceId updated state.resources}, Right (Map.keys users, resource.destroy))
        | otherwise -> (state, Left ResourceUnavailable)

mayDestroy :: ResourceAccess -> ResourceOwner -> Bool
mayDestroy access owner =
  access.owner == owner
    || (access.superuser && access.owner.platform == owner.platform && access.owner.chatId == owner.chatId)

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
    void (trySync resource.destroy)
