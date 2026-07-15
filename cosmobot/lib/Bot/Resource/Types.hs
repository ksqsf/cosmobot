{-# LANGUAGE AllowAmbiguousTypes #-}

module Bot.Resource.Types
  ( ResourceId
  , ResourceOwner (..)
  , ResourceAccess (..)
  , Init (..)
  , ResourceObject (..)
  , SomeResourceObject (..)
  , ResourceError (..)
  , ownerFromMessage
  , accessFromMessage
  )
where

import Bot.Core.Message
import Bot.Prelude

type ResourceId = Text

data ResourceOwner = ResourceOwner
  { platform :: !ChatPlatform
  , chatId :: !Integer
  , senderId :: !Text
  }
  deriving stock (Eq, Ord, Show)

data ResourceAccess = ResourceAccess
  { owner :: !ResourceOwner
  , superuser :: !Bool
  }
  deriving stock (Eq, Show)

data Init a = Init
  { message :: !IncomingMessage
  , agentId :: !Text
  , arguments :: !a
  }

class Typeable a => ResourceObject m a where
  type CreationArgs a
  resourceTypeName :: proxy a -> Text
  createResourceObject :: Init (CreationArgs a) -> m (Either Text a)
  destroyResourceObject :: a -> m (Either Text ())
  describeResourceObject :: a -> Either Text Text -> m Text
  probeResourceObject :: a -> m (Either Text Text)

data SomeResourceObject = SomeResourceObject
  { resourceId :: !ResourceId
  , resourceType :: !Text
  , agentId :: !Text
  , description :: !Text
  , probeResult :: !(Either Text Text)
  }
  deriving stock (Eq, Show)

data ResourceError
  = MissingResourceIdentity
  | ResourceNotFoundOrNotOwned
  | ResourceTypeMismatch
  | ResourceUnavailable
  | ResourceCreationFailed !Text
  | ResourceCleanupFailed !Text
  deriving stock (Eq, Show)

ownerFromMessage :: IncomingMessage -> Either ResourceError ResourceOwner
ownerFromMessage message =
  ResourceOwner message.platform <$> maybeToRight MissingResourceIdentity message.chatId <*> maybeToRight MissingResourceIdentity message.senderId

accessFromMessage :: IncomingMessage -> Either ResourceError ResourceAccess
accessFromMessage message =
  ResourceAccess <$> ownerFromMessage message <*> pure message.digest.senderIsSuperuser
