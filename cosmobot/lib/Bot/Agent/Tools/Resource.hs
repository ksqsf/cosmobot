{-|
Module      : Bot.Agent.Tools.Resource
Description : Agent tools for managed resource lifecycle
Stability   : experimental
-}
module Bot.Agent.Tools.Resource
  ( destroyResourceTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude

destroyResourceTool :: Resource.Resource :> es => Tool es
destroyResourceTool = Tool
  { name = "destroy_resource"
  , description = "Destroy a resource (e.g. sandbox) owned by the current chat and sender."
  , parameters = objectSchema
      [fieldText "resource" "Resource id to destroy."]
      ["resource"]
  , noisy = False
  , allowed = isRight . Resource.accessFromMessage . (.message)
  , start = \context -> pure \_ args ->
      withTextArg "resource" (destroy context) args
  }
  where
    destroy context resourceId =
      case Resource.accessFromMessage context.message of
        Left err -> pure (resourceToolFailure err)
        Right access -> Resource.destroy access resourceId <&> \case
          Left err -> resourceToolFailure err
          Right () -> toolText "Resource destroyed."
