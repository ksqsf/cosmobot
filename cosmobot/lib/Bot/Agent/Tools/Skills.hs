{-|
Module      : Bot.Agent.Tools.Skills
Description : Agent skill tools
Stability   : experimental
-}

module Bot.Agent.Tools.Skills
  ( loadSkillTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import qualified Bot.Effect.Skills as Skills
import Bot.Prelude

loadSkillTool :: Skills.Skills :> es => Tool es
loadSkillTool = Tool
  { name = "load_skill"
  , description = "Load the full instructions for an available skill by name."
  , parameters = objectSchema
      [ fieldText "name" "Skill name advertised in the system prompt."
      ]
      ["name"]
  , noisy = False
  , allowed = everyone
  , start = \_ -> pure \_ args -> withTextArg "name" (\name ->
      toolText . fromMaybe "Skill not found." <$> Skills.loadSkill name
      ) args
  }
