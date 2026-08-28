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
import Bot.Agent.Tool
import Bot.Agent.Types
import qualified Bot.Effect.Skills as Skills
import Bot.Prelude
import qualified Data.Text as Text

loadSkillTool :: Skills.Skills :> es => Tool (Eff es)
loadSkillTool =
  withDescription "Load an available skill's instructions or a UTF-8 text file from its directory into context."
  $ tool "load_skill"
      ( requiredText "name" "Skill name advertised in the system prompt."
      , optionalText "path" "Optional file path relative to the skill directory."
      )
      \name -> \case
        Nothing ->
          toolText . fromMaybe "Skill not found." <$> Skills.loadSkill name
        Just path ->
          Skills.loadSkillFile name (Text.unpack path) >>= \case
            Nothing ->
              pure (toolText "Skill file not found, not UTF-8 text, or path is outside the skill directory.")
            Just content ->
              pure (toolText content)
