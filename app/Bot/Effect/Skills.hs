{-|
Module      : Bot.Effect.Skills
Description : Application effect for startup-loaded skill metadata
Stability   : experimental
-}

module Bot.Effect.Skills
  ( Skills
  , skillsSystemPrompt
  , runSkills
  )
where

import Bot.Prelude
import qualified Bot.Skills as SkillsStore

data Skills :: Effect where
  SkillsSystemPrompt :: Skills m Text

type instance DispatchOf Skills = Dynamic

skillsSystemPrompt :: Skills :> es => Eff es Text
skillsSystemPrompt =
  send SkillsSystemPrompt

runSkills
  :: IOE :> es
  => SkillsStore.SkillsConfig
  -> Eff (Skills : es) a
  -> Eff es a
runSkills cfg action = do
  prompt <- SkillsStore.loadSkillsPrompt cfg
  interpret (\_ -> \case SkillsSystemPrompt -> pure prompt.systemPrompt) action
