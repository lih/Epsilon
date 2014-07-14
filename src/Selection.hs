{-| A module for the selection -}
module Selection(select,selectGoal) where

import Graphics
import Utils

-- |The selection box
select = mkRef (v2 (v3 0 0 0) (v3 1 1 (1::GLfloat)))
-- |The selection box's goal
selectGoal = mkRef (v2 (pure (-1)<&>(/2)) (pure 1 :: Vector3 GLfloat))
