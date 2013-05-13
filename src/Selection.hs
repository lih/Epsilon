{-| A module for the selection -}
module Selection(select,selectGoal,initSelection) where

import Graphics
import Utils
import Time

-- |The selection box
select = mkRef (v2 (v3 0 0 0) (v3 1 1 (1::GLfloat)))
-- |The selection box's goal
selectGoal = mkRef (v2 (pure (-1)<&>(/2)) (pure 1 :: Vector3 GLfloat))
-- |Registers the smooth transition between 'select' and 'selectGoal'
initSelection = smoothTr (liftA2 . liftA2 . inter) select selectGoal
