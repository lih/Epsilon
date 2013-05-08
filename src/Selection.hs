module Selection where

import Graphics
import Utils
import Time

select = mkRef (v2 (v3 0 0 0) (v3 1 1 (1::GLfloat)))
selectGoal = mkRef (v2 (pure (-1)<&>(/2)) (pure 1 :: Vector3 GLfloat))
initSelection = smoothTr (liftA2 . liftA2 . inter) select selectGoal
