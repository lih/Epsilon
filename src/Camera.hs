module Camera(
  -- * Primitives
  angleX,aXGoal,angleY,aYGoal,scl,sclGoal,center,centerGoal,

  -- * Utilities
  initCamera,
  zoomIn,zoomOut,
  cameraLeft,cameraRight,cameraUp,cameraDown
  )
       where

import Graphics
import Utils
import Time
import Input

-- |The horizontal angle
angleX = mkRef (0::GLfloat)
-- |The horizontal angle goal
aXGoal = mkRef (25::GLfloat)
-- |The vertical angle
angleY = mkRef (0::GLfloat)
-- |The vertical angle goal
aYGoal = mkRef (15::GLfloat)
-- |The scale
scl = mkRef (1::GLfloat)
-- |The scale goal
sclGoal = mkRef (1::GLfloat)
-- |The center of view
center = mkRef (pure 0::Vector3 GLfloat)
-- |The center goal
centerGoal = mkRef (pure 0::Vector3 GLfloat)

angleDel = 7

initCamera = do
  smoothTr inter scl sclGoal
  smoothTr inter angleX aXGoal
  smoothTr inter angleY aYGoal
  smoothTr (liftA2 . inter) center centerGoal
  mouseHooks <+< mouse
  dragHooks <+< drag
  where mouse WheelUp _ _ = zoomIn ; mouse WheelDown _ _ = zoomOut
        mouse _ _ _ = return ()
        drag (Vector2 dx dy) = left (fromIntegral dx/3) >> up (fromIntegral dy/3)

zoomIn = sclGoal $~ (*1.25) ; zoomOut = sclGoal $~ (*0.80)
cameraLeft = left angleDel
cameraRight = right angleDel
cameraUp = up angleDel
cameraDown = down angleDel

left angle = aXGoal $~ subtract angle
right angle = aXGoal $~ (+angle)
up angle = aYGoal $~ (inRange (-90,90).(+angle))
down angle = aYGoal $~ (inRange (-90,90).subtract angle)
