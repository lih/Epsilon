module Camera  where

import Graphics
import Utils
import Time
import Input

angleX = mkRef (0::GLfloat) ; aXGoal = mkRef (30::GLfloat)
angleY = mkRef (0::GLfloat) ; aYGoal = mkRef (30::GLfloat)
scl = mkRef (1::GLfloat) ; sclGoal = mkRef (1::GLfloat)
center = mkRef (pure 0::Vector3 GLfloat); centerGoal = mkRef (pure 0::Vector3 GLfloat)
angleDel = 7

zoomIn = sclGoal $~ (*1.25) ; zoomOut = sclGoal $~ (*0.80)
left angle = aXGoal $~ subtract angle
right angle = aXGoal $~ (+angle)
up angle = aYGoal $~ (inRange (-90,90).(+angle))
down angle = aYGoal $~ (inRange (-90,90).subtract angle)
cameraLeft = left angleDel
cameraRight = right angleDel
cameraUp = up angleDel
cameraDown = down angleDel

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


