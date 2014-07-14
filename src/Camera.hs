module Camera(
  -- * Primitives
  yaw,yawGoal,pitch,pitchGoal,scl,sclGoal,center,centerGoal,

  -- * Utilities
  zoomIn,zoomOut,
  cameraLeft,cameraRight,cameraUp,cameraDown
  )
       where

import Graphics
import Utils

-- |The horizontal angle
yaw = mkRef (0::GLfloat)
-- |The horizontal angle goal
yawGoal = mkRef (25::GLfloat)
-- |The vertical angle
pitch = mkRef (0::GLfloat)
-- |The vertical angle goal
pitchGoal = mkRef (15::GLfloat)
-- |The scale
scl = mkRef (1::GLfloat)
-- |The scale goal
sclGoal = mkRef (1::GLfloat)
-- |The center of view
center = mkRef (pure 0::Vector3 GLfloat)
-- |The center goal
centerGoal = mkRef (pure 0::Vector3 GLfloat)

zoomIn = sclGoal $~ (*1.05) ; zoomOut = sclGoal $~ (/1.05)

cameraLeft angle = yawGoal $~ subtract angle
cameraRight angle = yawGoal $~ (+angle)
cameraUp angle = pitchGoal $~ (inRange (-90,90).(+angle))
cameraDown angle = pitchGoal $~ (inRange (-90,90).subtract angle)
