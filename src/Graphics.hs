{-# LANGUAGE NoMonomorphismRestriction #-}
module Graphics (module GL,module GLUT,module FTGL
                ,v2,v3,v,c,scalev,green,red,grey,vx,vy,vz
                ,(*-*),(*+*)) where

import Graphics.Rendering.OpenGL as GL hiding (RenderMode,Back,Front)
import Graphics.UI.GLUT as GLUT hiding (RenderMode,Font,Back,Front)
import Graphics.Rendering.FTGL as FTGL 
import Control.Lens
import Control.Applicative

vx = lens (\(Vector3 x _ _) -> x) (\(Vector3 _ y z) x -> Vector3 x y z)
vy = lens (\(Vector3 _ y _) -> y) (\(Vector3 x _ z) y -> Vector3 x y z)
vz = lens (\(Vector3 _ _ z) -> z) (\(Vector3 x y _) z -> Vector3 x y z)
(*-*) = liftA2 (-)
(*+*) = liftA2 (+)

v2 = Vector2
v3 = Vector3
v x y z = vertex $ Vertex3 (x::GLfloat) y z
c r g b = color $ Color3 (r::GLfloat) g b
scalev (Vector3 x y z) = scale x y z

red = Color3 1 0 (0::GLfloat)
green = Color3 0 1 (0::GLfloat)
grey = Color3 0.5 0.5 (0.5::GLfloat)


