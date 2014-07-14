{-# LANGUAGE NoMonomorphismRestriction #-}
{-| A bundle module for the OpenGL libraries -}
module Graphics (
  -- * Modules
  module Graphics.Rendering.OpenGL,
  module Graphics.UI.GLUT,
  module Graphics.Rendering.FTGL,

  -- * Utils
  preservingMatrix,
  
  -- * Vectors
  vx,vy,vz,(*-*),(*+*),v2,v3,v,scalev,

  -- * Colors
  c,green,red,grey,brown                   
  ) where

import Graphics.Rendering.OpenGL hiding (preservingMatrix,withMatrix,RenderMode,Back,Front)
import Graphics.UI.GLUT hiding (preservingMatrix,withMatrix,RenderMode,Font,Back,Front)
import Graphics.Rendering.FTGL 
import Control.Lens
import Control.Applicative

-- |A lens for the first component of a vector
vx = lens (\(Vector3 x _ _) -> x) (\(Vector3 _ y z) x -> Vector3 x y z)
-- |A lens for the second component of a vector
vy = lens (\(Vector3 _ y _) -> y) (\(Vector3 x _ z) y -> Vector3 x y z)
-- |A lens for the third component of a vector
vz = lens (\(Vector3 _ _ z) -> z) (\(Vector3 x y _) z -> Vector3 x y z)
-- |Vector difference
(*-*) = liftA2 (-)
-- |Vector sum
(*+*) = liftA2 (+)

-- |A shortcut for Vector2
v2 = Vector2
-- |A shortcut for Vector3
v3 = Vector3
-- |A type-specialized version of 'vertex'
v x y z = vertex $ Vertex3 (x::GLfloat) y z
-- |Scale by the given vector
scalev (Vector3 x y z) = scale x y z

-- |A type-specialized version of 'color'
c r g b = color $ Color3 (r::GLfloat) g b
red = Color3 1 0 (0::GLfloat)
green = Color3 0 1 (0::GLfloat)
grey = Color3 0.5 0.5 (0.5::GLfloat)
brown = Color3 0.5 0.3 (0.1::GLfloat)

-- |OpenGL's modelview stack is too small so we wrap a bigger one around
withMatrix f = get (matrix (Just (Modelview 0))) >>= f
preservingMatrix ma = withMatrix $ \old ->
  ma <* (matrix (Just (Modelview 0)) $= (old :: GLmatrix GLfloat))
    
