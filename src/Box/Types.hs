{-# LANGUAGE TemplateHaskell, DeriveFunctor, RankNTypes #-}
module Box.Types where

import Graphics
import Utils
import Data.Tree

-- |A 3-dimensional box with a center of mass
data Box a = Box { _bPos :: Vector3 a, _bCenter :: Vector3 a, _bSize :: Vector3 a }
           deriving (Functor,Show)
$( makeLenses ''Box )

-- |A size
type Sz = Vector3 GLfloat
-- |A position
type Pos = Vector3 GLfloat
-- |A drawable, box-bounded object
data DrawBox = DrawBox {
  _box :: Box GLfloat,
  _drawB :: IO ()
  }
$( makeLenses ''DrawBox )

-- |The class of all objects that have box-like properties
class Boxed d where
  _size :: Lens' d (Vector3 GLfloat)
  _pos :: Lens' d (Vector3 GLfloat)
  _center :: Lens' d (Vector3 GLfloat)
  appendBy :: (forall a. Lens' (Vector3 a) a) -> [d] -> d
-- |The class of all objects that may be drawn
class Drawable d where
  nullDraw :: d
  _draw :: Lens' d  (IO ())
-- |The class of all scalable objects
class Scalable d where
  scaled :: Vector3 GLfloat -> d -> d

-- |A tree of drawable elements
data DrawTree a = DrawTree { _droot :: a, _dsubs :: [(Int,Tree a)] }
$( makeLenses ''DrawTree )
-- |Transforms a DrawTree into a Tree
toTree (DrawTree r s) = Node r (map snd s)
