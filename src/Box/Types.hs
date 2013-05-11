{-# LANGUAGE TemplateHaskell, DeriveFunctor, RankNTypes #-}
module Box.Types where

import Graphics
import Utils
import Data.Tree

data Box a = Box { _bPos :: Vector3 a, _bCenter :: Vector3 a, _bSize :: Vector3 a }
           deriving (Functor,Show)
$( makeLenses ''Box )
type Sz = Vector3 GLfloat
type Pos = Vector3 GLfloat
data DrawBox = DrawBox {
  _box :: Box GLfloat,
  _drawB :: IO ()
  }
$( makeLenses ''DrawBox )

class Drawable d where
  nullDraw :: d
  appendBy :: (forall a. Lens' (Vector3 a) a) -> [d] -> d
  _draw :: Lens' d  (IO ())
  _size :: Lens' d (Vector3 GLfloat)
  _pos :: Lens' d (Vector3 GLfloat)
  _center :: Lens' d (Vector3 GLfloat)
class Scalable d where
  scaled :: Vector3 GLfloat -> d -> d

data DrawTree a = DrawTree { _droot :: a, _dsubs :: [(Int,Tree a)] }
$( makeLenses ''DrawTree )
toTree (DrawTree r s) = Node r (map snd s)
