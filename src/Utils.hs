{-# LANGUAGE NoMonomorphismRestriction #-}
{-| Utility module exporting the most common modules and a few utility functions. -}
module Utils (
  -- * Exported modules
  module Safe,module Data.IORef,
  module Control.Lens, module Control.Lens.At,
  module Control.Applicative,module Control.Monad,
  module Data.Traversable,module Data.List,module Data.Maybe,module Data.Function,
  -- * Applicative/Monad utilities
  (<!>),(<%>),
  -- * Utility lenses
  _drop,_at,_h,negated,
  -- * Miscellaneous
  inRange,inter,mkRef,compose,mergeBy
  ) where

import Safe hiding (at)
import Control.Lens hiding (para,at,_at)
import Control.Lens.At hiding (_at)
import Control.Applicative
import Control.Monad hiding (mapM,sequence,forM)
import Data.Traversable
import Data.IORef
import Data.List (intercalate,intersperse,groupBy,tails)
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import Data.Function (on)

f <!> x = join (f <*> x)
infixl 3 <!>
f <%> x = f <*> pure x

_h = iso head (:[])
_drop n = compose  [_tail | _ <- [1..n]]
_at n = _drop n._head
negated = iso negate negate

inRange (a,b) x = max a (min b x)
inter c a b = (c*a)+((1-c)*b)
mkRef = unsafePerformIO . newIORef
compose = foldr (.) id

mergeBy (<) = merge
  where merge (x:xs) (y:ys) | x<y = x:merge xs (y:ys)
                            | otherwise = y:merge (x:xs) ys
        merge xs ys = xs++ys
