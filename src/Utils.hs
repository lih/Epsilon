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
  _drop,_at,_h,
  -- * Miscellaneous
  inRange,inter,mkRef,compose,mergeBy,findM,unsafePerformIO
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

-- |Apply the function to the value and join the result
f <!> x = join (f <*> x)
infixl 3 <!>
-- |To insert pure values into an applicative expression
f <%> x = f <*> pure x

-- |An Iso between a singleton list and the element contained
_h = iso head (:[])
-- |A lens for the nth tail of a list
_drop n = compose  [_tail | _ <- [1..n]]
-- |A lens for the nth element of a list
_at n = _drop n._head

-- |Is the value in the range ?
inRange (a,b) x = max a (min b x)
-- |@inter c a b@ interpolates @a@ and @b@ by the coefficient @c@
inter c a b = (c*a)+((1-c)*b)
-- |UNSAFE creates an IORef (used to create global references)
mkRef = unsafePerformIO . newIORef
-- |Used mostly to compose lenses
compose = foldr (.) id

-- |A merge function
mergeBy (<) = merge
  where merge (x:xs) (y:ys) | x<y = x:merge xs (y:ys)
                            | otherwise = y:merge (x:xs) ys
        merge xs ys = xs++ys

-- |A lazy monadic 'find'
findM p = foldr f (return Nothing)
  where f ma t = ma >>= \a -> if p a then return (Just a) else t
