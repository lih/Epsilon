{-# LANGUAGE NoMonomorphismRestriction #-}
module Utils (module Safe,module IORef, module L, module Ap, module Mon, module Tr,module At
             ,(<!>),(<%>),inRange,mkRef,inter,compose,_drop,_at,_h,negated) where

import Safe hiding (at)
import Control.Lens as L hiding (para,at,_at)
import Control.Lens.At as At hiding (_at)
import Control.Applicative as Ap
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad as Mon hiding (mapM,sequence,forM)
import Data.Traversable as Tr
import Data.IORef as IORef

f <!> x = join (f <*> x)
infixl 3 <!>
f <%> x = f <*> pure x
inRange (a,b) x = max a (min b x)
mkRef = unsafePerformIO . newIORef
inter c a b = (c*a)+((1-c)*b)
compose = foldr (.) id

_h = iso head (:[])
_drop n = compose  [_tail | _ <- [1..n]]
_at n = _drop n._head
negated x = iso negate negate
