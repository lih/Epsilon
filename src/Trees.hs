{-# LANGUAGE NoMonomorphismRestriction, Rank2Types, DeriveFunctor #-}
module Trees (module Data.Tree, module Data.Tree.Lens, Syntax(..), _Group, _Symbol, TreeIx, Treelike(..)
             , _nodeAt, _forestAt,_group
             ,($#),g,s) where

import Prelude hiding (foldr)
import Data.Foldable
import Data.Tree 
import Data.Tree.Lens
import Utils

data Syntax a = Group [Syntax a]
              | Symbol a
              deriving (Show,Read,Functor)
instance Foldable Syntax where
  foldr f e (Symbol a) = f a e
  foldr f e (Group g) = foldr (flip $ foldr f) e g
instance Traversable Syntax where
  traverse f (Symbol a) = Symbol <$> f a
  traverse f (Group g) = Group <$> traverse (traverse f) g
instance Applicative Syntax where
  pure = Symbol
  Symbol f <*> t = fmap f t
  Group g <*> t = Group $ map (<*>t) g
_Group = prism' Group (\x -> case x of Group s -> Just s ; _ -> Nothing)
_Symbol = prism' Symbol (\x -> case x of Symbol s -> Just s ; _ -> Nothing)
_group = iso Group (\(Group l) -> l)

type TreeIx = [Int]
class Treelike t where
  _subs :: Traversal' (t a) [t a]
instance Treelike Tree where
  _subs = branches
instance Treelike Syntax where
  _subs = _Group

f $# x = Group $ f:x
s = Symbol
g = Group

_nodeAt is = compose [_subs._drop i._head | i <- is]
_forestAt is = compose [_head._subs._drop i | i <- is]
