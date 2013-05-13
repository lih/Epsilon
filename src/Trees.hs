{-# LANGUAGE NoMonomorphismRestriction, Rank2Types, DeriveFunctor #-}
{-| A module for tree-structure handling -}
module Trees (
  -- * Modules
  module Data.Tree, module Data.Tree.Lens,
  -- * Types
  Syntax(..), _Group, _Symbol, Treelike(..),

  -- * Accessors
  _nodeAt, _forestAt,_group) where

import Prelude hiding (foldr)
import Data.Foldable
import Data.Tree 
import Data.Tree.Lens
import Utils

-- |The syntax tree data type
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
-- |A prism for the Group case
_Group = prism' Group (\x -> case x of Group s -> Just s ; _ -> Nothing)
-- |A prism for the Symbol case
_Symbol = prism' Symbol (\x -> case x of Symbol s -> Just s ; _ -> Nothing)
-- |An iso between a Group and a list
_group = iso Group (\(Group l) -> l)

-- |A tree-like structure allows traveling down
class Treelike t where
  _subs :: Traversal' (t a) [t a]
instance Treelike Tree where
  _subs = branches
instance Treelike Syntax where
  _subs = _Group

-- |A lens for the node at a given index
_nodeAt is = compose [_subs._drop i._head | i <- is]
-- |A lens for the forest at a given index
_forestAt is = compose [_head._subs._drop i | i <- is]
