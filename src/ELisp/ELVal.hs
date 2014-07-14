{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-| The Epsilon Lisp datatype -}
module ELisp.ELVal(
  ELVal(..),ELValLike(..),
  intern,mkSym,newSym,
  ELSym(..),One(..),Sym_(..),Lam_(..)
  ) where

import Graphics (($=),get,GLfloat)
import qualified Graphics as G
import Trees
import qualified Data.Map as M
import ELisp.Joinable
import Utils
import Input
import Box.Types

-- |The Epsilon Lisp datatype
data ELVal = List [ELVal]
           | Sym Int (Maybe String)
           | Char Char
           | Int Int
           | GLfloat GLfloat
           | DT (DrawTree DrawBox)
           | Table (M.Map Int ELVal)
           | Special ELVal (M.Map Int ELVal -> [ELVal] -> Joinable ELVal)
           | Lambda ELVal ([ELVal] -> Joinable ELVal)
instance Show ELVal where
  show (List l) = concat["[",intercalate " " (map show l),"]"]
  show (Sym n s) = fromMaybe ("#"++show n) s
  show (Char c) = show c
  show (Int n) = show n
  show (GLfloat f) = show f
  show (DT _) = "#<drawtree>"
  show (Table t) = show t
  show (Special l _) = show l
  show (Lambda l _) = show l

obarray = mkRef ((M.empty,0) :: (M.Map String Int,Int))
intern' s = get obarray >>= \(o,n) -> case M.lookup<$>s<!>pure o of
  Just n -> return n
  Nothing -> obarray $= (o&maybe id (\s -> at s?~n) s,n+1) >> intern' s
-- |Returns the unique symbol associated to the given name
intern = unsafePerformIO . intern' . Just
-- ^Although destructive in practice, 'intern' behaves just like a pure function, so it can be made unsafe
-- |The symbol associated to the given name
mkSym s = Sym (intern s) (Just s)
-- |Creates a new unique symbol
newSym = intern' Nothing

-- |The class of all types morphic to ELVal
class ELValLike t where
  toELVal :: t -> ELVal
  fromELVal :: ELVal -> Maybe t

instance ELValLike ELVal where
  fromELVal = Just
  toELVal = id
instance ELValLike () where
  fromELVal (List []) = return ()
  fromELVal _ = mzero
  toELVal _ = List []
instance ELValLike Bool where
  fromELVal l = return $ case l of Int 0 -> False ; _ -> True
  toELVal b = Int $ if b then 0 else 1
instance ELValLike Int where
  fromELVal (Int n) = Just n
  fromELVal _ = Nothing
  toELVal = Int
instance ELValLike GLfloat where
  fromELVal (GLfloat n) = Just n
  fromELVal _ = Nothing
  toELVal = GLfloat
instance ELValLike Char where
  fromELVal (Char c) = Just c
  fromELVal _ = Nothing
  toELVal = Char
instance ELValLike (DrawTree DrawBox) where
  fromELVal (DT t) = Just t
  fromELVal _ = Nothing
  toELVal = DT
instance (ELValLike a) => ELValLike (M.Map Int a) where
  fromELVal (Table t) = traverse fromELVal t
  fromELVal _ = Nothing
  toELVal = Table . M.map toELVal 
instance ELValLike a => ELValLike (Maybe a) where
  fromELVal (List [Sym s _,v]) | s==intern "Just" = Just<$>fromELVal v
  fromELVal (Sym s _) | s==intern "Nothing" = pure (Nothing)
  fromELVal _ = mzero
  toELVal (Just a) = List [mkSym "Just",toELVal a]
  toELVal Nothing = mkSym "Nothing"
instance ELValLike a => ELValLike [a] where
  fromELVal (List l) = traverse fromELVal l
  fromELVal _ = Nothing
  toELVal = List . map toELVal
instance ELValLike a => ELValLike (Syntax a) where
  fromELVal v@(List l) = msum [Symbol <$> fromELVal v
                              ,Group <$> traverse fromELVal l]
  fromELVal l = Symbol <$> fromELVal l
  toELVal (Group l) = List (map toELVal l)
  toELVal (Symbol a) = toELVal a
newtype One a = One a
instance ELValLike a => ELValLike (One a) where
  fromELVal (List [a]) = One<$>fromELVal a
  fromELVal _ = mzero
  toELVal (One a) = List [toELVal a]
newtype Sym_ = Sym_ Int
instance ELValLike Sym_ where
  fromELVal (Sym n _) = return (Sym_ n)
  fromELVal _ = mzero
  toELVal (Sym_ n) = Sym n Nothing
data Lam_ = Lam_ ELVal ([ELVal] -> Joinable ELVal)
instance ELValLike Lam_ where
  fromELVal (Lambda a f) = return (Lam_ a f)
  fromELVal _ = mzero
  toELVal (Lam_ a f) = Lambda a f
-- |Morphism: '(a,b) -> [a,b]'
instance (ELValLike a,ELValLike b) => ELValLike (a,b) where
  fromELVal (List [a,b]) = (,) <$> fromELVal a <*> fromELVal b
  fromELVal _ = Nothing
  toELVal (a,b) = List [toELVal a,toELVal b]
-- |Morphism: '(a,b,c) -> [a,b,c]'
instance (ELValLike a,ELValLike b,ELValLike c) => ELValLike (a,b,c) where
  fromELVal (List [a,b,c]) = (,,) <$> fromELVal a <*> fromELVal b <*> fromELVal c
  fromELVal _ = Nothing
  toELVal (a,b,c) = List [toELVal a,toELVal b,toELVal c]
instance ELValLike G.Key where
  fromELVal (Char c) = Just (G.Char c)
  fromELVal _ = Nothing
  toELVal (G.Char c) = Char c
  toELVal _ = mkSym "#special"
instance ELValLike G.KeyState where
  fromELVal (Int 0) = Just G.Up
  fromELVal (Int 1) = Just G.Down
  fromELVal _ = Nothing
  toELVal G.Up = Int 0
  toELVal G.Down = Int 1
instance ELValLike Event where
  fromELVal (List [Sym _ (Just "kb"),s,c,a,k]) =
    KB<$>((,,)<$>fromELVal s<*>fromELVal c<*>fromELVal a)<*>fromELVal k
  fromELVal _ = Nothing
  toELVal (KB (s,c,a) k) = List [mkSym "kb",toELVal s,toELVal c,toELVal a,toELVal k]

-- |A type wrapper around String that allows writing strings, numbers and characters
newtype ELSym = ELSym { unELSym :: String }
              deriving Show
instance ELValLike ELSym where
  fromELVal l = ELSym <$> msum [case l of Sym _ n -> n ; _ -> mzero
                              ,show <$> (fromELVal l :: Maybe Char)
                              ,show <$> (fromELVal l :: Maybe Int)
                              ,show <$> (fromELVal l :: Maybe GLfloat)
                              ,show <$> (fromELVal l :: Maybe String)]
  toELVal (ELSym s) = fromMaybe (mkSym s) $ msum [
    toELVal <$> (readMay s :: Maybe String),
    toELVal <$> (readMay s :: Maybe Char),
    toELVal <$> (readMay s :: Maybe Int),
    toELVal <$> (readMay s :: Maybe GLfloat)]

