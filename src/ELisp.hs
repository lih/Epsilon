{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
{- |Module describing the Epsilon Lisp environment. -}
module ELisp(
  -- * Types
  ELVal(..),ELValLike(..),
  -- * Environment
  initELisp,
  nil,
  intern,newSym,setVal,getVal,
  eval,
  -- * Parsing
  ELSym(..)
  ) where

import Prelude hiding (mapM,sequence)
import Utils hiding (focus)
import Graphics (($=),($~),get)
import qualified Data.Map as M
import ELisp.Joinable
import ELisp.ELVal
import Input
import Model

values = mkRef (M.empty :: M.Map Int ELVal)

-- |The nil value
nil = List []
-- ^Although destructive in practice, 'intern' behaves just like a pure function, so it can be made unsafe
-- |Creates a unique symbol 
newSym = intern' Nothing
-- |Gets the value of the symbol
getVal s = fromMaybe nil . M.lookup s <$> get values
-- |Changes the value of the symbol
setVal s n = values $~ M.insert s n

{-|
Initializes the environment, adding builtins and special forms:

  * arithmetic builtins: @+@,@-@,@*@ and @/@

  * the @match@ special special form creates a pattern-matching lambda.
    It requires arguments of the form @(pattern expr)@ where @pattern@ is
    an s-expr and @expr@ is the expression to be evaluated when @pattern@
    matches and where sub-patterns of the form @(? symbol)@ are captured
    in the local variable @symbol@

  * the backquote (@`@) works like Lisp's, where @,@ is replaced by @!@

  * the @set@ builtin sets the global value of the given symbol

  * the @do@ builtin sequences its arguments and returns the last

  * the @macro@ builtin promotes a lambda to a macro

  * the @bind@ builtin binds a handler to an event of the form @(kb s c a c)@
    where @s@,@c@ and @a@ are bits describing whether the shift, control or
    alt key is pressed, and @c@ is a character.

  * other builtins: @map@ and @print@
-}
initELisp = do
  mapM_ (uncurry (setVal . intern)) [
    ("match",lambdaSp),
    ("`",backquoteSp),
    builtin "macro" (\ [Lambda s f] -> pure (Special s (const f))),
    builtin "+" (pure . foldl (\(Int n) (Int n') -> Int (n+n')) (Int 0)),
    builtin "*" (pure . foldl (\(Int n) (Int n') -> Int (n*n')) (Int 1)),
    builtin "/" (pure . foldl1 (\(Int n) (Int n') -> Int (n`div`n'))),
    builtin "-" (pure . foldl1 (\(Int n) (Int n') -> Int (n-n'))),
    
    builtin "do" (pure . lastDef nil),
    builtin "print" (\[a] -> action (print a)),
    builtin "map" bMap,

    builtin "bind" (\ [e,Lambda _ f] -> action $ case fromELVal e of
                       Just e -> bindEv e (void $ joined $ f [])
                       Nothing -> return ()),
    builtin "set" (\ [Sym s _,v] -> action (setVal s v)),
    builtin "get-focus" (\ _ -> impure (toELVal<$>get focus)),
    builtin "swap-focus" (\ [Lambda _ f] -> action $ do
                             foc <- joined =<< (f . return . toELVal<$>get focus)
                             maybe (return()) (\e -> focus $= e) (fromELVal foc)),
    builtin "get-selection" (\ _ -> impure (toELVal<$>(get focus>>=getF))),
    builtin "swap-selection" (\ [Lambda _ f] -> action $ do
                               for <-  getF =<< get focus
                               for' <- joined $ f [toELVal for]
                               id ~~ maybe id const (fromELVal for'))
    ]
  where bMap [Lambda _ f,List l] = List <$> sequenceA [f [x] | x <- l]
        bMap l = pure (List l)
        builtin n f = (n,Lambda (mkSym $ "#<builtin:"++n++">") f)
        action a = impure (a >> return nil)

-- |Evaluates the given expression
eval = joined . eval' M.empty
eval' locals = eval''
  where 
    eval'' (Sym s _) = case M.lookup s locals of
      Just v -> pure v
      Nothing -> impure (getVal s)
    eval'' (List (f:as)) = eval'' f >>= k
      where k (Special _ sp) = eval'' =<< sp locals as
            k (Lambda _ l) = l =<< traverse eval'' as
            k _ = error $ "Cannot apply "++show f
    eval'' v = pure v

lambdaSp = Special (mkSym "#<spe:lambda>") lambda
  where lambda locals args = pure $ case fromELVal (List args) of
          Just l -> Lambda (List (lambdaSp:args)) (foldr f (const (pure nil)) l)
            where f (pat,expr) k v = case match pat (List v) of
                    Just l -> eval' (M.union l locals) expr
                    Nothing -> k v
          Nothing -> error $ "Invalid syntax for lambda special form: "++show args
match (List [Sym s _,Sym n _]) x | s==intern "?" = return (M.singleton n x)
match (List a) (List b) | length (zip a b)>=length a-1 = 
  M.unions <$> sequence (zipWith match' (tails a) (tails b))
  where match' [List [Sym s _,Sym n _]] l | s==intern "|" =
          return (M.singleton n (List l))
        match' (p:_) (x:_) = match p x
        match' [] [] = return M.empty
        match' _ _ = mzero
match (Sym n _) (Sym n' _) | n==n' = return M.empty
match (Int n) (Int n') | n==n' = return M.empty
match (Char c) (Char c') | c==c' = return M.empty
match _ _ = mzero

backquoteSp = Special (mkSym "#<spe:backquote>") bq
  where bq locals (arg:_) = pure $ List [Lambda (List [backquoteSp,arg]) (const (head <$> ev arg))]
          where ev (List [Sym _ (Just "!"),x]) = return <$> eval' locals x
                ev (List [Sym _ (Just "@"),x]) = eval' locals x<&> \(List l) -> l
                ev (List l) = return . List . concat <$> traverse ev l
                ev x = return (pure x)
        bq _ _ = pure nil 

