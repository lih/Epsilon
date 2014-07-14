{-# LANGUAGE Rank2Types, ScopedTypeVariables, NoMonomorphismRestriction #-}
{- |Module describing the Epsilon Lisp environment. -}
module ELisp(
  -- * Modules
  module ELisp.Joinable,
  -- * Types
  ELVal(..),ELValLike(..),
  action,
  -- * Environment
  initELisp,
  nil,
  intern,newSym,setVal,defVal,getVal,
  eval,
  registerBuiltin,registerBuiltinW,wrap,unwrap,
  -- * Parsing
  ELSym(..)
  ) where

import Prelude hiding (mapM,sequence)
import Utils hiding (focus)
import Graphics (($~),get)
import qualified Data.Map as M
import ELisp.Joinable
import ELisp.ELVal

data ValType = Static | Dynamic
             deriving Eq
values = mkRef (M.empty :: M.Map Int (ValType,ELVal))
getValDef s = fromMaybe (Dynamic,nil) . M.lookup s

unwrap f = \a -> case toELVal a of
  List l -> fromELVal <$> f l
  _ -> pure Nothing
wrap f = \a -> case fromELVal (List a) of
  Just a -> toELVal <$> f a
  Nothing -> pure nil
  
-- |The nil value
nil = List []
-- |Creates a return value from an IO action
action a = impure (a >> return nil)
-- |Gets the value information (type and value) of a symbol
getValInfo s = getValDef s <$> get values
-- |Gets the global value of a symbol
getVal s = snd <$> getValInfo s
-- |Changes the global value of a symbol
setVal s n = values $~ M.insert s (Dynamic,n)
-- |Changes the global value of a symbol (by name
setValS = setVal . intern
-- |Sets the static value of a symbol
defVal s n = values $~ M.insert s (Static,n)

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
  mapM_ (uncurry setValS) [("match",lambdaSp),("`",backquoteSp)]
  registerBuiltinW "macro" (\ (One (Lam_ s f)) ->
                            pure (Special (List [mkSym "#<macro>",s]) (const f)))
  registerBuiltinW "set" (\ (Sym_ s,v) -> action (setVal s v))
  registerBuiltinW "def" (\ (Sym_ s,v) -> action (defVal s v))
  registerBuiltinW "apply" (\ (Lam_ _ f,l) -> f l)

  registerBuiltinW "+" (pure . (sum :: [Int] -> Int))
  registerBuiltinW "*" (pure . (product :: [Int] -> Int))
  registerBuiltinW "/" (pure . foldl1 (div :: Int -> Int -> Int))
  registerBuiltinW "-" (pure . foldl1 ((-) :: Int -> Int -> Int))

  registerBuiltinW "type-of" (\(One l) -> pure $ mkSym $ case l of
                                 Int _ -> "integer"
                                 GLfloat _ -> "float"
                                 List _ -> "list"
                                 Char _ -> "char"
                                 Sym _ _ -> "symbol"
                                 DT _ -> "drawtree"
                                 Table _ -> "table"
                                 Special _ _ -> "special-form"
                                 Lambda _ _ -> "function")
  registerBuiltinW "string?" (\(One l) -> pure $ isJust (fromELVal l::Maybe String)) 

  registerBuiltin "do" (pure . lastDef nil)
  registerBuiltinW "print" (\(One a) -> action (print (a::ELVal)))
  registerBuiltinW "intern" (\ (One s) -> pure (mkSym s))
  registerBuiltinW "gensym" (\ () -> impure (newSym<&> \n -> Sym n Nothing))

  registerBuiltinW "map" (\ (Lam_ _ f,l) -> traverse (f . return) (l::[ELVal]))

  registerBuiltinW "empty" (\ () -> pure (M.empty :: M.Map Int ELVal))
  registerBuiltinW "insert" (\ (Sym_ n,v,m) ->
                              pure (Table (M.insert n v m)))
  registerBuiltinW "delete" (\ (Sym_ n,m) -> pure (Table (M.delete n m)))
  registerBuiltinW "lookup" (\ (Sym_ n,m) -> pure (M.lookup n m :: Maybe ELVal))
  
-- |evaluates the given expression
eval e = get values >>= \vs ->
  joined (eval' (snd<$>M.filter ((==Static).fst) vs) e)
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
match (Sym n _) (Sym n' _) | n==n' = return M.empty
match (Sym n _) _ | n==intern "_" = return M.empty
match (List [Sym s _,Sym n _]) x | s==intern "?" = return (M.singleton n x)
match (List a) (List b) | length (zip a b)>=length a-1 = 
  M.unions <$> sequence (zipWith match' (init $ tails a) (tails b))
  where match' [List [Sym s _,Sym n _]] l | s==intern "|" =
          return (M.singleton n (List l))
        match' [_] (_:_:_) = mzero
        match' (p:_) (x:_) = match p x
        match' _ _ = mzero
match (Char c) (Char c') | c==c' = return M.empty
match (Int n) (Int n') | n==n' = return M.empty
match (GLfloat a) (GLfloat b) | a==b = return M.empty 
match _ _ = mzero

backquoteSp = Special (mkSym "#<spe:backquote>") bq
  where bq locals (arg:_) = pure $ List [Lambda (List [backquoteSp,arg]) (const (head <$> ev arg))]
          where ev (List [Sym _ (Just "!"),x]) = return <$> eval' locals x
                ev (List [Sym _ (Just "@"),x]) = eval' locals x<&> \(List l) -> l
                ev (List l) = return . List . concat <$> traverse ev l
                ev x = return (pure x)
        bq _ _ = pure nil 

registerBuiltinW b f = registerBuiltin b (wrap f)
registerBuiltin b f = defVal (intern b) (Lambda (mkSym name) f)
  where name = "#<builtin:"++b++">"
