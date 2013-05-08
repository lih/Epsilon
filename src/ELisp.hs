{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module ELisp() where

import Prelude hiding (mapM,sequence)
import Trees
import Data.List
import Utils
import Graphics (($=),($~),get)
import qualified Data.Map as M
import Data.Maybe

data LVal = List [LVal]
          | Sym Int (Maybe String)
          | Char Char
          | Int Int
          | Special LVal (M.Map Int LVal -> [LVal] -> IO LVal)
          | Lambda LVal ([LVal] -> IO LVal)
instance Show LVal where
  show (List l) = concat["[",intercalate " " (map show l),"]"]
  show (Sym n s) = fromMaybe ("#"++show n) s
  show (Char c) = show c
  show (Int n) = show n
  show (Special l _) = show l
  show (Lambda l _) = show l

class LValLike t where
  toLVal :: t -> LVal
  fromLVal :: LVal -> Maybe t

instance LValLike LVal where
  fromLVal = Just
  toLVal = id
instance LValLike Int where
  fromLVal (Int n) = Just n
  fromLVal _ = Nothing
  toLVal = Int
instance LValLike Char where
  fromLVal (Char c) = Just c
  fromLVal _ = Nothing
  toLVal = Char
instance LValLike a => LValLike [a] where
  fromLVal (List l) = mapM fromLVal l
  fromLVal _ = Nothing
  toLVal = List . map toLVal
instance LValLike a => LValLike (Syntax a) where
  fromLVal (List l) = Group <$> mapM fromLVal l
  fromLVal l = Symbol <$> fromLVal l
  toLVal (Group l) = List (map toLVal l)
  toLVal (Symbol a) = toLVal a
instance (LValLike a,LValLike b) => LValLike (a,b) where
  fromLVal (List [a,b]) = (,) <$> fromLVal a <*> fromLVal b
  fromLVal _ = Nothing
  toLVal (a,b) = List [toLVal a,toLVal b]
  
_lVal :: LValLike l => Prism' LVal l
_lVal = prism toLVal (\l -> maybe (Left l) Right (fromLVal l))

obarray = mkRef ((M.empty,M.empty,0) :: (M.Map String Int,M.Map Int LVal,Int))

nil = List []
intern s = get obarray >>= \(o,e,n) -> case M.lookup<$>s<!>pure o of
  Just n -> return n
  Nothing -> obarray $= (o&maybe id (\s -> at s?~n) s,e,n+1) >> intern s
getVal s = get obarray<&> \(_,vs,_) -> fromMaybe nil (M.lookup s vs)

eval locals (Sym s _) = case M.lookup s locals of
  Just v -> return v
  Nothing -> getVal s
eval locals (List (f:args)) = eval locals f >>= \f -> case f of
  Special _ sp -> sp locals args >>= eval locals
  Lambda _ l -> mapM (eval locals) args >>= l
  _ -> error $ "Cannot apply "++show f
eval _ v = return v

lambdaSp = Special (Sym 0 (Just "#<spe:lambda>")) lambda
  where lambda locals args = return $ case fromLVal (List args) of
          Just l -> Lambda (List args) (foldr f (const (return nil)) l)
            where f (pat,expr) k v = case match pat (List v) of
                    Just l -> eval (M.union l locals) expr
                    Nothing -> k v
          Nothing -> error $ "Invalid syntax for lambda special form: "++show args

backquoteSp = Special (Sym 0 (Just "#<spe:backquote>")) bq
  where bq locals (arg:_) = return $ List [Lambda arg (const (head <$> ev arg))]
          where ev (List [Sym _ (Just "!"),x]) = return <$> eval locals x
                ev (List [Sym _ (Just "@"),x]) = eval locals x<&> \(List l) -> l
                ev (List l) = return . List . concat <$> mapM ev l
                ev x = return (return x)

match (List [Sym _ (Just ('?':_)),Sym n _]) x = return (M.singleton n x)
match (List a) (List b) | length a==length b = M.unions <$> sequence (zipWith match a b)
match (Sym n _) (Sym n' _) | n==n' = return M.empty
match (Int n) (Int n') | n==n' = return M.empty
match (Char c) (Char c') | c==c' = return M.empty
match _ _ = Nothing

