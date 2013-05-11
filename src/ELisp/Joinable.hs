-- |A type describing potentially pure or impure values (to preserve laziness when possible)
module ELisp.Joinable(Joinable,joined,impure) where

import Utils

-- |A type describing potentially pure or impure values (to preserve laziness when possible)
newtype Joinable a = Joinable (Either a (IO a))
instance Functor Joinable where
  fmap f (Joinable j) = Joinable (either (Left . f) (Right . fmap f) j)
instance Applicative Joinable where
  -- |Create a Joinable from a pure value
  pure = Joinable . Left
  Joinable (Left f) <*> Joinable (Left a) = Joinable (Left (f a))
  a <*> b = Joinable (Right $ joined a <*> joined b)
instance Monad Joinable where
  return = pure
  Joinable (Left a) >>= k = k a
  Joinable (Right a) >>= k = impure (a >>= joined . k)

-- |Evaluate a Joinable value
joined (Joinable j) = either return id j
-- |Create a Joinable from an IO value
impure = Joinable . Right
