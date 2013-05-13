{-| A module for time handling -}
module Time(
  initTime,
  smoothTr,
  time
  ) where

import Graphics
import Utils
import System.Clock
import Foreign.C.Types
import Hooks

class Approximate a where
  near :: a -> a -> Bool
instance Approximate CFloat where
  near a b = abs (a-b)<=0.001
instance Approximate a => Approximate (Vector3 a) where
  near va vb = near<$>va<*>vb & \(Vector3 a b c) -> a&&b&&c
instance Approximate a => Approximate (Vector2 a) where
  near va vb = near<$>va<*>vb & \(Vector2 a b) -> a&&b

-- |Initializes the ticks timer
initTime = animate (\d -> runHooks tickHooks ($d) >> return True) 40

tickHooks = mkHooks ([] :: [Integer -> IO ()])

{-|
@smoothTr i v g@ registers a smooth transition callback for the variable
@v@ to the variable @g@ by the interpolating function @i@
-}
smoothTr i v g = tickHooks <+< \d -> do
  old <- get v
  get g >>= \g -> v$~i (fromIntegral d*7/1000000) g
  new <- get v
  unless (near old new) $ postRedisplay Nothing
animate f delay = animate =<< (microseconds <$> getTime Realtime)
  where animate start = do
          cur <- microseconds <$> getTime Realtime
          b <- f (cur-start)
          if b then addTimerCallback delay (animate cur) else return ()

currentTime = microseconds <$> getTime Realtime
microseconds (TimeSpec s p) = (fromIntegral s*1000000000+fromIntegral p)`div`1000 :: Integer

-- |Execute an IO action and print the elapsed time
time s m = currentTime >>= \t -> m <* (currentTime >>= \t' -> putStrLn (s++": "++show (t,t',t'-t)))
