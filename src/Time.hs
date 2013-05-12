module Time where

import Graphics
import Utils
import System.Clock
import Foreign.C.Types

class Approximate a where
  near :: a -> a -> Bool
instance Approximate CFloat where
  near a b = abs (a-b)<=0.001
instance Approximate a => Approximate (Vector3 a) where
  near va vb = near<$>va<*>vb & \(Vector3 a b c) -> a&&b&&c
instance Approximate a => Approximate (Vector2 a) where
  near va vb = near<$>va<*>vb & \(Vector2 a b) -> a&&b

smoothTr i v g = flip animate 40 $ \d -> do
  old <- get v
  get g >>= \g -> v$~i (fromIntegral d*7/1000000) g
  new <- get v
  unless (near old new) $ postRedisplay Nothing
  return True
animate f delay = animate =<< (microseconds <$> getTime Realtime)
  where animate start = do
          cur <- microseconds <$> getTime Realtime
          b <- f (cur-start)
          if b then addTimerCallback delay (animate cur) else return ()

currentTime = microseconds <$> getTime Realtime
microseconds (TimeSpec s p) = (fromIntegral s*1000000000+fromIntegral p)`div`1000 :: Integer

time s m = currentTime >>= \t -> m <* (currentTime >>= \t' -> putStrLn (s++": "++show (t,t',t'-t)))
