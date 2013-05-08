module Time where

import Graphics
import Utils
import System.Clock

smoothTr i v g = animate (\d -> (get g >>= \g -> v$~i (fromIntegral d*7/1000000) g)
                                >> postRedisplay Nothing
                                >> return True) 40
animate f delay = animate =<< (microseconds <$> getTime Realtime)
  where animate start = do
          cur <- microseconds <$> getTime Realtime
          b <- f (cur-start)
          if b then addTimerCallback delay (animate cur) else return ()

currentTime = microseconds <$> getTime Realtime
microseconds (TimeSpec s p) = (fromIntegral s*1000000000+fromIntegral p)`div`1000 :: Integer

time s m = currentTime >>= \t -> m <* (currentTime >>= \t' -> putStrLn (s++": "++show (t,t',t'-t)))
