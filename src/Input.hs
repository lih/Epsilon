module Input (initInput,(<+<),mouseHooks,motionHooks,dragHooks,DragCallback(..),keyboardHooks) where

import Graphics
import Utils

type Hook h = IORef [h]

keyboardHooks = mkRef [] :: Hook KeyboardMouseCallback
mouseHooks = mkRef [] :: Hook MouseCallback
motionHooks = mkRef [] :: Hook MotionCallback
type DragCallback = Vector2 GLint -> IO ()
dragHooks = mkRef [] :: Hook DragCallback


hs <+< h = hs $~ (h:)

initInput = do
  keyboardMouseCallback $= Just (\k s m p -> get keyboardHooks >>= mapM_ (\h -> h k s m p))
  mouseCallback $= Just (\b s p -> mapM_ (\h -> h b s p) =<< get mouseHooks)
  motionCallback $= Just (\p -> mapM_ ($p) =<< get motionHooks)
  mouseHooks <+< dragH
  motionHooks <+< dragMotH
  
dragSt = mkRef (False,v2 0 (0::GLint))
dragH LeftButton Down (Position x y) = dragSt $= (True,v2 x y)
dragH LeftButton Up _ = dragSt $~ (_1.~False)
dragH _ _ _ = return ()

dragMotH (Position x y) = do
  let next = v2 x y
  (dragging,prev) <- get dragSt
  when dragging $ do
    dragSt $~ (_2.~next)
    get dragHooks >>= mapM_ ($liftA2 (-) next prev)




