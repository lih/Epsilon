{-| A helper module to handle keyboard and mouse input -}
module Input (
  -- * Hooks
  Hooks,
  mouseHooks,motionHooks,
  keyboardHooks,
  DragCallback,dragHooks,
  -- * Environment
  initInput,(<+<),
  -- * Utils
  Event(..),bindEv,unbindEv,
  key,ctl,ctl_shift,alt_
  ) where

import Graphics
import Utils
import qualified Data.Map as M
import Hooks
import Data.Char

-- |A drag callback takes the delta vector as an argument
type DragCallback = Vector2 GLint -> IO ()

-- |The hook for keyboard events
keyboardHooks = mkHooks [] :: Hooks KeyboardMouseCallback
-- |The hook for mouse events
mouseHooks = mkHooks [] :: Hooks MouseCallback
-- |The hook for mouse motion events
motionHooks = mkHooks [] :: Hooks MotionCallback
-- |The hook for drag events
dragHooks = mkHooks [] :: Hooks DragCallback

-- |Initializes the keyboard and mouse callbacks and registers the drag hooks
initInput = do
  keyboardMouseCallback $= (Just $ \k s m p -> do
                               bs <- get evMap
                               case M.lookup (mkEv m k) bs of
                                 Just a | s==Down -> a
                                 _ -> runHooks keyboardHooks $ \h -> h k s m p)
  mouseCallback $= Just (\b s p -> runHooks mouseHooks (\h -> h b s p))
  motionCallback $= Just (\p -> runHooks motionHooks ($p))
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
    runHooks dragHooks ($ liftA2 (-) next prev)

-- |A keyboard event
data Event = KB (KeyState,KeyState,KeyState) Key
           deriving (Eq,Ord)
mkEv (Modifiers s c a) k = KB (s,c,a) k
evMap = mkRef (M.empty :: M.Map Event (IO ()))

newEvs (KB (s,c,a) (Char ch)) | isLower ch =
  return $ KB (s,c,a) (Char (chr (if ch'>=0 then ch' else ord ch)))
  where ch' = ord ch-96*toBit c
newEvs (KB (s,Down,a) (Char ' ')) = return $ KB (s,Down,a) (Char '\NUL')
newEvs _ = mzero
toBit Up = 0 ; toBit Down = 1
-- |Binds an action to a keyboard event
bindEv e m = forM_ (e:newEvs e) $ \e -> evMap $~ M.insert e m
-- |Unbinds the given keyboard event
unbindEv e = forM_ (e:newEvs e) $ \e -> evMap $~ M.delete e

-- |Creates an event where no modifier keys are pressed
key = KB (Up,Up,Up)
-- |Creates an event where only the control key is pressed
ctl = KB (Up,Down,Up)
-- |Creates an event where only the control and shift keys are pressed
ctl_shift = KB (Down,Down,Up)
-- |Creates an event where only the alt key is pressed
alt_ = KB (Up,Up,Down)
