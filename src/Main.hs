{-# LANGUAGE NoMonomorphismRestriction, Rank2Types, ImplicitParams, DeriveFunctor, ParallelListComp #-}
module Main(
  -- * The main function
  main,
  -- * Caching
  cache,resetCache,resetSelection,
  -- * Expression wheel
  wAngle,wAGoal,resetAngle,ia,
  -- * Callbacks
  display,reshape,keyboardMouse,
  -- * Environment
  bindings,evalNode,quit
  ) where 

import Prelude hiding (sequence,mapM)
import Graphics
import Time
import Utils hiding (focus)
import Box
import Trees
import Camera
import Selection
import Input
import Font
import Save
import ELisp hiding (Char)
import Model
import Hooks
import Data.Char

-- |The main function
main :: IO ()
main = do
  (_,args) <- getArgsAndInitialize
  let file = case args of
        [f] -> f
        _ -> "code.eps"
  withFont $ do
    initialDisplayMode $= [DoubleBuffered,WithDepthBuffer,WithAlphaComponent]
    createWindow "Epsilon"
    initELisp
    varHooks expr <+< (resetAngle >> resetCache)
    varHooks focus <+< (resetAngle >> resetSelection)
    initSave file >>= \es -> expr $= es
    clearColor $= Color4 0.1 0.1 0.1 1.0
    displayCallback $= display
    reshapeCallback $= Just reshape
    initInput ; initCamera ; initSelection
    depthFunc $= Just Lequal
    blend $= Enabled
    blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
    smoothTr inter wAngle wAGoal
    keyboardHooks <+< keyboardMouse
    mapM_ (uncurry bindEv) bindings
    mainLoop

-- |The drawtree cache (updated only when the expression changes)
cache = mkRef ([] :: [DrawTree DrawBox])
-- |Resets the expression drawtree cache (called when the expression is changed)
resetCache = do
  bs <- mapM (fmap centered . syntaxBox) =<< get expr
  cache $= bs
  resetSelection
  postRedisplay Nothing
-- |Resets the selection goal coordinates (called when the focus or expression change)
resetSelection = do
  (cur,f) <- get focus
  ch <- get cache
  let (p,_,s) | length ch==cur = (pure (-5),pure 5,pure 10)
              | otherwise = (pos,t^?!i._center,t^?!i._size)
      t = ch^?!_at cur.to toTree
      lenses = scanl (.) id [branches._at i | i <- f] ; i = last lenses.root
      pos = foldl1 (*+*) [t^?!l.root._pos | l <- lenses]
  selectGoal $= v2 (p*-*pure 5) (s*+*pure 10)
  centerGoal $= p*+*(s<&>(/2))

-- |The angle on the expression wheel
wAngle = mkRef (0 :: GLfloat)
-- |The goal angle on the expression wheel (used for animating the transitions)
wAGoal = mkRef (0 :: GLfloat)
-- |Index Angle: the wheel angle associated with the given index in the given list
ia exprs i = 360*fromIntegral i/fromIntegral (length exprs+1) :: GLfloat
-- |Resets the wheel angle goal (called when the focus or expression is changed)
resetAngle = do
  a <- ia<$>get expr<*>(fst<$>get focus)
  a' <- get wAngle
  when (abs (a-a')>=180) $ wAngle $= a'+(360*signum (a-a'))
  wAGoal $= a

-- |The display callback
display = do 
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity
  scalev . pure =<< get scl
  scalev $ pure (0.001 :: GLfloat)
  -- axes
  preservingMatrix $ do
    exprs <- get cache
    wa <- get wAngle

    rotate <$> get angleY <!> pure (Vector3 (-1) 0 0)
    rotate <$> get angleX <!> pure (Vector3 0 1 0)
    translate . fmap negate =<< get center
    preservingMatrix $ do
      translate $ v3 0 0 (10000::GLfloat)
      rotate wa (Vector3 0 1 0)
        
      c 0.8 0.8 0.8
      sequence_ [when (abs (ia exprs i-wa) < 0.90*(360.0/fromIntegral (length exprs)))
                 $ preservingMatrix $ do
                    rotate (ia exprs i) (Vector3 0 (-1) 0)
                    translate $ v3 0 0 (-10000::GLfloat)
                    draw b
                | b <- exprs | i <- [0..]]

    color $ Color4 0.5 0.5 0.5 (0.4 :: GLfloat)
    Vector2 pos size <- get select
    para pos size
  swapBuffers
-- |@para p s@ draws a parallelogram of size @s@ at position @p@
para (Vector3 x y z) (Vector3 w h d) = renderPrimitive Quads $ sequence_
         [p1, p3, p7, p5
         , p5, p6, p8, p7
         , p1, p2, p4, p3
         , p1, p2, p6, p5
         , p3, p4, p8, p7
         , p2, p4, p8, p6]
  where [p1,p2,p3,p4,p5,p6,p7,p8] = liftM3 v [x,x+w] [y,y+h] [z,z+d]
-- cube w = para (pure (-w/2)) (pure w)
-- axes = renderPrimitive Lines $ sequence_ [c 1 0 0, v 0 0 0, v 1 0 0
--                                          ,c 0 1 0, v 0 0 0, v 0 1 0
--                                          ,c 0 0 1, v 0 0 0, v 0 0 1]
-- The reshape callback that ensures that text is not deformed when the window is resized.
reshape (Size w h) = do
  let m = max w h
  viewport $= (Position ((w-m)`div`2) ((h-m)`div`2),Size m m)
-- |The default keyboard callback
keyboardMouse (Char c) Down _ _ | isPrint c = pushChar c
keyboardMouse k Down _ _ = putStrLn $ "Unhandled key: "++show k
keyboardMouse _ _ _ _ = return ()

-- |The initial keybindings
bindings = [
  -- Movement
  (key (SpecialKey KeyLeft),focusLeft),
  (key (SpecialKey KeyRight),focusRight),
  (key (SpecialKey KeyUp),focusUp),
  (key (SpecialKey KeyDown),focusDown),
  (ctl (SpecialKey KeyLeft),dragLeft),
  (ctl (SpecialKey KeyRight),dragRight),
  (ctl (SpecialKey KeyUp),dragUp),
  (ctl (SpecialKey KeyDown),dragDown),
  -- Manipulation
  (ctl (Char 's'),insertSym),
  (ctl (Char 'g'),insertGroup),
  (ctl_shift (Char 's'),replaceWithSym),
  (ctl_shift (Char 'g'),wrapNode),
  (key (Char '\DEL'),deleteNode),
  (ctl (Char 'c'),copyNode),
  (ctl (Char 'v'),pasteNode),
  (key (Char '('),openGroup),
  (key (Char ')'),closeGroup),
  (key (Char ' '),outsertSym),
  (key (Char '\r'),outsertSym),
  (key (Char '\b'),delChar),
  -- Evaluation
  (ctl (Char 'e'),evalNode),
  -- Other
  (ctl (Char ' '),pushChar ' '),
  (ctl (Char 'w'),writeSaveFile),
  (key (Char '\ESC'),quit)
  ]

-- |Evaluates the current node if selected
evalNode = (get >=> getF) focus >>= \l -> case l of
  [] -> return ()
  (e:_) -> print =<< eval (toELVal (ELSym <$> e))
-- |Save and exit
quit = writeSaveFile >> leaveMainLoop
