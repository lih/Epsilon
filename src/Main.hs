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
import Utils
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
import Foreign.C.Types (CFloat(..))

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
    initELisp ; initTime
    initInput ; initCamera ; initSelection
    clearColor $= Color4 0.1 0.1 0.1 1.0
    displayCallback $= display
    reshapeCallback $= Just reshape
    depthFunc $= Just Lequal
    blend $= Enabled
    blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
    smoothTr inter wAngle wAGoal
    mapM_ (uncurry bindEv) bindings
    registerAll
    keyboardHooks <+< keyboardMouse
    varHooks currentNode <+< (resetAngle >> resetCache)
    varHooks selection <+< (resetAngle >> resetSelection)
    initSave file >>= \es -> forM_ (reverse es) $ \e -> currentNode $= Insert e
    mainLoop
-- |Initializes the transition timers for the camera parameters
initCamera = do
  smoothTr inter scl sclGoal
  smoothTr inter yaw yawGoal
  smoothTr inter pitch pitchGoal
  smoothTr (liftA2 . inter) center centerGoal
  mouseHooks <+< mouse
  dragHooks <+< drag
  where mouse WheelUp _ _ = zoomIn ; mouse WheelDown _ _ = zoomOut
        mouse _ _ _ = return ()
        drag (Vector2 dx dy) = cameraLeft (fromIntegral dx/3)
                               >> cameraUp (fromIntegral dy/3)
-- |Registers the smooth transition between 'select' and 'selectGoal'
initSelection = smoothTr (liftA2 . liftA2 . inter) select selectGoal

-- |The drawtree cache (updated only when the expression changes)
cache = mkRef ([] :: [DrawTree DrawBox])
-- |Resets the expression drawtree cache (called when the expression is changed)
resetCache = do
  bs <- mapM (fmap centered . syntaxBox) =<< get trees
  cache $= bs
  resetSelection
  postRedisplay Nothing
-- |Resets the selection goal coordinates (called when the selection or expression change)
resetSelection = do
  (cur,f) <- get selection
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
-- |Resets the wheel angle goal (called when the selection or expression is changed)
resetAngle = do
  a <- ia<$>get trees<*>(fst<$>get selection)
  a' <- get wAngle
  when (abs (a-a')>=180) $ wAngle $= a'+(360*signum (a-a'))
  wAGoal $= a

-- |The display callback
display = do 
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity
  -- axes
  preservingMatrix $ do
    exprs <- get cache
    wa <- get wAngle

    scalev . pure =<< get scl
    scalev $ pure (0.001 :: GLfloat)
    rotate <$> get pitch <!> pure (Vector3 (-1) 0 0)
    rotate <$> get yaw <!> pure (Vector3 0 1 0)
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
    draw (para pos size)

  depthMask $= Disabled
  (Position x y,Size s _) <- get viewport
  ortho2D (fromIntegral x) (fromIntegral (x+s)) (fromIntegral y) (fromIntegral (s+y))
  text <- get selection<&> \(h,_) -> ("<wheel>#"++show h)
  [_,y,_,_,y',_] <- getFontBBox ?statusFont text<&>map CFloat
  c 0 0 0
  renderPrimitive Quads $ do
    let v2 x y = vertex $ Vertex2 x (y::GLfloat)
    v2 0 y; v2 0 y'; v2 (fromIntegral s) y'; v2 (fromIntegral s) y    

  renderFont ?statusFont text All
  depthMask $= Enabled
  
  swapBuffers
-- axes = renderPrimitive Lines $ sequence_ [c 1 0 0, v 0 0 0, v 1 0 0
--                                          ,c 0 1 0, v 0 0 0, v 0 1 0
--                                          ,c 0 0 1, v 0 0 0, v 0 0 1]
-- |The reshape callback that ensures that text is not deformed when the window is resized.
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
  (key (SpecialKey KeyLeft),selectLeft),
  (key (SpecialKey KeyRight),selectRight),
  (key (SpecialKey KeyUp),selectUp),
  (key (SpecialKey KeyDown),selectDown),
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

-- Registers the Epsilon builtins into the ELisp environment
registerAll = do
  registerBuiltinW "text#" (\ [s] -> impure (DT . atomic <$> textBox s))
  registerBuiltinW "horiz#" (\ l -> pure (glH (l :: [DTB])))
  registerBuiltinW "vert#" (\ l -> pure (glV (l :: [DTB])))
  registerBuiltinW "al-left#" (\ [b] -> pure (align L (b::DTB)))
  registerBuiltinW "color#" (\ ((r,g,b),t) -> pure (colored (Color3 (r::GLfloat) g b) (t::DTB)))
  registerBuiltinW "pad#" (\ (x,y,z) -> pure (padding (v3 x y z) :: DrawTree DrawBox))
  registerBuiltinW "subs#" (\[l] -> impure (mkSubs (map pure l)))
  registerBuiltinW "match#" (\ (p,e) -> impure (matchBox p e))
  registerBuiltinW "syntax#" (\ [s] -> impure (syntaxBox s))

  registerBuiltinW "bind" (\ (e,Lambda x f) -> action (print x >> print e >> bindEv e (void $ joined $ f [])))
  registerBuiltinW "unbind" (\ [e] -> action (unbindEv e))

  registerBuiltin "get-selection" (\ _ -> impure (toELVal<$>get selection))
  registerBuiltin "swap-selection" (\ [Lambda _ f] -> action $
                                   (unwrap f . (:[])<$>get selection) >>= joined
                                   >>= maybe (return()) (selection $=))
  registerBuiltin "get-selected" (\ _ -> impure (toELVal<$>get currentForest))
  registerBuiltin "swap-selected" (\ [Lambda _ f] -> action $ do
                                       for <-  get currentNode
                                       for' <- joined $ unwrap f [for]
                                       currentNode $~ maybe id const for')

  registerBuiltin "add-layout" (\ [Lambda _ f] -> let h s = do
                                                        x <- joined (f [toELVal s])
                                                        return (fromELVal x)
                                                  in action $ (layoutHooks <+< h
                                                              >> resetCache))
  
-- |Evaluates the current node if selected
evalNode = get currentNode >>= \n -> case n of
  Delete -> return ()
  Current e -> print =<< eval (toELVal (ELSym <$> e))
  _ -> error "Impossible"
-- |Save and exit
quit = writeSaveFile >> leaveMainLoop
