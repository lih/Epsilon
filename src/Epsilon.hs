{-# LANGUAGE NoMonomorphismRestriction, Rank2Types, ImplicitParams, DeriveFunctor, ParallelListComp #-}
import Prelude hiding (sequence,mapM)
import Graphics
import Time
import Utils hiding (focus)
import Box
import Trees
import Camera
import Selection
import Input
import Data.Maybe
import Font
import Save
-- import ELisp

ia exprs i = 360*fromIntegral i/fromIntegral (length exprs+1) :: GLfloat
resetAngle = do
  a <- ia<$>get expr<*>(fst<$>get focus)
  a' <- get wAngle
  when (abs (a-a')>=180) $ wAngle $= a'+(360*signum (a-a'))
  wAGoal $= a
resetCache = do
  bs <- mapM (fmap centered . sBox) =<< get expr
  cache $= bs
  resetSelection
  postRedisplay Nothing
resetSelection = do
  (cur,f) <- get focus
  ch <- get cache
  let (p,c,s) | length ch==cur = (pure (-5),pure 5,pure 10)
              | otherwise = (pos,t^?!i._center,t^?!i._size)
      t = ch^?!_at cur.to toTree
      lenses = scanl (.) id [branches._at i | i <- f] ; i = last lenses.root
      pos = foldl1 (*+*) [t^?!l.root._pos | l <- lenses]
  selectGoal $= v2 p s
  centerGoal $= p*+*(s<&>(/2))
triggerVar v tr = makeStateVar (get v) (\x -> v $= x >> tr)
expr = triggerVar (mkRef ([] :: [Syntax String])) (resetAngle >> resetCache)
cache = mkRef ([] :: [DrawTree DrawBox])
focus = triggerVar (mkRef ((0,[]) :: (Int,[Int]))) (resetAngle >> resetSelection)
wAngle = mkRef (0 :: GLfloat) ; wAGoal = mkRef (0 :: GLfloat)

main :: IO ()
main = do
  (_,args) <- getArgsAndInitialize
  let file = case args of
        [f] -> f
        _ -> "code.eps"
  withFont $ do
    initialDisplayMode $= [DoubleBuffered,WithDepthBuffer,WithAlphaComponent]
    createWindow "Epsilon"
    initSave file >>= \es -> expr $= es
    clearColor $= Color4 0.1 0.1 0.1 1.0
    displayCallback $= display
    reshapeCallback $= Just reshape
    initCamera ; initSelection ; initInput
    depthFunc $= Just Lequal
    blend $= Enabled
    blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
    smoothTr inter wAngle wAGoal
    keyboardHooks <+< keyboardMouse
    dragHooks <+< drag
    mainLoop

reshape (Size w h) = do
  let m = max w h
  viewport $= (Position ((w-m)`div`2) ((h-m)`div`2),Size m m)

_focus = iso (\(x,t) -> reverse (x:t)) (\l -> case reverse l of (x:t) -> (x,t) ; _ -> (0,[]))
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
      foc <- get focus
      sequence_ [preservingMatrix $ do
                    rotate (ia exprs i) (Vector3 0 (-1) 0)
                    translate $ v3 0 0 (-10000::GLfloat)
                    draw b
                | b <- exprs | i <- [0..]]

    color $ Color4 0.5 0.5 0.5 (0.4 :: GLfloat)
    Vector2 pos size <- get select
    para pos size
  swapBuffers
quit = join (liftA2 writeFile (get saveFile) (show <$> get expr)) >> leaveMainLoop

drag (Vector2 dx dy) = left (fromIntegral dx/3) >> up (fromIntegral dy/3)

l ~~ f = get focus >>= \i -> expr $~ (_group.from _h._forestAt (reverse (i^._focus)).l%~f)
getF i = view (_group.from _h._forestAt (reverse (i^._focus))) <$> get expr 
infixr 4 ~~
modText f = _head._Symbol ~~ f >> postRedisplay Nothing
delChar = modText initSafe
keyboardMouse k Down (Modifiers { ctrl = Down, shift = Down }) _ = shiftKeyDown k
keyboardMouse k Down (Modifiers { ctrl = Down }) _ = ctrlKeyDown k
keyboardMouse k Down _ _ = keyDown k
keyboardMouse _ _ _ _ = return ()

tryMod f p = get focus >>= \i -> fmap p (getF i) >>= \b -> if b then focus $= f i else return ()

focusLeft = do
  f <- getF =<< get focus
  let m 0 = length f ; m n = n-1
  focus $~ (_focus._head%~m)
focusRight = do
  f <- getF =<< get focus
  let m n | null f = 0 ; m n = n+1
  focus $~ (_focus._head%~m)
focusDown = tryMod (_focus%~(0:)) (has (_head._Group))
focusUp = focus $~ (_2%~initSafe)

keyDown (Char '(') = deleteNode >> insertGroup >> insertSym
keyDown (Char ')') = focusUp >> focusRight >> insertSym
keyDown (Char ' ') = focusRight >> insertSym
keyDown (Char '\r') = focusRight >> insertSym
keyDown (Char '\b') = delChar
keyDown (SpecialKey KeyDelete) = delChar
keyDown (Char '\ESC') = quit
keyDown (Char '\DEL') = deleteNode
keyDown (SpecialKey KeyLeft) = focusLeft
keyDown (SpecialKey KeyRight) = focusRight
keyDown (SpecialKey KeyDown) = focusDown
keyDown (SpecialKey KeyUp) = focusUp
keyDown (Char c) = modText (++[c])
keyDown _ = return ()
ctrlKeyDown (SpecialKey KeyLeft) = dragLeft
ctrlKeyDown (SpecialKey KeyRight) = dragRight
ctrlKeyDown (SpecialKey KeyUp) = dragUp
ctrlKeyDown (SpecialKey KeyDown) = dragDown
ctrlKeyDown (Char 'c') = copyNode
ctrlKeyDown (Char '\ETX') = copyNode
ctrlKeyDown (Char 'v') = pasteNode
ctrlKeyDown (Char '\SYN') = pasteNode 
ctrlKeyDown (Char 's') = insertSym
ctrlKeyDown (Char '\DC3') = insertSym
ctrlKeyDown (Char 'g') = insertGroup
ctrlKeyDown (Char '\a') = insertGroup
ctrlKeyDown (Char 'q') = quit
ctrlKeyDown c = print c
shiftKeyDown (Char 'S') = deleteNode >> insertSym
shiftKeyDown (Char '\DC3') = deleteNode >> insertSym
shiftKeyDown (Char 'G') = wrapNode
shiftKeyDown (Char '\a') = wrapNode
shiftKeyDown c = print c

pasteNode = get clipboard >>= maybe (return ()) (\n -> id ~~ (n:))
copyNode = get focus >>= getF >>= \l -> clipboard $= listToMaybe l
wrapNode = _head ~~ (Group . return) >> focusDown >> focusRight
insertSym = id ~~ (Symbol "":)
insertGroup = id ~~ (Group []:) >> focusDown
deleteNode = id ~~ tailSafe
dragBy foc = get focus >>= getF >>= \f -> case f of
  [] -> return ()
  (x:_) -> deleteNode >> foc >> id ~~ (x:)
dragLeft = dragBy focusLeft
dragRight = dragBy focusRight
dragUp = dragBy focusUp
dragDown = dragBy focusDown

axes = renderPrimitive Lines $ sequence_ [c 1 0 0, v 0 0 0, v 1 0 0
                                         ,c 0 1 0, v 0 0 0, v 0 1 0
                                         ,c 0 0 1, v 0 0 0, v 0 0 1]
para (Vector3 x y z) (Vector3 w h d) = renderPrimitive Quads $ sequence_
         [p1, p3, p7, p5
         , p5, p6, p8, p7
         , p1, p2, p4, p3
         , p1, p2, p6, p5
         , p3, p4, p8, p7
         , p2, p4, p8, p6]
  where [p1,p2,p3,p4,p5,p6,p7,p8] = liftM3 v [x,x+w] [y,y+h] [z,z+d]
cube w = para (pure (-w/2)) (pure w)





