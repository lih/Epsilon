{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, ImplicitParams, ParallelListComp, FlexibleInstances #-}
module Box(
  -- * Modules
  module Box.Types,

  -- * Layout
  atomic,
  moved,movedTo,centered,padded,scaled',colored,
  align,Alignment(..),
  glH,glV,
  (#-),(#-#),(#|),(#|#),

  -- * Contruction
  para,cube,padding,
  textBox,
  mkSubs,matchBox,
  syntaxBox,

  -- * Other
  layoutHooks,draw
  ) where

import Graphics.Rendering.OpenGL.GL.QueryUtils
import Data.Maybe
import Data.List
import Prelude hiding (sequence,mapM)
import Graphics hiding (cursor,T,R)
import Utils
import Foreign.C.Types
import Box.Types
import Hooks
import Trees
import Font

glue :: (forall a. Lens' (Vector3 a) a) -> [(Sz,Pos)] -> (Sz,Pos,[Pos])
glue x bs = (sz,cen,zipWith f cs offsets)
  where vs = sequenceA ds ; (ds,cs) = unzip bs
        offsets = scanl (+) 0 (vs^.x)
        max' = fmap $ foldl max 0
        sz = cen *+* max' (sequenceA $ zipWith (*-*) ds cs) & x.~last offsets
        cen = max' (sequenceA cs) & x.~(last offsets/2)
        f c off = cen*-*c & x.~off

instance Boxed (Box GLfloat) where
  _size = bSize
  _pos = bPos
  _center = bCenter
  appendBy l bs = Box (pure 0) c sz
    where (sz,c,_) = glue l [(b^._size,b^._center) | b <- bs]
instance Boxed DrawBox where
  _pos = box.bPos
  _size = box.bSize
  _center = box.bCenter
  appendBy l bs = DrawBox (Box (pure 0) c sz) dr
    where (sz,c,ps) = glue l [(b^._size,b^._center) | b <- bs]
          dr = sequence_ [drawP (movedTo b p) | b <- bs | p <- ps]
instance Boxed a => Boxed (DrawTree a) where
  appendBy l ts = DrawTree (appendBy l bs) ss'
    where (bs,ss) = unzip [(b,s) | DrawTree b s <- ts]
          ss' = foldr (mergeBy ((<)`on`fst)) []
                [map (_2.root%~(`moved`p)) s | (p,s) <- zip ps ss]
          (_,_,ps) = glue l [(b^._size,b^._center) | b <- bs]
  _pos = droot._pos
  _size = droot._size
  _center = droot._center

instance Drawable DrawBox where
  nullDraw = DrawBox (Box (pure 0) (pure 0) (pure 0)) (return())
  _draw = drawB
instance Drawable a => Drawable (DrawTree a) where
  nullDraw = DrawTree nullDraw []
  _draw = droot._draw

instance Scalable (Box GLfloat) where
  scaled sc (Box p c s) = liftA2 (*) sc&(Box<$>($p)<*>($c)<*>($s))
instance Scalable DrawBox where
  scaled sc (DrawBox b d) = DrawBox (scaled sc b) (scalev sc >> d)
instance Scalable a => Scalable (DrawTree a) where
  scaled sc (DrawTree a ss) = DrawTree (scaled sc a) (map (_2%~fmap (scaled sc)) ss)

-- |Construct a tree from a single box
atomic t = DrawTree t []
-- |Move the Boxed by the given vector
moved db v = db&_pos %~ (v*+*)
-- |Move the Boxed to the given position
movedTo db v = db&_pos .~ v
-- |Centers the Boxed
centered db = movedTo db (negate <$> c)&_center.~c
  where c = (db^._size <&> (/2))
-- |Pads the boxed value by the given vector
padded v b = b&_size%~(*+*v)&_center%~(*+*hv)&_draw.~draw (moved b hv)
  where hv = v<&>(/2)
-- |Scale all dimensions
scaled' x = scaled (pure x)
-- |Color the box
colored c b = b&_draw%~ \dr -> getFloat4 Color4 GetCurrentColor >>= \c' -> color c >> dr >> color c'

align' :: Boxed d => (forall a. Lens' (Vector3 a) a) -> (GLfloat -> GLfloat) -> d -> d
align' x f b = b&_center.x.~f (b^._size.x)
data Alignment = T | B | L | R | C
align T = align' vy (max 0)
align B = align' vy (min 0)
align L = align' vx (min 0)
align R = align' vx (max 0)
align C = align' vx (/2) . align' vy (/2)

padding v = nullDraw&_size.~v&_center.~(v<&>(/2))
hPad w = padding (v3 w 0 0) ; vPad h = padding (v3 0 h 0)

-- |Glue Horizontally
glH = appendBy vx
-- |Glue Vertically
glV = appendBy vy . reverse
-- |Apppend horizontally
a #- b = glH [a,b]
-- |Apppend horizontally with a small padding
a #| b = glV [a,b]
-- |Apppend vertically
a #-# b = glH [a,hPad 10,b]
-- |Apppend vertically with a small padding
a #|# b = glV [a,vPad 10,b]
interBy :: Boxed d => (forall a. Lens' (Vector3 a) a) -> [d] -> d -> d
interBy l bs b = appendBy l (intersperse b bs)
(%-%) = interBy vx ; (%|%) = interBy vy . reverse
infixl 5 %-% ; infixl 4 %|% ; infixl 3 #- ; infixl 2 #| ; infixl 3 #-# ; infixl 2 #|#

-- |@para p s@ is a full box of size s at position p
para p s@(Vector3 w h d) =
  DrawBox (Box p (s<&>(/2)) s) $ renderPrimitive Quads $ sequence_
         [p1, p3, p7, p5
         , p5, p6, p8, p7
         , p1, p2, p4, p3
         , p1, p2, p6, p5
         , p3, p4, p8, p7
         , p2, p4, p8, p6]
  where [p1,p2,p3,p4,p5,p6,p7,p8] = liftM3 v [0,w] [0,h] [0,d]
-- |A cube
cube w = para (pure (-w/2)) (pure w)

-- |A DrawBox drawing the given text
textBox text = do
  [x,y,z,x',_,_] <- getFontBBox ?font text <&> map CFloat
  let vl = v3 x y z ; vc = v3 (vs^.vx/2) (?desc+36) (?depth/2+5)
      vs = v3 (x'-x) fontSize (?depth+10)
  return $ DrawBox (Box (pure 0) vc vs)
    $ translate (v3 0 (y- ?desc) 5*-*vl)
    >> renderFont ?font text All

cursor = padding (pure 0)
mkSubs :: [IO (DrawTree DrawBox)] -> IO ([DrawTree DrawBox],DrawTree DrawBox)
mkSubs ms = do
   s <- sequence ms
   let s' = [DrawTree a [(i,Node (movedTo a (pure 0)) (map snd s'))]
            | DrawTree a s' <- s | i <- [0..]]
       c = DrawTree cursor [(length s,Node (movedTo cursor (pure 0)) [])]
   return (s',c)
withSubs ms f = uncurry f<$>mkSubs ms

syntaxes = map syntaxBox
-- |A syntax's DrawTree 
syntaxBox s = runHooksMay layoutHooks ($s) >>= \t -> case t of
  Just t -> return t
  Nothing -> syntaxBox' s
syntaxBox' (Symbol a) = atomic . col . glH
                  <$> sequence [if has (_head.only ' ') g
                                then pure (hPad (30*fromIntegral (length g)))
                                else textBox g
                               | g <- "":groupBy (\a b -> (a==' ')==(b==' ')) a]
  where col | isJust (readMay a :: Maybe String) = colored brown
            | otherwise = id

syntaxBox' (Group g@[Symbol op,_]) | op`elem`["?","!","|","`","@"] = 
  withSubs (syntaxes g) $ \[op,a] cur -> op#-#a#- cur
syntaxBox' (Group g@(Symbol op:_:_:_)) | op`elem`["+","*","-","%","/",">>=",",","<=",">=","<",">",".","<>","=","<-","->"] = do
  o <- head subs
  withSubs subs $ \(op:arg:args) cur -> glH [arg,big op,(args%-%big o),cur]
  where toUnicode x = fromMaybe x $ lookup x [("<-","\x2190"),("<>","\x2260")
                                             ,(">=","\x2265"),("<=","\x2264")]
        subs = syntaxes (g&_head._Symbol%~toUnicode)
        big a = hPad 30#-a#-hPad 30
syntaxBox' (Group g@(Symbol "vert#":_:_:_)) = do
  ss <- sequence (syntaxes (tail g)) 
  let o = atomic (para (pure 0) (big^._size&vy.~5)&_center.~(big^._center))
      big = glV ss
  ((op:arg:args),cur) <- mkSubs $ map pure (o:ss)
  return $ glV [arg,op,(args%|%o),cur]
syntaxBox' (Group g@(Symbol "horiz#":_:_:_)) = do
  ss <- sequence (syntaxes (tail g)) 
  let o = atomic $ padded (pure 30) $ para (pure 0) (big^._size&vx.~5)&_center.~(big^._center)
      big = glH ss
  ((op:arg:args),cur) <- mkSubs $ map pure (o:ss)
  return $ glH [arg,op,(args%-%o),cur]
syntaxBox' (Group g@(Symbol "aref":_:_)) = do
  comma <- atomic <$> textBox ","
  withSubs subs $ \(aref:a:is) cur ->
    glH [a,aref,(scaled (pure 0.7) $ is%-%comma)&_center.vy%~(+30),cur]
  where subs = pure nullDraw:syntaxes (tail g)
syntaxBox' (Group (Symbol "lambda":Group vs:body)) = do
  dot <- atomic <$> textBox "."
  withSubs subs $ \(l:vars:subs) cur ->
    glH [([colored grey l,vars,dot]++subs) %-% hPad 10,cur]
  where subs = (atomic<$>textBox "\x03bb"):withSubs (syntaxes vs) lamVars:syntaxes body
syntaxBox' (Group (Symbol "match":subs)) | all (has (_Group.to length.only 2)) subs = do
  lam <- colored grey . atomic <$> textBox "\x03bb"
  withSubs (pure lam:map (\(Group [p,e]) -> matchBox p e) subs) $ \(l:bs) cur ->
    l#-#((map (align L) bs%|%vPad 10)
         #|# cur)
syntaxBox' (Group g@(Symbol "do":_)) = withSubs (syntaxes g) $ \(h:t) cur ->
  glV $ map (align L) (colored grey h:t++[cur])
syntaxBox' (Group g@(Symbol "define":_:_)) =
  withSubs (syntaxes g) $ \(h:d:t) cur -> align L (header h d)
                                          #|# align L (hPad 30 #- ((map (align L) t %|% vPad 10)
                                                                   #|# cur))
  where header h d = [colored grey h,d]%-%hPad 30
syntaxBox' (Group g@[Symbol "if",_,_,_]) = do
  oth <- atomic . colored grey <$> textBox "otherwise"
  withSubs (syntaxes g) $ \[o,c,th,el] cur ->
    centered $ glH $ map (align T) [column [glH [align T (colored grey o),hPad 30,align T c]
                                           ,vPad 10
                                           ,th]
                                   ,hPad 30
                                   ,column [oth,vPad 10,el]
                                   ,cur]
  where column = glV . map (align L)

syntaxBox' (Group g@(Symbol _:_)) = do
  [open,close,comma] <- mapM (fmap atomic . textBox) ["(",")",","]
  withSubs (syntaxes g) $ \(f:args) cur ->
    ([f,open]++intersperse comma args++[cur,close])%-%hPad 10
syntaxBox' (Group g) = do 
  [open,close] <- mapM (fmap atomic . textBox) ["[","]"]
  withSubs (syntaxes g) $ \subs cur ->
    glH $ [open]++intersperse (hPad 30) subs++[cur,close]

lamVars vs cur = glH [vs %-% hPad 30,cur]
matchBox p e = do
  arr <- padded (v3 30 0 0) . atomic<$>textBox "\x2192" 
  withSubs [patSyn p,syntaxBox e] $ \[p,e] cur -> p#-#arr#-#e#-cur
  where patSyn e@(Symbol _) = syntaxBox e
        patSyn (Group ps) = withSubs (syntaxes ps) lamVars

-- |The layout hooks
layoutHooks = mkHooks ([] :: [Syntax String -> IO (Maybe (DrawTree DrawBox))])

-- |Draw a drawable
draw b = translate (b^._pos) >> b^._draw
drawP = preservingMatrix . draw

