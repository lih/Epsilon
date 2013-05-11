{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, ImplicitParams, ParallelListComp, ImpredicativeTypes #-}
module Box(module Box.Types, move,moveTo,centered,textBox,pad,sBox,draw) where

import Graphics.Rendering.OpenGL.GL.QueryUtils
import Data.Maybe
import Data.List
import Prelude hiding (sequence,mapM)
import Graphics hiding (cursor,T,R)
import Utils hiding (moveTo)
import Foreign.C.Types
import Box.Types
import Trees
import Font
import Data.List.HIUtils

glue :: (forall a. Lens' (Vector3 a) a) -> [(Sz,Pos)] -> (Sz,Pos,[Pos])
glue x bs = (sz,cen,zipWith f cs offsets)
  where vs = sequenceA ds ; (ds,cs) = unzip bs
        offsets = scanl (+) 0 (vs^.x)
        max' = fmap $ foldl max 0
        sz = cen *+* max' (sequenceA $ zipWith (*-*) ds cs) & x.~last offsets
        cen = max' (sequenceA cs) & x.~(last offsets/2)
        f c off = cen*-*c & x.~off
instance Drawable DrawBox where
  nullDraw = DrawBox (Box (pure 0) (pure 0) (pure 0)) (return())
  appendBy l bs = DrawBox (Box (pure 0) c sz) dr
    where (sz,c,ps) = glue l [(b^._size,b^._center) | b <- bs]
          dr = sequence_ [drawP (moveTo b p) | b <- bs | p <- ps]
  _draw = drawB
  _pos = box.bPos
  _size = box.bSize
  _center = box.bCenter
instance Drawable a => Drawable (DrawTree a) where
  nullDraw = DrawTree nullDraw []
  appendBy l ts = DrawTree (appendBy l bs) [(n,t) | (n,ts) <- ss', t <- ts]
    where (bs,ss) = unzip [(b,s) | DrawTree b s <- ts]
          ss' = aggregateAL $ concat [map (_2.root%~(`move`p)) s | (p,s) <- zip ps ss]
          (_,_,ps) = glue l [(b^._size,b^._center) | b <- bs]
  _draw = droot._draw
  _pos = droot._pos
  _size = droot._size
  _center = droot._center

makeDraw (Box p c s) d = nullDraw&_size.~s&_pos.~p&_center.~c&_draw.~d
draw b = translate (b^._pos) >> b^._draw
drawP = preservingMatrix . draw
move db v = db&_pos %~ (v*+*)
moveTo db v = db&_pos .~ v
centered db = moveTo db (negate <$> c)&_center.~c
  where c = (db^._size <&> (/2))
scaled s db = db&_center%~scl&_size%~scl&_draw%~ \d -> scalev s >> d
  where scl = liftA2 (*) s
scaled' x = scaled (pure x) 
align' :: Drawable d => (forall a. Lens' (Vector3 a) a) -> (GLfloat -> GLfloat) -> d -> d
align' x f b = b&_center.x.~f (b^._size.x)
data Alignment = T | B | L | R | C
align T = align' vy (max 0)
align B = align' vy (min 0)
align L = align' vx (min 0)
align R = align' vx (max 0)
align C = align' vx (/2) . align' vy (/2)

pad v b = b&_size%~(*+*v)&_center%~(*+*hv)&_draw.~draw (move b hv)
  where hv = v<&>(/2)
padding v = nullDraw&_size.~v&_center.~(v<&>(/2))
hPad w = padding (v3 w 0 0) ; vPad h = padding (v3 0 h 0)
colored c b = b&_draw%~ \dr -> getFloat4 Color4 GetCurrentColor >>= \c' -> color c >> dr >> color c'

glH = appendBy vx ; glV = appendBy vy . reverse
a #- b = glH [a,b] ; a #| b = glV [a,b]
a #-# b = glH [a,hPad 10,b] ; a #|# b = glV [a,vPad 10,b]
bs %-% b' = glH $ intersperse b' bs
bs %|% b' = glV $ intersperse b' bs
infixl 5 %-% ; infixl 4 %|% ; infixl 3 #- ; infixl 2 #| ; infixl 3 #-# ; infixl 2 #|#

textBox text = do
  [x,y,z,x',y',z'] <- getFontBBox ?font text <&> map CFloat
  let vl = v3 x y z ; vu = v3 x' y' z'
      vs = v3 (x'-x) fontSize (?depth+10) ; vc = v3 (vs^.vx/2) (?desc+36) (?depth/2+5)
  return $ DrawBox (Box (pure 0) vc vs) (translate (v3 0 (y- ?desc) 5*-*vl) >> renderFont ?font text All)

atomic = fmap $ \t -> DrawTree t []
cursor = padding (pure 5)

withSubs ms f = do
   s <- sequence ms
   let s' = [DrawTree a [(i,Node (moveTo a (pure 0)) (map snd s'))] | DrawTree a s' <- s | i <- [0..]]
       c = DrawTree cursor [(length s,Node (moveTo cursor (pure 0)) [])]
   return (f s' c)
syntaxes = map sBox 
sBox (Symbol a) = atomic (textBox a)
sBox (Group g@[Symbol op,_]) | op`elem`["?","!","|","`","@"] = 
  withSubs (syntaxes g) $ \[op,a] cur -> op#-#a#- cur
sBox (Group g@[Symbol op,_,_]) | op`elem`["+","*","-","%","/",">>=",",","<=",">=","<",">",".","<>","=","<-"] =
  withSubs subs $ \[op,a,b] cur -> glH [[a,op,b]%-%hPad 30,cur]
  where toUnicode x = fromMaybe x $ lookup x [("<-","\x2190"),("<>","\x2260")
                                             ,(">=","\x2265"),("<=","\x2264")]
        subs = syntaxes (g&_head._Symbol%~toUnicode)
sBox (Group g@(Symbol "aref":_:_)) = do
  comma <- atomic (textBox ",")
  withSubs subs $ \(aref:a:is) cur ->
    glH [a,aref,(is%-%comma)&_center.vy%~(+30),cur]
  where subs = pure nullDraw:syntaxes (tail g)
sBox (Group g@(Symbol "lambda":Group vs:body)) = do
  dot <- atomic (textBox ".")
  withSubs subs $ \(l:vars:subs) cur ->
    glH [([colored grey l,vars,dot]++subs) %-% hPad 10,cur]
  where subs = atomic (textBox "\x03bb"):withSubs (syntaxes vs) vars:syntaxes body
        vars vs cur = glH [vs %-% hPad 30,cur]
sBox (Group g@(Symbol "match":subs)) | all (has (_Group.to length.only 2)) subs = do
  lam <- colored grey <$> atomic (textBox "\x03bb")
  arr <- pad (v3 30 0 0) <$> atomic (textBox "\x2192")
  let matchBox (Group s) = withSubs (syntaxes s) $ \[p,e] cur -> p#-#arr#-#e#-cur
  withSubs (pure lam:map matchBox subs) $ \(l:bs) cur ->
    l#-#((map (align L) bs%|%vPad 10)
         #|# cur)
sBox (Group g@(Symbol "do":_)) = withSubs (syntaxes g) $ \(h:t) cur ->
  glV $ map (align L) (colored grey h:t++[cur])
sBox (Group g@(Symbol "define":_:_)) = withSubs (syntaxes g) f
  where f (h:d:t) cur = align L header
                        #|# align L (hPad 30 #- ((map (align L) t %|% vPad 10)
                                                 #|# cur))
          where header = [colored grey h,d]%-%hPad 30
sBox (Group g@[Symbol "if",_,_,_]) = do
  oth <- atomic (colored grey <$> textBox "otherwise")
  withSubs (syntaxes g) $ \[o,c,th,el] cur ->
    centered $ glH $ map (align T) [column [glH [align T (colored grey o),hPad 30,align T c]
                                           ,vPad 10
                                           ,th]
                                   ,hPad 30
                                   ,column [oth,vPad 10,el]
                                   ,cur]
  where column = glV . map (align L)

sBox (Group g@(Symbol _:_)) = do
  [open,close,comma] <- mapM (atomic . textBox) ["(",")",","]
  withSubs (syntaxes g) $ \(f:args) cur ->
    ([f,open]++intersperse comma args++[cur,close])%-%hPad 10
sBox (Group g) = groupBox g

groupBox g = do 
  [open,close] <- mapM (atomic . textBox) ["[","]"]
  withSubs (syntaxes g) $ \subs cur ->
    glH $ [open]++intersperse (hPad 30) subs++[cur,close]
