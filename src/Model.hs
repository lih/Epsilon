module Model where

import Hooks
import Utils hiding (focus)
import Graphics
import Trees

expr = mkHookVar ([] :: [Syntax String])
focus = mkHookVar ((0,[]) :: (Int,[Int]))
clipboard = mkRef Nothing
_focus :: Lens' (Int,[Int]) [Int]
_focus = iso (\(x,t) -> reverse (x:t)) (\l -> case reverse l of (x:t) -> (x,t) ; _ -> (0,[]))

l ~~ f = get focus >>= \i -> expr $~ (_group.from _h._forestAt (reverse (i^._focus)).l%~f)
getF i = view (_group.from _h._forestAt (reverse (i^._focus))) <$> get expr 
infixr 4 ~~

focusLeft = do
  f <- getF =<< get focus
  let m 0 = length f ; m n = n-1
  focus $~ (_focus._head%~m)
focusRight = do
  f <- getF =<< get focus
  let m _ | null f = 0 ; m n = n+1
  focus $~ (_focus._head%~m)
focusDown = tryMod (_focus%~(0:)) (has (_head._Group))
focusUp = focus $~ (_2%~initSafe)
tryMod f p = get focus >>= \i -> fmap p (getF i) >>= \b -> if b then focus $= f i else return ()

dragLeft = dragBy focusLeft
dragRight = dragBy focusRight
dragUp = dragBy focusUp
dragDown = dragBy focusDown
dragBy foc = get focus >>= getF >>= \f -> case f of
  [] -> return ()
  (x:_) -> deleteNode >> foc >> id ~~ (x:)

insertSym = id ~~ (Symbol "":)
insertGroup = id ~~ (Group []:) >> focusDown
replaceWithSym = deleteNode >> insertSym
wrapNode = _head ~~ (Group . return) >> focusDown >> focusRight
deleteNode = id ~~ tailSafe
openGroup = deleteNode >> insertGroup >> insertSym
closeGroup = focusUp >> focusRight >> insertSym
outsertSym = focusRight >> insertSym
copyNode = get focus >>= getF >>= \l -> clipboard $= listToMaybe l
pasteNode = get clipboard >>= maybe (return ()) (\n -> id ~~ (n:))

modText f = _head._Symbol ~~ f >> postRedisplay Nothing
delChar = modText initSafe

