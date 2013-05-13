{-# LANGUAGE DeriveFunctor, Rank2Types #-}
module Model(
  -- * Control variables
  trees,selection,clipboard,
  InsertMode(..),
  currentNode,currentForest,

  -- * Utilities
  _focus,

  selectLeft,selectRight,selectDown,selectUp,
  dragLeft,dragRight,dragDown,dragUp,

  insertSym,insertGroup,deleteNode,

  replaceWithSym,wrapNode,
  openGroup,closeGroup,outsertSym,
  
  copyNode,pasteNode,

  modText,delChar,pushChar
  ) where

import Hooks
import Utils
import Graphics
import Trees
import ELisp.ELVal

-- |The insertion mode (used in expressions like @currentNode $= Insert n@)
data InsertMode a = Insert a | Current a | Delete
                  deriving Functor
instance ELValLike a => ELValLike (InsertMode a) where
  fromELVal (List [Sym n _,a]) | intern "cur rent"==n = Current<$>fromELVal a
                               | n==intern "insert" = Insert<$>fromELVal a
                               | otherwise = Nothing
  fromELVal (Sym n _) | n==intern "delete" = Just Delete
                      | otherwise = Nothing
  fromELVal _ = Nothing
  toELVal (Current a) = List [mkSym "current",toELVal a] 
  toELVal (Insert a) = List [mkSym "insert",toELVal a]
  toELVal Delete = mkSym "delete" 

-- |A read-only view of the model
trees = makeGettableStateVar (get trees')
trees' = mkRef ([] :: [Syntax String])

-- |The index of the current node
selection = mkHookVar ((0,[]) :: (Int,[Int]))
-- |The clipboard used by copy/paste operations
clipboard = mkRef Nothing

-- |A read-only view of the current subForest
currentForest = makeGettableStateVar (get currentForest')
currentForest' = makeStateVar getC setC
  where getC = get selection >>= \i -> get trees'<&>view (l i)
        setC v = get selection >>= \i -> trees'$~(l i.~v)
        l :: (Int,[Int]) -> Traversal' [Syntax String] [Syntax String]
        l (r,t) = _group.from _h._forestAt (r:t)
-- |The current node
currentNode = hookVar (makeStateVar getN setN) (mkHooks [])
  where getN = get currentForest<&> \x -> case x of
          [] -> Delete
          (x:_) -> Current x
        setN n = currentForest'$~(case n of
                                     Current n -> _head.~n
                                     Insert n -> (n:)
                                     Delete -> tailSafe)

-- |A lens from a selection to a focus
_focus :: Lens' (Int,[Int]) [Int]
_focus = iso (\(x,t) -> reverse (x:t)) (\l -> case reverse l of (x:t) -> (x,t) ; _ -> (0,[]))

-- |Move the selection left with wrap-around
selectLeft = do
  f <- get currentForest
  let m 0 = length f ; m n = n-1
  selection $~ (_focus._head%~m)
-- |Move the selection right with wrap-around
selectRight = do
  f <- get currentForest
  let m | null f = const 0 | otherwise = (1+)
  selection $~ (_focus._head%~m)
-- |Move the selection down the selected group
selectDown = tryMod (_focus%~(0:)) (has (_head._Group))
-- |Move the selection up
selectUp = selection $~ (_2%~initSafe)
tryMod f p = do
  i <- get selection
  b <- p<$>get currentForest
  when b (selection $= f i)

-- |Drag the selected node left with wrap-around
dragLeft = dragBy selectLeft
-- |Drag the selected node right with wrap-around
dragRight = dragBy selectRight
-- |Drag the selected node up
dragUp = dragBy selectUp
-- |Drag the selected node down the next group
dragDown = dragBy selectDown
dragBy foc = get currentNode >>= \f -> case f of
  Delete -> return ()
  Current x -> deleteNode >> foc >> currentNode $= Insert x
  _ -> error "Impossible: get currentNode should not return an Insert"

insertSym = currentNode $= Insert (Symbol "")
insertGroup = currentNode $= Insert (Group []) >> selectDown
replaceWithSym = deleteNode >> insertSym
wrapNode = currentNode $~ fmap (Group . return) >> selectDown >> selectRight
deleteNode = currentNode $= Delete
openGroup = deleteNode >> insertGroup >> insertSym
closeGroup = selectUp >> selectRight >> insertSym
outsertSym = selectRight >> insertSym
copyNode = get currentForest >>= (clipboard $=) . listToMaybe
pasteNode = get clipboard >>= maybe (return ()) ((currentNode $=) . Insert)

modText f = currentNode $~ fmap (_Symbol%~f)
delChar = modText initSafe
pushChar c = modText (++[c])

