{-| A module for hooks -}
module Hooks(
  -- * Hooks
  Hooks,
  mkHooks,(<+<),
  runHooks,runHooksMay,
  -- * HookVars
  HookVar,
  hookVar,mkHookVar,varHooks
  ) where

import Graphics(($~),HasSetter(..),HasGetter(..))
import Utils

-- |An opaque type describing a reference that you may only add to
newtype Hooks a = H (IORef [a])
-- |UNSAFE creates hooks from an initial list
mkHooks l = unsafePerformIO $ H <$> newIORef l
-- |Adds a hook to the given Hooks
H hs <+< h = hs $~ (h:)

-- |Run the given hooks, applying the function to each in turn
runHooks (H h) rh = mapM_ rh =<< get h
-- |Run the given hooks until the first to return a Just value
runHooksMay (H h) rh = join <$> (findM isJust . map rh =<< get h)

-- |An opaque type describing a variable with hooks that get called after the variable gets modified.
newtype HookVar v a = HV (v a,Hooks (IO()))
-- |Create a HookVar from a variable and Hooks
hookVar v hs = HV (v,hs)
-- |UNSAFE Create a HookVar from a pure value
mkHookVar = unsafePerformIO . mkHookVar'
mkHookVar' v = hookVar<$>newIORef v<*>(H<$>newIORef [])
instance HasGetter v => HasGetter (HookVar v) where
  get (HV (v,_)) = get v
instance HasSetter v => HasSetter (HookVar v) where
  HV (v,hs) $= x = v $= x >> runHooks hs id
-- |The hooks associated with the given HookVar
varHooks (HV (_,hs)) = hs
