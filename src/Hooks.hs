module Hooks(Hooks,mkHooks,runHooks,(<+<),HookVar,mkHookVar,varHooks) where

import Graphics(($~),HasSetter(..),HasGetter(..))
import Utils
import System.IO.Unsafe

-- |An opaque type describing a reference that you may only add to
newtype Hooks a = H (IORef [a])
mkHooks l = unsafePerformIO $ H <$> newIORef l
runHooks (H h) rh = mapM_ rh =<< get h
H hs <+< h = hs $~ (h:)
  
-- |An opaque type describing a reference which may be hooked
newtype HookVar a = HV (IORef a,Hooks (IO()))
mkHookVar = unsafePerformIO . mkHookVar'
mkHookVar' = newIORef >=> \ra -> do
  hs <- H <$> newIORef []
  return $ HV (ra,hs)
instance HasGetter HookVar where
  get (HV (v,_)) = get v
instance HasSetter HookVar where
  HV (v,hs) $= x = v $= x >> runHooks hs id
varHooks (HV (_,hs)) = hs


