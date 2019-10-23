{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Unsafe identity monad.
-- Operations tagged as unsafe must be claimed explicitly.
module Typekernel.Unsafe where
    import Control.Monad.Trans.Identity
    import Control.Monad.Trans.Class
    import Control.Monad.Fix
    import Typekernel.Transpiler
    newtype Unsafe m a = Unsafe {toIdentity :: IdentityT m a} deriving (Monad, Applicative, Functor, MonadTrans, MonadFix)

    instance (MonadC m)=>MonadC (Unsafe m) where
        liftC a = Unsafe $ lift $ liftC a
        
    claimUnsafe = Unsafe . IdentityT
    runUnsafe = runIdentityT . toIdentity