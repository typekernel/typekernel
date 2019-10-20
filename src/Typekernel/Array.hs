{-# LANGUAGE FlexibleContexts, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
module Typekernel.Array where
    import Typekernel.C4mAST
    import Data.Proxy
    import Typekernel.Nat

    ptrType :: (Ptr a)->Proxy a
    ptrType _=Proxy
    promotePtr :: (Proxy a)->(Proxy (Ptr a))
    promotePtr _=Proxy
    demotePtr :: (Proxy (Ptr a))->(Proxy a)
    demotePtr _=Proxy
   