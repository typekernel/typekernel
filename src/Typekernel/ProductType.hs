{-# LANGUAGE FunctionalDependencies, DataKinds, FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies, GADTs, FlexibleContexts, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Typekernel.ProductType where
    import Typekernel.Structure
    import Typekernel.Nat
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Data.Proxy
    import Typekernel.Memory
    -- Experimental Product Type. Using fstS and sndS to distinguish two sides.
    class ProductType a b (m::Nat) (n::Nat) | a->m, b->n where
        data Product a b
        fstS :: Product a b->C a
        sndS :: Product a b->C b
    proda :: (ProductType a b m n)=>Product a b->Proxy a
    prodb :: (ProductType a b m n)=>Product a b->Proxy b
    proda _=Proxy
    prodb _=Proxy
    prodm :: (ProductType a b m n)=>Product a b->Proxy (m::Nat)
    prodm _=Proxy
    prodn :: (ProductType a b m n)=>Product a b->Proxy (n::Nat)
    prodn _=Proxy
    
    
    type instance SizeOf (Product a b)=(NAdd (NUpRound8 (SizeOf a)) (NUpRound8 (SizeOf b)))
    instance (KnownNat m, KnownNat n, Structure m a, Structure n b)=>ProductType a b m n where
        data Product a b=Product {productMem :: Memory (NAdd (NUpRound8 (SizeOf a)) (NUpRound8 (SizeOf b)))}
        fstS prod=do
            let offset=Proxy :: Proxy Z
            let size=prodm prod
            submem<-unsafeSubmemory (offset, size) (productMem prod)
            restore (proda prod) submem
        sndS prod=do
            let size=prodn prod
            msz<-immUSize $ fromIntegral $ roundUp $ prodm prod
            submem<-unsafeSubmemory' (msz, size) (productMem prod)
            restore (prodb prod) submem
        
    instance (ProductType a b m n, Structure m a, Structure n b, (NAdd (NUpRound8 (SizeOf a)) (NUpRound8 (SizeOf b))) ~ c)=>Structure c (Product a b) where
        restore _=return . Product
    
    ctorProd :: (KnownNat m, KnownNat n, MonadC env, ProductType a b m n, Structure m a, Structure n b)=>(Memory m->env a)->(Memory n->env b)->(Memory (SizeOf (Product a b)))->env (Product a b)
    ctorProd ca cb mem = do
        prod<-liftC $ restore (Proxy::Proxy (Product a b)) mem
        let offset=Proxy :: Proxy Z
        let size=prodm prod
        submem<-liftC $ unsafeSubmemory (offset, size) (productMem prod)
        ca submem
        let size=prodn prod
        msz<-liftC $ immUSize $ fromIntegral $ roundUp $ prodm prod
        submem<-liftC $ unsafeSubmemory' (msz, size) (productMem prod)
        cb submem
        return prod

    instance (MonadC env, KnownNat m, KnownNat n, ProductType a b m n, Structure m a, Structure n b, Lifetime a env, Lifetime b env)=>Lifetime (Product a b) env where
        --finalize :: a->m ()
        finalize prod=do
            n1<-liftC $ fstS prod
            finalize n1
            n2<-liftC $ sndS prod
            finalize n2