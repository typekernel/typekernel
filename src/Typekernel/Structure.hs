{-# LANGUAGE FunctionalDependencies, DataKinds, FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies, GADTs, FlexibleContexts, RankNTypes, UndecidableInstances #-}
module Typekernel.Structure where
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Typekernel.Nat
    import Data.Proxy
    import Typekernel.MLens
    import Typekernel.RAII
    import Typekernel.Memory
    class (KnownNat n)=>Sized a n | a->n where
        size :: Proxy a->Proxy n

    class Structure (n::Nat) a  | a->n where
        move :: (Memory n)->a->C a
        finalize :: a->C ()
        finalize _ = return ()

    sizeof :: (Structure n a)=>a->Proxy n
    sizeof _=Proxy
    instance (Structure n a)=>Resource a where
        ondrop=finalize
    memSize :: (Structure n a)=>Proxy a->Proxy n
    memSize _=Proxy
    ctorType :: (Memory n->C a)->Proxy a
    ctorType _=Proxy


    construct :: (KnownNat n, Structure n a, MonadC m)=>(Memory n->C a)->RAII s m (Scoped a (RAII s m))
    construct ctor= do
        mem<-liftC $ defarr (memSize $ ctorType ctor)
        obj<-liftC $ ctor mem
        scope obj

    -- Represents some heap memory.
    class MemoryProvider p where
        malloc :: p->(Proxy n)->C (Memory n)
        free :: p->(Memory n)->C ()
    alloc :: MemoryProvider p=>p->Proxy n->C (Memory n, C ())
    alloc p proxy=do
        mem<-malloc p proxy
        return $ (mem, free p mem)
    