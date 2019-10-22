{-# LANGUAGE DataKinds, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
module Typekernel.Box where
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Typekernel.Structure
    import Typekernel.Nat
    import Data.Proxy
    import Typekernel.Memory
    import Typekernel.MLens
    data Box a =Box {boxPointer :: Memory N8, boxFinalizer :: C (), contentFinalizer :: C ()}
    instance (Structure n a)=>Structure N8 (Box a) where
        finalize box = do
            contentFinalizer box
            boxFinalizer box
        move mem box=do
            mem `assign` boxPointer box
            return $ box{boxPointer=mem}
    -- Get memory of box
    derefBox :: (Structure n a)=>Box a->C (Memory n)
    derefBox box = do
        addr<-mget (dword (Proxy :: Proxy Z)) $ boxPointer box
        ptr<-cast (Proxy :: Proxy (Ptr USize)) addr
        return $ Memory ptr
    
    -- Move something out of scope
    boxWith :: (KnownNat a, MemoryProvider p, Structure n a)=>p->(Memory n->C a)->(Memory N8->C (Box a))
    boxWith p ctor mem=do
        let boxsz=memSize (ctorType ctor)
        -- allocate memory
        ptr<-liftC $ malloc p boxsz
        a<-ctor ptr
        addr<-cast (Proxy :: Proxy USize) (memStart ptr)
        mset (dword (Proxy :: Proxy Z)) mem addr 
        return $ Box mem (free p ptr) (finalize a)
