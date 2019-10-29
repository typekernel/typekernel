{-# LANGUAGE DataKinds, FlexibleInstances, FunctionalDependencies, UndecidableInstances, TypeFamilies, AllowAmbiguousTypes #-}
module Typekernel.Std.Box where
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Typekernel.Structure
    import Typekernel.Nat
    import Data.Proxy
    import Typekernel.Memory
    import Typekernel.MLens
    import Control.Monad.Trans.Class
    import Typekernel.Std.Basic

    data Box'

    type Box a = Typedef Box' (Basic USize)

    --data Box a =Box {boxPointer :: Memory N8}
    --type instance SizeOf (Box a)=N8
    boxType :: Box a->Proxy a
    boxType _ =Proxy
    --instance (Structure n a)=>Structure N8 (Box a) where
    --    restore _=return . Box
        {-
        finalize box = do
            content<-derefBox box
            finalize $ restore content
        move mem box=do
            mem `assign` boxPointer box
            return $ box{boxPointer=mem}
        -}
    --instance (MonadHeap m, Structure n a, MonadC m, Lifetime a m)=>Lifetime (Box a) m where
    --    finalize box=do
    --        content<-liftC $ derefBox box
    --        ref<-liftC $ restore (boxType box) content
    --        finalize ref
    -- Get memory of box
    boxPointer :: Box a->Memory N8
    boxPointer (Typedef mem)=mem
    derefBox :: (Structure n a)=>Box a->C (Memory n)
    derefBox box = do
        addr<-mget (dword (Proxy :: Proxy Z)) $ boxPointer box
        ptr<-cast (Proxy :: Proxy (Ptr USize)) addr
        return $ Memory ptr
    
    -- Move something out of scope
    boxWith :: (KnownNat n, Structure n a, MonadHeap m, MonadC m)=>(Memory n->m a)->(Memory N8->m (Box a))
    boxWith ctor mem=do
        let boxsz=memSize (ctorType ctor)
        -- allocate memory
        ptr<-mallocL boxsz
        a<-ctor ptr
        addr<-liftC $ cast (Proxy :: Proxy USize) (memStart ptr)
        liftC $ mset (dword (Proxy :: Proxy Z)) mem addr 
        return $ Typedef mem
    