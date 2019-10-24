{-# LANGUAGE FunctionalDependencies, DataKinds, FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies, GADTs, FlexibleContexts, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Typekernel.Structure where
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Typekernel.Nat
    import Data.Proxy
    import Typekernel.MLens
    import Typekernel.RAII
    import Typekernel.Memory
    import Control.Monad.Trans.Reader
    import Data.IORef
    import Control.Monad
    import Control.Monad.Fix
    import Control.Applicative
    import Control.Monad.Trans.Class
    import Control.Monad.IO.Class
    import Typekernel.Unsafe
    class (KnownNat n)=>Sized a n | a->n where
        size :: Proxy a->Proxy n

    -- Instance a is a structure, with size n.
    class Structure (n::Nat) a  | a->n where
        --move :: (Memory n)->a->m a
        restore :: Proxy a->(Memory n)->C a
        --finalize :: a->m ()
        --finalize _ = return ()
    -- Something that can only be constructed and finalized in monad m.
    class (Monad m)=>Lifetime a m where
        finalize :: a->m ()
        finalize _=return ()
    sizeof :: (Structure n a)=>a->Proxy n
    sizeof _=Proxy
    --instance (Structure n a)=>Resource a where
    --    ondrop=finalize
    memSize :: (Structure n a)=>Proxy a->Proxy n
    memSize _=Proxy
    ctorType :: (Memory n->m a)->Proxy a
    ctorType _=Proxy

    data Scoped a s m=Scoped {scopedValue :: a, scopeFinHandle :: FinalizerHandle s m}
    scope :: (MonadIO m, Lifetime a m)=>a->RAII s m (Scoped a s m)
    scope v = do
        handle<-onExit (finalize v)
        return $ Scoped v handle
    useScope :: (Scoped a s m)->(a->RAII s m ())->RAII s m ()
    useScope x op =op $ scopedValue x
    instance (MonadIO m, Lifetime a m)=>Move m (Scoped a) where
        dup v=do
            forget $ scopeFinHandle v
            handle<-lift $ onExit (finalize $ scopedValue v)
            return $ Scoped (scopedValue v) handle
    instance Forget (Scoped a) where
        forget=forget . scopeFinHandle
    construct :: (KnownNat n, Structure n a, Lifetime a m, MonadC m, MonadIO m)=>(Memory n->m a)->RAII s m (Scoped a s m)
    construct ctor= do
        mem<-liftC $ defarr (memSize $ ctorType ctor)
        obj<-lift $ ctor mem
        scope obj

    -- Represents some heap memory allocator.
    class MemoryProvider p where
        malloc :: (KnownNat n)=>p->(Proxy n)->C (Memory n)
        free :: (KnownNat n)=>p->(Memory n)->C ()
        malloc' :: p->USize->C (Ptr USize)
        free' :: p->USize->(Ptr USize)->C ()
        malloc p pn=do
            let sz=natToInt pn
            csz<-immUSize $ fromIntegral sz
            ptr<-malloc' p csz
            return $ Memory ptr
        free p mem=do
            let sz=natToInt $ proxyMVal mem
            csz<-immUSize $ fromIntegral sz
            let (Memory ptr)=mem
            free' p csz ptr
            return ()
    alloc :: (MemoryProvider p, KnownNat n)=>p->Proxy n->C (Memory n, C ())
    alloc p proxy=do
        mem<-malloc p proxy
        return $ (mem, free p mem)
    
    -- Represents some calculation that requires heap.
    -- Of course you can leak something out of heap monad and use it freely.
    -- But the general good idea is to use the heap monad globally.
    -- Anyway, this allows us to use multiple heaps, without having to use a "heap" parameter everywhere.
    newtype Heap p m a = Heap {heapToReader :: ReaderT p m a}
        deriving ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadFix
        , MonadTrans
        , MonadIO
        )
    instance MonadC m=>MonadC (Heap p m) where
        liftC a=Heap $ lift $ liftC a
    runHeap :: (MemoryProvider p, MonadC m)=>p->(Heap p m a)-> m a
    runHeap allocator heap=do
        let reader=heapToReader heap
        runReaderT reader allocator
    mallocM :: (MemoryProvider p, MonadC m, KnownNat n)=>Proxy n->Heap p m (Memory n)
    mallocM p= Heap $ do
            allocator<-ask
            lift $ liftC $ malloc allocator p
    freeM :: (MemoryProvider p, MonadC m, KnownNat n)=>Memory n->Heap p m ()
    freeM m=Heap $ do
            allocator<-ask
            lift $ liftC $ free allocator m
    mallocM' :: (MemoryProvider p, MonadC m)=>USize->Unsafe (Heap p m) (Ptr USize)
    mallocM' sz=claimUnsafe $ Heap $ do
            allocator<-ask
            lift $ liftC $ malloc' allocator sz
    freeM' :: (MemoryProvider p, MonadC m)=>USize->(Ptr USize)->Unsafe (Heap p m) ()
    freeM' sz ptr=claimUnsafe $ Heap $ do
            allocator<-ask
            lift $ liftC $ free' allocator sz ptr
    class (Monad m)=>MonadHeap m where
        mallocL :: (KnownNat n)=>Proxy n->m (Memory n)
        freeL :: (KnownNat n)=>Memory n->m ()
        mallocL' :: USize->Unsafe m (Ptr USize)
        freeL' :: USize->Ptr USize->Unsafe m ()
    instance (MonadC m, MemoryProvider p)=>MonadHeap (Heap p m) where
        mallocL=mallocM
        freeL=freeM
        mallocL'=mallocM'
        freeL'=freeM'

    
    instance (MonadHeap m)=>MonadHeap (RAII s m) where
        mallocL=RAII . lift . mallocL
        freeL=RAII . lift . freeL
        mallocL'=claimUnsafe . RAII . lift . runUnsafe . mallocL'
        freeL'=((claimUnsafe .RAII . lift . runUnsafe) .). freeL'

    type family SizeOf a :: Nat
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
    round8 :: Proxy (a::Nat)->Proxy (NUpRound8 a)
    round8 _=Proxy
    
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

    
    data Typedef t a=Typedef {newtypeMem :: Memory (SizeOf a)}

    type instance SizeOf (Typedef t a)=SizeOf a

    untypeProxy :: (Typedef t a)->Proxy a
    untypeProxy _=Proxy
    untypedef :: (Structure m a, SizeOf a ~ m)=>Typedef t a->C a
    untypedef x=restore (untypeProxy x) $ newtypeMem x
    instance (SizeOf a ~ m, Structure m a)=>(Structure m (Typedef t a)) where
        restore _ =return . Typedef
    instance (MonadC env, Lifetime a env, Structure m a, SizeOf a ~ m)=>Lifetime (Typedef t a) env where
        finalize x = do
            y<-liftC $ untypedef x
            finalize y
    newtypeSize :: Typedef t a->Proxy (SizeOf a)
    newtypeSize _ = Proxy
    ctorNewtype :: (KnownNat n, MonadC m, Structure n a, SizeOf a ~ n)=>(Memory n->m a)->(Memory n->m (Typedef t a))
    ctorNewtype ctor mem = do
        obj<-liftC $ restore (Proxy::Proxy (Typedef t a)) mem
        let offset=Proxy :: Proxy Z
        let size=newtypeSize obj
        submem<-liftC $ unsafeSubmemory (offset, size) (newtypeMem obj)
        ctor submem
        return obj

    data Phantom = Phantom {phantomMem :: Memory Z}
    type instance SizeOf Phantom=Z
    instance Structure Z Phantom where
        restore _ = return . Phantom

    type family SumSize a b :: Nat where
        SumSize a () = NAdd (SizeOf a) N8
        SumSize a b = NMax (S (SizeOf a)) (SizeOf b)
    class SumType a b (m::Nat) (n::Nat) | a->m, b->n where
        data Sum a b
        matchS :: (FirstClassList r, MonadC env)=>(a->env r)->(b->env r)->env r

    type instance SizeOf (Sum a b) = SumSize a b

    suma :: (SumType a b m n)=>Sum a b->Proxy a
    sumb :: (SumType a b m n)=>Sum a b->Proxy b
    suma _=Proxy
    sumb _=Proxy
    summ :: (SumType a b m n)=>Sum a b->Proxy (m::Nat)
    summ _=Proxy
    sumn :: (SumType a b m n)=>Sum a b->Proxy (n::Nat)
    sumn _=Proxy
    instance (KnownNat m, Structure m a)=>SumType a () m Z where
        data Sum a ()=Sum {sumMem :: Memory (SizeOf (Sum a ()))}

    --instance (KnownNat m, KnownNat n, Structure q c, SumType a b m n)=>SumType 