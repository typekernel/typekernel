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