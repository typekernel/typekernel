{-# LANGUAGE RankNTypes, TypeOperators, GeneralizedNewtypeDeriving, KindSignatures #-}
module Typekernel.RAII where
    import Control.Monad.Trans.Reader
    import Data.IORef
    import Typekernel.Transpiler
    import Typekernel.C4mAST
    import Control.Monad
    import Control.Monad.Fix
    import Control.Applicative
    import Control.Monad.Trans.Class
    import Control.Monad.IO.Class

    newtype RAII s m a=RAII {
            unRAII :: ReaderT (IORef [RefCountedFinalizer]) m a
            }deriving ( Functor
            , Applicative
            , Alternative
            , Monad
            , MonadPlus
            , MonadFix
            , MonadTrans
            , MonadIO
            
            )
    
    type Finalizer = C ()
    type RefCnt=Int
    data RefCountedFinalizer = RefCountedFinalizer {
        finalizer :: !Finalizer,
        refCount :: !(IORef RefCnt)
    }
    --
    data FinalizerHandle (r :: * -> * ) = FinalizerHandle RefCountedFinalizer

    onExit :: MonadC m=>Finalizer->RAII s m (FinalizerHandle (RAII s m))
    onExit finalizer=RAII $ do
        ioref<-ask
        lift $ liftC $ do
                rc<-liftIO $ newIORef 1
                let h=RefCountedFinalizer finalizer rc
                liftIO $ modifyIORef ioref (h:)
                return $ FinalizerHandle h
    instance MonadC m=>MonadC (RAII s m) where
        liftC a=RAII $ lift $ liftC a
    runRAII :: MonadC m=>(forall s. RAII s m a)->m a
    runRAII r=bracketc before after thing
        where
            bracketc before after thing=do
                val<-before
                ret<-thing val
                after val
                return $ ret
            before=liftC $ liftIO $ newIORef []
            thing=runReaderT (unRAII r)
            after ref=do
                droppers<-liftC $ liftIO $ readIORef ref
                forM_ droppers $ \fin->do
                    refCnt <- liftC $ liftIO $ decrement $ refCount fin
                    when (refCnt == 0) $ liftC $ finalizer fin
                    where
                        decrement :: IORef RefCnt -> IO RefCnt
                        decrement ioRef = atomicModifyIORef ioRef $ \refCnt ->
                                            let refCnt' = refCnt - 1
                                            in (refCnt', refCnt')
    
    
    class Move h where
        dup :: MonadC m=> h (RAII cs (RAII ps m))->RAII cs (RAII ps m) (h (RAII ps m))

    instance Move FinalizerHandle where
        dup handle=do
            let increment ioRef = atomicModifyIORef ioRef $ \refCnt ->
                    let refCnt' = refCnt + 1
                    in (refCnt', refCnt')
            let FinalizerHandle h=handle
            liftC $ liftIO $ increment $ refCount h
            return (FinalizerHandle h)
    
    data Scoped a (r :: * -> * )=Scoped {scopedValue :: a, resFinalizer :: FinalizerHandle r}

    class Resource a where
        ondrop :: a->C ()
    --instance (Resource a)=>Move (Scoped a) where
        --dup handle=do
            --return dup $ 
    scope :: (Resource a, MonadC m)=>a->RAII s m (Scoped a (RAII s m))
    scope a = do
        handle<-onExit $ ondrop a
        return $ Scoped a handle