{-# LANGUAGE RankNTypes, TypeOperators, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses #-}
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
            unRAII :: ReaderT (IORef [RefCountedFinalizer m]) m a
            }deriving ( Functor
            , Applicative
            , Alternative
            , Monad
            , MonadPlus
            , MonadFix
            , MonadIO
            
            )
    instance MonadTrans (RAII s) where
        lift x=RAII $ lift x
    type Finalizer m= m ()
    type RefCnt=Int
    data RefCountedFinalizer m= RefCountedFinalizer {
        finalizer :: !(Finalizer m),
        refCount :: !(IORef RefCnt)
    }
    --
    data FinalizerHandle s m = FinalizerHandle (RefCountedFinalizer m)


    onExit :: MonadIO m=>Finalizer m->RAII s m (FinalizerHandle s m)
    onExit finalizer=RAII $ do
        ioref<-ask
        lift $ do
                rc<-liftIO $ newIORef 1
                let h=RefCountedFinalizer finalizer rc
                liftIO $ modifyIORef ioref (h:)
                return $ FinalizerHandle h
    instance MonadC m=>MonadC (RAII s m) where
        liftC a=RAII $ lift $ liftC a
    runRAII :: MonadIO m=>(forall s. RAII s m a)->m a
    runRAII r=bracketc before after thing
        where
            bracketc before after thing=do
                val<-before
                ret<-thing val
                after val
                return $ ret
            before=liftIO $ newIORef []
            thing=runReaderT (unRAII r)
            after ref=do
                droppers<-liftIO $ readIORef ref
                forM_ droppers $ \fin->do
                    refCnt <- liftIO $ decrement $ refCount fin
                    when (refCnt == 0) $  finalizer fin
                    where
                        decrement :: IORef RefCnt -> IO RefCnt
                        decrement ioRef = atomicModifyIORef ioRef $ \refCnt ->
                                            let refCnt' = refCnt - 1
                                            in (refCnt', refCnt')
    
    -- You can move h out as long as the basis monad of parent scope is m.
    class Move m h where
        dup :: (MonadIO m)=> (h cs (RAII ps m))->RAII cs (RAII ps m) (h ps m)
        
    class Forget h where
        forget :: MonadIO m => (h cs m)->RAII cs m ()
    -- In our means of FinalizerHandle, moving something out of scope is no longer a trivial thing.
    -- You have to prove that you may move something out by yourself.
    -- This is done by the sense that you can construct (RAII cs (RAII ps m) (h ps m)) by h itself.
    
    instance Forget FinalizerHandle where
        forget handle=do
            let increment ioRef = atomicModifyIORef ioRef $ \refCnt ->
                    let refCnt' = refCnt + 1
                    in (refCnt', refCnt')
            let FinalizerHandle h=handle
            liftIO $ increment $ refCount h
            return ()
    
    {-
    data Scoped a s m=Scoped {scopedValue :: a, resFinalizer :: FinalizerHandle s m}

    
    class Resource a where
        ondrop :: a->C ()
        --onmove :: a->C a
        -- By default: do nothing when dropped.
        ondrop=const $ return ()
        
    instance (Resource a)=>Move (Scoped a) where
        dup handle=do
            fin<-dup $ resFinalizer handle
            return $ handle{resFinalizer=fin}
    scope :: (Resource a, MonadC m)=>a->RAII s m (Scoped a (RAII s m))
    scope a = do
        handle<-onExit $ ondrop a
        return $ Scoped a handle

    -}