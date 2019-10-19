{-# LANGUAGE RankNTypes #-}
module Typekernel.MLens where
    import Data.Functor.Constant
    import Data.Functor.Identity
    import Control.Monad.Representable.Reader
    import qualified Data.Traversable as T
    type MLens m s a=forall f. (Traversable f)=>(a->m (f a))->(s-> m (f ()))
    
    mkMLens :: (Monad m)=>(s->m a)->(s->a->m ())->MLens m s a
    mkMLens g s f x=g x >>= f >>= T.mapM (s x)

    mget :: (Monad m)=>MLens m s a->s->m a
    mget l s=do
        let getter a=return $ Constant a
        v<-l getter s
        return $ getConstant v
    
    mset :: (Monad m)=>MLens m s a->(s->a->m ())
    mset l s v=do
        let setter a=return $ Identity v
        v<-l setter s
        return $ runIdentity v
    mover :: (Monad m)=>MLens m s a->(a->m a)->s->m ()
    mover l f s=do
        let mapper a=do
                a'<-f a
                return $ Identity a'
        v<-l mapper s
        return $ runIdentity v
    mview :: (MonadReader s m, Monad m)=>MLens m s a->m a
    mview l = do
        s<-ask
        mget l s
    
    -- CoW Lens. Create a duplicate of element on every modification.
    type CLens m s a=forall f. (Traversable f)=>(a->m (f a))->(s-> m (f s))

    mkCLens :: (Monad m)=>(s->m a)->(s->a->m s)->CLens m s a
    mkCLens g s f x=g x >>= f >>= T.mapM (s x)

    cget :: (Monad m)=>CLens m s a->s->m a
    cget l s=do
        let getter a=return $ Constant a
        v<-l getter s
        return $ getConstant v
    
    cset :: (Monad m)=>CLens m s a->(s->a->m s)
    cset l s v=do
        let setter a=return $ Identity v
        v<-l setter s
        return $ runIdentity v
    cover :: (Monad m)=>CLens m s a->(a->m a)->s->m s
    cover l f s=do
        let mapper a=do
                a'<-f a
                return $ Identity a'
        v<-l mapper s
        return $ runIdentity v
    cview :: (MonadReader s m, Monad m)=>CLens m s a->m a
    cview l = do
        s<-ask
        cget l s

    -- Lens composation
    -- Composation of MLens
    (|>) :: (Monad m)=>MLens m s a->MLens m a b->MLens m s b
    (|>) la lb=mkMLens getter setter where
        getter s=do
            a<-mget la s
            mget lb a
        setter s b=do
            a<-mget la s
            mset lb a b
            mset la s a
    -- Composation of CLens: function composition is enough.

    -- Composation of CLens after MLens
    (|.) :: (Monad m)=>MLens m s a->CLens m a b->MLens m s b
    (|.) la lb=mkMLens getter setter where
        getter s=do
            a<-mget la s
            cget lb a
        setter s b=do
            a<-mget la s
            c<-cset lb a b
            mset la s a
    
    -- Composation of MLens after CLens is a bad idea!
    -- No lens used in code generation should allow this to happen.