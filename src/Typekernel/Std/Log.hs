{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Typekernel.Std.Log where
    import Typekernel.C4mAST
    import Typekernel.Std.StringLiteral
    class (MonadLog m)=>Loggable m a where
        putLog :: a->m ()

    instance (MonadLog m)=>(Loggable m ()) where
        putLog _=return ()
    instance (MonadLog m, Loggable m a, Loggable m b)=>Loggable m (a, b) where
        putLog (a, b)=putLog a >> putLog b
    class (Monad m)=>MonadLog m where
        logStringLiteral :: StringLiteral->m ()

    instance (MonadLog m)=>Loggable m StringLiteral where
        putLog=logStringLiteral