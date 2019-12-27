{-# LANGUAGE RankNTypes, FlexibleInstances, UndecidableInstances, PolyKinds, KindSignatures #-}
module Typekernel.Bound where
    import Control.Monad.Trans.Class
    import Control.Monad.Trans.Reader
    import Control.Monad.Trans.State
    import Control.Monad.IO.Class
    import Typekernel.Transpiler
    import Typekernel.C4mAST
    import Typekernel.RAII
    import Typekernel.Structure
    import Data.List
    import Typekernel.IR
    class (MonadTrans t)=>Bound t where
        (>>>=) :: (Monad m, Monad (t m))=>t m a->(a->m b)->t m b
        (>>>=) a f=a >>= (lift . f)

    class (MonadC m)=>BoundC m where
        bindC :: m a->(a->C b)->m b
        bindC a f=a >>= (liftC . f)
    -- Ifstmt with RAII scope. You should use this.


    ifS :: (MonadC m, FirstClassList a)=>Boolean->(forall s. RAII s m a)->(forall s. RAII s m a)->m a

    ifS val btrue bfalse = do
        let proxy=proxyMVal btrue
        scopeTrue<-subScope $ runRAII btrue
        scopeFalse<-subScope $ runRAII bfalse
        temp<-liftC $ initList proxy
        liftC $ emitIR $ IRTernary 
            (zip (fmap typeToName $ listctype $ proxyVal temp) (listmetadata temp))
            (metadata val)
            (snd scopeTrue, listmetadata $ fst scopeTrue)
            (snd scopeFalse, listmetadata $ fst scopeFalse)
        return temp
        --temp<-liftC $ initList proxy
        --liftC $ emit $ "if("++(metadata val)++")"
        --liftC $ emit "{"
        --indented $ do
        --    vtrue<-runRAII btrue
        --    liftC $ assignList temp vtrue
        --liftC $ emit "}"
        --liftC $ emit "else {"
        --indented $ do
        --    vfalse<-runRAII bfalse
        --    liftC $ assignList temp vfalse
        --liftC $ emit "}"
        --return temp
    
    -- Tag function with a monad, preventing unwanted escape.
    data SFn m a b=SFn {toFun :: Fn a b}
    wrapSFn :: (Monad m)=>(Fn a b)->m (SFn m a b)
    wrapSFn = return . SFn
    defunS :: (MonadC m, FirstClassList a, FirstClass b)=>( a->(forall s. RAII s m b))->m (SFn m a b)
    defunS fn = do
        name<-liftC $ newFunc
        defunSNamed name fn
    defunSNamed :: (MonadC m, FirstClassList a, FirstClass b)=>String->( a->(forall s. RAII s m b))->m (SFn m a b)
    defunSNamed name fn=do
        let invokedfn a = runRAII $ fn a
        fn<-namedFunction name invokedfn
        wrapSFn fn
    invokeS :: (MonadC m, FirstClassList a, FirstClass b)=>SFn m a b->a->m b
    invokeS sfn args = do
        let fn=toFun sfn
        let (aproxy, bproxy)=fnProxyVal fn
        let rettype=ctype bproxy
        let (Fn fnname)=fn
        k<-liftC $ newIdent

        let arglist=listmetadata args
        let argstr=intercalate ", " arglist
        let argtypes=fmap typeToName $ listctype aproxy
        liftC $ emitIR $ IRInvoke fnname (typeToName rettype, k) arglist
        --emit $ rettype++" "++k++" = "++fnname++"("++argstr++");"
        return $ wrap bproxy k

    
    --liftB2 :: (MonadTrans t, Monad m, Monad (t m))=>(a->b->m c)->t m a->t m b->t m c
    recursion :: (FirstClass b, FirstClassList a, MonadC m)=>((a->m b)->a->(forall s. RAII s m b))->m (SFn m a b)
    recursion fn = do
        fname<-liftC $ newFunc
        shortcircuit<-wrapSFn $ Fn fname
        let partial=invokeS shortcircuit
        defunSNamed fname (fn partial)
        
    foreverLoop' :: (MonadC m)=>((forall s. RAII s m ()))->m (SFn m Void USize)
    foreverLoop' fn = recursion $ (\rc _->fn >> (lift $ rc Void))

    foreverLoop :: (MonadC m)=>(forall s. RAII s m ())->m ()
    foreverLoop f = do
        sfn<-foreverLoop' f
        invokeS sfn Void
        return ()