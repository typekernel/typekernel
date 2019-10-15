{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, DataKinds, InstanceSigs, TemplateHaskell, GADTs, TypeFamilies, AllowAmbiguousTypes #-}
module Typekernel.Transpiler where
    import Control.Monad.State.Lazy
    import Typekernel.C4mAST
    import Data.Word
    import Data.Proxy
    import Control.Lens
    import Data.List
    import qualified Data.Set as Set
    import Debug.Trace
    data C4m=C4m {_generatedCodes :: [String], _symbolAlloc :: Int, _definedFuncs :: Set.Set String, _indent :: Int} deriving (Show)
    makeLenses ''C4m
    newtype C4mParser a=C4mParser {toState :: StateT C4m IO a} deriving (Monad, Applicative, Functor, MonadFix)

    emptyParser :: C4m
    emptyParser=C4m [] 0 Set.empty 0
    runParser :: C4mParser a->C4m->IO (a, C4m)
    runParser=runStateT . toState

    compile :: C4mParser a->IO String
    compile ast=do
            (_, parser)<-runParser ast emptyParser
            return $ intercalate "\n" $ reverse $ _generatedCodes parser
    newIdent :: C4mParser String
    newIdent = C4mParser $ do
        val<-zoom symbolAlloc $ do
            v<-get
            modify (+1)
            return v
        return $ "t_"++ show val
    emit :: String->C4mParser ()
    emit s=C4mParser $ do
            indent<-fmap _indent get
            zoom generatedCodes $ modify ((:) $ (replicate (4*indent) ' ')++s)
    incIndent :: C4mParser ()
    incIndent=C4mParser $ zoom indent $ modify (+1) 
    decIndent :: C4mParser ()
    decIndent=C4mParser $ zoom indent $ modify (flip (-) 1) 
    indented :: C4mParser a->C4mParser a
    indented x=do
        incIndent
        val<-x
        decIndent
        return val
    proxyVal :: a->Proxy a
    proxyVal _ = Proxy

    proxyMVal :: m a->Proxy a
    proxyMVal _ = Proxy
    initList :: (FirstClassList a)=>Proxy a->C4mParser a
    initList proxy=do
            let argtypes=listctype proxy
            arglist<-mapM (\arg->do {ident<-newIdent; return (arg, ident)}) argtypes
            mapM_ (\(arg, ident)->emit $ arg++" "++ident++";") arglist
            let vals=fmap snd arglist
            return $ wraplist proxy vals
    assignList :: (FirstClassList a)=>a->a->C4mParser ()
    assignList arga argb=do
            let lista=listmetadata arga
            let listb=listmetadata argb
            mapM_ (\(a, b)->emit $ a++"="++b++";") $ zip lista listb
    instance C4mAST C4mParser where
        imm :: (Literal a l)=>l->C4mParser a
        imm literal=let proxy=literalProxy (Proxy :: Proxy a) in do
                let t=rawtype proxy
                k<-newIdent
                let v=rawvalue proxy literal
                emit $ t++" "++k++"="++v++";"
                return $ wrapliteral proxy k
        unary :: (Unary op b a)=>Proxy op->b->C4mParser a
        --binary :: (COperator op, BinaryFun op t1 t2 ~ a, FirstClass t1, FirstClass t2)=>Proxy op->t1->t2->m a
        unary operator v=let vproxy=proxyVal v
                             retproxy=unaryProxy operator vproxy in do
                    let id=metadata v
                    let cop=coperator operator
                    k<-newIdent
                    let t=ctype retproxy
                    emit $ t++" "++k++"="++cop++id++";"
                    return $ wrap retproxy k
        binary :: (Binary op b c a)=>Proxy op->b->c->C4mParser a
        --binary :: (COperator op, BinaryFun op t1 t2 ~ a, FirstClass t1, FirstClass t2)=>Proxy op->t1->t2->m a
        binary operator vb vc=let vbproxy=proxyVal vb
                                  vcproxy=proxyVal vc
                                  retproxy=binaryProxy operator vbproxy vcproxy in do
                        let idb=metadata vb
                        let idc=metadata vc
                        let cop=coperator operator
                        k<-newIdent
                        let t=ctype retproxy
                        emit $ t++" "++k++"="++idb++cop++idc++";"
                        return $ wrap retproxy k

        --binary :: (COperator op, BinaryFun op t1 t2 ~ a)=>Proxy op->t1->t2->m a
        defun :: (FirstClass b, FirstClassList a)=>(a->C4mParser b)->C4mParser (Fn a b)
        defun fn =do
            let (aproxy, bproxy)=fnProxy fn
            let rettype=ctype bproxy
            funname<-newIdent
            let argtypes=listctype aproxy
            arglist<-mapM (\arg->do {ident<-newIdent; return (arg, ident)}) argtypes
            let argstr=intercalate ", " $ fmap (\(t, k)->t++" "++k) arglist
            emit $ rettype++" "++funname++"("++argstr++")"
            emit "{"
            indented $ do
                let arguments=fmap snd arglist
                valb<-fn $ wraplist aproxy arguments
                emit $ "return "++(metadata valb)++";"
            emit "}"
            return $ Fn funname
        invoke :: (FirstClassList a, FirstClass b)=>(Fn a b)->a->C4mParser b
        invoke fn args=do
            let (aproxy, bproxy)=fnProxyVal fn
            let rettype=ctype bproxy
            let (Fn fnname)=fn
            k<-newIdent
            let arglist=listmetadata args
            let argstr=intercalate ", " arglist
            emit $ rettype++" "++k++"="++fnname++"("++argstr++");"
            return $ wrap bproxy k

        if' :: (FirstClassList a)=>Boolean->C4mParser a->C4mParser a->C4mParser a
        if' val btrue bfalse=do
            let proxy=proxyMVal btrue
            temp<-initList proxy
            emit $ "if("++(metadata val)++")"
            emit "{"
            indented $ do
                vtrue<-btrue
                assignList temp vtrue
            emit "}"
            emit "else {"
            indented $ do
                vfalse<-bfalse
                assignList temp vfalse
            emit "}"
            return temp
    