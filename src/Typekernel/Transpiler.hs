{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, PolyKinds, DataKinds, InstanceSigs, TemplateHaskell, GADTs, TypeFamilies, AllowAmbiguousTypes #-}
module Typekernel.Transpiler where
    import Control.Monad.State.Lazy
    import Control.Monad.IO.Class
    import Typekernel.C4mAST
    import Data.Word
    import Data.Proxy
    import Control.Lens
    import Data.List
    import qualified Data.Map.Lazy as Map
    import Debug.Trace
    data C4m=C4m {_generatedCodes :: [String], _symbolAlloc :: Int, _definedFuncs :: Int, _arrayAlloc :: Int, _indent :: Int} deriving (Show)
    makeLenses ''C4m
    newtype C4mParser a=C4mParser {toState :: StateT C4m IO a} deriving (Monad, Applicative, Functor, MonadFix, MonadIO)
    
    emptyParser :: C4m
    emptyParser=C4m [] 0 0 0 0
    runParser :: C4mParser a->C4m->IO (a, C4m)
    runParser=runStateT . toState

    compile :: C4mParser a->IO String
    compile ast=do
            let new_ast=do
                    emit "#include <stdint.h>"
                    emit "#include <stdbool.h>"
                    ast
            (_, parser)<-runParser new_ast emptyParser
            return $ intercalate "\n" $ reverse $ _generatedCodes parser
    defMain :: C4mParser b->C4mParser ()
    defMain m = (namedFunction "main" $  (const (m >> (immInt32 0)) :: Void->C4mParser Int32)) >> return ()
    newIdent :: C4mParser String
    newIdent = C4mParser $ do
        val<-zoom symbolAlloc $ do
            v<-get
            modify (+1)
            return v
        return $ "t_"++ show val
    newFunc :: C4mParser String
    newFunc = C4mParser $ do
        val<-zoom definedFuncs $ do
            v<-get
            modify (+1)
            return v
        return $ "f_"++ show val
    newArray :: C4mParser String
    newArray = C4mParser $ do
        val<-zoom arrayAlloc $ do
            v<-get
            modify (+1)
            return v
        return $ "a_"++ show val
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
            mapM_ (\(a, b)->emit $ a++" = "++b++";") $ zip lista listb
    namedFunction :: (FirstClass b, FirstClassList a)=>String->(a->C4mParser b)->C4mParser (Fn a b)
    namedFunction funname fn=do
        let (aproxy, bproxy)=fnProxy fn
        let rettype=ctype bproxy
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
    instance C4mAST C4mParser where
        imm :: (Literal a l)=>l->C4mParser a
        imm literal=let proxy=literalProxy (Proxy :: Proxy a) in do
                let t=rawtype proxy
                k<-newIdent
                let v=rawvalue proxy literal
                emit $ t++" "++k++" = "++v++";"
                return $ wrapliteral proxy k
        unary :: (Unary op b a)=>Proxy op->b->C4mParser a
        --binary :: (COperator op, BinaryFun op t1 t2 ~ a, FirstClass t1, FirstClass t2)=>Proxy op->t1->t2->m a
        unary operator v=let vproxy=proxyVal v
                             retproxy=unaryProxy operator vproxy in do
                    let id=metadata v
                    let cop=coperator operator
                    k<-newIdent
                    let t=ctype retproxy
                    emit $ t++" "++k++" = "++cop++" "++id++";"
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
                        emit $ t++" "++k++" = "++idb++" "++cop++" "++idc++";"
                        return $ wrap retproxy k

        --binary :: (COperator op, BinaryFun op t1 t2 ~ a)=>Proxy op->t1->t2->m a
        defun :: (FirstClass b, FirstClassList a)=>(a->C4mParser b)->C4mParser (Fn a b)
        defun fn = do
            funname<-newFunc
            namedFunction funname fn
        invoke :: (FirstClassList a, FirstClass b)=>(Fn a b)->a->C4mParser b
        invoke fn args=do
            let (aproxy, bproxy)=fnProxyVal fn
            let rettype=ctype bproxy
            let (Fn fnname)=fn
            k<-newIdent
            let arglist=listmetadata args
            let argstr=intercalate ", " arglist
            emit $ rettype++" "++k++" = "++fnname++"("++argstr++");"
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
        cast :: (Castable a b, FirstClass a, FirstClass b)=>Proxy b->a->C4mParser b
        cast proxyb a=do
            let proxya=proxyVal a
            let val=metadata a
            id<-newIdent
            emit $ ctype proxyb ++" "++id++" = ("++ ctype proxyb ++")"++val++";"
            return $ wrap proxyb id
    -- Using transpiler by default.
    type C=C4mParser

    
   

    immInt8 :: Integer->C Int8
    immInt8=imm . fromIntegral

    immUInt8 :: Integer->C UInt8
    immUInt8=imm . fromIntegral

    immInt16 :: Integer->C Int16
    immInt16=imm . fromIntegral

    immUInt16 :: Integer->C UInt16
    immUInt16=imm . fromIntegral

    immInt32 :: Integer->C Int32
    immInt32=imm . fromIntegral

    immUInt32 :: Integer->C UInt32
    immUInt32=imm . fromIntegral

    immInt64 :: Integer->C Int64
    immInt64=imm . fromIntegral

    immUInt64 :: Integer->C UInt64
    immUInt64=imm . fromIntegral


    immBool :: Bool->C Boolean
    immBool=imm

    class (Monad m)=>MonadC m where
        liftC :: C a->m a
    instance MonadC C4mParser where liftC = id