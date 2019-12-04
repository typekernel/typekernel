{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, PolyKinds, DataKinds, InstanceSigs, TemplateHaskell, GADTs, TypeFamilies, AllowAmbiguousTypes, FlexibleInstances, UndecidableInstances #-}
module Typekernel.Transpiler where
    import Control.Monad.State.Lazy
    import Control.Monad.IO.Class
    import Typekernel.C4mAST
    import Data.Word
    import Data.Proxy
    import Control.Lens
    import Data.List
    import Typekernel.Nat
    import qualified Data.Set as Set
    import Debug.Trace
    import Prelude
    import Typekernel.Array
    import Typekernel.IR

    data C4mIR=C4mIR {_generatedCodes :: [IRStmt], 
        _symbolAlloc :: Int, 
        _definedFuncs :: Int, 
        _arrayAlloc :: Int, 
        _indent :: Int, 
        _declaredArrTypes :: Set.Set Int, 
        _topDecls :: [IRStmt], 
        _cDecls :: [String], 
        _scopeIds :: Int, 
        _ensuredObjects :: Set.Set String} deriving (Show)
    makeLenses ''C4mIR
    newtype C4mIRParser a=C4mIRParser {toState :: StateT C4mIR IO a} deriving (Monad, Applicative, Functor, MonadFix)
    
    compile :: C4mIRParser a->IO String
    compile ast=do
            (_, parser)<-runParser ast emptyParser
            let program=(_cDecls parser, (_topDecls parser)++(_generatedCodes parser))
            return $ trace (show program) compileProgram program
    emptyParser :: C4mIR
    emptyParser=C4mIR [] 0 0 0 0 Set.empty [] [] 0 Set.empty
    runParser :: C4mIRParser a->C4mIR->IO (a, C4mIR)
    runParser=runStateT . toState

    ensureObject :: String->C4mIRParser Bool
    ensureObject token = C4mIRParser $ zoom ensuredObjects $ do
            val<-gets (Set.member token)
            modify (Set.insert token)
            return $ not val

    onceC :: String->C4mIRParser ()->C4mIRParser ()
    onceC token command = do
        val<-ensureObject token
        if val
            then command
            else return ()

    emitIR :: IRStmt->C4mIRParser ()
    emitIR s=C4mIRParser $ do
        zoom generatedCodes $ modify (\list->list++[s])
    emitIRTop :: IRStmt->C4mIRParser ()
    emitIRTop s=C4mIRParser $ do
        zoom topDecls $ modify (\list->list++[s])
    emitCDecl :: [String]->C4mIRParser ()
    emitCDecl s=C4mIRParser $ do
        zoom cDecls $ modify (\list->list++s)
    newIdent :: C4mIRParser String
    newIdent = C4mIRParser $ do
        val<-zoom symbolAlloc $ do
            v<-get
            modify (+1)
            return v
        return $ "t_"++ show val
    newFunc :: C4mIRParser String
    newFunc = C4mIRParser $ do
        val<-zoom definedFuncs $ do
            v<-get
            modify (+1)
            return v
        return $ "f_"++ show val
    newArray :: C4mIRParser String
    newArray = C4mIRParser $ do
        val<-zoom arrayAlloc $ do
            v<-get
            modify (+1)
            return v
        return $ "a_"++ show val
    newScope :: C4mIRParser String
    newScope = C4mIRParser $ do
        val<-zoom scopeIds $ do
            v<-get
            modify (+1)
            return v
        return $ show val

    subScope :: (MonadC m)=>m a->m (a, [IRStmt])
    subScope ma = do
        val<-liftC $ C4mIRParser get
        let val'=val {_generatedCodes=[]}
        liftC $ C4mIRParser $ put val'
        -- New scope.
        ret<-ma
        val2<-liftC $ C4mIRParser get
        let code=_generatedCodes val2
        liftC $ C4mIRParser $ put (val2 {_generatedCodes=_generatedCodes val})
        -- New scope end.
        return (ret, code)
    proxyVal :: a->Proxy a
    proxyVal _ = Proxy

    proxyMVal :: m a->Proxy a
    proxyMVal _ = Proxy

    namedFunction :: (FirstClass b, FirstClassList a, MonadC m)=>String->(a->m b)->m (Fn a b)
    namedFunction funname fn=do
        let (aproxy, bproxy)=fnProxy fn
        let rettype=ctype bproxy
        let argtypes=listctype aproxy
        arglist<-liftC $ mapM (\arg->do {ident<-newIdent; return (typeToName arg, ident)}) argtypes
        (returnvalue, code)<-subScope (fn $ wraplist aproxy $ fmap snd arglist)
        liftC $ emitIR $ IRFun funname (typeToName rettype, metadata returnvalue) arglist code
        --let argstr=intercalate ", " $ fmap (\(t, k)->t++" "++k) arglist
        --liftC $ emit $ rettype++" "++funname++"("++argstr++")"
        --liftC $ emit "{"
        --indented $ do
        --    let arguments=fmap snd arglist
        --    valb<-fn $ wraplist aproxy arguments
        --    liftC $ emit $ "return "++(metadata valb)++";"
        --liftC $ emit "}"
        return $ Fn funname
   
    initList :: (FirstClassList a)=>Proxy a->C4mIRParser a
    initList proxy=do
            let argtypes=listctype proxy
            arglist<-mapM (\arg->do {ident<-newIdent; return (arg, ident)}) argtypes
            --mapM_ (\(arg, ident)->emit $ arg++" "++ident++";") arglist
            let vals=fmap snd arglist
            return $ wraplist proxy vals


    externFunction :: (FirstClass b, FirstClassList a)=>String->Proxy a->Proxy b->C (Fn a b)
    externFunction fn aproxy bproxy = do
        let rettype=ctype bproxy
        let argtypes=listctype aproxy
        onceC ("!!extern_function_"++fn) $ emitIRTop $ IRExternFun fn (typeToName rettype) (map typeToName argtypes)
        return $ Fn fn
    instance C4mAST C4mIRParser where
        imm :: (Literal a l)=>l->C4mIRParser a
        imm literal=let proxy=literalProxy (Proxy :: Proxy a) in do
                let t=rawtype proxy
                k<-newIdent
                let v=rawvalue proxy literal
                emitIR $ IRImm (typeToName t, k) v
                return $ wrapliteral proxy k
        unary :: (Unary op b a)=>Proxy op->b->C4mIRParser a
        --binary :: (COperator op, BinaryFun op t1 t2 ~ a, FirstClass t1, FirstClass t2)=>Proxy op->t1->t2->m a
        unary operator v=let vproxy=proxyVal v
                             retproxy=unaryProxy operator vproxy in do
                    let id=metadata v
                    let cop=coperator operator
                    k<-newIdent
                    let t=ctype retproxy
                    --emit $ t++" "++k++" = "++cop++" "++id++";"
                    emitIR $ IRUnary (typeToName t, k) cop id
                    return $ wrap retproxy k
        binary :: (Binary op b c a)=>Proxy op->b->c->C4mIRParser a
        --binary :: (COperator op, BinaryFun op t1 t2 ~ a, FirstClass t1, FirstClass t2)=>Proxy op->t1->t2->m a
        binary operator vb vc=let vbproxy=proxyVal vb
                                  vcproxy=proxyVal vc
                                  retproxy=binaryProxy operator vbproxy vcproxy in do
                        let idb=metadata vb
                        let idc=metadata vc
                        let cop=coperator operator
                        k<-newIdent
                        let t=ctype retproxy
                        --emit $ t++" "++k++" = "++idb++" "++cop++" "++idc++";"
                        emitIR $ IRBinary (typeToName t, k) cop idb idc
                        return $ wrap retproxy k

        --binary :: (COperator op, BinaryFun op t1 t2 ~ a)=>Proxy op->t1->t2->m a
        defun :: (FirstClass b, FirstClassList a)=>(a->C4mIRParser b)->C4mIRParser (Fn a b)
        defun fn = do
            funname<-newFunc
            namedFunction funname fn
        invokep :: (FirstClassList a, FirstClass b)=>(Ptr (Fn a b))->a->C4mIRParser b
        invokep fn args=do
            return undefined
            --let (aproxy, bproxy)=fnPtrProxyVal fn
            --let rettype=ctype bproxy
            --let (Ptr fnname)=fn
            --k<-newIdent
            --let arglist=listmetadata args
            --let argstr=intercalate ", " arglist
            --emit $ rettype++" "++k++" = (*"++fnname++")("++argstr++");"
            --return $ wrap bproxy k
        invoke :: (FirstClassList a, FirstClass b)=>(Fn a b)->a->C4mIRParser b
        invoke fn args=do
            let (aproxy, bproxy)=fnProxyVal fn
            let rettype=ctype bproxy
            let (Fn fnname)=fn
            k<-newIdent
            let arglist=listmetadata args
            let argstr=intercalate ", " arglist
            let argtypes=fmap typeToName $ listctype aproxy
            emitIR $ IRInvoke fnname (typeToName rettype, k) arglist
            --emit $ rettype++" "++k++" = "++fnname++"("++argstr++");"
            return $ wrap bproxy k

        
        if' :: (FirstClassList a)=>Boolean->C4mIRParser a->C4mIRParser a->C4mIRParser a
        if' val btrue bfalse=do
            let proxy=proxyMVal btrue
            scopeTrue<-subScope btrue
            scopeFalse<-subScope bfalse
            temp<-initList proxy
            emitIR $ IRTernary 
                (zip (fmap typeToName $ listctype $ proxyVal temp) (listmetadata temp))
                (metadata val)
                (snd scopeTrue, listmetadata $ fst scopeTrue)
                (snd scopeFalse, listmetadata $ fst scopeFalse)
            return temp
            --temp<-initList proxy
            --emit $ "if("++(metadata val)++")"
            --emit "{"
            --indented $ do
            --    vtrue<-btrue
            --    assignList temp vtrue
            --emit "}"
            --emit "else {"
            --indented $ do
            --    vfalse<-bfalse
            --    assignList temp vfalse
            --emit "}"
            --return temp
        cast :: (Castable a b, FirstClass a, FirstClass b)=>Proxy b->a->C4mIRParser b
        cast proxyb a=do
            let proxya=proxyVal a
            let val=metadata a
            id<-newIdent
            emitIR $ IRCast (typeToName $ ctype proxyb, id) val
            --emit $ ctype proxyb ++" "++id++" = ("++ ctype proxyb ++")"++val++";"
            return $ wrap proxyb id
        defarr :: (KnownNat n)=>Proxy n->C4mIRParser (Memory n)
        defarr pn=do
            let arrSize=((7+natToInt pn ) `quot` 8)
            --assureArrType arrSize
            id<-newArray
            
            --emit $ "arr_"++(show arrSize)++" "++id++" = {0};"
            idp<-newIdent
            emitIR $ IRAlloca idp (natToInt pn)
            --emit $ "uint64_t* "++idp++" = &("++id++".data[0]);"
            return $ Memory (Ptr idp)

        assign :: (KnownNat n)=>Memory n->Memory n->C4mIRParser ()
        assign to from=do
            let pn=proxyMVal to
            let arrSize=((7+natToInt pn ) `quot` 8)
            emitIR $ IRMemcpy (natToInt pn) (metadata $ memStart to) (metadata $ memStart from)
            return ()
            --assureArrType arrSize
            --let arrtype="arr_"++(show arrSize)
            --emit $ "*("++arrtype++"*)"++(metadata $ memStart to)++" = *("++arrtype++"*)"++(metadata $ memStart from)++";"
        --readarr :: (Memory n)->Size->m USize
        --writearr :: (Memory n)->Size->USize->m USize
        deref :: (FirstClass a)=>Ptr a->C4mIRParser a
        deref p=do
            let pa=ptrType p
            id<-newIdent
            emitIR $ IRDeref id (metadata p)
            --emit $ (ctype pa)++" "++id++" = *"++(metadata p)++";"
            return $ wrap pa id 
        mref :: (FirstClass a)=>Ptr a->a->C4mIRParser ()
        mref p a=
            emitIR $ IRModifyRef (metadata p) (metadata a)
            --emit $ "*"++(metadata p)++" = "++""++(metadata a)++";"


    type C = C4mIRParser

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

    immSize :: Integer->C Size
    immSize = imm. fromIntegral
    immUSize :: Integer->C USize
    immUSize = imm. fromIntegral
    
    -- Monads that are compatible with "C" monad.
    class (Monad m)=>MonadC m where
        liftC :: C a->m a
    instance MonadC C4mIRParser where liftC = id

    instance (MonadC m, Monad m)=>MonadIO m where
        liftIO = liftC . C4mIRParser . lift