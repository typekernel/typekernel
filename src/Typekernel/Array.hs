{-# LANGUAGE FlexibleContexts, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
module Typekernel.Array where
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Data.Proxy
    import Typekernel.Nat
    class (FirstClass a)=>Deref r a | r->a where
        deref :: Size->r->(String, String)
    instance (FirstClass a)=>Deref (Arr n a) a where
        deref sz arr=let (pn, pa)=arrType arr
                         (Arr name)=arr
                     in (ctype pa, name++"["++(metadata sz)++"]")
    instance (FirstClass a)=>Deref (Ptr a) a where
        deref sz ptr=let pa=ptrType ptr
                         (Ptr name)=ptr
                     in (ctype pa, name++"["++(metadata sz)++"]")
    derefType :: (Deref r a)=>r->Proxy a
    derefType _=Proxy
    ptrType :: (Ptr a)->Proxy a
    ptrType _=Proxy
    arrType :: (Arr n a)->(Proxy n, Proxy a)
    arrType _=(Proxy, Proxy)
    promotePtr :: (Proxy a)->(Proxy (Ptr a))
    promotePtr _=Proxy
    demotePtr :: (Proxy a)->(Proxy (Ptr a))
    demotePtr _=Proxy
    mkArray :: (FirstClass a, KnownNat n)=>Proxy a->Proxy n->C (Arr n a)
    mkArray pa pn=do
        arr<-newArray
        let t=ctype pa
        let size=natToInt pn
        emit $ t++" "++arr++"["++(show size)++"]={0};"
        return $ Arr arr
    ref :: (FirstClass a)=>(Arr n a)->C (Ptr a)
    ref arr=do
        let (pn, pa)=arrType arr
        k<-newIdent
        let tptr=promotePtr pa
        let (Arr m)=arr;
        emit $ (ctype tptr)++" "++k++" = &"++m++"[0];"
        return $ Ptr k
    readArray :: Deref r a=>r->Size->C a
    readArray r n= do
        let (t, expr)=deref n r
        id<-newIdent
        emit $ t++" "++id++" = "++expr++";"
        let pa=derefType r
        return $ wrap pa id
    writeArray :: Deref r a=>r->Size->a->C ()
    writeArray r n val= do
        let (t, expr)=deref n r
        emit $ expr++" = "++(metadata val)++";"
        return ()

    unsafeCastPointer :: (FirstClass a, FirstClass b)=>Proxy b->Ptr a->C (Ptr b)
    unsafeCastPointer proxyb =cast (promotePtr proxyb)