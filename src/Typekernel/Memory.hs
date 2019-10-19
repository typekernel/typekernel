{-# LANGUAGE TemplateHaskell, DataKinds, KindSignatures, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Typekernel.Memory where
    import Typekernel.Nat
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Data.Proxy
    import Typekernel.MLens
    import Typekernel.Array
    data Memory (n::Nat)=Memory {memStart :: Ptr USize}

    roundUp :: (KnownNat n)=>Proxy n->Int
    roundUp n=(((natToInt n)+7) `quot` 8)*8
    

    byte :: (KnownNat m, KnownNat n, PeanoLT m n True)=>Proxy m->MLens C (Memory n) UInt8
    byte pm=
        let getter s=do
                let ptr=memStart s
                byteptr<-unsafeCastPointer (Proxy :: Proxy UInt8) ptr
                index<-immInt64 $ fromIntegral (natToInt pm)
                readArray byteptr index
            setter s a=do
                let ptr=memStart s
                byteptr<-unsafeCastPointer (Proxy :: Proxy UInt8) ptr
                index<-immInt64 $ fromIntegral (natToInt pm)
                writeArray byteptr index a
        in mkMLens getter setter
    
    bit :: (KnownNat n, PeanoLT n N8 True)=>Proxy n->CLens C UInt8 Boolean
    bit pn = mkCLens getter setter where
                tup tt=(tt, Void)
                untup (tt, Void)=tt
                getter s = do
                    bit<-immUInt8 (2^(natToInt pn))
                    mask<-binary opOr bit s
                    zero<-immUInt8 0
                    binary opCGE mask zero
                setter s a = do
                    bit<-immUInt8 (2^(natToInt pn))
                    unmask<-unary opInvert bit
                    let b1=fmap tup $ binary opOr s bit
                    let b2=fmap tup $ binary opAnd s unmask
                    t<-if' a b1 b2
                    return $ untup t
                    