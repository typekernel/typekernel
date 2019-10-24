{-# LANGUAGE TemplateHaskell, DataKinds, KindSignatures, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Typekernel.Memory where
    import Typekernel.Nat
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Data.Proxy
    import Typekernel.MLens

    roundUp :: (KnownNat n)=>Proxy (n::Nat)->Int
    roundUp n=(((natToInt n)+7) `quot` 8)*8
    
    unzipProxy :: Proxy (a,b)->(Proxy a, Proxy b)
    unzipProxy _ =(Proxy, Proxy)

    mult8 :: Proxy b-> Proxy (NMul N8 b)
    mult8 _ =Proxy
    -- We force structure to generate aligned memory reference.
    -- Reference to unaligned memory is unallowed.

    submemory :: (KnownNat o, PeanoLT (NMul o N8) n True, PeanoLT (NAdd (NMul o N8) m) (S n) True)=>(Proxy o, Proxy m)->Memory n->C (Memory m)
    submemory=unsafeSubmemory
    unsafeSubmemory :: (KnownNat (o::Nat))=>(Proxy o, Proxy (m::Nat))->Memory n->C (Memory m)
    unsafeSubmemory p mem=do
        let (po, pm)=p
        offset<-immUSize $ fromIntegral $ (natToInt po)*8
        unsafeSubmemory' (offset, pm) mem
    unsafeSubmemory' :: (USize, Proxy (m::Nat))->Memory n->C (Memory m)
    unsafeSubmemory' p mem=do
        let ptr=memStart mem
        let (po, pm)=p
        byteptr<-cast (Proxy :: Proxy USize) ptr
        let offset=po
        newptr<-binary opAdd byteptr offset
        Memory <$> cast (Proxy::Proxy (Ptr USize)) newptr
    {-
    unsafeByteC :: (KnownNat m)=>Proxy (m::Nat)->MLens C (Memory n) UInt8
    unsafeHalfC :: (KnownNat m, PeanoMod2 m Z)=>Proxy (m::Nat)->MLens C (Memory n) UInt16
    unsafeWordC :: (KnownNat m, PeanoMod4 m Z)=>Proxy (m::Nat)->MLens C (Memory n) UInt32
    unsafeDwordC :: (KnownNat m, PeanoMod8 m Z)=>Proxy (m::Nat)->MLens C (Memory n) UInt64
    byteC :: (KnownNat m, PeanoLT m n True)=>Proxy (m::Nat)->MLens C (Memory n) UInt8
    halfC :: (KnownNat m, PeanoLT (S m) n True, PeanoMod2 m Z)=>Proxy (m::Nat)->MLens C (Memory n) UInt16
    wordC :: (KnownNat m, PeanoLT  (S (S (S m))) n True, PeanoMod4 m Z)=>Proxy (m::Nat)->MLens C (Memory n) UInt32
    dwordC :: (KnownNat m, PeanoLT (S (S (S (S (S (S (S m))))))) n True, PeanoMod8 m Z)=>Proxy (m::Nat)->MLens C (Memory n) UInt64
    -}
    unsafeByte :: (KnownNat m, MonadC env)=>Proxy (m::Nat)->MLens env (Memory n) UInt8
    unsafeHalf :: (KnownNat m, PeanoMod2 m Z, MonadC env)=>Proxy (m::Nat)->MLens env (Memory n) UInt16
    unsafeWord :: (KnownNat m, PeanoMod4 m Z, MonadC env)=>Proxy (m::Nat)->MLens env (Memory n) UInt32
    unsafeDword :: (KnownNat m, PeanoMod8 m Z, MonadC env)=>Proxy (m::Nat)->MLens env (Memory n) UInt64
    byte :: (KnownNat m, PeanoLT m n True, MonadC env)=>Proxy (m::Nat)->MLens env (Memory n) UInt8
    half :: (KnownNat m, PeanoLT (S m) n True, PeanoMod2 m Z, MonadC env)=>Proxy (m::Nat)->MLens env (Memory n) UInt16
    word :: (KnownNat m, PeanoLT  (S (S (S m))) n True, PeanoMod4 m Z, MonadC env)=>Proxy (m::Nat)->MLens env (Memory n) UInt32
    dword :: (KnownNat m, PeanoLT (S (S (S (S (S (S (S m))))))) n True, PeanoMod8 m Z, MonadC env)=>Proxy (m::Nat)->MLens env (Memory n) UInt64
    byte=unsafeByte
    half=unsafeHalf
    word=unsafeWord
    dword=unsafeDword
    unsafeByte pm=
        let getter s=liftC $ do
                let ptr=memStart s
                byteptr<-cast (Proxy :: Proxy USize) ptr
                offset<-immUSize $ fromIntegral (natToInt pm)
                newptr<-binary opAdd byteptr offset
                pp<-(cast (Proxy::Proxy (Ptr UInt8)) newptr)
                deref pp
            setter s a=liftC $ do
                let ptr=memStart s
                byteptr<-cast (Proxy :: Proxy USize) ptr
                offset<-immUSize $ fromIntegral (natToInt pm)
                newptr<-binary opAdd byteptr offset
                pp<-(cast (Proxy::Proxy (Ptr UInt8)) newptr)
                mref pp a
        in mkMLens getter setter
        
    
    unsafeHalf pm =
        let getter s=liftC $ do
                let ptr=memStart s
                byteptr<-cast (Proxy :: Proxy USize) ptr
                offset<-immUSize $ fromIntegral (natToInt pm)
                newptr<-binary opAdd byteptr offset
                pp<-(cast (Proxy::Proxy (Ptr UInt16)) newptr)
                deref pp
            setter s a=liftC $ do
                let ptr=memStart s
                byteptr<-cast (Proxy :: Proxy USize) ptr
                offset<-immUSize $ fromIntegral (natToInt pm)
                newptr<-binary opAdd byteptr offset
                pp<-(cast (Proxy::Proxy (Ptr UInt16)) newptr)
                mref pp a
            in mkMLens getter setter
    
    unsafeWord pm =
        let getter s=liftC $ do
                let ptr=memStart s
                byteptr<-cast (Proxy :: Proxy USize) ptr
                offset<-immUSize $ fromIntegral (natToInt pm)
                newptr<-binary opAdd byteptr offset
                pp<-(cast (Proxy::Proxy (Ptr UInt32)) newptr)
                deref pp
            setter s a=liftC $ do
                let ptr=memStart s
                byteptr<-cast (Proxy :: Proxy USize) ptr
                offset<-immUSize $ fromIntegral (natToInt pm)
                newptr<-binary opAdd byteptr offset
                pp<-(cast (Proxy::Proxy (Ptr UInt32)) newptr)
                mref pp a
            in mkMLens getter setter
    
    unsafeDword pm =
        let getter s=liftC $ do
                let ptr=memStart s
                byteptr<-cast (Proxy :: Proxy USize) ptr
                offset<-immUSize $ fromIntegral (natToInt pm)
                newptr<-binary opAdd byteptr offset
                pp<-(cast (Proxy::Proxy (Ptr UInt64)) newptr)
                deref pp
            setter s a=liftC $ do
                let ptr=memStart s
                byteptr<-cast (Proxy :: Proxy USize) ptr
                offset<-immUSize $ fromIntegral (natToInt pm)
                newptr<-binary opAdd byteptr offset
                pp<-(cast (Proxy::Proxy (Ptr UInt64)) newptr)
                mref pp a
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
                    