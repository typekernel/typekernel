{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
-- Generating DATA section at compile time.
-- Generating BSS section somewhere.
module Typekernel.Constant where
    import Typekernel.Transpiler
    import Typekernel.C4mAST
    import Typekernel.Nat
    import Data.Word
    import Data.Proxy
    import Data.Bits
    import Data.List (intersperse)

    data Vec a (n::Nat) where 
        Nil :: Vec a Z
        (:-) :: a->Vec a n -> Vec a (S n)

    infixr :-


    instance (Show a)=>Show (Vec a n) where
        show = show . toListV
    mapV :: (a -> b) -> Vec a n -> Vec b n

    mapV _ Nil=Nil
    mapV f (x:-xs)=(f x):-(mapV f xs)


    toListV :: Vec a n->[a]

    toListV Nil=[]
    toListV (x:-xs)=x:(toListV xs)

    sizeV :: Vec a n->Proxy n
    sizeV _=Proxy


    dataSeg :: (KnownNat n)=>Vec Word8 n->C (Memory n)
    dataSeg vec = do
        let l=toListV vec
        let sz=sizeV vec
        arrname<-newArray
        ptrname<-newIdent
        emitDecl $ "char "++arrname++" ["++(show $ natToInt $ sz)++"]={"
        indented $ emit $ concat $ intersperse ", " $ map show l
        emitDecl $ "};"
        emitDecl $ "uint64_t* "++ptrname++" = "++arrname++";"
        return $ Memory $ Ptr ptrname

    bssSeg :: (KnownNat n)=>Proxy n->C (Memory n)
    bssSeg sz=do
        arrname<-newArray
        ptrname<-newIdent
        emitDecl $ "char "++arrname++" ["++(show $ natToInt $ sz)++"];"
        emitDecl $ "uint64_t* "++ptrname++" = "++arrname++";"
        return $ Memory $ Ptr ptrname

       
    concatV :: Vec a m->Vec a n->Vec a (NAdd m n)
    concatV Nil xs = xs
    concatV (x:-xs) ys = x:-(concatV xs ys)

    singleByte :: Word8->Vec Word8 N1
    singleByte w = w :-Nil

    singleHalf :: Word16->Vec Word16 N2
    singleHalf h = (fromIntegral $ (h .&. 0xff)) :- (fromIntegral $ ((h `shiftR` 8) .&. 0xff)) :- Nil

    singleWord :: Word32->Vec Word32 N4
    singleWord h = (fromIntegral $ (h .&. 0xff)) :- (fromIntegral $ ((h `shiftR` 8) .&. 0xff)) 
                    :- (fromIntegral $ ((h `shiftR` 16) .&. 0xff)) :- (fromIntegral $ ((h `shiftR` 24) .&. 0xff)) :- Nil

    singleDword :: Word64->Vec Word64 N8
    singleDword h = (fromIntegral $ (h .&. 0xff)) :- (fromIntegral $ ((h `shiftR` 8) .&. 0xff)) 
                    :- (fromIntegral $ ((h `shiftR` 16) .&. 0xff)) :- (fromIntegral $ ((h `shiftR` 24) .&. 0xff))
                    :- (fromIntegral $ ((h `shiftR` 32) .&. 0xff)) :- (fromIntegral $ ((h `shiftR` 40) .&. 0xff))
                    :- (fromIntegral $ ((h `shiftR` 48) .&. 0xff)) :- (fromIntegral $ ((h `shiftR` 56) .&. 0xff)) :- Nil

    (++:) = concatV
    