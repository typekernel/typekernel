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
    import Typekernel.Vec
    


    --dataSeg :: (KnownNat n)=>Vec Word8 n->C (Memory n)
    --dataSeg vec = do
    --    let l=toListV vec
    --    let sz=sizeV vec
    --    arrname<-newArray
    --    ptrname<-newIdent
    --    emitDecl $ "char "++arrname++" ["++(show $ natToInt $ sz)++"]={"
    --    indented $ emit $ concat $ intersperse ", " $ map show l
    --    emitDecl $ "};"
    --    emitDecl $ "uint64_t* "++ptrname++" = "++arrname++";"
    --    return $ Memory $ Ptr ptrname

    --bssSeg :: (KnownNat n)=>Proxy n->C (Memory n)
    --bssSeg sz=do
    --    arrname<-newArray
    --    ptrname<-newIdent
    --    emitDecl $ "char "++arrname++" ["++(show $ natToInt $ sz)++"];"
    --    emitDecl $ "uint64_t* "++ptrname++" = "++arrname++";"
    --    return $ Memory $ Ptr ptrname

       
    

    singleByte :: Word8->Vec Word8 N1
    singleByte w = w -: nilV

    singleHalf :: Word16->Vec Word16 N2
    singleHalf h = (fromIntegral $ (h .&. 0xff)) -: (fromIntegral $ ((h `shiftR` 8) .&. 0xff)) -: nilV

    singleWord :: Word32->Vec Word32 N4
    singleWord h = (fromIntegral $ (h .&. 0xff)) -: (fromIntegral $ ((h `shiftR` 8) .&. 0xff)) 
                    -: (fromIntegral $ ((h `shiftR` 16) .&. 0xff)) -: (fromIntegral $ ((h `shiftR` 24) .&. 0xff)) -: nilV

    singleDword :: Word64->Vec Word64 N8
    singleDword h = (fromIntegral $ (h .&. 0xff)) -: (fromIntegral $ ((h `shiftR` 8) .&. 0xff)) 
                    -: (fromIntegral $ ((h `shiftR` 16) .&. 0xff)) -: (fromIntegral $ ((h `shiftR` 24) .&. 0xff))
                    -: (fromIntegral $ ((h `shiftR` 32) .&. 0xff)) -: (fromIntegral $ ((h `shiftR` 40) .&. 0xff))
                    -: (fromIntegral $ ((h `shiftR` 48) .&. 0xff)) -: (fromIntegral $ ((h `shiftR` 56) .&. 0xff)) -: nilV

    