{-# LANGUAGE DataKinds, PolyKinds, NumericUnderscores#-}
module Typekernel.Std.X86_64 where
    import Typekernel.Structure
    import Typekernel.ProductType
    import Typekernel.Nat
    import Typekernel.Memory
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Typekernel.MLens
    import Typekernel.Constant
    type IDTItem = Memory N16


    -- TODO: a beautiful compile-time evaluation.
    kcode = singleDword 0x0020980000000000
    ucode = singleDword 0x0020F80000000000
    kdata=singleDword 0x0000920000000000
    udata=singleDword 0x0000F20000000000
    ucode32=singleDword 0x00cffa00_0000ffff
    udata32=singleDword 0x00cff200_0000ffff



    -- genGDTTable = dataSeg $ (singleDword 0) ++: kcode ++: ucode ++: kdata ++: udata ++: ucode32 ++: udata32 ++: (singleDword 0)

    newtype IDTHandler = IDTHandler {handlePtr :: UInt64}
    --idtHandler :: (MonadC env)=>MLens env IDTItem IDTHandler
    --idtHandler=mkLens getter setter where
    --    getter 
    