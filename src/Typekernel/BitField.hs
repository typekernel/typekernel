{-# LANGUAGE DataKinds, KindSignatures #-}
module Typekernel.BitField where
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Typekernel.Memory
    import Typekernel.Nat
    import Typekernel.MLens
    newtype BitField (n::Nat)=BitField {bits :: UInt64}

    