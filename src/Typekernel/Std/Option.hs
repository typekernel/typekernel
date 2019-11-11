module Typekernel.Std.Option where
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Typekernel.Structure
    import Data.Proxy
    import Typekernel.Nat
    import Typekernel.Constant
    -- One important usage for Option is to simulate uninitialized static variables.
    -- For example, we leave out a static var filled with None, and initialize it when we need to use it.
    staticVar :: (Structure n a, KnownNat n)=>Proxy a->C (Memory n)

    staticVar pn = bssSeg (memSize pn)