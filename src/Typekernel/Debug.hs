{-# LANGUAGE FlexibleInstances #-}
module Typekernel.Debug where
    import Typekernel.Transpiler
    import Typekernel.C4mAST
    import System.IO.Unsafe
    data ShowIO a=ShowIO a

    -- Warning: this kills your cat!
    -- This is only used for testing code generation in GHCi.
    instance (Show a)=>Show (C a) where
        show a=unsafePerformIO $ fmap show $ runParser a emptyParser