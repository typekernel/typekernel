{-# LANGUAGE FlexibleInstances #-}
module Typekernel.Lib
    ( someFunc
    ) where
import Typekernel.Transpiler
import Typekernel.C4mAST
someFunc :: IO ()
someFunc = putStrLn "someFunc"

