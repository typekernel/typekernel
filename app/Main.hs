{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
module Main where

import Typekernel.Transpiler
import Typekernel.C4mAST
import Typekernel.Array
import Data.Proxy
import Typekernel.Memory
import Typekernel.Nat
import Typekernel.Std.Basic
import Typekernel.RAII
import Typekernel.Structure
import Typekernel.MLens
import qualified Typekernel.Loader.Main
import System.IO
expr :: C ()
expr=defMain $ mdo
        emit "// RAII Start"
        runRAII $ do
            mem<-construct $ zeroBasic (Proxy :: Proxy UInt32) 
            a<-liftC $ immUInt32 114514
            liftC $ mset (basic (Proxy :: Proxy UInt32)) (scopedValue mem) a
            return ()
        emit "// RAII End"
        a<-immInt32 10
        b<-immInt32 20
        arr<-defarr (Proxy :: Proxy N100)
        fsum<-(defun $ \(x::Int32, Void)->do
            zero<-immInt32 0
            one<-immInt32 1
            cmp<-binary opCEQ x zero
            sup<-binary opSub x one
            result<- if' cmp (return (zero, Void)) $ do
                v<-(invoke fsum (sup, Void))
                return $ (v, Void)
            let (r, Void)=result
            binary opAdd r x)
        s<-invoke fsum (a, Void)
        ret<-binary opAdd a s

        emit $ "printf(\"The sum is %d\\n\"," ++ (metadata ret) ++ ");"
generateCode :: String->C ()->IO ()
generateCode name ast=do
    putStrLn $ "Generating "++name++".c"
    code<-compile ast
    writeFile (name++".c") code
    return ()
main :: IO ()
main = do
    putStrLn "********************************\nTypekernel Code Generator\n********************************"
    generateCode "expr" expr
    generateCode "bootloader" Typekernel.Loader.Main.main
    putStrLn "********************************\nTypekernel Code Generator Done.\n********************************"