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
import System.Environment
import System.Exit
import qualified Data.Map as Map
import System.Console.GetOpt
import Data.List
expr :: C ()
expr=do
    onceC "echo_test" $ emitCDecl ["uint64_t echo_test(int32_t val){printf(\"The sum is %d\\n\", val);}"]
    namedFunction "main" (\(x::Void)->mdo
        echo<-externFunction "echo_test" (Proxy :: Proxy (Int32, Void)) (Proxy :: Proxy UInt64)
        --emit "// RAII Start"
        runRAII $ do
            mem<-construct $ zeroBasic
            a<-liftC $ immUInt32 114514
            liftC $ mset basic (scopedValue mem) a
            return ()
        --emit "// RAII End"
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
        invoke echo (ret, Void)
        zero<-immInt32 0;
        return zero)
    return ()
        --emit $ "printf(\"The sum is %d\\n\"," ++ (metadata ret) ++ ");"
generateCode :: String->C ()->IO ()
generateCode name ast=do
    putStrLn $ "Generating "++name
    code<-compile ast
    writeFile (name) code
    return ()

snippets = Map.fromList [
    ("expr", expr),
    ("bootloader", Typekernel.Loader.Main.main)
    ]

exit    = exitWith ExitSuccess
pdie     = exitWith (ExitFailure 1)
generate :: String->String->IO ()
generate code output=
    case Map.lookup code snippets of
        Nothing-> putStrLn "Program name not found." >> pdie
        (Just c)-> generateCode output c

main :: IO ()
main = do
    putStrLn "********************************\nTypekernel Code Generator\n********************************"
    args<-getArgs
    case args of
        [code]->generate code (code++".c")
        [code, output]->generate code output
        _ ->  putStrLn $ "Programs: "++(intercalate ", " $ Map.keys snippets)
    --generateCode "expr" expr
    --generateCode "bootloader" Typekernel.Loader.Main.main
    -- putStrLn "********************************\nTypekernel Code Generator Done.\n********************************"