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
import qualified Typekernel.Kernel.Main
import System.IO
import System.Environment
import System.Exit
import qualified Data.Map as Map
import System.Console.GetOpt
import Data.List
import Typekernel.ProductType

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

exprEmpty :: C ()
exprEmpty = do
    namedFunction "main" (\(x::Void)->do
        a<-immInt32 100 
        b<-immInt32 200
        binary opAdd a b)
    return ()

cstdio :: C (Fn Void UInt8, Fn (UInt8, Void) UInt64)
cstdio = do
    onceC "stdio.h" $ emitCDecl ["#include <stdio.h>"]
    onceC "readC" $ emitCDecl ["uint8_t readC(){return getchar();}"]
    onceC "writeC" $ emitCDecl ["uint64_t writeC(uint8_t chr){putchar(chr);}"]
    readC<-externFunction "readC" (Proxy :: Proxy Void) (Proxy :: Proxy UInt8) 
    writeC<-externFunction "writeC" (Proxy :: Proxy (UInt8, Void)) (Proxy :: Proxy UInt64)
    return (readC, writeC)
exprStdio :: C ()
exprStdio = do
    (readC, writeC)<-cstdio
    namedFunction "main" (\(x::Void)->do
        chr<-invoke readC Void
        one<-immUInt8 1
        schr<-binary opAdd chr one
        invoke writeC (schr, Void)
        immInt32 0)
    return ()
exprFun :: C ()
exprFun = do
    plusOne<-defun (\(x::Int32, _::Void)->do
        one<-immInt32 1
        binary opAdd x one
        )
    namedFunction "main" (\(x::Void)->do
        a<-immInt32 100 
        invoke plusOne (a, Void))
    return ()
    
exprTernary :: C ()
exprTernary = do
    mult_hundred<-imm True
    (a, (b, (c, _)))<-if' mult_hundred 
        (do
            a<-immUInt64 100
            b<-immUInt64 200
            c<-immUInt64 300
            return (a, (b, (c, Void))))
        (do
            a<-immUInt64 1
            b<-immUInt64 2
            c<-immUInt64 3
            return (a, (b, (c, Void))))
            
    return ()

data Triplet' a
type Triplet a = Typedef (Triplet' a) (Product a (Product a a))
type TripletUInt8=Triplet (Basic UInt8)
exprMemory :: C ()
exprMemory = do
    (readC, writeC)<-cstdio
    runRAII $ do
        zero<-liftC $ immUInt8 0
        triplet<-construct (ctorProd (ctorBasic zero) (ctorProd (ctorBasic zero) (ctorBasic zero)))
        first_elem<-liftC $ fstS $ scopedValue triplet
        second_elem<-liftC $ (flip (>>=) fstS) $ sndS $ scopedValue triplet
        third_elem<-liftC $ (flip (>>=) sndS) $ sndS $ scopedValue triplet
        value<-liftC $ immUInt8 48
        one<-liftC $ immUInt8 1
        value<-liftC $ binary opAdd value one
        liftC $ mset basic first_elem value
        value<-liftC $ binary opAdd value one
        liftC $ mset basic second_elem value
        value<-liftC $ binary opAdd value one
        liftC $ mset basic third_elem value
        return ()
    return ()
    
generateCode :: String->C ()->IO ()
generateCode name ast=do
    putStrLn $ "Generating "++name
    code<-compile ast
    writeFile (name) code
    return ()

snippets = Map.fromList [
    ("expr", expr),
    ("exprEmpty", exprEmpty),
    ("exprStdio", exprStdio),
    ("exprFun", exprFun),
    ("exprTernary", exprTernary),
    ("exprMemory", exprMemory),
    ("bootloader", Typekernel.Loader.Main.main),
    ("kernel", Typekernel.Kernel.Main.main)
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
        [code]->generate code (code++".tirr")
        [code, output]->generate code output
        _ ->  putStrLn $ "Programs: "++(intercalate ", " $ Map.keys snippets)
    --generateCode "expr" expr
    --generateCode "bootloader" Typekernel.Loader.Main.main
    -- putStrLn "********************************\nTypekernel Code Generator Done.\n********************************"