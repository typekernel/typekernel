{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
module Main where

import Typekernel.Transpiler
import Typekernel.C4mAST
import Typekernel.Array
import Data.Proxy
import Typekernel.Memory
import Typekernel.Nat

expr :: C ()
expr=defMain $ mdo
        a<-immInt32 10
        b<-immInt32 20
        arr<-mkArray (Proxy :: Proxy Int32) (Proxy :: Proxy N100)
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
main :: IO ()
main = compile expr >>= putStrLn
