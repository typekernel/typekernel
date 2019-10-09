{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, DataKinds, TemplateHaskell, UndecidableInstances, DeriveFunctor, ScopedTypeVariables, RecursiveDo #-}
-- C---- AST.
-- C---- is a chosen subset from C that works as code generation target.
-- The ideal result should be something that has TAC-style assignments and structured control flow.
module C4mAST where
    import Data.Proxy
    import qualified Data.Word as HW
    import TH
    import Language.Haskell.TH
    import Data.Monoid
    import Control.Monad.Reader
    import Control.Monad

    type Metadata=String
    $(return $ do
        t<-["Int8", "UInt8", "Int16", "UInt16", "Int32", "UInt32", "Int64", "UInt64", "Boolean"]
        let name=mkName t
        return $ DataD [] name [] Nothing [NormalC name [(Bang NoSourceUnpackedness NoSourceStrictness,ConT ''Metadata)]] [])
    
    data Fn a b=Fn Metadata
    data Ptr a=Ptr Metadata
    data Arr (n::Nat) a=Arr Metadata

    data Nat=Z | S Nat

    

    class Literal t l | t->l

    class FirstClass a where
        ctype :: Proxy a->String

    class FirstClassList a

    
    instance FirstClass Int8 where {ctype _="int8_t"}
    instance FirstClass Int16 where {ctype _="int16_t"}
    instance FirstClass Int32 where {ctype _="int32_t"}
    instance FirstClass Int64 where {ctype _="int64_t"}
    instance FirstClass UInt8 where {ctype _="uint8_t"}
    instance FirstClass UInt16 where {ctype _="uint16_t"}
    instance FirstClass UInt32 where {ctype _="uint32_t"}
    instance FirstClass UInt64 where {ctype _="uint64_t"}

    instance (FirstClass a)=>FirstClass (Ptr a) where {ctype _=(ctype (Proxy:: Proxy a))++"*"}


    instance (FirstClass a)=>FirstClassList (a, ())
    instance (FirstClass a, FirstClassList b)=>FirstClassList (a, (b, ()))
    --instance (CIntegral t ~ isint, I ntegerLiteral isint l)=>Literal t l

    --class IntegerLiteral (isint::Bool) l | isint ->l
    --instance IntegerLiteral True Integer
    instance Literal Boolean Bool 
    instance Literal Int8 HW.Word8
    instance Literal UInt8 HW.Word8
    instance Literal Int16 HW.Word16
    instance Literal UInt16 HW.Word16
    instance Literal Int32 HW.Word32
    instance Literal UInt32 HW.Word32
    instance Literal Int64 HW.Word64
    instance Literal UInt64 HW.Word64

   
    

    --data UnaryOp = Invert | Not
    --data BinaryOp = Add | Sub | Mul | Div | Mod | Xor | Or | And | CEQ | CLT | CGT | CLE | CGE | CNE

    -- Declaring operators, both unary and binary.
    $(do
        proxy<-[e|Proxy|]
        return $ do
            op<-["Invert", "Not", "Add", "Sub", "Mul", "Div", "Mod", "Xor", "Or", "And", "CEQ", "CLT", "CGT", "CLE", "CGE", "CNE", "LShift", "RShift", "LShiftA", "RShiftA"]
            let name=mkName op
            let name2=mkName ("op"++op)
            -- data T
            -- opT = Proxy :: Proxy T
            [DataD [] name [] Nothing [] [], ValD (VarP name2) (NormalB (SigE proxy (AppT (ConT ''Proxy) (ConT name)))) []])
    -- Unary operators.
    type family UnaryFun op t

    -- Boolean
    type instance UnaryFun Not Boolean=Boolean
    type instance UnaryFun Invert Boolean=Boolean
    
    -- Bits
    $(return $ do
        op<-[''Invert]
        inttype<-[''Int8, ''UInt8, ''Int16, ''UInt16, ''Int32, ''UInt32, ''Int64, ''UInt64]
        return $ typeInstance ''UnaryFun [op, inttype] inttype)

    -- Binary operators.
    type family BinaryFun op t1 t2
    -- Cartesian product using list.
    -- Arithmetic operations
    $(return $ do
        op<-[''Add, ''Sub, ''Mul, ''Div, ''Mod, ''Xor, ''Or, ''And]
        inttype<-[''Int8, ''UInt8, ''Int16, ''UInt16, ''Int32, ''UInt32, ''Int64, ''UInt64]
        return $ typeInstance ''BinaryFun [op, inttype, inttype] inttype)
    -- Shifting operations. We limit shifting operand to UInt8.
    $(return $ do
        op<-[''LShift, ''RShift, ''LShiftA, ''RShiftA]
        inttype<-[''Int8, ''UInt8, ''Int16, ''UInt16, ''Int32, ''UInt32, ''Int64, ''UInt64]
        return $ typeInstance ''BinaryFun [op, inttype, ''UInt8] inttype)
    -- Logic operations.
    $(return $ do
        op<-[''Xor, ''Or, ''And]
        return $ typeInstance ''BinaryFun [op, ''Boolean, ''Boolean] ''Boolean)
    -- Compare operations.
    $(return $ do
        op<-[''CEQ, ''CLT, ''CGT, ''CLE, ''CGE, ''CNE]
        inttype<-[''Int8, ''UInt8, ''Int16, ''UInt16, ''Int32, ''UInt32, ''Int64, ''UInt64]
        return $ typeInstance ''BinaryFun [op, inttype, inttype] ''Boolean)

    data C4mFn a b
    --type CPS f a b=(f a->b)->f b
    class MonadFix m=>C4mAST m where
        imm :: (Literal a l)=>l->m a
        unary :: (UnaryFun op b ~ a)=>Proxy op->b->m a
        binary :: (BinaryFun op t1 t2 ~ a)=>Proxy op->t1->t2->m a
        defun :: (FirstClass b, FirstClassList a)=>String->(a->m b)->m (Fn a b)
        invoke :: (FirstClassList a)=>(Fn a b)->a->m b
        if' :: Boolean->m a->m a->m a
        --Let :: C4mAST b->(C4mAST b->a)->C4mAST a


        --UpCast :: (UpCast b a)=>BasicExpr b->BasicExpr a
        --DownCast :: (DownCast b a)=>BasicExpr b->BasicExpr a
        --Tuple :: BasicExpr a->BasicExpr b->BasicExpr (a,b)
        --Defun :: String->(BasicExpr a->BasicExpr b)->BasicExpr (a->b)
        --Invoke :: BasicExpr (a->b)->BasicExpr a->BasicExpr b
    --expr=Let $ Binary And (immBool False) (immBool True)

    immInt8 :: (C4mAST m)=>Integer->m Int8
    immInt8=imm . fromIntegral

    immUInt8 :: (C4mAST m)=>Integer->m UInt8
    immUInt8=imm . fromIntegral


    immBool :: (C4mAST m)=>Bool->m Boolean
    immBool=imm

    expr :: (C4mAST m)=>m Int8
    expr=mdo
        a<-immInt8 10
        b<-immInt8 20
        sum<-(defun "sum" $ \(x::Int8, ())->do
                    zero<-immInt8 0
                    one<-immInt8 1
                    cmp<-binary opCEQ x zero
                    sup<-binary opSub x one
                    result<- if' cmp (return zero) (invoke sum (sup, ()))
                    ret<-binary opAdd result x
                    return ret)
        binary opAdd a b
        
        


    