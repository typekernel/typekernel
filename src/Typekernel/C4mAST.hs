{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TemplateHaskell, UndecidableInstances, DeriveFunctor, ScopedTypeVariables, RecursiveDo #-}
-- C---- AST.
-- C---- is a chosen subset from C that works as code generation target.
-- The ideal result should be something that has TAC-style assignments and structured control flow.
module Typekernel.C4mAST where
    import Data.Proxy
    import qualified Data.Word as HW
    import Typekernel.TH
    import Language.Haskell.TH
    import Data.Monoid
    import Control.Monad.Reader
    import Control.Monad
    import Typekernel.Nat
    type Metadata=String
    $(return $ do
        t<-["Int8", "UInt8", "Int16", "UInt16", "Int32", "UInt32", "Int64", "UInt64", "Boolean"]
        let name=mkName t
        return $ DataD [] name [] Nothing [NormalC name [(Bang NoSourceUnpackedness NoSourceStrictness,ConT ''Metadata)]] [DerivClause Nothing [ConT ''Show]])
    
    data Fn a b=Fn Metadata deriving Show
    data Ptr a=Ptr Metadata deriving Show
    
    data Arr (n::Nat) a=Arr Metadata deriving Show

    arraySize :: Arr n a->Proxy n
    arraySize _=Proxy

    fnProxy :: (a->m b)->(Proxy a, Proxy b)
    fnProxy _=(Proxy, Proxy)
    fnProxyVal :: (Fn a b)->(Proxy a, Proxy b)
    fnProxyVal _=(Proxy, Proxy)
    fnPtrProxyVal :: (Ptr (Fn a b))->(Proxy a, Proxy b)
    fnPtrProxyVal _=(Proxy, Proxy)
    class Literal t l | t->l where
        rawtype :: Proxy (t, l)->String
        rawvalue :: Proxy (t, l)->l->String
        wrapliteral :: Proxy (t, l)->Metadata->t
        
    -- For GHC inferring l from t.
    literalProxy :: (Literal t l)=>Proxy t->Proxy (t, l)
    literalProxy _=Proxy
    class FirstClass a where
        ctype :: Proxy a->String
        metadata :: a->String
        wrap :: Proxy a->String->a
    class FirstClassList a where
        listctype :: Proxy a->[String]
        listmetadata :: a->[String]
        wraplist :: Proxy a->[String]->a


    instance FirstClass Boolean where {ctype _="int8_t"; metadata (Boolean m)=m; wrap _=Boolean}
    instance FirstClass Int8 where {ctype _="int8_t"; metadata (Int8 m)=m; wrap _=Int8}
    instance FirstClass Int16 where {ctype _="int16_t"; metadata (Int16 m)=m; wrap _=Int16}
    instance FirstClass Int32 where {ctype _="int32_t"; metadata (Int32 m)=m; wrap _=Int32}
    instance FirstClass Int64 where {ctype _="int64_t"; metadata (Int64 m)=m; wrap _=Int64}
    instance FirstClass UInt8 where {ctype _="uint8_t"; metadata (UInt8 m)=m; wrap _=UInt8}
    instance FirstClass UInt16 where {ctype _="uint16_t"; metadata (UInt16 m)=m; wrap _=UInt16}
    instance FirstClass UInt32 where {ctype _="uint32_t"; metadata (UInt32 m)=m; wrap _=UInt32}
    instance FirstClass UInt64 where {ctype _="uint64_t"; metadata (UInt64 m)=m; wrap _=UInt64}

    instance (FirstClass a)=>FirstClass (Ptr a) where {ctype _=(ctype (Proxy:: Proxy a))++"*"; metadata (Ptr m)=m; wrap _=Ptr}


    data Void=Void
    instance FirstClassList Void where
        listctype _=[]
        listmetadata _=[]
        wraplist _ []=Void

    instance (FirstClass a, FirstClassList b)=>FirstClassList (a,b) where
        listctype _=(ctype (Proxy::Proxy a)):(listctype (Proxy::Proxy b))
        listmetadata (a, b)=(metadata a): (listmetadata b)
        wraplist _ (x:xs)=((wrap (Proxy::Proxy a) x), wraplist (Proxy::Proxy b) xs)
    --instance (CIntegral t ~ isint, I ntegerLiteral isint l)=>Literal t l

    --class IntegerLiteral (isint::Bool) l | isint ->l
    --instance IntegerLiteral True Integer
    instance Literal Boolean Bool where
        rawtype _=ctype (Proxy :: Proxy Boolean)
        rawvalue _ v=if v then "1" else "0"
        wrapliteral _=Boolean
    instance Literal Int8 HW.Word8 where
        rawtype _=ctype (Proxy :: Proxy Int8)
        rawvalue _ =show
        wrapliteral _=Int8
    instance Literal UInt8 HW.Word8 where
        rawtype _=ctype (Proxy :: Proxy UInt8)
        rawvalue _ =show
        wrapliteral _=UInt8
    instance Literal Int16 HW.Word16 where
        rawtype _=ctype (Proxy :: Proxy Int16)
        rawvalue _ =show
        wrapliteral _=Int16
    instance Literal UInt16 HW.Word16 where
        rawtype _=ctype (Proxy :: Proxy UInt16)
        rawvalue _ =show
        wrapliteral _=UInt16
    instance Literal Int32 HW.Word32 where
        rawtype _=ctype (Proxy :: Proxy Int32)
        rawvalue _ =show
        wrapliteral _=Int32
    instance Literal UInt32 HW.Word32 where
        rawtype _=ctype (Proxy :: Proxy UInt32)
        rawvalue _ =show
        wrapliteral _=UInt32
    instance Literal Int64 HW.Word64 where
        rawtype _=ctype (Proxy :: Proxy Int64)
        rawvalue _ =show
        wrapliteral _=Int64
    instance Literal UInt64 HW.Word64 where
        rawtype _=ctype (Proxy :: Proxy UInt64)
        rawvalue _ =show
        wrapliteral _=UInt64

   
    

    --data UnaryOp = Invert | Not
    --data BinaryOp = Add | Sub | Mul | Div | Mod | Xor | Or | And | CEQ | CLT | CGT | CLE | CGE | CNE
    class COperator t where coperator :: Proxy t->String
    -- Declaring operators, both unary and binary.
    $(do
        proxy<-[e|Proxy|]
        return $ do
            (op, sym)<-[("Invert", "~"),
                ("Neg", "-"),
                ("Not", "!"), 
                ("Add", "+"), 
                ("Sub", "-"), 
                ("Mul", "*"), 
                ("Div", "/"), 
                ("Mod", "%"), 
                ("Xor", "^"), 
                ("Or", "|"), 
                ("And", "&"), 
                ("CEQ", "=="), 
                ("CLT", "<"), 
                ("CGT", ">"),
                ("CLE", "<="),
                ("CGE", ">="),
                ("CNE", "!="),
                ("LShift", "<<"),
                ("RShift", ">>")]
            let name=mkName op
            let name2=mkName ("op"++op)
            let name3=mkName "coperator"
            -- data T
            -- opT = Proxy :: Proxy T
            [DataD [] name [] Nothing [] [], ValD (VarP name2) (NormalB (SigE proxy (AppT (ConT ''Proxy) (ConT name)))) [],
                InstanceD Nothing [] (AppT (ConT ''COperator) (ConT name)) [FunD name3 [Clause [WildP] (NormalB (LitE (StringL sym))) []]]])
            
    
    -- Unary operators.
    type family UnaryFun op t

    class (COperator op, FirstClass a, FirstClass b)=>Unary op a b | op a->b
    class (COperator op, FirstClass a, FirstClass b, FirstClass c)=>Binary op a b c | op a b->c
    unaryProxy :: (Unary op a b)=>Proxy op->Proxy a->Proxy b
    unaryProxy _ _ =Proxy
    binaryProxy :: (Binary op a b c)=>Proxy op->Proxy a->Proxy b->Proxy c
    binaryProxy _ _ _=Proxy
    -- Boolean
    type instance UnaryFun Not Boolean=Boolean
    instance Unary Not Boolean Boolean
    type instance UnaryFun Invert Boolean=Boolean
    instance Unary Invert Boolean Boolean
    -- Bits
    $(return $ do
        op<-[''Invert, ''Neg]
        inttype<-[''Int8, ''UInt8, ''Int16, ''UInt16, ''Int32, ''UInt32, ''Int64, ''UInt64]
        [typeInstance ''UnaryFun [op, inttype] inttype, classInstance [''Unary, op, inttype, inttype]])

    -- Binary operators.
    type family BinaryFun op t1 t2
    -- Cartesian product using list.
    -- Arithmetic operations
    $(return $ do
        op<-[''Add, ''Sub, ''Mul, ''Div, ''Mod, ''Xor, ''Or, ''And]
        inttype<-[''Int8, ''UInt8, ''Int16, ''UInt16, ''Int32, ''UInt32, ''Int64, ''UInt64]
        [typeInstance ''BinaryFun [op, inttype, inttype] inttype, classInstance [''Binary, op, inttype, inttype, inttype]])
    -- Shifting operations. We limit shifting operand to UInt8.
    $(return $ do
        op<-[''LShift, ''RShift]
        inttype<-[''Int8, ''UInt8, ''Int16, ''UInt16, ''Int32, ''UInt32, ''Int64, ''UInt64]
        [typeInstance ''BinaryFun [op, inttype, ''UInt8] inttype, classInstance [''Binary, op, inttype, ''UInt8, inttype]])
    -- Logic operations.
    $(return $ do
        op<-[''Xor, ''Or, ''And]
        let inttype=''Boolean
        [typeInstance ''BinaryFun [op, inttype, inttype] inttype, classInstance [''Binary, op, inttype, inttype, inttype]])
    -- Compare operations.
    $(return $ do
        op<-[''CEQ, ''CLT, ''CGT, ''CLE, ''CGE, ''CNE]
        inttype<-[''Int8, ''UInt8, ''Int16, ''UInt16, ''Int32, ''UInt32, ''Int64, ''UInt64]
        [typeInstance ''BinaryFun [op, inttype, inttype] ''Boolean, classInstance [''Binary, op, inttype, inttype, ''Boolean]])

    data C4mFn a b
    --type CPS f a b=(f a->b)->f b

    class Castable a b
    
    -- Integer casting
    $(return $ do
        let integers=[''Int8, ''UInt8, ''Int16, ''UInt16, ''Int32, ''UInt32, ''Int64, ''UInt64]
        t1<-integers
        t2<-integers
        [classInstance [''Castable, t1, t2]])

    -- Pointer casting back and forth
    instance Castable UInt64 (Ptr a)
    instance Castable (Ptr a) UInt64
    instance Castable (Ptr a) (Ptr b)

    data Memory (n::Nat)=Memory {memStart :: Ptr USize} deriving Show
    class MonadFix m=>C4mAST m where
        imm :: (Literal a l)=>l->m a
        unary :: (Unary op b a)=>Proxy op->b->m a
        binary :: (Binary op t1 t2 a)=>Proxy op->t1->t2->m a 
        defun :: (FirstClass b, FirstClassList a)=>(a->m b)->m (Fn a b)
        invoke :: (FirstClassList a, FirstClass b)=>(Fn a b)->a->m b
        -- We don't support taking function pointer out. However, invoking pointers is necessary.
        invokep :: (FirstClassList a, FirstClass b)=>(Ptr (Fn a b))->a->m b
        if' :: (FirstClassList a)=>Boolean->m a->m a->m a
        cast :: (Castable a b, FirstClass a, FirstClass b)=>Proxy b->a->m b
        defarr :: (KnownNat n)=>Proxy n->m (Memory n)
        assign :: (KnownNat n)=>Memory n->Memory n->m ()
        deref :: (FirstClass a)=>Ptr a->m a
        mref :: (FirstClass a)=>Ptr a->a->m ()
        --Let :: C4mAST b->(C4mAST b->a)->C4mAST a


        --UpCast :: (UpCast b a)=>BasicExpr b->BasicExpr a
        --DownCast :: (DownCast b a)=>BasicExpr b->BasicExpr a
        --Tuple :: BasicExpr a->BasicExpr b->BasicExpr (a,b)
        --Defun :: String->(BasicExpr a->BasicExpr b)->BasicExpr (a->b)
        --Invoke :: BasicExpr (a->b)->BasicExpr a->BasicExpr b
    --expr=Let $ Binary And (immBool False) (immBool True)

    

    
    type USize=UInt64
    type Size=Int64