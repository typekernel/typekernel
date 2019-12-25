{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, TypeOperators, TypeInType, ConstraintKinds #-}
module Typekernel.Nat where
    import Typekernel.TH
    import Data.Proxy
    import qualified GHC.TypeLits as N

    type KnownNat=N.KnownNat
    -- Migrate to fast GHC natural number.
    type Nat=N.Nat
    --data Nat=Z | S Nat
    $(return $ generateNat 4096)

    type Z=0
    type family S (x::Nat) :: Nat where
        S x = (N.+) x 1
    --class KnownNat n where
    --    natToInt :: Proxy n->Int
    --instance (N.KnownNat n)=>KnownNat n where
    --    natToInt =fromIntegral . N.natVal
    
    natToInt :: (KnownNat n)=>Proxy n->Int
    natToInt = fromIntegral . N.natVal

    extractNat :: a n->Proxy n
    extractNat _ = Proxy
    -- Peano algebras

    type family NEq (ord::Ordering) :: Bool where
        NEq EQ = True
        NEq _ = False
    class PeanoEqual a b t | a b->t
    instance (N.CmpNat a b ~ x, NEq x ~ y)=>PeanoEqual a b y

    type family NLt (ord::Ordering) :: Bool where
        NLt LT = True
        NLt _ = False
    
    class PeanoLT a b t | a b ->t
    instance (N.CmpNat a b ~ x, NLt x ~ y)=>PeanoLT a b y
    
    
    class PeanoAdd a b t | a b->t
    instance ( ((N.+) a b) ~ t )=>PeanoAdd a b t


    -- Shorcuts for memory size calculation.

    class PeanoUpRound8 a t | a->t
    instance ((N.Div ((N.+) a 7) 8) ~ t)=>PeanoUpRound8 a t
    

    class PeanoMod8 a t | a->t
    instance (N.Mod a 8 ~ t)=>PeanoMod8 a t

    class PeanoMod4 a t | a->t
    instance (N.Mod a 4 ~ t)=>PeanoMod4 a t

    class PeanoMod2 a t | a->t
    instance (N.Mod a 2 ~ t)=>PeanoMod2 a t

    type family NAdd (a::Nat) (b::Nat) :: Nat where
        NAdd a b = (N.+) a b

    type family NMul (a::Nat) (b::Nat) :: Nat where
        NMul a b = (N.*) a b
    type family NUpRound8 (a::Nat) :: Nat where
        NUpRound8 0=8
        NUpRound8 a = (N.*) (N.Div ((N.+) a 7) 8) 8
        

    type family NCeil8 (a::Nat) :: Nat where
        NCeil8 a=(N.Div ((N.+) a 7) 8)
        
    type family NMax' (a::Nat) (b::Nat) (r::Ordering) :: Nat where
        NMax' a b LT=b
        NMax' a b _ = a
    type family NMax (a::Nat) (b::Nat) :: Nat where
        NMax a b=NMax' a b (N.CmpNat a b)


    type family NDec (a::Nat) :: Nat where
        NDec 0 = 0
        NDec a = (N.-) a 1
    class PeanoDec a t | a->t
        
    instance (NDec a ~ t)=>PeanoDec a t 
    
    decN :: (PeanoDec a t)=>Proxy a->Proxy t
    decN _ = Proxy