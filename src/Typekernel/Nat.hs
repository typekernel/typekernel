{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Typekernel.Nat where
    import Typekernel.TH
    import Data.Proxy

    data Nat=Z | S Nat
    
    -- may need -freduction-depth=0 to use deeper depth.
    $(return $ generateNat 1024 'Z 'S)
    

    
    class KnownNat n where
        natToInt :: Proxy n->Int
    instance KnownNat Z where
        natToInt _=0
    instance KnownNat n=>KnownNat (S n) where
        natToInt _=1+natToInt (Proxy :: Proxy n)

    -- Peano algebras

    class PeanoEqual a b t | a b->t
    instance PeanoEqual Z Z True
    instance PeanoEqual (S a) Z False
    instance PeanoEqual Z (S a) False
    instance (PeanoEqual a b t)=>PeanoEqual (S a) (S b) t
    
    class PeanoLT a b t | a b ->t
    instance PeanoLT Z Z False
    instance PeanoLT (S x) Z False
    instance PeanoLT Z (S x) True
    instance (PeanoLT a b t)=>PeanoLT (S a) (S b) t
    
    class PeanoAbsDiff a b c | a b ->c
    instance PeanoAbsDiff Z Z Z
    instance PeanoAbsDiff Z (S b) (S b)
    instance PeanoAbsDiff (S a) Z (S a)
    instance (PeanoAbsDiff a b c)=>PeanoAbsDiff (S a) (S b) c

    class PeanoAdd a b t | a b->t
    instance PeanoAdd Z b b
    instance (PeanoAdd a b t)=>(PeanoAdd (S a) b (S t))


    -- Shorcuts for memory size calculation.

    class PeanoUpRound8 a t | a->t
    instance PeanoUpRound8 Z N0
    instance PeanoUpRound8 N1 N1
    instance PeanoUpRound8 N2 N1
    instance PeanoUpRound8 N3 N1
    instance PeanoUpRound8 N4 N1
    instance PeanoUpRound8 N5 N1
    instance PeanoUpRound8 N6 N1
    instance PeanoUpRound8 N7 N1
    instance (PeanoUpRound8 a t)=>PeanoUpRound8 (S(S(S(S(S(S(S(S a)))))))) (S t)

    class PeanoMod8 a t | a->t
    instance PeanoMod8 Z N0
    instance PeanoMod8 N1 N1
    instance PeanoMod8 N2 N2
    instance PeanoMod8 N3 N3
    instance PeanoMod8 N4 N4
    instance PeanoMod8 N5 N5
    instance PeanoMod8 N6 N6
    instance PeanoMod8 N7 N7
    instance (PeanoMod8 a t)=>PeanoMod8 (S(S(S(S(S(S(S(S a)))))))) t

    class PeanoMod4 a t | a->t
    instance PeanoMod4 Z N0
    instance PeanoMod4 N1 N1
    instance PeanoMod4 N2 N2
    instance PeanoMod4 N3 N3
    instance (PeanoMod4 a t)=>PeanoMod4 (S(S(S(S a)))) t

    class PeanoMod2 a t | a->t
    instance PeanoMod2 Z N0
    instance PeanoMod2 N1 N1
    instance (PeanoMod2 a t)=>PeanoMod2 (S(S a)) t

    type family NAdd (a::Nat) (b::Nat) :: Nat where
        NAdd Z b=b
        NAdd (S a) b=S (NAdd a b)

    type family NMul (a::Nat) (b::Nat) :: Nat where
        NMul a Z=Z
        NMul a (S b)=NAdd (NMul a b) a
    type family NUpRound8 (a::Nat) :: Nat where
        NUpRound8 Z=N8
        NUpRound8 N1=N8
        NUpRound8 N2=N8
        NUpRound8 N3=N8
        NUpRound8 N4=N8
        NUpRound8 N5=N8
        NUpRound8 N6=N8
        NUpRound8 N7=N8
        NUpRound8 N8=N8
        NUpRound8 (S(S(S(S(S(S(S(S a)))))))) = NUpRound8 (S a)

    type family NMax (a::Nat) (b::Nat) :: Nat where
        NMax Z b = b
        NMax b Z = b
        NMax (S a) (S b)=S (NMax a b)