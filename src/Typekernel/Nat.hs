{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
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