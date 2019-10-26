{-# LANGUAGE FunctionalDependencies, DataKinds, FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies, GADTs, FlexibleContexts, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Typekernel.SumType where
    import Typekernel.Structure
    import Typekernel.Nat
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Typekernel.Memory
    import Data.Proxy
    type family SumSize a b :: Nat where
        SumSize a () = NAdd (SizeOf a) N8
        SumSize a b = NMax (S (SizeOf a)) (SizeOf b)
    class SumType a b (m::Nat) (n::Nat) | a->m, b->n where
        data Sum' a b
        matchS :: (FirstClassList r, MonadC env)=>(a->env r)->(b->env r)->env r

    type instance SizeOf (Sum' a b) = SumSize a b

    suma :: (SumType a b m n)=>Sum' a b->Proxy a
    sumb :: (SumType a b m n)=>Sum' a b->Proxy b
    suma _=Proxy
    sumb _=Proxy
    summ :: (SumType a b m n)=>Sum' a b->Proxy (m::Nat)
    summ _=Proxy
    sumn :: (SumType a b m n)=>Sum' a b->Proxy (n::Nat)
    sumn _=Proxy
    instance (KnownNat m, Structure m a)=>SumType a () m Z where
        data Sum' a ()=Sum' {sumMem :: Memory (SizeOf (Sum' a ()))}

    type family Sum a b where
        Sum a (Sum' b c)=Sum' a (Sum' b c)
        Sum a b=Sum' a (Sum' b ())
    --instance (KnownNat m, KnownNat n, Structure q c, SumType a b m n)=>SumType 