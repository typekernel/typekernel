{-# LANGUAGE FunctionalDependencies, DataKinds, FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies, GADTs, FlexibleContexts, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, AllowAmbiguousTypes, ScopedTypeVariables #-}
module Typekernel.SumType where
    import Typekernel.Structure
    import Typekernel.Nat
    import Typekernel.C4mAST
    import Typekernel.Transpiler
    import Typekernel.Memory
    import Data.Proxy
    import Typekernel.RAII
    import Control.Monad.Trans.Class
    type family SumSize a b :: Nat where
        SumSize a () = NAdd (SizeOf a) N8
        SumSize a b = NMax (S (SizeOf a)) (SizeOf b)
    class SumType a b (m::Nat) (n::Nat) | a->m, b->n where
        data Sum' a b
        matchS :: (FirstClassList r, MonadC env)=>Sum' a b->(a->(forall s. RAII s env r))->(b->(forall s. RAII s env r))->env r

    type instance SizeOf (Sum' a b) = SumSize a b

    suma :: (SumType a b m n)=>Sum' a b->Proxy a
    sumb :: (SumType a b m n)=>Sum' a b->Proxy b
    suma _=Proxy
    sumb _=Proxy
    -- Left element size
    summ :: (SumType a b m n)=>Sum' a b->Proxy (m::Nat)
    summ _=Proxy
    -- Left index
    sumn :: (SumType a b m n)=>Sum' a b->Proxy (n::Nat)
    sumn _=Proxy
    instance (KnownNat m, Structure m a)=>SumType a () m Z where
        data Sum' a ()=Sum' {sumMem :: Memory (SizeOf (Sum' a ()))}
        --matchS fa fb = do
    instance (KnownNat m, Structure m c, KnownNat ms, SumType a b ms idx)=>SumType c (Sum' a b) m (S idx) where
        data Sum' c (Sum' a b)=Sum'' {sumMem2 :: Memory (SizeOf (Sum' c (Sum' a b)))}
        --matchS fa fb = do

    type family Sum a b where
        Sum a (Sum' b c)=Sum' a (Sum' b c)
        Sum a b=Sum' a (Sum' b ())
    --instance (KnownNat m, KnownNat n, Structure q c, SumType a b m n)=>SumType 

    --match3 :: (FirstClassList r, MonadC m)=>(Sum' a (Sum' b c))->(a->(forall s. RAII s m r))->(b->(forall s. RAII s m r))->(c->(forall s. RAII s m r))->m r
    --match3 abc fa fb fc=matchS abc fa (\bc->lift $ matchS bc fb fc)

    match3 abc (fa::a->(forall s. RAII s m r)) (fb::b->(forall s. RAII s m r)) (fc:: c->(forall s. RAII s m r))=matchS abc fa (\bc->lift $ matchS bc fb fc)

    {-
    class SumtypeMatch s l r  where
        match :: s->l->r

    type ScopeF a env r=a->(forall s. (RAII s env r))
    instance (KnownNat m1, Structure m1 a, FirstClassList r, MonadC env, SumType a () m1 i1)=>SumtypeMatch (Sum' a ()) (ScopeF a env r) (env r) where
        match s fa=matchS s fa undefined -- and hope that the second argument will never be used.

    instance (FirstClassList r, MonadC env, SumtypeMatch s l r, SumType a s m0 i0, SumtypeMatch )=>SumtypeMatch (Sum' a s)
    -}  