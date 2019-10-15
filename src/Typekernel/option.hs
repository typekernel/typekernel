{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, DataKinds, TemplateHaskell, UndecidableInstances, DeriveFunctor, ScopedTypeVariables, RecursiveDo #-}
import Data.Maybe
import Data.Proxy
newtype Ptr = Ptr {toInt :: Integer}
data Nat=Z|S Nat

class RNat (s::Nat) where
    vnat:: Proxy s->Int

instance RNat Z where vnat _=0
instance (RNat s)=>RNat (S s) where
    vnat _ = 1+(vnat (Proxy::Proxy s))

--type family Sized a :: Nat
data UInt8
--type instance Sized UInt8=S Z
type family Sized a :: Nat
class Option' a where
    data family Option a
    --isSome :: Option a->Bool

data Pack a

data Memory (s::Nat)=Memory

instance (Sized a ~ s)=>Option' (Pack a) where
    data Option (Pack a)=
        OptionPack (Memory (Sized (Option (Pack a))))

type instance Sized (Option (Pack a)) =(S (Sized a))

data UInt64

type N0=Z
type N1=S Z
type N2=S N1
type N3=S N2
type N4=S N3
type N5=S N4
type N6=S N5
type N7=S N6
type N8=S N7
type N9=S N8
type instance Sized UInt64 = N8

expr :: Option (Pack UInt64)
expr=OptionPack (Memory :: Memory (N9))